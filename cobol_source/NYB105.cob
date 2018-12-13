       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB105R.
      **********************************************  Z-WIN-RPG2   ****
      *  UTPLUKK FRA BESTILLINGS-ARKIVET                             *
      *  UPSI-0    FIRMANR I TABELL LEGGES IKKE UT                   *
      *  UPSI-1    FIRMANR I TABELL LEGGES UT                        *
      *  AVD. SATT TIL 1 25.05.94 STEIN                              *
      ****************************************************************
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB105.rpg
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
           SELECT KORTTAB
               ASSIGN TO UT-S-KORTTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTTAB-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT NYEBEST
               ASSIGN TO NYEBEST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS NYEBEST-STATUS
               RECORD KEY IS NYEBEST-KEY1.
           SELECT BEST
               ASSIGN TO UT-S-BEST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BEST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORTTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KORTTAB-IO-AREA.
           05  KORTTAB-IO-AREA-X           PICTURE X(80).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD NYEBEST
               RECORD CONTAINS 150.
       01  NYEBEST-IO-AREA.
           05  NYEBEST-IO-AREA-X.
               10  NYEBEST-KEY1.
                   15  NYEBEST-KEY1N       PICTURE S9(16).
               10  FILLER                  PICTURE X(134).
       FD BEST
               BLOCK CONTAINS 4096
               RECORD CONTAINS 128.
       01  BEST-IO-AREA.
           05  BEST-IO-AREA-X              PICTURE X(128).
       WORKING-STORAGE SECTION.
       77  TABFNR-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFNR-TABLE.
               10  TABFNR-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY TABFNR-I
                                                      TABFNR-S.
                   15  TABFNR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KORTTAB-STATUS              PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  NYEBEST-STATUS              PICTURE 99 VALUE 0.
           10  BEST-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTTAB-EOF-OFF         VALUE '0'.
               88  KORTTAB-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  NYEBEST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-EOF-OFF         VALUE '0'.
               88  NYEBEST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-READ-OFF        VALUE '0'.
               88  NYEBEST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-PROCESS-OFF     VALUE '0'.
               88  NYEBEST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  NYEBEST-LEVEL-INIT-OFF  VALUE '0'.
               88  NYEBEST-LEVEL-INIT      VALUE '1'.
           05  PARAM-DATA-FIELDS.
               10  PUKE                    PICTURE X(2).
           05  NYEBEST-LEVEL-02.
               10  NYEBEST-02-L3.
                   15  NYEBEST-02-L3-FIRMA PICTURE X(3).
               10  NYEBEST-02-L2.
                   15  NYEBEST-02-L2-BESNR PICTURE X(5).
               10  NYEBEST-02-L1.
                   15  NYEBEST-02-L1-POSNR PICTURE X(4).
           05  NYEBEST-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  BESNR                   PICTURE X(5).
               10  POSNR                   PICTURE X(4).
               10  BEST1                   PICTURE X(1).
      *                                      17  17 AVD
               10  LAAR                    PICTURE X(2).
               10  LUKE                    PICTURE X(2).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VAREN                   PICTURE X(30).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALUT                   PICTURE X(1).
               10  RABA-IO.
                   15  RABA                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RABB-IO.
                   15  RABB                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  BANT-IO.
                   15  BANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TANT-IO.
                   15  TANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEST-XX                 PICTURE X(2).
               10  BESDAT-IO.
                   15  BESDAT              PICTURE S9(6).
               10  LEVDAT-IO.
                   15  LEVDAT              PICTURE S9(6).
               10  BEKR                    PICTURE X(1).
      *                                     119 123 VGR
               10  TEKST                   PICTURE X(1).
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  SLETT                   PICTURE X(1).
               10  RECART                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(5).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  LEV                     PICTURE X(6).
               10  BS                      PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  UKE                     PICTURE X(2).
               10  BKREFT                  PICTURE X(1).
               10  EDB-IO.
                   15  EDB                 PICTURE S9(7).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  LEVNR-N-IO.
                   15  LEVNR-N             PICTURE S9(7).
               10  ALF                     PICTURE X(3).
               10  ART                     PICTURE X(20).
               10  VARE                    PICTURE X(30).
               10  PRI-IO.
                   15  PRI                 PICTURE S9(6)V9(2).
               10  PRIS-N-IO.
                   15  PRIS-N              PICTURE S9(7)V9(2).
               10  VAL                     PICTURE X(1).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  BDATO                   PICTURE X(6).
               10  LDATO                   PICTURE X(6).
               10  ANTB-IO.
                   15  ANTB                PICTURE S9(7).
               10  ANTT-IO.
                   15  ANTT                PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  XO-62P-EF.
                 15  XO-62P                PICTURE S9(6)V9(2) USAGE
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  NYEBEST-PROCESS
               SET NYEBEST-PROCESS-OFF     TO TRUE
               SET NYEBEST-READ            TO TRUE
           END-IF
 
           IF  NYEBEST-READ
           AND RECORD-SELECTED-OFF
               PERFORM NYEBEST-GET
               SET NYEBEST-READ-OFF        TO TRUE
               IF  NOT NYEBEST-EOF
                   SET NYEBEST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-IDSET
           END-IF
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  NYEBEST-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SET NOT-I-33                TO TRUE
               MOVE '      '               TO LEV
               MOVE '  '                   TO BS
               MOVE '  '                   TO AAR
               MOVE '  '                   TO UKE
      *  L2                MOVE " "       BAVD    1
           END-IF
           IF  (I-L2)
               MOVE ' '                    TO BKREFT
               SET NOT-I-66                TO TRUE
               IF  SLETT = 'S'
                   SET I-66                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-66)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-25                TO TRUE
      *
           END-IF
           IF  (I-L1 AND I-U1)
               SET NOT-I-25                TO TRUE
               SET TABFNR-S                TO TABFNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFNR-I FROM 1 BY 1
                         UNTIL TABFNR-I >= TABFNR-MAX
                            OR I-25
                   IF  FIRMA = TABFNR (TABFNR-I)
                       SET I-25            TO TRUE
                       SET TABFNR-S        TO TABFNR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-L1 AND NOT-I-U1)
               SET NOT-I-24                TO TRUE
               SET TABFNR-S                TO TABFNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFNR-I FROM 1 BY 1
                         UNTIL TABFNR-I >= TABFNR-MAX
                            OR I-24
                   IF  FIRMA = TABFNR (TABFNR-I)
                       SET I-24            TO TRUE
                       SET TABFNR-S        TO TABFNR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-L1 AND NOT-I-U1 AND NOT-I-24)
               SET I-25                    TO TRUE
      *
           END-IF
           IF  (I-02)
               SET NOT-I-30                TO TRUE
               IF  BEST1 = '9'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  RECART = '1'
               SET I-30                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  RECART = '2'
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           SET NOT-I-32                    TO TRUE
           IF  RECART > '3'
               SET I-33                    TO TRUE
           END-IF
           IF  RECART = '3'
               SET I-32                    TO TRUE
           END-IF
      *
           IF  (I-31)
               GO TO SLUTT-T
           END-IF
           IF  (I-32)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-30)
               MOVE BEST-XX                TO BS
               MOVE BEKR                   TO BKREFT
      *
           END-IF
           IF  (I-33)
               SET NOT-I-33                TO TRUE
               IF  TEKST NOT = 'T'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-33)
               SET NOT-I-33                TO TRUE
               IF  TEKST NOT = 'S'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-33)
               MOVE EDBNR                  TO EDBNR-N
               MOVE EDBNR-N-IO             TO EDB-IO
               MOVE LEVNR                  TO LEVNR-N
               MOVE LEVNR-N-IO (2:6)       TO LEV
               MOVE LAAR                   TO AAR
               MOVE LUKE                   TO UKE
      *  33                MOVE AVD       BAVD
           END-IF
           IF  (I-33)
               MOVE ALFA                   TO ALF
               MOVE ARTNR                  TO ART
               MOVE VAREN                  TO VARE
               MOVE PRIS                   TO PRIS-N
               MOVE PRIS-N-IO (2:8)        TO PRI-IO
               MOVE VALUT                  TO VAL
               ADD RABA TO ZERO        GIVING RAB1
               ADD RABB TO ZERO        GIVING RAB2
               MOVE BESDAT                 TO BDATO
               MOVE LEVDAT                 TO LDATO
               ADD BANT TO ZERO        GIVING ANTB
               ADD TANT TO ZERO        GIVING ANTT
               MOVE LEVDAT                 TO LDATO
      *
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARAM-IO-AREA (16:2)   TO PUKE (1:2)
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-01                        TO TRUE.
 
       NYEBEST-GET SECTION.
       NYEBEST-GET-P.
           IF  NYEBEST-EOF-OFF
               READ NYEBEST
               AT END
                   SET NYEBEST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYEBEST-FLDSET SECTION.
       NYEBEST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEBEST-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE NYEBEST-IO-AREA (5:5)  TO BESNR (1:5)
               MOVE NYEBEST-IO-AREA (11:4) TO POSNR (1:4)
               MOVE NYEBEST-IO-AREA (5:1)  TO BEST1 (1:1)
               MOVE NYEBEST-IO-AREA (18:2) TO LAAR (1:2)
               MOVE NYEBEST-IO-AREA (20:2) TO LUKE (1:2)
               MOVE NYEBEST-IO-AREA (22:3) TO ALFA (1:3)
               MOVE NYEBEST-IO-AREA (25:20) TO ARTNR (1:20)
               MOVE NYEBEST-IO-AREA (45:30) TO VAREN (1:30)
               MOVE NYEBEST-IO-AREA (75:4) TO EDBNR-IO
               MOVE NYEBEST-IO-AREA (79:5) TO PRIS-IO
               MOVE NYEBEST-IO-AREA (84:1) TO VALUT (1:1)
               MOVE NYEBEST-IO-AREA (85:2) TO RABA-IO
               MOVE NYEBEST-IO-AREA (87:2) TO RABB-IO
               MOVE NYEBEST-IO-AREA (89:5) TO BANT-IO
               MOVE NYEBEST-IO-AREA (94:5) TO TANT-IO
               MOVE NYEBEST-IO-AREA (97:2) TO BEST-XX (1:2)
               MOVE NYEBEST-IO-AREA (104:6) TO BESDAT-IO
               INSPECT BESDAT-IO REPLACING ALL ' ' BY '0'
               MOVE NYEBEST-IO-AREA (110:6) TO LEVDAT-IO
               INSPECT LEVDAT-IO REPLACING ALL ' ' BY '0'
               MOVE NYEBEST-IO-AREA (105:1) TO BEKR (1:1)
               MOVE NYEBEST-IO-AREA (124:1) TO TEKST (1:1)
               MOVE NYEBEST-IO-AREA (131:4) TO LEVNR-IO
               MOVE NYEBEST-IO-AREA (149:1) TO SLETT (1:1)
               MOVE NYEBEST-IO-AREA (150:1) TO RECART (1:1)
           END-EVALUATE.
 
       NYEBEST-IDSET SECTION.
       NYEBEST-IDSET-P.
           SET I-02                        TO TRUE.
 
       NYEBEST-CHK-LEVEL SECTION.
       NYEBEST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO NYEBEST-LEVEL-02
               MOVE NYEBEST-IO-AREA (2:3)  TO NYEBEST-02-L3-FIRMA
               MOVE NYEBEST-IO-AREA (5:5)  TO NYEBEST-02-L2-BESNR
               MOVE NYEBEST-IO-AREA (11:4) TO NYEBEST-02-L1-POSNR
               IF  NYEBEST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYEBEST-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  NYEBEST-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  NYEBEST-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYEBEST-02-L3         TO THE-PRIOR-L3
               MOVE  NYEBEST-02-L2         TO THE-PRIOR-L2
               MOVE  NYEBEST-02-L1         TO THE-PRIOR-L1
               SET NYEBEST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KORTTAB-LOAD SECTION.
       KORTTAB-LOAD-P.
           OPEN INPUT KORTTAB
           SET TABFNR-I                    TO 1
           PERFORM UNTIL KORTTAB-EOF
               READ KORTTAB
               AT END
                   SET KORTTAB-EOF         TO TRUE
               NOT AT END
                   MOVE KORTTAB-IO-AREA (1:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KORTTAB.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-25 AND I-33)
           AND (NOT-I-66)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE FIRMA                  TO BEST-IO-AREA (2:3)
               MOVE BESNR                  TO BEST-IO-AREA (5:5)
               MOVE POSNR                  TO BEST-IO-AREA (10:4)
      *                        BAVD      14
               MOVE '1'                    TO BEST-IO-AREA (14:1)
               MOVE LEV                    TO BEST-IO-AREA (15:6)
               MOVE BS                     TO BEST-IO-AREA (21:2)
               MOVE AAR                    TO BEST-IO-AREA (23:2)
               MOVE UKE                    TO BEST-IO-AREA (25:2)
               MOVE BDATO                  TO BEST-IO-AREA (27:6)
               INITIALIZE BDATO
               MOVE ALF                    TO BEST-IO-AREA (33:3)
               INITIALIZE ALF
               MOVE ART                    TO BEST-IO-AREA (37:20)
               INITIALIZE ART
               MOVE VARE                   TO BEST-IO-AREA (57:30)
               INITIALIZE VARE
               MOVE EDB                    TO XO-70P
               MOVE XO-70P-EF              TO BEST-IO-AREA (87:4)
               INITIALIZE EDB
               MOVE RAB1                   TO XO-21P
               MOVE XO-21P-EF              TO BEST-IO-AREA (91:2)
               INITIALIZE RAB1
               MOVE RAB2                   TO XO-21P
               MOVE XO-21P-EF              TO BEST-IO-AREA (93:2)
               INITIALIZE RAB2
               MOVE VAL                    TO BEST-IO-AREA (95:1)
               INITIALIZE VAL
               MOVE PRI                    TO XO-62P
               MOVE XO-62P-EF              TO BEST-IO-AREA (96:5)
               INITIALIZE PRI
               MOVE ANTB                   TO XO-70P
               MOVE XO-70P-EF              TO BEST-IO-AREA (101:4)
               INITIALIZE ANTB
               MOVE ANTT                   TO XO-70P
               MOVE XO-70P-EF              TO BEST-IO-AREA (105:4)
               INITIALIZE ANTT
               MOVE LDATO                  TO BEST-IO-AREA (109:6)
               INITIALIZE LDATO
               MOVE BKREFT                 TO BEST-IO-AREA (116:1)
               MOVE PUKE                   TO BEST-IO-AREA (117:2)
               WRITE BEST-IO-AREA
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
           PERFORM KORTTAB-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET NYEBEST-LEVEL-INIT          TO TRUE
           INITIALIZE NYEBEST-DATA-FIELDS
           SET NYEBEST-EOF-OFF             TO TRUE
           SET NYEBEST-PROCESS             TO TRUE
           OPEN INPUT NYEBEST
           OPEN OUTPUT BEST.
           SET TABFNR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE NYEBEST
           CLOSE BEST.
 
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
