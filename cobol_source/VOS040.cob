       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS040R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: VOS040, GRUNNLAG MEKANIKER STATISTIKK.       *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VOS02A                                       *
      *  LAGET DATO....: 10.10.96                                     *
      *  ENDRET........: 24.03.00 LEGGER INN AVD FRA LONNMAS.         *
      *  RETTET........:                                              *
      *  INPUT.........: PAR M/FILE DER MEK-NAVN SKAL HENTES (MEKTAB),*
      *                  DAGENS VERKDTEDORDRE (DAGMEK),               *
      *                  TIDLIGERE VERKSTEDORDRE (MEKFILE).           *
      *  BEHANDLING....: MERGER DAGENS ORDRE MED VERKSTED ORDRE FILE  *
      *                  MED TIDLIGERE TRANSER TIL NY MASTER.         *
      *  OUTPUT........: MEKANINEKER STATISTIKK-FILE (MEKSTAO),       *
      *                  KONTROLL-LISTE MED DAGENS TRANSER NÅR UPSI 1 *
      *                  ER PÅ, OG GAMLE TRANSER I TILLEGG NÅR UPSI 2 *
      *                  ER PÅ.                                       *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS040.rpg
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
           SELECT MEKTAB
               ASSIGN TO UT-S-MEKTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKTAB-STATUS.
           SELECT DAGMEKI
               ASSIGN TO UT-S-DAGMEKI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGMEKI-STATUS.
           SELECT MEKSTAI
               ASSIGN TO UT-S-MEKSTAI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKSTAI-STATUS.
           SELECT LONNMAS
               ASSIGN TO LONNMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS LONNMAS-STATUS
               RECORD KEY IS LONNMAS-KEY1.
           SELECT MEKSTAO
               ASSIGN TO UT-S-MEKSTAO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKSTAO-STATUS.
           SELECT MEKDATO
               ASSIGN TO UT-S-MEKDATO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKDATO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MEKTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  MEKTAB-IO-AREA.
           05  MEKTAB-IO-AREA-X            PICTURE X(80).
       FD DAGMEKI
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  DAGMEKI-IO-AREA.
           05  DAGMEKI-IO-AREA-X           PICTURE X(120).
       FD MEKSTAI
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  MEKSTAI-IO-AREA.
           05  MEKSTAI-IO-AREA-X           PICTURE X(120).
       FD LONNMAS
               RECORD CONTAINS 1025.
       01  LONNMAS-IO-AREA.
           05  LONNMAS-IO-AREA-X.
               10  LONNMAS-KEY1            PICTURE X(6).
               10  FILLER                  PICTURE X(1019).
       FD MEKSTAO
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  MEKSTAO-IO-AREA.
           05  MEKSTAO-IO-AREA-X           PICTURE X(120).
       FD MEKDATO
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  MEKDATO-IO-AREA.
           05  MEKDATO-IO-AREA-X           PICTURE X(80).
      *FLISTEO O   F  80  80            PRINTERSYSLST
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABMEK-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       77  TABFIL-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABMEK-TABLE.
               10  TABMEK-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY TABMEK-I
                                                      TABMEK-S
                                                      TABFIL-I
                                                      TABFIL-S.
                   15  TABMEK              PICTURE X(3).
                   15  TABFIL              PICTURE X(7).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  MEKTAB-STATUS               PICTURE 99 VALUE 0.
           10  DAGMEKI-STATUS              PICTURE 99 VALUE 0.
           10  MEKSTAI-STATUS              PICTURE 99 VALUE 0.
           10  LONNMAS-STATUS              PICTURE 99 VALUE 0.
           10  MEKSTAO-STATUS              PICTURE 99 VALUE 0.
           10  MEKDATO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKTAB-EOF-OFF          VALUE '0'.
               88  MEKTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGMEKI-EOF-OFF         VALUE '0'.
               88  DAGMEKI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGMEKI-READ-OFF        VALUE '0'.
               88  DAGMEKI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGMEKI-PROCESS-OFF     VALUE '0'.
               88  DAGMEKI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  DAGMEKI-LEVEL-INIT-OFF  VALUE '0'.
               88  DAGMEKI-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKSTAI-EOF-OFF         VALUE '0'.
               88  MEKSTAI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKSTAI-READ-OFF        VALUE '0'.
               88  MEKSTAI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKSTAI-PROCESS-OFF     VALUE '0'.
               88  MEKSTAI-PROCESS         VALUE '1'.
           05  LONNMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KLISTEO-DATA-FIELDS.
               10  KLISTEO-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-CLR-IO          PICTURE X VALUE 'Y'.
           05  DAGMEKI-LEVEL-01.
               10  DAGMEKI-01-L3.
                   15  DAGMEKI-01-L3-FNR   PICTURE X(3).
               10  DAGMEKI-01-L2.
                   15  DAGMEKI-01-L2-MEK   PICTURE X(3).
           05  DAGMEKI-DATA-FIELDS.
               10  REC01                   PICTURE X(120).
               10  FNR                     PICTURE X(3).
               10  MEK                     PICTURE X(3).
               10  LMKEY                   PICTURE X(6).
               10  ARH                     PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  MND                     PICTURE X(2).
               10  DAG                     PICTURE X(2).
               10  OBA-IO.
                   15  OBA                 PICTURE S9(9)V9(2).
               10  OBD-IO.
                   15  OBD                 PICTURE S9(9)V9(2).
               10  KBA-IO.
                   15  KBA                 PICTURE S9(9)V9(2).
               10  KBD-IO.
                   15  KBD                 PICTURE S9(9)V9(2).
               10  ANTO-IO.
                   15  ANTO                PICTURE S9(3).
               10  ANTTIM-IO.
                   15  ANTTIM              PICTURE S9(7)V9(2).
           05  MEKSTAI-DATA-FIELDS.
               10  REC02                   PICTURE X(120).
               10  PER02-IO.
                   15  PER02               PICTURE S9(8).
           05  LONNMAS-DATA-FIELDS.
               10  LMAVD                   PICTURE X(4).
      *****************************************************************
      * HOUSEKEEPING.                                                 *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  DATO                    PICTURE X(6).
               10  DDMM                    PICTURE X(4).
               10  DD                      PICTURE X(2).
               10  MM                      PICTURE X(2).
               10  AA                      PICTURE X(2).
               10  HH                      PICTURE X(2).
               10  HHAA                    PICTURE X(4).
               10  MMDD                    PICTURE X(4).
               10  SYSPER                  PICTURE X(8).
               10  SYSPEN-IO.
                   15  SYSPEN              PICTURE S9(8).
               10  SLEPER-IO.
                   15  SLEPER              PICTURE S9(8).
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(6).
               10  ANTNY-IO.
                   15  ANTNY               PICTURE S9(6).
               10  ANT02-IO.
                   15  ANT02               PICTURE S9(6).
               10  ANTSLE-IO.
                   15  ANTSLE              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-30YY9R               PICTURE ZZ9-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-80YNZR               PICTURE ZZZZZZZZ-.
               10  XO-60YY9R               PICTURE ZZZ.ZZ9-.
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
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  DAGMEKI-PROCESS
               SET DAGMEKI-PROCESS-OFF     TO TRUE
               SET DAGMEKI-READ            TO TRUE
           END-IF
 
           IF  DAGMEKI-READ
           AND RECORD-SELECTED-OFF
               PERFORM DAGMEKI-GET
               SET DAGMEKI-READ-OFF        TO TRUE
               IF  NOT DAGMEKI-EOF
                   SET DAGMEKI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  MEKSTAI-PROCESS
               SET MEKSTAI-PROCESS-OFF     TO TRUE
               SET MEKSTAI-READ            TO TRUE
           END-IF
 
           IF  MEKSTAI-READ
           AND RECORD-SELECTED-OFF
               PERFORM MEKSTAI-GET
               SET MEKSTAI-READ-OFF        TO TRUE
               IF  NOT MEKSTAI-EOF
                   SET MEKSTAI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  DAGMEKI-PROCESS
               PERFORM DAGMEKI-IDSET
           END-IF
 
           IF  MEKSTAI-PROCESS
               PERFORM MEKSTAI-IDSET
           END-IF
 
           IF  DAGMEKI-PROCESS
               PERFORM DAGMEKI-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
 
           IF  DAGMEKI-PROCESS
               PERFORM DAGMEKI-FLDSET
           END-IF
 
           IF  MEKSTAI-PROCESS
               PERFORM MEKSTAI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  DAGMEKI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-99)
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET I-98                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
               MOVE UDATE                  TO DATO
               MOVE DATO (1:4)             TO DDMM
               MOVE DDMM (1:2)             TO DD
               MOVE DDMM (3:2)             TO MM
               MOVE DATO (5:2)             TO AA
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               IF  AA NOT < '90'
                   SET I-10                TO TRUE
               END-IF
               IF  AA < '90'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-99 AND I-10)
               MOVE '19'                   TO HH
           END-IF
           IF  (I-99 AND I-11)
               MOVE '20'                   TO HH
           END-IF
           IF  (I-99)
               MOVE HH                     TO HHAA (1:2)
               MOVE AA                     TO HHAA (3:2)
               MOVE MM                     TO MMDD (1:2)
               MOVE DD                     TO MMDD (3:2)
               MOVE HHAA                   TO SYSPER (1:4)
               MOVE MMDD                   TO SYSPER (5:4)
               MOVE SYSPER                 TO SYSPEN-IO
               SUBTRACT 20000 FROM SYSPEN GIVING SLEPER
      *****************************************************************
      * MERGER GAMMEL MASTER MED DAGENS TRANSER TIL NY MASTER         *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-20                TO TRUE
               SET TABMEK-S                TO TABMEK-I
               PERFORM WITH TEST AFTER
                       VARYING TABMEK-I FROM 1 BY 1
                         UNTIL TABMEK-I >= TABMEK-MAX
                            OR I-20
                   IF  FNR = TABMEK (TABMEK-I)
                       SET I-20            TO TRUE
                       SET TABMEK-S        TO TABMEK-I
                   END-IF
               END-PERFORM
               SET TABMEK-I                TO TABMEK-S
               IF  I-20
               AND TABMEK-I NOT > TABFIL-MAX
                   SET TABFIL-I            TO TABMEK-I
               END-IF
           END-IF
           IF  (I-L3 AND I-20)
               SET NOT-I-21                TO TRUE
               IF  TABFIL(TABFIL-I) = 'LONNMAS'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-21)
               MOVE LMKEY                  TO LONNMAS-KEY1
               READ LONNMAS RECORD KEY IS LONNMAS-KEY1
               INVALID KEY
                   SET I-22                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-22            TO TRUE
                   PERFORM LONNMAS-FLDSET
                   PERFORM LONNMAS-IDSET
               END-READ
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT01
               ADD 1                       TO ANTNY
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT02
               SET NOT-I-14                TO TRUE
               SET NOT-I-13                TO TRUE
               IF  PER02 NOT < SLEPER
                   SET I-14                TO TRUE
               END-IF
               IF  PER02 < SLEPER
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-13)
               ADD 1                       TO ANTSLE
           END-IF
           IF  (I-02 AND I-14)
               ADD 1                       TO ANTNY
      *****************************************************************
      * DAGENS VERKSTEDORDRE                                          *
      *****************************************************************
           END-IF
           .
 
       DAGMEKI-GET SECTION.
       DAGMEKI-GET-P.
           IF  DAGMEKI-EOF-OFF
               READ DAGMEKI
               AT END
                   SET DAGMEKI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGMEKI-FLDSET SECTION.
       DAGMEKI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGMEKI-IO-AREA (1:120) TO REC01 (1:120)
               MOVE DAGMEKI-IO-AREA (1:3)  TO FNR (1:3)
               MOVE DAGMEKI-IO-AREA (4:3)  TO MEK (1:3)
               MOVE DAGMEKI-IO-AREA (1:6)  TO LMKEY (1:6)
               MOVE DAGMEKI-IO-AREA (7:2)  TO ARH (1:2)
               MOVE DAGMEKI-IO-AREA (9:2)  TO AAR (1:2)
               MOVE DAGMEKI-IO-AREA (11:2) TO MND (1:2)
               MOVE DAGMEKI-IO-AREA (13:2) TO DAG (1:2)
               MOVE DAGMEKI-IO-AREA (15:11) TO OBA-IO
               INSPECT OBA-IO REPLACING ALL ' ' BY '0'
               MOVE DAGMEKI-IO-AREA (26:11) TO OBD-IO
               INSPECT OBD-IO REPLACING ALL ' ' BY '0'
               MOVE DAGMEKI-IO-AREA (37:11) TO KBA-IO
               INSPECT KBA-IO REPLACING ALL ' ' BY '0'
               MOVE DAGMEKI-IO-AREA (48:11) TO KBD-IO
               INSPECT KBD-IO REPLACING ALL ' ' BY '0'
               MOVE DAGMEKI-IO-AREA (59:3) TO ANTO-IO
               INSPECT ANTO-IO REPLACING ALL ' ' BY '0'
               MOVE DAGMEKI-IO-AREA (62:9) TO ANTTIM-IO
               INSPECT ANTTIM-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       DAGMEKI-IDSET SECTION.
       DAGMEKI-IDSET-P.
           SET I-01                        TO TRUE.
 
       DAGMEKI-CHK-LEVEL SECTION.
       DAGMEKI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGMEKI-LEVEL-01
               MOVE DAGMEKI-IO-AREA (1:3)  TO DAGMEKI-01-L3-FNR
               MOVE DAGMEKI-IO-AREA (4:3)  TO DAGMEKI-01-L2-MEK
               IF  DAGMEKI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGMEKI-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  DAGMEKI-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  DAGMEKI-01-L3         TO THE-PRIOR-L3
               MOVE  DAGMEKI-01-L2         TO THE-PRIOR-L2
               SET DAGMEKI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       MEKSTAI-GET SECTION.
       MEKSTAI-GET-P.
           IF  MEKSTAI-EOF-OFF
               READ MEKSTAI
               AT END
                   SET MEKSTAI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MEKSTAI-FLDSET SECTION.
       MEKSTAI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MEKSTAI-IO-AREA (1:120) TO REC02 (1:120)
               MOVE MEKSTAI-IO-AREA (1:3)  TO FNR (1:3)
               MOVE MEKSTAI-IO-AREA (4:3)  TO MEK (1:3)
               MOVE MEKSTAI-IO-AREA (7:8)  TO PER02-IO
               INSPECT PER02-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAI-IO-AREA (7:2)  TO ARH (1:2)
               MOVE MEKSTAI-IO-AREA (9:2)  TO AAR (1:2)
               MOVE MEKSTAI-IO-AREA (11:2) TO MND (1:2)
               MOVE MEKSTAI-IO-AREA (13:2) TO DAG (1:2)
               MOVE MEKSTAI-IO-AREA (15:11) TO OBA-IO
               INSPECT OBA-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAI-IO-AREA (26:11) TO OBD-IO
               INSPECT OBD-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAI-IO-AREA (37:11) TO KBA-IO
               INSPECT KBA-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAI-IO-AREA (48:11) TO KBD-IO
               INSPECT KBD-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAI-IO-AREA (59:3) TO ANTO-IO
               INSPECT ANTO-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAI-IO-AREA (62:9) TO ANTTIM-IO
               INSPECT ANTTIM-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       MEKSTAI-IDSET SECTION.
       MEKSTAI-IDSET-P.
           SET I-02                        TO TRUE.
 
       LONNMAS-FLDSET SECTION.
       LONNMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LONNMAS-IO-AREA (08:4) TO LMAVD (1:4)
           END-EVALUATE.
 
       LONNMAS-IDSET SECTION.
       LONNMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       KLISTEO-PRINT-LINE SECTION.
       KLISTEO-PRINT-LINE-P.
           IF  KLISTEO-BEFORE-SKIP > 0
               PERFORM KLISTEO-SKIP-BEFORE
           END-IF
           IF  KLISTEO-BEFORE-SPACE > 0
               PERFORM KLISTEO-SPACE-BEFORE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               IF  KLISTEO-AFTER-SPACE > 0
                   PERFORM KLISTEO-SPACE-AFTER
               END-IF
           ELSE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               PERFORM KLISTEO-SPACE-AFTER
           END-IF
           IF  KLISTEO-LINE-COUNT NOT < KLISTEO-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       KLISTEO-SKIP-BEFORE SECTION.
       KLISTEO-SKIP-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-BEFORE-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-BEFORE SECTION.
       KLISTEO-SPACE-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER KLISTEO-BEFORE-SPACE
                                                                 LINES
           ADD KLISTEO-BEFORE-SPACE        TO KLISTEO-LINE-COUNT
           MOVE SPACES TO KLISTEO-IO-AREA
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-BEFORE-SPACE.
 
       KLISTEO-SKIP-AFTER SECTION.
       KLISTEO-SKIP-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-AFTER-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-AFTER SECTION.
       KLISTEO-SPACE-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE KLISTEO-AFTER-SPACE LINES
           ADD KLISTEO-AFTER-SPACE         TO KLISTEO-LINE-COUNT
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-AFTER-SPACE.
 
       MEKTAB-LOAD SECTION.
       MEKTAB-LOAD-P.
           OPEN INPUT MEKTAB
           SET TABMEK-I                    TO 1
           PERFORM UNTIL MEKTAB-EOF
               READ MEKTAB
               AT END
                   SET MEKTAB-EOF          TO TRUE
               NOT AT END
                   MOVE MEKTAB-IO-AREA (1:10) TO TABMEK-ENTRY
                                                            (TABMEK-I)
                   SET TABMEK-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE MEKTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO MEKSTAO-IO-AREA
               INITIALIZE MEKSTAO-IO-AREA
               MOVE REC01                  TO MEKSTAO-IO-AREA (1:120)
               MOVE '0000'                 TO MEKSTAO-IO-AREA (71:4)
               IF  (I-21 AND NOT-I-22)
                   MOVE LMAVD              TO MEKSTAO-IO-AREA (71:4)
               END-IF
               MOVE SYSPER                 TO MEKSTAO-IO-AREA (107:8)
               MOVE TIDSP-IO               TO MEKSTAO-IO-AREA (115:6)
               WRITE MEKSTAO-IO-AREA
           END-IF
           IF  (I-02 AND I-14)
               MOVE SPACES TO MEKSTAO-IO-AREA
               INITIALIZE MEKSTAO-IO-AREA
               MOVE REC02                  TO MEKSTAO-IO-AREA (1:120)
               WRITE MEKSTAO-IO-AREA
           END-IF
           IF  (I-99)
               MOVE SPACES TO MEKDATO-IO-AREA
               INITIALIZE MEKDATO-IO-AREA
               MOVE 'KL:'                  TO MEKDATO-IO-AREA (1:3)
               MOVE TIDSP-IO               TO MEKDATO-IO-AREA (4:6)
               MOVE 'DATO:'                TO MEKDATO-IO-AREA (11:5)
               MOVE SYSPER                 TO MEKDATO-IO-AREA (16:8)
               MOVE 'FIR:***'              TO MEKDATO-IO-AREA (25:7)
               WRITE MEKDATO-IO-AREA
           END-IF
           IF  (I-U1 AND I-01)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE FNR                    TO KLISTEO-IO-AREA (1:3)
               MOVE MEK                    TO KLISTEO-IO-AREA (6:3)
               MOVE DAG                    TO KLISTEO-IO-AREA (11:2)
               MOVE MND                    TO KLISTEO-IO-AREA (14:2)
               MOVE AAR                    TO KLISTEO-IO-AREA (17:2)
               MOVE ARH                    TO KLISTEO-IO-AREA (20:2)
               MOVE OBA                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (24:15)
               MOVE OBD                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (41:15)
               MOVE KBA                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (56:15)
               MOVE KBD                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (71:15)
               MOVE ANTO                   TO XO-30YY9R
               MOVE XO-30YY9R              TO KLISTEO-IO-AREA (87:4)
               MOVE ANTTIM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO KLISTEO-IO-AREA (93:13)
               MOVE 'NY '                  TO KLISTEO-IO-AREA (108:3)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-U1 AND I-U2 AND I-02)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE FNR                    TO KLISTEO-IO-AREA (1:3)
               MOVE MEK                    TO KLISTEO-IO-AREA (6:3)
               MOVE DAG                    TO KLISTEO-IO-AREA (11:2)
               MOVE MND                    TO KLISTEO-IO-AREA (14:2)
               MOVE AAR                    TO KLISTEO-IO-AREA (17:2)
               MOVE ARH                    TO KLISTEO-IO-AREA (20:2)
               MOVE OBA                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (24:15)
               MOVE OBD                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (41:15)
               MOVE KBA                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (56:15)
               MOVE KBD                    TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (71:15)
               MOVE ANTO                   TO XO-30YY9R
               MOVE XO-30YY9R              TO KLISTEO-IO-AREA (87:4)
               MOVE ANTTIM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO KLISTEO-IO-AREA (93:13)
               MOVE 'GML'                  TO KLISTEO-IO-AREA (108:3)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-U1 AND I-99)
           OR  (I-U2 AND I-99)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTEDORDRE. GRUNNLAG ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'MEKANIKER-STATISTIKK.   ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (68:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (79:8)
               MOVE 'JOB=VOS02A'           TO KLISTEO-IO-AREA (89:10)
               MOVE 'PROGRAM=VOS040'       TO KLISTEO-IO-AREA (101:14)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SUM PR MEKANIKER PR DATO' TO KLISTEO-IO-AREA
                                                               (54:24)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'FIR'                  TO KLISTEO-IO-AREA (1:3)
               MOVE 'MEK'                  TO KLISTEO-IO-AREA (6:3)
               MOVE 'DD MM ÅÅ HH'          TO KLISTEO-IO-AREA (11:11)
               MOVE '  ORD. BEL ARB '      TO KLISTEO-IO-AREA (24:15)
               MOVE ' ORD BELØP DEL '      TO KLISTEO-IO-AREA (41:15)
               MOVE '  KRE. BEL ARB '      TO KLISTEO-IO-AREA (56:15)
               MOVE ' KRE BELØP DEL '      TO KLISTEO-IO-AREA (71:15)
               MOVE 'ANT'                  TO KLISTEO-IO-AREA (88:3)
               MOVE 'TIM'                  TO KLISTEO-IO-AREA (103:3)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-U1 AND I-OF)
           OR  (I-U2 AND I-OF)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTEDORDRE. GRUNNLAG ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'MEKANIKER-STATISTIKK.   ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (68:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (79:8)
               MOVE 'JOB=VOS02A'           TO KLISTEO-IO-AREA (89:10)
               MOVE 'PROGRAM=VOS040'       TO KLISTEO-IO-AREA (101:14)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SUM PR MEKANIKER PR DATO' TO KLISTEO-IO-AREA
                                                               (54:24)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'FIR'                  TO KLISTEO-IO-AREA (1:3)
               MOVE 'MEK'                  TO KLISTEO-IO-AREA (6:3)
               MOVE 'DD MM ÅÅ HH'          TO KLISTEO-IO-AREA (11:11)
               MOVE '  ORD. BEL ARB '      TO KLISTEO-IO-AREA (24:15)
               MOVE ' ORD BELØP DEL '      TO KLISTEO-IO-AREA (41:15)
               MOVE '  KRE. BEL ARB '      TO KLISTEO-IO-AREA (56:15)
               MOVE ' KRE BELØP DEL '      TO KLISTEO-IO-AREA (71:15)
               MOVE 'ANT'                  TO KLISTEO-IO-AREA (88:3)
               MOVE 'TIM'                  TO KLISTEO-IO-AREA (103:3)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-U1 AND I-LR)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SLETTE-DATO         :' TO KLISTEO-IO-AREA (5:21)
               MOVE SLEPER                 TO XO-80YNZR
               MOVE XO-80YNZR              TO KLISTEO-IO-AREA (27:9)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-BEFORE-SPACE
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL NYE          :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANT01                  TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL GAMLE        :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANT02                  TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL SLETTET      :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTSLE                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL NY MASTER    :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTNY                  TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTNY
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-U1 AND I-LR AND I-03)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE '*                    ' TO KLISTEO-IO-AREA (5:21)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
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
           PERFORM MEKTAB-LOAD
           SET DAGMEKI-LEVEL-INIT          TO TRUE
           INITIALIZE DAGMEKI-DATA-FIELDS
           SET DAGMEKI-EOF-OFF             TO TRUE
           SET DAGMEKI-PROCESS             TO TRUE
           OPEN INPUT DAGMEKI
           INITIALIZE MEKSTAI-DATA-FIELDS
           SET MEKSTAI-EOF-OFF             TO TRUE
           SET MEKSTAI-PROCESS             TO TRUE
           OPEN INPUT MEKSTAI
           INITIALIZE LONNMAS-DATA-FIELDS
           OPEN INPUT LONNMAS
           OPEN OUTPUT MEKSTAO
           OPEN OUTPUT MEKDATO
           IF I-U1
               OPEN OUTPUT KLISTEO
           END-IF
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           SET TABMEK-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE DAGMEKI
           CLOSE MEKSTAI
           CLOSE LONNMAS
           CLOSE MEKSTAO
           CLOSE MEKDATO
           IF I-U1
               CLOSE KLISTEO
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
