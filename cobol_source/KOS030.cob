       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOS030R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: KOS030, SANERING AV KONTANT.ORDRE            *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: KONTORD, REORG. AV KONTANT.ORDRE             *
      *  LAGET DATO....: 19.02.04                                     *
      *  ENDRET........: 13.07.12 SANERER OPPGJØR ETTER 1 ÅR          *
      *                           SKRIVER SANERTE RECORDS PÅ BACKUP   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KOS030.rpg
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
           SELECT KORDREI
               ASSIGN TO UT-S-KORDREI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORDREI-STATUS.
           SELECT FIRMAKT
               ASSIGN TO UT-S-FIRMAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRMAKT-STATUS.
           SELECT KORDREO
               ASSIGN TO UT-S-KORDREO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORDREO-STATUS.
           SELECT ORDSANO
               ASSIGN TO UT-S-ORDSANO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSANO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
           SELECT BUGFILO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BUGFILO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORDREI
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  KORDREI-IO-AREA.
           05  KORDREI-IO-AREA-X           PICTURE X(40).
       FD FIRMAKT
               BLOCK CONTAINS 5
               RECORD CONTAINS 5.
       01  FIRMAKT-IO-AREA.
           05  FIRMAKT-IO-AREA-X           PICTURE X(5).
       FD KORDREO
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  KORDREO-IO-AREA.
           05  KORDREO-IO-AREA-X           PICTURE X(40).
       FD ORDSANO
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  ORDSANO-IO-AREA.
           05  ORDSANO-IO-AREA-X           PICTURE X(40).
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
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
           10  KORDREI-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAKT-STATUS              PICTURE 99 VALUE 0.
           10  KORDREO-STATUS              PICTURE 99 VALUE 0.
           10  ORDSANO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
           10  BUGFILO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORDREI-EOF-OFF         VALUE '0'.
               88  KORDREI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORDREI-READ-OFF        VALUE '0'.
               88  KORDREI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORDREI-PROCESS-OFF     VALUE '0'.
               88  KORDREI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KORDREI-LEVEL-INIT-OFF  VALUE '0'.
               88  KORDREI-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-EOF-OFF         VALUE '0'.
               88  FIRMAKT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-READ-OFF        VALUE '0'.
               88  FIRMAKT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-PROCESS-OFF     VALUE '0'.
               88  FIRMAKT-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FIRMAKT-LEVEL-INIT-OFF  VALUE '0'.
               88  FIRMAKT-LEVEL-INIT      VALUE '1'.
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
           05  KORDREI-LEVEL-01.
               10  KORDREI-01-L1.
                   15  KORDREI-01-L1-FIR01 PICTURE X(3).
           05  KORDREI-DATA-FIELDS.
               10  FIR01                   PICTURE X(3).
               10  RECART                  PICTURE X(1).
               10  SUMNR-IO.
                   15  SUMNR               PICTURE S9(13) USAGE
                                                       PACKED-DECIMAL.
               10  ORDDAG                  PICTURE X(2).
               10  ORDMND                  PICTURE X(2).
               10  ORDAAR                  PICTURE X(2).
               10  REC040                  PICTURE X(40).
           05  KORDREI-MP                  PICTURE X(3).
           05  KORDREI-MC                  PICTURE X(3).
           05  KORDREI-M-01            REDEFINES KORDREI-MC.
               10  KORDREI-M-01-M1.
                   15  KORDREI-M-01-M1-FIR01-G.
                       20  KORDREI-M-01-M1-FIR01 PICTURE X(3).
           05  FIRMAKT-LEVEL-02.
               10  FIRMAKT-02-L1.
                   15  FIRMAKT-02-L1-FIR02 PICTURE X(3).
           05  FIRMAKT-DATA-FIELDS.
               10  FIR02                   PICTURE X(3).
           05  FIRMAKT-MP                  PICTURE X(3).
           05  FIRMAKT-MC                  PICTURE X(3).
           05  FIRMAKT-M-02            REDEFINES FIRMAKT-MC.
               10  FIRMAKT-M-02-M1.
                   15  FIRMAKT-M-02-M1-FIR02-G.
                       20  FIRMAKT-M-02-M1-FIR02 PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  SANAAR-IO.
                   15  SANAAR              PICTURE S9(2).
               10  SANDTO                  PICTURE X(8).
               10  BUGFL1                  PICTURE X(8).
               10  KLOKKE-IO.
                   15  KLOKKE              PICTURE S9(6).
               10  L1TINN-IO.
                   15  L1TINN              PICTURE S9(7).
               10  L1OINN-IO.
                   15  L1OINN              PICTURE S9(7).
               10  L1SINN-IO.
                   15  L1SINN              PICTURE S9(7).
               10  L1SAN-IO.
                   15  L1SAN               PICTURE S9(7).
               10  L1OSAN-IO.
                   15  L1OSAN              PICTURE S9(7).
               10  L1SSAN-IO.
                   15  L1SSAN              PICTURE S9(7).
               10  L1UT-IO.
                   15  L1UT                PICTURE S9(7).
               10  L1OUT-IO.
                   15  L1OUT               PICTURE S9(7).
               10  L1SUT-IO.
                   15  L1SUT               PICTURE S9(7).
               10  LRFIRI-IO.
                   15  LRFIRI              PICTURE S9(7).
               10  LRFIRS-IO.
                   15  LRFIRS              PICTURE S9(7).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(7).
               10  ANTSIN-IO.
                   15  ANTSIN              PICTURE S9(7).
               10  ANTOIN-IO.
                   15  ANTOIN              PICTURE S9(7).
               10  ANTSSA-IO.
                   15  ANTSSA              PICTURE S9(7).
               10  ANTOSA-IO.
                   15  ANTOSA              PICTURE S9(7).
               10  AAAA                    PICTURE X(4).
               10  MMDD                    PICTURE X(4).
               10  DATO8                   PICTURE X(8).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7).
               10  ANTOUT-IO.
                   15  ANTOUT              PICTURE S9(7).
               10  NUM130-IO.
                   15  NUM130              PICTURE S9(13).
               10  OPPDTO                  PICTURE X(8).
               10  ANTSUT-IO.
                   15  ANTSUT              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KORDREI-PROCESS
               SET KORDREI-PROCESS-OFF     TO TRUE
               SET KORDREI-READ            TO TRUE
           END-IF
 
           IF  KORDREI-READ
               PERFORM KORDREI-GET
               SET KORDREI-READ-OFF        TO TRUE
               IF  NOT KORDREI-EOF
                   PERFORM KORDREI-MATCH-SET
               END-IF
           END-IF
 
           IF  FIRMAKT-PROCESS
               SET FIRMAKT-PROCESS-OFF     TO TRUE
               SET FIRMAKT-READ            TO TRUE
           END-IF
 
           IF  FIRMAKT-READ
               PERFORM FIRMAKT-GET
               SET FIRMAKT-READ-OFF        TO TRUE
               IF  NOT FIRMAKT-EOF
                   PERFORM FIRMAKT-MATCH-SET
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
 
           IF  KORDREI-PROCESS
               PERFORM KORDREI-IDSET
           END-IF
 
           IF  FIRMAKT-PROCESS
               PERFORM FIRMAKT-IDSET
           END-IF
 
           IF  KORDREI-PROCESS
               PERFORM KORDREI-CHK-LEVEL
           END-IF
 
           IF  FIRMAKT-PROCESS
               PERFORM FIRMAKT-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  KORDREI-PROCESS
               PERFORM KORDREI-FLDSET
           END-IF
 
           IF  FIRMAKT-PROCESS
               PERFORM FIRMAKT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KORDREI-PROCESS
           OR  FIRMAKT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-98)
               SET NOT-I-98                TO TRUE
           END-IF
           IF  (NOT-I-99)
               MOVE UDATE                  TO DATO-IO
               SUBTRACT 1 FROM UYEAR   GIVING SANAAR
               MOVE SANAAR                 TO DATO (1:2)
               MOVE UDAY                   TO DATO-IO (5:2)
               SET NOT-I-31                TO TRUE
               IF  UYEAR > 80
                   SET I-31                TO TRUE
               END-IF
               MOVE DATO                   TO SANDTO (3:6)
           END-IF
           IF  (NOT-I-99 AND I-31)
               MOVE '19'                   TO SANDTO (1:2)
           END-IF
           IF  (NOT-I-99 AND NOT-I-31)
               MOVE '20'                   TO SANDTO (1:2)
           END-IF
           IF  (NOT-I-99 AND I-U4)
               MOVE 'SANDTO  '             TO BUGFL1
           END-IF
           IF  (NOT-I-99)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO KLOKKE (1:6)
               SET I-99                    TO TRUE
               SET I-98                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           SET NOT-I-13                    TO TRUE
           IF  (I-L1)
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-17                TO TRUE
               MOVE 0                      TO L1TINN
               MOVE 0                      TO L1OINN
               MOVE 0                      TO L1SINN
               MOVE 0                      TO L1SAN
               MOVE 0                      TO L1OSAN
               MOVE 0                      TO L1SSAN
               MOVE 0                      TO L1UT
               MOVE 0                      TO L1OUT
               MOVE 0                      TO L1SUT
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO LRFIRI
           END-IF
           IF  (I-L1 AND NOT-I-MR AND I-01)
               ADD 1                       TO LRFIRS
           END-IF
           IF  (I-L1 AND NOT-I-MR)
               SET I-16                    TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTINN
               ADD 1                       TO L1TINN
               SET NOT-I-10                TO TRUE
               IF  L1TINN > 0
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-MR)
               ADD 1                       TO ANTSAN
               SET NOT-I-11                TO TRUE
               IF  ANTSAN > 0
                   SET I-11                TO TRUE
               END-IF
               ADD 1                       TO L1SAN
               SET NOT-I-17                TO TRUE
               IF  L1SAN > 0
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-12                TO TRUE
               SET NOT-I-22                TO TRUE
               IF  RECART NOT = '0'
                   SET I-12                TO TRUE
               END-IF
               IF  RECART = '0'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-12)
               ADD 1                       TO ANTSIN
           END-IF
           IF  (I-01 AND I-22)
               ADD 1                       TO L1OINN
           END-IF
           IF  (I-01 AND I-12)
               ADD 1                       TO L1SINN
           END-IF
           IF  (I-01 AND I-22)
               ADD 1                       TO ANTOIN
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-12)
               ADD 1                       TO ANTSSA
               SET NOT-I-11                TO TRUE
               IF  ANTSSA > 0
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-22)
               ADD 1                       TO ANTOSA
               SET NOT-I-11                TO TRUE
               IF  ANTOSA > 0
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-12)
               ADD 1                       TO L1SSAN
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-22)
               ADD 1                       TO L1OSAN
           END-IF
           IF  (NOT-I-MR)
               OR  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND I-22)
               SET NOT-I-14                TO TRUE
               SET NOT-I-15                TO TRUE
               IF  ORDAAR NOT < '80'
                   SET I-14                TO TRUE
               END-IF
               IF  ORDAAR < '80'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-22 AND I-14)
               MOVE '19'                   TO AAAA (1:2)
           END-IF
           IF  (I-01 AND I-22 AND I-15)
               MOVE '20'                   TO AAAA (1:2)
           END-IF
           IF  (I-01 AND I-22)
               MOVE ORDAAR                 TO AAAA (3:2)
               MOVE ORDMND                 TO MMDD (1:2)
               MOVE ORDDAG                 TO MMDD (3:2)
               MOVE AAAA                   TO DATO8 (1:4)
               MOVE MMDD                   TO DATO8 (5:4)
               SET NOT-I-13                TO TRUE
               IF  DATO8 > SANDTO
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-22 AND NOT-I-13)
               ADD 1                       TO ANTSAN
               SET NOT-I-11                TO TRUE
               IF  ANTSAN > 0
                   SET I-11                TO TRUE
               END-IF
               ADD 1                       TO L1SAN
               ADD 1                       TO L1OSAN
               ADD 1                       TO ANTOSA
               SET NOT-I-11                TO TRUE
               IF  ANTOSA > 0
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-13)
               ADD 1                       TO ANTUT
               ADD 1                       TO ANTOUT
               ADD 1                       TO L1OUT
               ADD 1                       TO L1UT
           END-IF
           IF  (I-22)
               GO TO SLUTT-T
           END-IF
           ADD SUMNR TO ZERO           GIVING NUM130
           MOVE NUM130 (1:8)               TO OPPDTO
           SET NOT-I-13                    TO TRUE
           IF  OPPDTO > SANDTO
               SET I-13                    TO TRUE
           END-IF
           IF  (I-13)
               ADD 1                       TO ANTUT
               ADD 1                       TO ANTSUT
               ADD 1                       TO L1UT
               ADD 1                       TO L1SUT
           END-IF
           IF  (NOT-I-13)
               ADD 1                       TO ANTSSA
               SET NOT-I-11                TO TRUE
               IF  ANTSSA > 0
                   SET I-11                TO TRUE
               END-IF
               ADD 1                       TO ANTSAN
               SET NOT-I-11                TO TRUE
               IF  ANTSAN > 0
                   SET I-11                TO TRUE
               END-IF
               ADD 1                       TO L1SAN
               SET NOT-I-17                TO TRUE
               IF  L1SAN > 0
                   SET I-17                TO TRUE
               END-IF
               ADD 1                       TO L1SSAN
           END-IF
           IF  (I-U4)
               MOVE 'OPPDTO  '             TO BUGFL1
      *  U4                MOVE "NUM130  "BUGFL1  8        LEDETXT DEBUG
      *  U4      BUGFL1    DEBUGBUGFILO   NUM130           VIS FELT/IND
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       KORDREI-GET SECTION.
       KORDREI-GET-P.
           IF  KORDREI-EOF-OFF
               READ KORDREI
               AT END
                   SET KORDREI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORDREI-FLDSET SECTION.
       KORDREI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KORDREI-IO-AREA (1:3)  TO FIR01 (1:3)
               MOVE KORDREI-IO-AREA (4:1)  TO RECART (1:1)
               MOVE KORDREI-IO-AREA (4:7)  TO SUMNR-IO
               MOVE KORDREI-IO-AREA (19:2) TO ORDDAG (1:2)
               MOVE KORDREI-IO-AREA (21:2) TO ORDMND (1:2)
               MOVE KORDREI-IO-AREA (23:2) TO ORDAAR (1:2)
               MOVE KORDREI-IO-AREA (1:40) TO REC040 (1:40)
           END-EVALUATE.
 
       KORDREI-IDSET SECTION.
       KORDREI-IDSET-P.
           SET I-01                        TO TRUE.
 
       KORDREI-CHK-LEVEL SECTION.
       KORDREI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KORDREI-LEVEL-01
               MOVE KORDREI-IO-AREA (1:3)  TO KORDREI-01-L1-FIR01
               IF  KORDREI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KORDREI-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KORDREI-01-L1         TO THE-PRIOR-L1
               SET KORDREI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KORDREI-MATCH-SET SECTION.
       KORDREI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KORDREI-IO-AREA (1:3)  TO KORDREI-M-01-M1-FIR01
           END-EVALUATE.
 
       FIRMAKT-GET SECTION.
       FIRMAKT-GET-P.
           IF  FIRMAKT-EOF-OFF
               READ FIRMAKT
               AT END
                   SET FIRMAKT-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FIRMAKT-FLDSET SECTION.
       FIRMAKT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAKT-IO-AREA (1:3)  TO FIR02 (1:3)
           END-EVALUATE.
 
       FIRMAKT-IDSET SECTION.
       FIRMAKT-IDSET-P.
           SET I-02                        TO TRUE.
 
       FIRMAKT-CHK-LEVEL SECTION.
       FIRMAKT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FIRMAKT-LEVEL-02
               MOVE FIRMAKT-IO-AREA (1:3)  TO FIRMAKT-02-L1-FIR02
               IF  FIRMAKT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FIRMAKT-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FIRMAKT-02-L1         TO THE-PRIOR-L1
               SET FIRMAKT-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAKT-MATCH-SET SECTION.
       FIRMAKT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAKT-IO-AREA (1:3)  TO FIRMAKT-M-02-M1-FIR02
           END-EVALUATE.
 
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  KORDREI-EOF
               MOVE HIGH-VALUES            TO KORDREI-MC
                                              KORDREI-MP
           END-IF
           IF  FIRMAKT-EOF
               MOVE HIGH-VALUES            TO FIRMAKT-MC
                                              FIRMAKT-MP
           END-IF
           IF  KORDREI-MC < KORDREI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FIRMAKT-MC < FIRMAKT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KORDREI-MC < FIRMAKT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KORDREI-PROCESS     TO TRUE
                   MOVE KORDREI-MC         TO KORDREI-MP
                   IF  KORDREI-MC = FIRMAKT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FIRMAKT-MC < KORDREI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRMAKT-PROCESS     TO TRUE
                   MOVE FIRMAKT-MC         TO FIRMAKT-MP
                   IF  FIRMAKT-MC = KORDREI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KORDREI-MC = FIRMAKT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KORDREI-PROCESS     TO TRUE
                   MOVE KORDREI-MC         TO KORDREI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR AND I-13)
               MOVE SPACES TO KORDREO-IO-AREA
               INITIALIZE KORDREO-IO-AREA
               MOVE REC040                 TO KORDREO-IO-AREA (1:40)
               WRITE KORDREO-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR)
           OR  (I-01 AND NOT-I-13)
               MOVE SPACES TO ORDSANO-IO-AREA
               INITIALIZE ORDSANO-IO-AREA
               MOVE REC040                 TO ORDSANO-IO-AREA (1:40)
               WRITE ORDSANO-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-U2)
           OR  (I-01 AND NOT-I-13 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SANERT:'              TO KLISTEO-IO-AREA (1:7)
               MOVE REC040                 TO KLISTEO-IO-AREA (11:40)
               IF  (NOT-I-MR)
                   MOVE ' NMR   '          TO KLISTEO-IO-AREA (51:7)
               END-IF
               IF  (I-MR AND NOT-I-13)
                   MOVE '  MRN13'          TO KLISTEO-IO-AREA (51:7)
               END-IF
               MOVE SANDTO                 TO KLISTEO-IO-AREA (59:8)
               IF  (NOT-I-13)
                   MOVE '<'                TO KLISTEO-IO-AREA (68:1)
               END-IF
               IF  (I-22)
                   MOVE DATO8              TO KLISTEO-IO-AREA (70:8)
               END-IF
               IF  (I-12)
                   MOVE OPPDTO             TO KLISTEO-IO-AREA (70:8)
               END-IF
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-98)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV KONTANT.ORDRE        ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO   :'             TO KLISTEO-IO-AREA (48:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (57:8)
               MOVE 'JOB    =KONTORD'      TO KLISTEO-IO-AREA (66:15)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KLOKKE :'             TO KLISTEO-IO-AREA (48:8)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (57:8)
               MOVE 'PROGRAM=KOS030 '      TO KLISTEO-IO-AREA (66:15)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV KONTANT.ORDRE        ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO   :'             TO KLISTEO-IO-AREA (48:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (57:8)
               MOVE 'JOB    =KONTORD'      TO KLISTEO-IO-AREA (66:15)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KLOKKE :'             TO KLISTEO-IO-AREA (48:8)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (57:8)
               MOVE 'PROGRAM=KOS030 '      TO KLISTEO-IO-AREA (66:15)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-10 AND I-U3)
           OR  (I-L1 AND I-11 AND I-U3)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'FIR: '                TO KLISTEO-IO-AREA (1:5)
               MOVE FIR01                  TO KLISTEO-IO-AREA (6:3)
               MOVE 'INN: '                TO KLISTEO-IO-AREA (11:5)
               MOVE L1TINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               MOVE ' ORDRERECS:'          TO KLISTEO-IO-AREA (26:11)
               MOVE L1OINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (37:10)
               MOVE ' SUMRECS:'            TO KLISTEO-IO-AREA (47:9)
               MOVE L1SINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (56:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE '- SAN: '              TO KLISTEO-IO-AREA (9:7)
               MOVE L1SAN                  TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               MOVE ' ORDRERECS:'          TO KLISTEO-IO-AREA (26:11)
               IF  (I-16)
                   MOVE 'ALT'              TO KLISTEO-IO-AREA (44:3)
               END-IF
               MOVE L1OSAN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (37:10)
               MOVE ' SUMRECS:'            TO KLISTEO-IO-AREA (47:9)
               IF  (I-16)
                   MOVE 'ALT'              TO KLISTEO-IO-AREA (63:3)
               END-IF
               MOVE L1SSAN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (56:10)
               IF  (I-17)
                   MOVE ' <======== OBS!'  TO KLISTEO-IO-AREA (66:15)
               END-IF
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE '= UT : '              TO KLISTEO-IO-AREA (9:7)
               MOVE L1UT                   TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               MOVE ' ORDRERECS:'          TO KLISTEO-IO-AREA (26:11)
               MOVE L1OUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (37:10)
               MOVE ' SUMRECS:'            TO KLISTEO-IO-AREA (47:9)
               MOVE L1SUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (56:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-LR)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SANERINGSDATO :'      TO KLISTEO-IO-AREA (1:15)
               MOVE SANDTO                 TO KLISTEO-IO-AREA (18:8)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL FØR SAN:'      TO KLISTEO-IO-AREA (1:15)
               MOVE ANTINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               MOVE ' ORDRERECS:'          TO KLISTEO-IO-AREA (26:11)
               MOVE ANTOIN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (37:10)
               MOVE ' SUMRECS:'            TO KLISTEO-IO-AREA (47:9)
               MOVE ANTSIN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (56:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL SANERT :'      TO KLISTEO-IO-AREA (1:15)
               MOVE ANTSAN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               MOVE ' ORDRERECS:'          TO KLISTEO-IO-AREA (26:11)
               MOVE ANTOSA                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (37:10)
               MOVE ' SUMRECS:'            TO KLISTEO-IO-AREA (47:9)
               MOVE ANTSSA                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (56:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL UT     :'      TO KLISTEO-IO-AREA (1:15)
               MOVE ANTUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               MOVE ' ORDRERECS:'          TO KLISTEO-IO-AREA (26:11)
               MOVE ANTOUT                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (37:10)
               MOVE ' SUMRECS:'            TO KLISTEO-IO-AREA (47:9)
               MOVE ANTSUT                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (56:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANT FIRMA INN :'      TO KLISTEO-IO-AREA (1:15)
               MOVE LRFIRI                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANT FIRMA SAN :'      TO KLISTEO-IO-AREA (1:15)
               MOVE LRFIRS                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (16:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-LR AND I-02)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE ' '                    TO KLISTEO-IO-AREA (24:1)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
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
           SET KORDREI-LEVEL-INIT          TO TRUE
           INITIALIZE KORDREI-DATA-FIELDS
           SET KORDREI-EOF-OFF             TO TRUE
           SET KORDREI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KORDREI-MC
                                              KORDREI-MP
           OPEN INPUT KORDREI
           SET FIRMAKT-LEVEL-INIT          TO TRUE
           INITIALIZE FIRMAKT-DATA-FIELDS
           SET FIRMAKT-EOF-OFF             TO TRUE
           SET FIRMAKT-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FIRMAKT-MC
                                              FIRMAKT-MP
           OPEN INPUT FIRMAKT
           OPEN OUTPUT KORDREO
           OPEN OUTPUT ORDSANO
           IF I-U1
               OPEN OUTPUT KLISTEO
           END-IF
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES
           IF I-U4
               OPEN OUTPUT BUGFILO
           END-IF
           INITIALIZE BUGFILO-IO-AREA
           INITIALIZE BUGFILO-DATA-FIELDS
           MOVE 57                         TO BUGFILO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KORDREI
           CLOSE FIRMAKT
           CLOSE KORDREO
           CLOSE ORDSANO
           IF I-U1
               CLOSE KLISTEO
           END-IF
           IF I-U4
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
