       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBS010R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: BBS010                          XX2000XXIRXXMT       *
      * DANNER KRYSSREF RESKNR/BANKKONTO.                             *
      * LAGET 04.02.03                                                *
      * E 12.03.03: SKRIVER IKKE RECORD N≈R RESKNR IKKE FINNS P≈      *
      *             KUNDEMASTER.                                      *
      * E 06.10.06: UTVIDET RESKBNK FRA 20 TIL 30 BYTE                *
      *             SKRIVER OPPHAV (KID) + DATO P≈ FILEN              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BBS010.rpg
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
           SELECT BBSRECI
               ASSIGN TO UT-S-BBSRECI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSRECI-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT RELMAST
               ASSIGN TO RELMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RELMAST-STATUS
               RECORD KEY IS RELMAST-KEY1.
           SELECT KUNBNKO
               ASSIGN TO UT-S-KUNBNKO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KUNBNKO-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BBSRECI
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  BBSRECI-IO-AREA.
           05  BBSRECI-IO-AREA-X           PICTURE X(80).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD RELMAST
               RECORD CONTAINS 80.
       01  RELMAST-IO-AREA.
           05  RELMAST-IO-AREA-X.
               10  RELMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(60).
       FD KUNBNKO
               BLOCK CONTAINS 300
               RECORD CONTAINS 30.
       01  KUNBNKO-IO-AREA.
           05  KUNBNKO-IO-AREA-X           PICTURE X(30).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BBSRECI-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  RELMAST-STATUS              PICTURE 99 VALUE 0.
           10  KUNBNKO-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSRECI-EOF-OFF         VALUE '0'.
               88  BBSRECI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSRECI-READ-OFF        VALUE '0'.
               88  BBSRECI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSRECI-PROCESS-OFF     VALUE '0'.
               88  BBSRECI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BBSRECI-LEVEL-INIT-OFF  VALUE '0'.
               88  BBSRECI-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RELMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  BBSRECI-LEVEL-03.
               10  BBSRECI-03-L1.
                   15  BBSRECI-03-L1-FIRMA PICTURE X(3).
           05  BBSRECI-DATA-FIELDS.
               10  BBSKTO                  PICTURE X(11).
               10  FIRMA                   PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  FAKID                   PICTURE X(1).
               10  FKREF1                  PICTURE X(1).
               10  FKREF                   PICTURE X(6).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  BNKKTO                  PICTURE X(11).
           05  KUNDEMA-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  RELMAST-DATA-FIELDS.
               10  RELKNR                  PICTURE X(6).
      * N11                MOVE UDATE     WDATO   6         DD,MM,≈≈
      * N11                MOVELWDATO     WDAG    2         DD
      * N11                MOVELWDATO     WDDMM   4         DDMM
      * N11                MOVE WDDMM     WMND    2         MM
      * N11                MOVE UDATE     WAAR    2         ≈≈
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FFNR                    PICTURE X(3).
               10  RELKY4                  PICTURE X(4).
               10  RELKEY                  PICTURE X(20).
               10  RELK16                  PICTURE X(16).
               10  RESKEY                  PICTURE X(9).
               10  TBEL-IO.
                   15  TBEL                PICTURE S9(7)V9(2).
               10  TANT-IO.
                   15  TANT                PICTURE S9(5).
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-10                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BBSRECI-PROCESS
               SET BBSRECI-PROCESS-OFF     TO TRUE
               SET BBSRECI-READ            TO TRUE
           END-IF
 
           IF  BBSRECI-READ
           AND RECORD-SELECTED-OFF
               PERFORM BBSRECI-GET
               SET BBSRECI-READ-OFF        TO TRUE
               IF  NOT BBSRECI-EOF
                   SET BBSRECI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BBSRECI-PROCESS
               PERFORM BBSRECI-IDSET
           END-IF
 
           IF  BBSRECI-PROCESS
               PERFORM BBSRECI-CHK-LEVEL
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
 
           IF  BBSRECI-PROCESS
               PERFORM BBSRECI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BBSRECI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-03)
               SET NOT-I-72                TO TRUE
               IF  FIRMA NOT = FFNR
                   SET I-72                TO TRUE
               END-IF
               MOVE FIRMA                  TO FFNR
           END-IF
           IF  (I-04)
               SET NOT-I-32                TO TRUE
               SET NOT-I-33                TO TRUE
               SET NOT-I-32                TO TRUE
               IF  FAKID = 'F'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-32)
               SET NOT-I-33                TO TRUE
               IF  FKREF1 = '0'
                   SET I-33                TO TRUE
               END-IF
               MOVE 'A'                    TO RELKY4 (1:1)
               MOVE FIRMA                  TO RELKY4 (2:3)
               MOVE RELKY4                 TO RELKEY (1:4)
               MOVE FKREF                  TO RELK16 (1:6)
               MOVE '000'                  TO RELK16 (14:3)
           END-IF
           IF  (I-04 AND I-33)
               MOVE '001'                  TO RELK16 (14:3)
           END-IF
           IF  (I-04 AND I-32)
               MOVE RELK16                 TO RELKEY (5:16)
           END-IF
           IF  (I-04)
               MOVE RELKEY                 TO RELMAST-KEY1
               READ RELMAST RECORD KEY IS RELMAST-KEY1
               INVALID KEY
                   SET I-22                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-22            TO TRUE
                   PERFORM RELMAST-FLDSET
                   PERFORM RELMAST-IDSET
               END-READ
      *  04 32             MOVE "RELKEY  "BUGFL2  8        LEDETXT DEBUG
      *  04 32   BUGFL2    DEBUGBUGFILO   RELKEY           VIS FELT/IND
           END-IF
           IF  (I-04)
               MOVE FIRMA                  TO RESKEY (1:3)
           END-IF
           IF  (I-04 AND NOT-I-32)
               MOVE KNR                    TO RESKEY (4:6)
           END-IF
           IF  (I-04 AND I-32 AND NOT-I-22)
               MOVE RELKNR                 TO RESKEY (4:6)
           END-IF
           IF  (I-04 AND I-32 AND I-22)
               MOVE '      '               TO RESKEY (4:6)
           END-IF
           IF  (I-04)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM KUNDEMA-IDSET
               END-READ
               ADD BEL                     TO TBEL
               ADD 1                       TO TANT
      ******************************************************
           END-IF
           .
 
       BBSRECI-GET SECTION.
       BBSRECI-GET-P.
           IF  BBSRECI-EOF-OFF
               READ BBSRECI
               AT END
                   SET BBSRECI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BBSRECI-FLDSET SECTION.
       BBSRECI-FLDSET-P.
           EVALUATE TRUE
           WHEN ( BBSRECI-IO-AREA (7:1) = '2'
            AND   BBSRECI-IO-AREA (8:1) = '0' )
               MOVE BBSRECI-IO-AREA (70:11) TO BBSKTO (1:11)
               MOVE BBSRECI-IO-AREA (57:3) TO FIRMA (1:3)
           WHEN ( BBSRECI-IO-AREA (7:1) = '3'
            AND   BBSRECI-IO-AREA (8:1) = '0' )
               MOVE BBSRECI-IO-AREA (56:6) TO KNR (1:6)
               MOVE BBSRECI-IO-AREA (67:1) TO FAKID (1:1)
               MOVE BBSRECI-IO-AREA (68:1) TO FKREF1 (1:1)
               MOVE BBSRECI-IO-AREA (68:6) TO FKREF (1:6)
               MOVE BBSRECI-IO-AREA (41:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
           WHEN ( BBSRECI-IO-AREA (7:1) = '3'
            AND   BBSRECI-IO-AREA (8:1) = '1' )
               MOVE BBSRECI-IO-AREA (48:11) TO BNKKTO (1:11)
           END-EVALUATE.
 
       BBSRECI-IDSET SECTION.
       BBSRECI-IDSET-P.
           EVALUATE TRUE
           WHEN ( BBSRECI-IO-AREA (7:1) = '2'
            AND   BBSRECI-IO-AREA (8:1) = '0' )
               SET I-03                    TO TRUE
           WHEN ( BBSRECI-IO-AREA (7:1) = '3'
            AND   BBSRECI-IO-AREA (8:1) = '0' )
               SET I-04                    TO TRUE
           WHEN ( BBSRECI-IO-AREA (7:1) = '3'
            AND   BBSRECI-IO-AREA (8:1) = '1' )
               SET I-06                    TO TRUE
           WHEN  OTHER
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       BBSRECI-CHK-LEVEL SECTION.
       BBSRECI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( BBSRECI-IO-AREA (7:1) = '2'
            AND   BBSRECI-IO-AREA (8:1) = '0' )
               MOVE LOW-VALUES             TO BBSRECI-LEVEL-03
               MOVE BBSRECI-IO-AREA (57:3) TO BBSRECI-03-L1-FIRMA
               IF  BBSRECI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BBSRECI-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BBSRECI-03-L1         TO THE-PRIOR-L1
               SET BBSRECI-LEVEL-INIT      TO TRUE
           WHEN ( BBSRECI-IO-AREA (7:1) = '3'
            AND   BBSRECI-IO-AREA (8:1) = '0' )
               CONTINUE
           WHEN ( BBSRECI-IO-AREA (7:1) = '3'
            AND   BBSRECI-IO-AREA (8:1) = '1' )
               CONTINUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-09                        TO TRUE.
 
       RELMAST-FLDSET SECTION.
       RELMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMAST-IO-AREA (21:6) TO RELKNR (1:6)
           END-EVALUATE.
 
       RELMAST-IDSET SECTION.
       RELMAST-IDSET-P.
           SET I-10                        TO TRUE.
 
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-06 AND NOT-I-21)
               MOVE SPACES TO KUNBNKO-IO-AREA
               INITIALIZE KUNBNKO-IO-AREA
               MOVE FIRMA                  TO KUNBNKO-IO-AREA (1:3)
               MOVE BNKKTO                 TO KUNBNKO-IO-AREA (4:11)
               MOVE KNR                    TO KUNBNKO-IO-AREA (15:6)
               IF  (I-32)
                   MOVE RELKNR             TO KUNBNKO-IO-AREA (15:6)
      *                30                24 "KID "
      *                31      WAAR      26
      *                31      WMND      28
      *                31      WDAG      30
               END-IF
               WRITE KUNBNKO-IO-AREA
           END-IF
           IF  (I-06 AND NOT-I-21)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KNR                    TO LISTE-IO-AREA (1:6)
               IF  (I-32)
                   MOVE RELKNR             TO LISTE-IO-AREA (1:6)
               END-IF
               MOVE BNKKTO                 TO LISTE-IO-AREA (9:11)
      *****************************************************************
      * DUMMY-LINJE FOR ≈ LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE 'DANNING AV RELASJON RESK' TO LISTE-IO-AREA (7:24)
               MOVE 'NR/BANKKONTO'         TO LISTE-IO-AREA (31:12)
               MOVE BBSKTO                 TO LISTE-IO-AREA (45:11)
               IF  I-U1
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESKNR'               TO LISTE-IO-AREA (1:6)
               MOVE 'BANKKONTONR'          TO LISTE-IO-AREA (9:11)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (57:24)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE 'DANNING AV RELASJON RESK' TO LISTE-IO-AREA (7:24)
               MOVE 'NR/BANKKONTO'         TO LISTE-IO-AREA (31:12)
               MOVE BBSKTO                 TO LISTE-IO-AREA (45:11)
               IF  I-U1
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESKNR'               TO LISTE-IO-AREA (1:6)
               MOVE 'BANKKONTONR'          TO LISTE-IO-AREA (9:11)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (57:24)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-U8 AND I-05 AND I-09)
           AND (I-10 AND I-72)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (1:1)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
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
           MOVE 1                          TO LR-CHECK
           SET BBSRECI-LEVEL-INIT          TO TRUE
           INITIALIZE BBSRECI-DATA-FIELDS
           SET BBSRECI-EOF-OFF             TO TRUE
           SET BBSRECI-PROCESS             TO TRUE
           OPEN INPUT BBSRECI
           OPEN INPUT KUNDEMA
           INITIALIZE RELMAST-DATA-FIELDS
           OPEN INPUT RELMAST
           OPEN OUTPUT KUNBNKO
           IF I-U1
               OPEN OUTPUT LISTE
           END-IF
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BBSRECI
           CLOSE KUNDEMA
           CLOSE RELMAST
           CLOSE KUNBNKO
           IF I-U1
               CLOSE LISTE
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
