       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS050R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: VOS045, SANERING MEKANIKER STATISTIKK.       *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VOS02A                                       *
      *  LAGET DATO....: 10.10.96                                     *
      *  ENDRET........:                                              *
      *  INPUT......: DAGENS VERKSTEDORDRE,                           *
      *               ORDRENR MASTER.                                 *
      *  BEHANDLING.: MERKER ORDRENR MASTER MED AT ORDRE ER TATT MED  *
      *               PÅ MEKANIKER-STATISTIKK.                        *
      *  OUTPUT.....: LISTE,                                          *
      *               ORDRENR MASTER.                                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS050.rpg
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
           SELECT DAGMEKI
               ASSIGN TO UT-S-DAGMEKI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGMEKI-STATUS.
           SELECT ORDNRM
               ASSIGN TO ORDNRM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDNRM-STATUS
               RECORD KEY IS ORDNRM-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD DAGMEKI
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  DAGMEKI-IO-AREA.
           05  DAGMEKI-IO-AREA-X           PICTURE X(60).
       FD ORDNRM
               RECORD CONTAINS 100.
       01  ORDNRM-IO-AREA.
           05  ORDNRM-IO-AREA-X.
               10  ORDNRM-KEY1             PICTURE X(9).
               10  FILLER                  PICTURE X(91).
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  DAGMEKI-STATUS              PICTURE 99 VALUE 0.
           10  ORDNRM-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
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
           05  ORDNRM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  DAGMEKI-LEVEL-01.
               10  DAGMEKI-01-L1.
                   15  DAGMEKI-01-L1-DMFNR PICTURE X(3).
           05  DAGMEKI-DATA-FIELDS.
               10  DMFNR                   PICTURE X(3).
               10  DMONR                   PICTURE X(6).
           05  ORDNRM-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(6).
               10  ONRKEY                  PICTURE X(9).
               10  TOTORD-IO.
                   15  TOTORD              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YN9R               PICTURE ZZZZZ9-.
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
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  DAGMEKI-PROCESS
               PERFORM DAGMEKI-IDSET
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
           END-IF
           IF  (I-L1)
               SUBTRACT ANTORD             FROM ANTORD
           END-IF
           SET NOT-I-60                    TO TRUE
           MOVE DMFNR                      TO ONRKEY (1:3)
           MOVE DMONR                      TO ONRKEY (4:6)
           MOVE ONRKEY                     TO ORDNRM-KEY1
           READ ORDNRM RECORD KEY IS ORDNRM-KEY1
           INVALID KEY
               SET I-61                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-61                TO TRUE
               PERFORM ORDNRM-IDSET
           END-READ
           IF  (NOT-I-61 AND I-02)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-60)
               ADD 1                       TO ANTORD
               ADD 1                       TO TOTORD
      *                    MOVE "ONRKEY  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGBUGFILO   ONRKEY           VIS INDIKATOR
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
               MOVE DAGMEKI-IO-AREA (1:3)  TO DMFNR (1:3)
               MOVE DAGMEKI-IO-AREA (16:6) TO DMONR (1:6)
           END-EVALUATE.
 
       DAGMEKI-IDSET SECTION.
       DAGMEKI-IDSET-P.
           SET I-01                        TO TRUE.
 
       DAGMEKI-CHK-LEVEL SECTION.
       DAGMEKI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGMEKI-LEVEL-01
               MOVE DAGMEKI-IO-AREA (1:3)  TO DAGMEKI-01-L1-DMFNR
               IF  DAGMEKI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGMEKI-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  DAGMEKI-01-L1         TO THE-PRIOR-L1
               SET DAGMEKI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       ORDNRM-IDSET SECTION.
       ORDNRM-IDSET-P.
           SET I-02                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-60)
               MOVE 'X'                    TO ORDNRM-IO-AREA (77:1)
               REWRITE ORDNRM-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = ORDNRM'
               END-REWRITE
           END-IF
           IF  (I-01 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DMFNR                  TO LISTE-IO-AREA (8:3)
               MOVE DMONR                  TO LISTE-IO-AREA (15:6)
               IF  (I-60)
                   MOVE 'OPPDATERT'        TO LISTE-IO-AREA (32:9)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-99 AND I-U1)
           OR  (I-1P AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MEKANIKER STATISTIKK.' TO LISTE-IO-AREA (30:21)
               MOVE 'JOBB: VOS02A'         TO LISTE-IO-AREA (101:12)
               MOVE 'PROGRAM: VOS050'      TO LISTE-IO-AREA (116:15)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV DAGENS' TO LISTE-IO-AREA (30:21)
               MOVE 'ORDRE.'               TO LISTE-IO-AREA (52:6)
               MOVE 'DATO:'                TO LISTE-IO-AREA (99:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'TIDSPUNKT:'           TO LISTE-IO-AREA (112:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (123:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-99 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (6:5)
               MOVE 'ORDRENR'              TO LISTE-IO-AREA (14:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MEKANIKER STATISTIKK.' TO LISTE-IO-AREA (30:21)
               MOVE 'JOBB: VOS02A'         TO LISTE-IO-AREA (101:12)
               MOVE 'PROGRAM: VOS050'      TO LISTE-IO-AREA (116:15)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV DAGENS' TO LISTE-IO-AREA (30:21)
               MOVE 'ORDRE.'               TO LISTE-IO-AREA (52:6)
               MOVE 'DATO:'                TO LISTE-IO-AREA (99:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'TIDSPUNKT:'           TO LISTE-IO-AREA (112:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (123:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-OF AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (6:5)
               MOVE 'ORDRENR'              TO LISTE-IO-AREA (14:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL ORDRE FIRMA :' TO LISTE-IO-AREA (11:20)
               MOVE ANTORD                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (31:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL ORDRE TOTALT:' TO LISTE-IO-AREA (11:20)
               MOVE TOTORD                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (31:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           SET DAGMEKI-LEVEL-INIT          TO TRUE
           INITIALIZE DAGMEKI-DATA-FIELDS
           SET DAGMEKI-EOF-OFF             TO TRUE
           SET DAGMEKI-PROCESS             TO TRUE
           OPEN INPUT DAGMEKI
           OPEN I-O ORDNRM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE DAGMEKI
           CLOSE ORDNRM
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
