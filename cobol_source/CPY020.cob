       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPY020R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM.......: CPY020, FLYTTER VERK010 FRA ET FIRMA TIL ET   *
      *                         ANNET.                                *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: CPY06A                                       *
      *  LAGET DATO....: 14.12.01                                     *
      *  ENDRET DATO...:                                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: CPY020.rpg
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
           SELECT FIRTAB
               ASSIGN TO UT-S-FIRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRTAB-STATUS.
           SELECT VERKINN
               ASSIGN TO UT-S-VERKINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKINN-STATUS.
           SELECT VERKUT
               ASSIGN TO UT-S-VERKUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRTAB
               BLOCK CONTAINS 10
               RECORD CONTAINS 10.
       01  FIRTAB-IO-AREA.
           05  FIRTAB-IO-AREA-X            PICTURE X(10).
       FD VERKINN
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  VERKINN-IO-AREA.
           05  VERKINN-IO-AREA-X           PICTURE X(120).
       FD VERKUT
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  VERKUT-IO-AREA.
           05  VERKUT-IO-AREA-X            PICTURE X(120).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABFRA-MAX   VALUE 99           PICTURE 9(4) USAGE BINARY.
       77  TABTIL-MAX   VALUE 99           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFRA-TABLE.
               10  TABFRA-ENTRY
                                           OCCURS 99 TIMES
                                           INDEXED BY TABFRA-I
                                                      TABFRA-S
                                                      TABTIL-I
                                                      TABTIL-S.
                   15  TABFRA              PICTURE X(4).
                   15  TABTIL              PICTURE X(6).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FIRTAB-STATUS               PICTURE 99 VALUE 0.
           10  VERKINN-STATUS              PICTURE 99 VALUE 0.
           10  VERKUT-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRTAB-EOF-OFF          VALUE '0'.
               88  FIRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKINN-EOF-OFF         VALUE '0'.
               88  VERKINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKINN-READ-OFF        VALUE '0'.
               88  VERKINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKINN-PROCESS-OFF     VALUE '0'.
               88  VERKINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VERKINN-LEVEL-INIT-OFF  VALUE '0'.
               88  VERKINN-LEVEL-INIT      VALUE '1'.
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
           05  VERKINN-LEVEL-01.
               10  VERKINN-01-L2.
                   15  VERKINN-01-L2-VKEY  PICTURE X(4).
           05  VERKINN-DATA-FIELDS.
               10  VREC                    PICTURE X(120).
               10  VKEY                    PICTURE X(4).
      *                                      47  47 VAVD
      ******************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  TILFA                   PICTURE X(4).
               10  KODE                    PICTURE X(1).
               10  HJ2                     PICTURE X(2).
               10  TILAVD                  PICTURE X(1).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(6).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(6).
               10  ANTKOP-IO.
                   15  ANTKOP              PICTURE S9(6).
               10  ANTFLY-IO.
                   15  ANTFLY              PICTURE S9(6).
           05  EDITTING-FIELDS.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VERKINN-PROCESS
               SET VERKINN-PROCESS-OFF     TO TRUE
               SET VERKINN-READ            TO TRUE
           END-IF
 
           IF  VERKINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM VERKINN-GET
               SET VERKINN-READ-OFF        TO TRUE
               IF  NOT VERKINN-EOF
                   SET VERKINN-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VERKINN-PROCESS
               PERFORM VERKINN-IDSET
           END-IF
 
           IF  VERKINN-PROCESS
               PERFORM VERKINN-CHK-LEVEL
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
 
           IF  VERKINN-PROCESS
               PERFORM VERKINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VERKINN-PROCESS
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
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-10                TO TRUE
               SET TABFRA-S                TO TABFRA-I
               PERFORM WITH TEST AFTER
                       VARYING TABFRA-I FROM 1 BY 1
                         UNTIL TABFRA-I >= TABFRA-MAX
                            OR I-10
                   IF  VKEY = TABFRA (TABFRA-I)
                       SET I-10            TO TRUE
                       SET TABFRA-S        TO TABFRA-I
                   END-IF
               END-PERFORM
               SET TABFRA-I                TO TABFRA-S
               IF  I-10
               AND TABFRA-I NOT > TABTIL-MAX
                   SET TABTIL-I            TO TABFRA-I
               END-IF
           END-IF
           IF  (I-L2 AND I-10)
               MOVE TABTIL(TABTIL-I) (1:4) TO TILFA
               MOVE TABTIL(TABTIL-I) (6:1)  TO KODE
               MOVE TABTIL(TABTIL-I) (5:2)  TO HJ2
               MOVE HJ2 (1:1)              TO TILAVD
               SET NOT-I-11                TO TRUE
               IF  KODE = 'K'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  KODE = 'F'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-01 AND NOT-I-10)
               ADD 1                       TO ANTUT
           END-IF
           IF  (I-01 AND I-11)
               ADD 1                       TO ANTKOP
           END-IF
           IF  (I-01 AND I-12)
               ADD 1                       TO ANTFLY
           END-IF
           IF  (I-01 AND I-11)
               ADD 2                       TO ANTUT
           END-IF
           IF  (I-01 AND I-12)
               ADD 1                       TO ANTUT
      *
           END-IF
           .
 
       VERKINN-GET SECTION.
       VERKINN-GET-P.
           IF  VERKINN-EOF-OFF
               READ VERKINN
               AT END
                   SET VERKINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VERKINN-FLDSET SECTION.
       VERKINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKINN-IO-AREA (1:120) TO VREC (1:120)
               MOVE VERKINN-IO-AREA (1:4)  TO VKEY (1:4)
           END-EVALUATE.
 
       VERKINN-IDSET SECTION.
       VERKINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       VERKINN-CHK-LEVEL SECTION.
       VERKINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VERKINN-LEVEL-01
               MOVE VERKINN-IO-AREA (1:4)  TO VERKINN-01-L2-VKEY
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-01-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
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
 
       FIRTAB-LOAD SECTION.
       FIRTAB-LOAD-P.
           OPEN INPUT FIRTAB
           SET TABFRA-I                    TO 1
           PERFORM UNTIL FIRTAB-EOF
               READ FIRTAB
               AT END
                   SET FIRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FIRTAB-IO-AREA (1:10) TO TABFRA-ENTRY
                                                            (TABFRA-I)
                   SET TABFRA-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FIRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC                   TO VERKUT-IO-AREA (1:120)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-01 AND I-11)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC                   TO VERKUT-IO-AREA (1:120)
               WRITE VERKUT-IO-AREA
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC                   TO VERKUT-IO-AREA (1:120)
               MOVE TILFA                  TO VERKUT-IO-AREA (1:4)
               MOVE TILAVD                 TO VERKUT-IO-AREA (47:1)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-01 AND I-12)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC                   TO VERKUT-IO-AREA (1:120)
               MOVE TILFA                  TO VERKUT-IO-AREA (1:4)
               MOVE TILAVD                 TO VERKUT-IO-AREA (47:1)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-01 AND I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FRA KEY:'             TO LISTE-IO-AREA (1:8)
               MOVE VKEY                   TO LISTE-IO-AREA (9:4)
               MOVE 'TIL KEY:'             TO LISTE-IO-AREA (14:8)
               MOVE TILFA                  TO LISTE-IO-AREA (23:4)
               MOVE 'AVD:'                 TO LISTE-IO-AREA (28:4)
               MOVE TILAVD                 TO LISTE-IO-AREA (33:1)
      *                        VAVD      33
               IF  (NOT-I-10)
                   MOVE '* UBEH.   * '     TO LISTE-IO-AREA (49:12)
               END-IF
               IF  (I-11)
                   MOVE '* KOPIERT * '     TO LISTE-IO-AREA (49:12)
               END-IF
               IF  (I-12)
                   MOVE '* FLYTTET * '     TO LISTE-IO-AREA (49:12)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INN      :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTINN                 TO XO-60YY9R
               MOVE XO-60YY9R              TO LISTE-IO-AREA (13:8)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FLYTTET  :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTFLY                 TO XO-60YY9R
               MOVE XO-60YY9R              TO LISTE-IO-AREA (13:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KOPIERT  :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTKOP                 TO XO-60YY9R
               MOVE XO-60YY9R              TO LISTE-IO-AREA (13:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'UT       :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTUT                  TO XO-60YY9R
               MOVE XO-60YY9R              TO LISTE-IO-AREA (13:8)
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
           PERFORM FIRTAB-LOAD
           SET VERKINN-LEVEL-INIT          TO TRUE
           INITIALIZE VERKINN-DATA-FIELDS
           SET VERKINN-EOF-OFF             TO TRUE
           SET VERKINN-PROCESS             TO TRUE
           OPEN INPUT VERKINN
           OPEN OUTPUT VERKUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABFRA-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VERKINN
           CLOSE VERKUT
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
