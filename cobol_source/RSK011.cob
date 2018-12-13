       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK011R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMNAVN: RSK011, PLUKKER UT TRANSER TIL FEILLISTE NÅR     *
      *                      MATCH MOT FEILFILE                       *
      * LAGET AV   : MORTEN TUVRØNNINGEN MAR 2006                     *
      * E 15.04.11 : UTVIDET REGFIL FRA 120 TIL 240 BYTE              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK011.rpg
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
           SELECT TILRELI
               ASSIGN TO UT-S-TILRELI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILRELI-STATUS.
           SELECT FEILFIL
               ASSIGN TO UT-S-FEILFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FEILFIL-STATUS.
           SELECT TILRELO
               ASSIGN TO UT-S-TILRELO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILRELO-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TILRELI
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  TILRELI-IO-AREA.
           05  TILRELI-IO-AREA-X           PICTURE X(240).
       FD FEILFIL
               BLOCK CONTAINS 30
               RECORD CONTAINS 3.
       01  FEILFIL-IO-AREA.
           05  FEILFIL-IO-AREA-X           PICTURE X(3).
       FD TILRELO
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  TILRELO-IO-AREA.
           05  TILRELO-IO-AREA-X           PICTURE X(240).
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
           10  TILRELI-STATUS              PICTURE 99 VALUE 0.
           10  FEILFIL-STATUS              PICTURE 99 VALUE 0.
           10  TILRELO-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELI-EOF-OFF         VALUE '0'.
               88  TILRELI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELI-READ-OFF        VALUE '0'.
               88  TILRELI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELI-PROCESS-OFF     VALUE '0'.
               88  TILRELI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FEILFIL-EOF-OFF         VALUE '0'.
               88  FEILFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FEILFIL-READ-OFF        VALUE '0'.
               88  FEILFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FEILFIL-PROCESS-OFF     VALUE '0'.
               88  FEILFIL-PROCESS         VALUE '1'.
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
           05  TILRELI-DATA-FIELDS.
               10  TFIRMA                  PICTURE X(3).
               10  REC240                  PICTURE X(240).
           05  TILRELI-MP                  PICTURE X(3).
           05  TILRELI-MC                  PICTURE X(3).
           05  TILRELI-M-01            REDEFINES TILRELI-MC.
               10  TILRELI-M-01-M1.
                   15  TILRELI-M-01-M1-TFIRMA-G.
                       20  TILRELI-M-01-M1-TFIRMA PICTURE X(3).
           05  FEILFIL-DATA-FIELDS.
               10  FFIRMA                  PICTURE X(3).
           05  FEILFIL-MP                  PICTURE X(3).
           05  FEILFIL-MC                  PICTURE X(3).
           05  FEILFIL-M-02            REDEFINES FEILFIL-MC.
               10  FEILFIL-M-02-M1.
                   15  FEILFIL-M-02-M1-FFIRMA-G.
                       20  FEILFIL-M-02-M1-FFIRMA PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTFIR-IO.
                   15  ANTFIR              PICTURE S9(4).
           05  EDITTING-FIELDS.
               10  XO-40YY9R               PICTURE Z.ZZ9-.
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  TILRELI-PROCESS
               SET TILRELI-PROCESS-OFF     TO TRUE
               SET TILRELI-READ            TO TRUE
           END-IF
 
           IF  TILRELI-READ
               PERFORM TILRELI-GET
               SET TILRELI-READ-OFF        TO TRUE
               IF  NOT TILRELI-EOF
                   PERFORM TILRELI-MATCH-SET
               END-IF
           END-IF
 
           IF  FEILFIL-PROCESS
               SET FEILFIL-PROCESS-OFF     TO TRUE
               SET FEILFIL-READ            TO TRUE
           END-IF
 
           IF  FEILFIL-READ
               PERFORM FEILFIL-GET
               SET FEILFIL-READ-OFF        TO TRUE
               IF  NOT FEILFIL-EOF
                   PERFORM FEILFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  TILRELI-PROCESS
               PERFORM TILRELI-IDSET
           END-IF
 
           IF  FEILFIL-PROCESS
               PERFORM FEILFIL-IDSET
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  TILRELI-PROCESS
               PERFORM TILRELI-FLDSET
           END-IF
 
           IF  FEILFIL-PROCESS
               PERFORM FEILFIL-FLDSET
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
           IF  (I-02 AND I-MR)
               ADD 1                       TO ANTFIR
           END-IF.
 
       TILRELI-GET SECTION.
       TILRELI-GET-P.
           IF  TILRELI-EOF-OFF
               READ TILRELI
               AT END
                   SET TILRELI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TILRELI-FLDSET SECTION.
       TILRELI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE TILRELI-IO-AREA (3:3)  TO TFIRMA (1:3)
               MOVE TILRELI-IO-AREA (1:240) TO REC240 (1:240)
           END-EVALUATE.
 
       TILRELI-IDSET SECTION.
       TILRELI-IDSET-P.
           SET I-01                        TO TRUE.
 
       TILRELI-MATCH-SET SECTION.
       TILRELI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE TILRELI-IO-AREA (3:3)  TO TILRELI-M-01-M1-TFIRMA
           END-EVALUATE.
 
       FEILFIL-GET SECTION.
       FEILFIL-GET-P.
           IF  FEILFIL-EOF-OFF
               READ FEILFIL
               AT END
                   SET FEILFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FEILFIL-FLDSET SECTION.
       FEILFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FEILFIL-IO-AREA (1:3)  TO FFIRMA (1:3)
           END-EVALUATE.
 
       FEILFIL-IDSET SECTION.
       FEILFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       FEILFIL-MATCH-SET SECTION.
       FEILFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FEILFIL-IO-AREA (1:3)  TO FEILFIL-M-02-M1-FFIRMA
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  TILRELI-EOF
               MOVE HIGH-VALUES            TO TILRELI-MC
                                              TILRELI-MP
           END-IF
           IF  FEILFIL-EOF
               MOVE HIGH-VALUES            TO FEILFIL-MC
                                              FEILFIL-MP
           END-IF
           IF  TILRELI-MC < TILRELI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FEILFIL-MC < FEILFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  TILRELI-MC < FEILFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILRELI-PROCESS     TO TRUE
                   MOVE TILRELI-MC         TO TILRELI-MP
                   IF  TILRELI-MC = FEILFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FEILFIL-MC < TILRELI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FEILFIL-PROCESS     TO TRUE
                   MOVE FEILFIL-MC         TO FEILFIL-MP
                   IF  FEILFIL-MC = TILRELI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  TILRELI-MC = FEILFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILRELI-PROCESS     TO TRUE
                   MOVE TILRELI-MC         TO TILRELI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE SPACES TO TILRELO-IO-AREA
               INITIALIZE TILRELO-IO-AREA
               MOVE REC240                 TO TILRELO-IO-AREA (1:240)
               WRITE TILRELO-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FIRMA : '      TO LISTE-IO-AREA (1:15)
               MOVE ANTFIR                 TO XO-40YY9R
               MOVE XO-40YY9R              TO LISTE-IO-AREA (17:6)
               IF  I-U8
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE TILRELI-DATA-FIELDS
           SET TILRELI-EOF-OFF             TO TRUE
           SET TILRELI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO TILRELI-MC
                                              TILRELI-MP
           OPEN INPUT TILRELI
           INITIALIZE FEILFIL-DATA-FIELDS
           SET FEILFIL-EOF-OFF             TO TRUE
           SET FEILFIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FEILFIL-MC
                                              FEILFIL-MP
           OPEN INPUT FEILFIL
           OPEN OUTPUT TILRELO
           IF I-U8
               OPEN OUTPUT LISTE
           END-IF
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILRELI
           CLOSE FEILFIL
           CLOSE TILRELO
           IF I-U8
               CLOSE LISTE
           END-IF.
 
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
