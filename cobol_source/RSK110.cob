       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK110R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK110, MATCHER RELMAST MOT RESKOMA OG DANNER*
      *                  FILE MED RELATERINGER SOM FORTSATT ER ÅPNE   *
      *                  POSTER OG IKKE SKAL SANERES.                 *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VSA.RELMAST                                  *
      *  LAGET DATO....: 21.08.2009                                   *
      *  ENDRET DATO...: 20.11.2009 TAR IKKE MED SEKVENSNUMMER I      *
      *                             I MATCH-FELT FOR SANERING.        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK110.rpg
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
           SELECT RESKOMA
               ASSIGN TO UT-S-RESKOMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKOMA-STATUS.
           SELECT RELMAST
               ASSIGN TO UT-S-RELMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELMAST-STATUS.
           SELECT IKKESAN
               ASSIGN TO UT-S-IKKESAN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS IKKESAN-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESKOMA
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RESKOMA-IO-AREA.
           05  RESKOMA-IO-AREA-X           PICTURE X(200).
       FD RELMAST
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  RELMAST-IO-AREA.
           05  RELMAST-IO-AREA-X           PICTURE X(80).
       FD IKKESAN
               BLOCK CONTAINS 200
               RECORD CONTAINS 20.
       01  IKKESAN-IO-AREA.
           05  IKKESAN-IO-AREA-X           PICTURE X(20).
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
           10  RESKOMA-STATUS              PICTURE 99 VALUE 0.
           10  RELMAST-STATUS              PICTURE 99 VALUE 0.
           10  IKKESAN-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-EOF-OFF         VALUE '0'.
               88  RESKOMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-READ-OFF        VALUE '0'.
               88  RESKOMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-PROCESS-OFF     VALUE '0'.
               88  RESKOMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKOMA-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKOMA-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMAST-EOF-OFF         VALUE '0'.
               88  RELMAST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMAST-READ-OFF        VALUE '0'.
               88  RELMAST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMAST-PROCESS-OFF     VALUE '0'.
               88  RELMAST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RELMAST-LEVEL-INIT-OFF  VALUE '0'.
               88  RELMAST-LEVEL-INIT      VALUE '1'.
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
           05  RESKOMA-LEVEL-01.
               10  RESKOMA-01-L4.
                   15  RESKOMA-01-L4-FIRMA PICTURE X(3).
               10  RESKOMA-01-L3.
                   15  RESKOMA-01-L3-RESKNR PICTURE X(6).
               10  RESKOMA-01-L2.
                   15  RESKOMA-01-L2-BILNR PICTURE X(6).
           05  RESKOMA-DATA-FIELDS.
               10  REC120                  PICTURE X(120).
               10  FIRMA                   PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  BILNR                   PICTURE X(6).
           05  RESKOMA-MP                  PICTURE X(15).
           05  RESKOMA-MC                  PICTURE X(15).
           05  RESKOMA-M-01            REDEFINES RESKOMA-MC.
               10  RESKOMA-M-01-M4.
                   15  RESKOMA-M-01-M4-FIRMA-G.
                       20  RESKOMA-M-01-M4-FIRMA PICTURE X(3).
               10  RESKOMA-M-01-M3.
                   15  RESKOMA-M-01-M3-RESKNR-G.
                       20  RESKOMA-M-01-M3-RESKNR PICTURE X(6).
               10  RESKOMA-M-01-M2.
                   15  RESKOMA-M-01-M2-BILNR-G.
                       20  RESKOMA-M-01-M2-BILNR PICTURE X(6).
           05  RELMAST-LEVEL-02.
               10  RELMAST-02-L4.
                   15  RELMAST-02-L4-FIRMA PICTURE X(3).
               10  RELMAST-02-L3.
                   15  RELMAST-02-L3-RESKNR PICTURE X(6).
               10  RELMAST-02-L2.
                   15  RELMAST-02-L2-BILNR PICTURE X(6).
           05  RELMAST-DATA-FIELDS.
               10  REC080                  PICTURE X(80).
               10  MRKEY                   PICTURE X(17).
           05  RELMAST-MP                  PICTURE X(15).
           05  RELMAST-MC                  PICTURE X(15).
           05  RELMAST-M-02            REDEFINES RELMAST-MC.
               10  RELMAST-M-02-M4.
                   15  RELMAST-M-02-M4-FIRMA-G.
                       20  RELMAST-M-02-M4-FIRMA PICTURE X(3).
               10  RELMAST-M-02-M3.
                   15  RELMAST-M-02-M3-RESKNR-G.
                       20  RELMAST-M-02-M3-RESKNR PICTURE X(6).
               10  RELMAST-M-02-M2.
                   15  RELMAST-M-02-M2-BILNR-G.
                       20  RELMAST-M-02-M2-BILNR PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(7).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(7).
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
           IF  RESKOMA-PROCESS
               SET RESKOMA-PROCESS-OFF     TO TRUE
               SET RESKOMA-READ            TO TRUE
           END-IF
 
           IF  RESKOMA-READ
               PERFORM RESKOMA-GET
               SET RESKOMA-READ-OFF        TO TRUE
               IF  NOT RESKOMA-EOF
                   PERFORM RESKOMA-MATCH-SET
               END-IF
           END-IF
 
           IF  RELMAST-PROCESS
               SET RELMAST-PROCESS-OFF     TO TRUE
               SET RELMAST-READ            TO TRUE
           END-IF
 
           IF  RELMAST-READ
               PERFORM RELMAST-GET
               SET RELMAST-READ-OFF        TO TRUE
               IF  NOT RELMAST-EOF
                   PERFORM RELMAST-MATCH-SET
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
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-IDSET
           END-IF
 
           IF  RELMAST-PROCESS
               PERFORM RELMAST-IDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-CHK-LEVEL
           END-IF
 
           IF  RELMAST-PROCESS
               PERFORM RELMAST-CHK-LEVEL
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
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-FLDSET
           END-IF
 
           IF  RELMAST-PROCESS
               PERFORM RELMAST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKOMA-PROCESS
           OR  RELMAST-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNMR
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO ANTMR
           END-IF.
 
       RESKOMA-GET SECTION.
       RESKOMA-GET-P.
           IF  RESKOMA-EOF-OFF
               READ RESKOMA
               AT END
                   SET RESKOMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKOMA-FLDSET SECTION.
       RESKOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (1:120) TO REC120 (1:120)
               MOVE RESKOMA-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE RESKOMA-IO-AREA (6:6)  TO RESKNR (1:6)
               MOVE RESKOMA-IO-AREA (30:6) TO BILNR (1:6)
           END-EVALUATE.
 
       RESKOMA-IDSET SECTION.
       RESKOMA-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESKOMA-CHK-LEVEL SECTION.
       RESKOMA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKOMA-LEVEL-01
               MOVE RESKOMA-IO-AREA (3:3)  TO RESKOMA-01-L4-FIRMA
               MOVE RESKOMA-IO-AREA (6:6)  TO RESKOMA-01-L3-RESKNR
               MOVE RESKOMA-IO-AREA (30:6) TO RESKOMA-01-L2-BILNR
               IF  RESKOMA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKOMA-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RESKOMA-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESKOMA-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  RESKOMA-01-L4         TO THE-PRIOR-L4
               MOVE  RESKOMA-01-L3         TO THE-PRIOR-L3
               MOVE  RESKOMA-01-L2         TO THE-PRIOR-L2
               SET RESKOMA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESKOMA-MATCH-SET SECTION.
       RESKOMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (3:3)  TO RESKOMA-M-01-M4-FIRMA
               MOVE RESKOMA-IO-AREA (6:6)  TO RESKOMA-M-01-M3-RESKNR
               MOVE RESKOMA-IO-AREA (30:6) TO RESKOMA-M-01-M2-BILNR
           END-EVALUATE.
 
       RELMAST-GET SECTION.
       RELMAST-GET-P.
           IF  RELMAST-EOF-OFF
               READ RELMAST
               AT END
                   SET RELMAST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RELMAST-FLDSET SECTION.
       RELMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMAST-IO-AREA (1:80) TO REC080 (1:80)
               MOVE RELMAST-IO-AREA (1:17) TO MRKEY (1:17)
               MOVE RELMAST-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE RELMAST-IO-AREA (21:6) TO RESKNR (1:6)
               MOVE RELMAST-IO-AREA (27:6) TO BILNR (1:6)
           END-EVALUATE.
 
       RELMAST-IDSET SECTION.
       RELMAST-IDSET-P.
           SET I-02                        TO TRUE.
 
       RELMAST-CHK-LEVEL SECTION.
       RELMAST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RELMAST-LEVEL-02
               MOVE RELMAST-IO-AREA (2:3)  TO RELMAST-02-L4-FIRMA
               MOVE RELMAST-IO-AREA (21:6) TO RELMAST-02-L3-RESKNR
               MOVE RELMAST-IO-AREA (27:6) TO RELMAST-02-L2-BILNR
               IF  RELMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RELMAST-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RELMAST-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RELMAST-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  RELMAST-02-L4         TO THE-PRIOR-L4
               MOVE  RELMAST-02-L3         TO THE-PRIOR-L3
               MOVE  RELMAST-02-L2         TO THE-PRIOR-L2
               SET RELMAST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RELMAST-MATCH-SET SECTION.
       RELMAST-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMAST-IO-AREA (2:3)  TO RELMAST-M-02-M4-FIRMA
               MOVE RELMAST-IO-AREA (21:6) TO RELMAST-M-02-M3-RESKNR
               MOVE RELMAST-IO-AREA (27:6) TO RELMAST-M-02-M2-BILNR
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
           IF  RESKOMA-EOF
               MOVE HIGH-VALUES            TO RESKOMA-MC
                                              RESKOMA-MP
           END-IF
           IF  RELMAST-EOF
               MOVE HIGH-VALUES            TO RELMAST-MC
                                              RELMAST-MP
           END-IF
           IF  RESKOMA-MC < RESKOMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RELMAST-MC < RELMAST-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RESKOMA-MC < RELMAST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKOMA-PROCESS     TO TRUE
                   MOVE RESKOMA-MC         TO RESKOMA-MP
                   IF  RESKOMA-MC = RELMAST-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RELMAST-MC < RESKOMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RELMAST-PROCESS     TO TRUE
                   MOVE RELMAST-MC         TO RELMAST-MP
                   IF  RELMAST-MC = RESKOMA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESKOMA-MC = RELMAST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKOMA-PROCESS     TO TRUE
                   MOVE RESKOMA-MC         TO RESKOMA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR)
               MOVE SPACES TO IKKESAN-IO-AREA
               INITIALIZE IKKESAN-IO-AREA
               MOVE MRKEY                  TO IKKESAN-IO-AREA (1:17)
               MOVE 'ISA'                  TO IKKESAN-IO-AREA (18:3)
               WRITE IKKESAN-IO-AREA
           END-IF
           IF  (I-01 AND I-MR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC120                 TO LISTE-IO-AREA (1:120)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-MR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC080                 TO LISTE-IO-AREA (1:80)
               MOVE '* MATCH     '         TO LISTE-IO-AREA (62:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-MR AND I-U2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC080                 TO LISTE-IO-AREA (1:80)
               MOVE '* IKKE MATCH'         TO LISTE-IO-AREA (62:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ÅPNE POSTER PÅ RESKOMA ' TO LISTE-IO-AREA (48:23)
               MOVE ' SOM ER PÅ RELMAST     ' TO LISTE-IO-AREA (70:23)
               MOVE 'RSK110'               TO LISTE-IO-AREA (105:6)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (125:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RECORD BYTE 1-60    ' TO LISTE-IO-AREA (1:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '  FFFRRRRRR      BBBBBB' TO LISTE-IO-AREA (1:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ÅPNE POSTER PÅ RESKOMA ' TO LISTE-IO-AREA (48:23)
               MOVE ' SOM ER PÅ RELMAST     ' TO LISTE-IO-AREA (70:23)
               MOVE 'RSK110'               TO LISTE-IO-AREA (105:6)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (125:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RECORD BYTE 1-60    ' TO LISTE-IO-AREA (1:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '  FFFRRRRRR      BBBBBB' TO LISTE-IO-AREA (1:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT UTEN MATCH:'      TO LISTE-IO-AREA (1:15)
               MOVE ANTNMR                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (16:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT M/MATCH   :'      TO LISTE-IO-AREA (1:15)
               MOVE ANTMR                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (16:10)
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
           SET RESKOMA-LEVEL-INIT          TO TRUE
           INITIALIZE RESKOMA-DATA-FIELDS
           SET RESKOMA-EOF-OFF             TO TRUE
           SET RESKOMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESKOMA-MC
                                              RESKOMA-MP
           OPEN INPUT RESKOMA
           SET RELMAST-LEVEL-INIT          TO TRUE
           INITIALIZE RELMAST-DATA-FIELDS
           SET RELMAST-EOF-OFF             TO TRUE
           SET RELMAST-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RELMAST-MC
                                              RELMAST-MP
           OPEN INPUT RELMAST
           OPEN OUTPUT IKKESAN
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKOMA
           CLOSE RELMAST
           CLOSE IKKESAN
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
