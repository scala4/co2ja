       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG104R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: REG104, SANERING AV REL.MASTER.              *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VSAMUKE                                      *
      *  LAGET DATO....: 10.12.01                                     *
      *  ENDRET........: 08.02.02 LAR RELASJON LIGGE I MIN 3 MND ETTER*
      *                           FORFALL.                            *
      *  ENDRET........: 13.09.02 SAMME REGEL FOR ALLE RELASJONER.    *
      *                           LIGGER I 4 MÅNEDER.                 *
      *  ENDRET........: 13.09.02 MERGER RELMAST MED REFNR SOM LIGGER *
      *                           SOM ÅPEN POST, OG SLETTER IKKE HVIS *
      *                           MATCH.                              *
      *  ENDRET........: 20.11.09 MERGER PÅ FIRMA, ART OG KID OG TAR  *
      *                           IKKE MED SEK.NR I MATCH.            *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG104.rpg
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
           SELECT RELMASI
               ASSIGN TO UT-S-RELMASI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELMASI-STATUS.
           SELECT IKKESAN
               ASSIGN TO UT-S-IKKESAN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS IKKESAN-STATUS.
           SELECT RELMASO
               ASSIGN TO UT-S-RELMASO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELMASO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RELMASI
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  RELMASI-IO-AREA.
           05  RELMASI-IO-AREA-X           PICTURE X(80).
       FD IKKESAN
               BLOCK CONTAINS 200
               RECORD CONTAINS 20.
       01  IKKESAN-IO-AREA.
           05  IKKESAN-IO-AREA-X           PICTURE X(20).
       FD RELMASO
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  RELMASO-IO-AREA.
           05  RELMASO-IO-AREA-X           PICTURE X(80).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      *****************************************************************
      * INPUT: KIDFILE                                                *
      *****************************************************************
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RELMASI-STATUS              PICTURE 99 VALUE 0.
           10  IKKESAN-STATUS              PICTURE 99 VALUE 0.
           10  RELMASO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMASI-EOF-OFF         VALUE '0'.
               88  RELMASI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMASI-READ-OFF        VALUE '0'.
               88  RELMASI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMASI-PROCESS-OFF     VALUE '0'.
               88  RELMASI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  IKKESAN-EOF-OFF         VALUE '0'.
               88  IKKESAN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  IKKESAN-READ-OFF        VALUE '0'.
               88  IKKESAN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  IKKESAN-PROCESS-OFF     VALUE '0'.
               88  IKKESAN-PROCESS         VALUE '1'.
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
           05  RELMASI-DATA-FIELDS.
               10  RELREC                  PICTURE X(80).
               10  MRKEY                   PICTURE X(17).
               10  PROAAR-IO.
                   15  PROAAR              PICTURE S9(2).
               10  PROMND-IO.
                   15  PROMND              PICTURE S9(2).
           05  RELMASI-MP                  PICTURE X(17).
           05  RELMASI-MC                  PICTURE X(17).
           05  RELMASI-M-01            REDEFINES RELMASI-MC.
               10  RELMASI-M-01-M1.
                   15  RELMASI-M-01-M1-MRKEY-G.
                       20  RELMASI-M-01-M1-MRKEY PICTURE X(17).
           05  IKKESAN-DATA-FIELDS.
      *****************************************************************
      * CALC:                                                         *
      *****************************************************************
      *                    MOVE "RELREC  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGBUGFILO   RELREC           VIS INDIKATOR
               10  FILLER                  PICTURE X.
           05  IKKESAN-MP                  PICTURE X(17).
           05  IKKESAN-MC                  PICTURE X(17).
           05  IKKESAN-M-02            REDEFINES IKKESAN-MC.
               10  IKKESAN-M-02-M1.
                   15  IKKESAN-M-02-M1-MRKEY-G.
                       20  IKKESAN-M-02-M1-MRKEY PICTURE X(17).
           05  TEMPORARY-FIELDS.
               10  MNDAAR                  PICTURE X(4).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  ANTMND-IO.
                   15  ANTMND              PICTURE S9(4).
               10  ANTRMN-IO.
                   15  ANTRMN              PICTURE S9(4).
               10  ANTOV-IO.
                   15  ANTOV               PICTURE S9(7).
               10  ANTIOV-IO.
                   15  ANTIOV              PICTURE S9(7).
               10  ANTMRO-IO.
                   15  ANTMRO              PICTURE S9(7).
               10  ANTMRI-IO.
                   15  ANTMRI              PICTURE S9(7).
               10  ANTGML-IO.
                   15  ANTGML              PICTURE S9(7).
               10  ANTREL-IO.
                   15  ANTREL              PICTURE S9(7).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-40YN9R               PICTURE ZZZ9-.
               10  XO-70YN9R               PICTURE ZZZZZZ9-.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RELMASI-PROCESS
               SET RELMASI-PROCESS-OFF     TO TRUE
               SET RELMASI-READ            TO TRUE
           END-IF
 
           IF  RELMASI-READ
               PERFORM RELMASI-GET
               SET RELMASI-READ-OFF        TO TRUE
               IF  NOT RELMASI-EOF
                   PERFORM RELMASI-MATCH-SET
               END-IF
           END-IF
 
           IF  IKKESAN-PROCESS
               SET IKKESAN-PROCESS-OFF     TO TRUE
               SET IKKESAN-READ            TO TRUE
           END-IF
 
           IF  IKKESAN-READ
               PERFORM IKKESAN-GET
               SET IKKESAN-READ-OFF        TO TRUE
               IF  NOT IKKESAN-EOF
                   PERFORM IKKESAN-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  RELMASI-PROCESS
               PERFORM RELMASI-IDSET
           END-IF
 
           IF  IKKESAN-PROCESS
               PERFORM IKKESAN-IDSET
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
 
           IF  RELMASI-PROCESS
               PERFORM RELMASI-FLDSET
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
           IF  (NOT-I-99)
               MOVE UDATE (3:4)            TO MNDAAR
               MOVE MNDAAR (1:2)           TO MND
               MOVE MNDAAR (3:2)           TO AAR-IO
               MULTIPLY 12 BY AAR      GIVING ANTMND
               ADD MND                     TO ANTMND
               SUBTRACT 1                  FROM ANTMND
               SET I-99                    TO TRUE
           END-IF
           SET NOT-I-98                    TO TRUE
           IF  (I-01)
               MULTIPLY 12 BY PROAAR   GIVING ANTRMN
               ADD PROMND                  TO ANTRMN
               ADD 3                       TO ANTRMN
               SET NOT-I-98                TO TRUE
               IF  ANTRMN NOT < ANTMND
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-98)
               ADD 1                       TO ANTOV
           END-IF
           IF  (I-01 AND I-98)
               ADD 1                       TO ANTIOV
           END-IF
           IF  (I-01 AND I-MR AND NOT-I-98)
               ADD 1                       TO ANTMRO
           END-IF
           IF  (I-01 AND I-MR AND I-98)
               ADD 1                       TO ANTMRI
           END-IF
           IF  (I-01 AND NOT-I-98 AND I-MR)
               SET I-98                    TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTGML
           END-IF
           IF  (I-01 AND I-98)
               ADD 1                       TO ANTREL
           END-IF
           IF  (I-01 AND NOT-I-98)
               ADD 1                       TO ANTSAN
      *****************************************************************
      * OUTPUT: NY RELMAST, LISTE                                     *
      *****************************************************************
           END-IF
           .
 
       RELMASI-GET SECTION.
       RELMASI-GET-P.
           IF  RELMASI-EOF-OFF
               READ RELMASI
               AT END
                   SET RELMASI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RELMASI-FLDSET SECTION.
       RELMASI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMASI-IO-AREA (1:80) TO RELREC (1:80)
               MOVE RELMASI-IO-AREA (1:17) TO MRKEY (1:17)
               MOVE RELMASI-IO-AREA (59:2) TO PROAAR-IO
               INSPECT PROAAR-IO REPLACING ALL ' ' BY '0'
               MOVE RELMASI-IO-AREA (61:2) TO PROMND-IO
               INSPECT PROMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RELMASI-IDSET SECTION.
       RELMASI-IDSET-P.
           SET I-01                        TO TRUE.
 
       RELMASI-MATCH-SET SECTION.
       RELMASI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMASI-IO-AREA (1:17) TO RELMASI-M-01-M1-MRKEY
           END-EVALUATE.
 
       IKKESAN-GET SECTION.
       IKKESAN-GET-P.
           IF  IKKESAN-EOF-OFF
               READ IKKESAN
               AT END
                   SET IKKESAN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       IKKESAN-IDSET SECTION.
       IKKESAN-IDSET-P.
           SET I-02                        TO TRUE.
 
       IKKESAN-MATCH-SET SECTION.
       IKKESAN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE IKKESAN-IO-AREA (1:17) TO IKKESAN-M-02-M1-MRKEY
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  RELMASI-EOF
               MOVE HIGH-VALUES            TO RELMASI-MC
                                              RELMASI-MP
           END-IF
           IF  IKKESAN-EOF
               MOVE HIGH-VALUES            TO IKKESAN-MC
                                              IKKESAN-MP
           END-IF
           IF  RELMASI-MC < RELMASI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  IKKESAN-MC < IKKESAN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RELMASI-MC < IKKESAN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RELMASI-PROCESS     TO TRUE
                   MOVE RELMASI-MC         TO RELMASI-MP
                   IF  RELMASI-MC = IKKESAN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  IKKESAN-MC < RELMASI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET IKKESAN-PROCESS     TO TRUE
                   MOVE IKKESAN-MC         TO IKKESAN-MP
                   IF  IKKESAN-MC = RELMASI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RELMASI-MC = IKKESAN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RELMASI-PROCESS     TO TRUE
                   MOVE RELMASI-MC         TO RELMASI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-98)
               MOVE SPACES TO RELMASO-IO-AREA
               INITIALIZE RELMASO-IO-AREA
               MOVE RELREC                 TO RELMASO-IO-AREA (1:80)
               WRITE RELMASO-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-98 AND I-U8)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE RELREC                 TO KLISTEO-IO-AREA (1:80)
               MOVE '<-SAN->'              TO KLISTEO-IO-AREA (74:7)
      *       D  1     01
      *                        RELREC    80
      *                                 100 "< RELREC >"
      *       D  1     02
      *                        RESREC    89
      *                                 100 "< RESREC >"
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV RELMAST.             ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VSAMUKE'          TO KLISTEO-IO-AREA (85:11)
               MOVE 'PROGRAM=REG104'       TO KLISTEO-IO-AREA (97:14)
      *       D  1     01 MR
      *      OR        01 98
      *                        RELREC    80
      *                98NMR             60 "<<<SISTE OMG>>>"
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV RELMAST.             ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VSAMUKE'          TO KLISTEO-IO-AREA (85:11)
               MOVE 'PROGRAM=REG104'       TO KLISTEO-IO-AREA (97:14)
      *       D  1     01 MR
      *      OR        01 98
      *                        RELREC    80
      *                98NMR             60 "<<<SISTE OMG>>>"
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL MND I GRENSE     ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTMND                 TO XO-40YN9R
               MOVE XO-40YN9R              TO KLISTEO-IO-AREA (36:5)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL OVER DATOGRENSE  ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTOV                  TO XO-70YN9R
               MOVE XO-70YN9R              TO KLISTEO-IO-AREA (33:8)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANT IKKE OVER DTO-GRENSE' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTIOV                 TO XO-70YN9R
               MOVE XO-70YN9R              TO KLISTEO-IO-AREA (33:8)
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ RESKOMA M/SAN ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTMRO                 TO XO-70YN9R
               MOVE XO-70YN9R              TO KLISTEO-IO-AREA (33:8)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ RESKOMA USAN  ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTMRI                 TO XO-70YN9R
               MOVE XO-70YN9R              TO KLISTEO-IO-AREA (33:8)
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ GAMMEL RELMAST' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTGML                 TO XO-70YN9R
               MOVE XO-70YN9R              TO KLISTEO-IO-AREA (33:8)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL FJERNET          ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTSAN                 TO XO-70YN9R
               MOVE XO-70YN9R              TO KLISTEO-IO-AREA (33:8)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ NY RELMAST    ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTREL                 TO XO-70YN9R
               MOVE XO-70YN9R              TO KLISTEO-IO-AREA (33:8)
      * DUMMY
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND I-02)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE ' '                    TO KLISTEO-IO-AREA (80:1)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
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
           INITIALIZE RELMASI-DATA-FIELDS
           SET RELMASI-EOF-OFF             TO TRUE
           SET RELMASI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RELMASI-MC
                                              RELMASI-MP
           OPEN INPUT RELMASI
           SET IKKESAN-EOF-OFF             TO TRUE
           SET IKKESAN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO IKKESAN-MC
                                              IKKESAN-MP
           OPEN INPUT IKKESAN
           OPEN OUTPUT RELMASO
           OPEN OUTPUT KLISTEO
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RELMASI
           CLOSE IKKESAN
           CLOSE RELMASO
           IF KLISTEO-IO-AREA NOT = SPACES
             WRITE KLISTEO-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO KLISTEO-IO-AREA
           END-IF
           CLOSE KLISTEO.
 
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
