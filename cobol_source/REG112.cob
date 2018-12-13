       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG112R.
      **********************************************  Z-WIN-RPG2   ****
      *    KONV. IFRA REG102 UTVIDET RECORD.     ***TXT***ok ss***    *
      *  PROGRAM.......: REG112, SANERING AV FAKTURA.KID.FILE         *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VSAMUKE                                      *
      *  LAGET DATO....: 14.05.96                                     *
      *  ENDRET........: 26.05.03 UTVIDET ANTALLSFELT FRA LENGDE 5    *
      *                           TIL 6, DA DETTE MEDFØRER DECIMAL    *
      *                                  OVERFLOW.                    *
      *  RETTET........:                                              *
      *  INPUT.........: SEKVENSIELL FAKTURA.KID.FILE (FAKKIDI).      *
      *  BEHANDLING....: FJERNER ALLE FAKIDI-RECORDS SOM IKKE MATCHER *
      *                  RESKONTRO-MASTER.                            *
      *  OUTPUT........: FAKKIDO (SEKV.)                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG112.rpg
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
           SELECT FAKKIDI
               ASSIGN TO UT-S-FAKKIDI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKKIDI-STATUS.
           SELECT RESFILE
               ASSIGN TO UT-S-RESFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILE-STATUS.
           SELECT FAKKIDO
               ASSIGN TO UT-S-FAKKIDO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKKIDO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKKIDI
               BLOCK CONTAINS 4000
               RECORD CONTAINS 40.
       01  FAKKIDI-IO-AREA.
           05  FAKKIDI-IO-AREA-X           PICTURE X(40).
       FD RESFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RESFILE-IO-AREA.
           05  RESFILE-IO-AREA-X           PICTURE X(200).
       FD FAKKIDO
               BLOCK CONTAINS 4000
               RECORD CONTAINS 40.
       01  FAKKIDO-IO-AREA.
           05  FAKKIDO-IO-AREA-X           PICTURE X(40).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      *****************************************************************
      * INPUT: KIDFILE, RESKOMA                                       *
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
           10  FAKKIDI-STATUS              PICTURE 99 VALUE 0.
           10  RESFILE-STATUS              PICTURE 99 VALUE 0.
           10  FAKKIDO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKKIDI-EOF-OFF         VALUE '0'.
               88  FAKKIDI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKKIDI-READ-OFF        VALUE '0'.
               88  FAKKIDI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKKIDI-PROCESS-OFF     VALUE '0'.
               88  FAKKIDI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-EOF-OFF         VALUE '0'.
               88  RESFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-READ-OFF        VALUE '0'.
               88  RESFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-PROCESS-OFF     VALUE '0'.
               88  RESFILE-PROCESS         VALUE '1'.
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
           05  FAKKIDI-DATA-FIELDS.
               10  FAKREC                  PICTURE X(40).
               10  KIDFIR                  PICTURE X(3).
               10  KIDREF                  PICTURE X(6).
               10  KIDBIL                  PICTURE X(6).
               10  KIDRES                  PICTURE X(6).
               10  FOFAAR-IO.
                   15  FOFAAR              PICTURE S9(2).
               10  FOFMND-IO.
                   15  FOFMND              PICTURE S9(2).
           05  FAKKIDI-MP                  PICTURE X(21).
           05  FAKKIDI-MC                  PICTURE X(21).
           05  FAKKIDI-M-01            REDEFINES FAKKIDI-MC.
               10  FAKKIDI-M-01-M4.
                   15  FAKKIDI-M-01-M4-KIDFIR-G.
                       20  FAKKIDI-M-01-M4-KIDFIR PICTURE X(3).
               10  FAKKIDI-M-01-M3.
                   15  FAKKIDI-M-01-M3-KIDRES-G.
                       20  FAKKIDI-M-01-M3-KIDRES PICTURE X(6).
               10  FAKKIDI-M-01-M2.
                   15  FAKKIDI-M-01-M2-KIDBIL-G.
                       20  FAKKIDI-M-01-M2-KIDBIL PICTURE X(6).
               10  FAKKIDI-M-01-M1.
                   15  FAKKIDI-M-01-M1-KIDREF-G.
                       20  FAKKIDI-M-01-M1-KIDREF PICTURE X(6).
           05  RESFILE-DATA-FIELDS.
               10  RESFIR                  PICTURE X(3).
               10  RESRES                  PICTURE X(6).
               10  RESBIL                  PICTURE X(6).
               10  RESREF                  PICTURE X(6).
      *****************************************************************
      * CALC:                                                         *
      *****************************************************************
           05  RESFILE-MP                  PICTURE X(21).
           05  RESFILE-MC                  PICTURE X(21).
           05  RESFILE-M-02            REDEFINES RESFILE-MC.
               10  RESFILE-M-02-M4.
                   15  RESFILE-M-02-M4-RESFIR-G.
                       20  RESFILE-M-02-M4-RESFIR PICTURE X(3).
               10  RESFILE-M-02-M3.
                   15  RESFILE-M-02-M3-RESRES-G.
                       20  RESFILE-M-02-M3-RESRES PICTURE X(6).
               10  RESFILE-M-02-M2.
                   15  RESFILE-M-02-M2-RESBIL-G.
                       20  RESFILE-M-02-M2-RESBIL PICTURE X(6).
               10  RESFILE-M-02-M1.
                   15  RESFILE-M-02-M1-RESREF-G.
                       20  RESFILE-M-02-M1-RESREF PICTURE X(6).
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
               10  ANTGML-IO.
                   15  ANTGML              PICTURE S9(6).
               10  ANTKID-IO.
                   15  ANTKID              PICTURE S9(6).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(6).
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
           IF  FAKKIDI-PROCESS
               SET FAKKIDI-PROCESS-OFF     TO TRUE
               SET FAKKIDI-READ            TO TRUE
           END-IF
 
           IF  FAKKIDI-READ
               PERFORM FAKKIDI-GET
               SET FAKKIDI-READ-OFF        TO TRUE
               IF  NOT FAKKIDI-EOF
                   PERFORM FAKKIDI-MATCH-SET
               END-IF
           END-IF
 
           IF  RESFILE-PROCESS
               SET RESFILE-PROCESS-OFF     TO TRUE
               SET RESFILE-READ            TO TRUE
           END-IF
 
           IF  RESFILE-READ
               PERFORM RESFILE-GET
               SET RESFILE-READ-OFF        TO TRUE
               IF  NOT RESFILE-EOF
                   PERFORM RESFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  FAKKIDI-PROCESS
               PERFORM FAKKIDI-IDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-IDSET
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
 
           IF  FAKKIDI-PROCESS
               PERFORM FAKKIDI-FLDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-FLDSET
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
           END-IF
           SET I-99                        TO TRUE
           IF  (I-01)
               MULTIPLY 12 BY FOFAAR   GIVING ANTRMN
               ADD FOFMND                  TO ANTRMN
               ADD 3                       TO ANTRMN
               SET NOT-I-98                TO TRUE
               IF  ANTRMN NOT < ANTMND
                   SET I-98                TO TRUE
               END-IF
               ADD 1                       TO ANTGML
           END-IF
           IF  (I-01 AND I-MR)
               OR  (I-01 AND I-98)
               ADD 1                       TO ANTKID
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-98)
               ADD 1                       TO ANTSAN
      *                    MOVE "RDN8    "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGFLISTEO   RDN8             VIS INDIKATOR
      *****************************************************************
      * OUTPUT: NY FAKTKID, LISTE                                     *
      *****************************************************************
           END-IF
           .
 
       FAKKIDI-GET SECTION.
       FAKKIDI-GET-P.
           IF  FAKKIDI-EOF-OFF
               READ FAKKIDI
               AT END
                   SET FAKKIDI-EOF         TO TRUE
               END-READ
           END-IF.
 
       FAKKIDI-FLDSET SECTION.
       FAKKIDI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKKIDI-IO-AREA (1:40) TO FAKREC (1:40)
               MOVE FAKKIDI-IO-AREA (2:3)  TO KIDFIR (1:3)
               MOVE FAKKIDI-IO-AREA (5:6)  TO KIDREF (1:6)
               MOVE FAKKIDI-IO-AREA (5:6)  TO KIDBIL (1:6)
               MOVE FAKKIDI-IO-AREA (12:6) TO KIDRES (1:6)
               MOVE FAKKIDI-IO-AREA (27:2) TO FOFAAR-IO
               INSPECT FOFAAR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKKIDI-IO-AREA (29:2) TO FOFMND-IO
               INSPECT FOFMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKKIDI-IDSET SECTION.
       FAKKIDI-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKKIDI-MATCH-SET SECTION.
       FAKKIDI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKKIDI-IO-AREA (2:3)  TO FAKKIDI-M-01-M4-KIDFIR
               MOVE FAKKIDI-IO-AREA (12:6) TO FAKKIDI-M-01-M3-KIDRES
               MOVE FAKKIDI-IO-AREA (5:6)  TO FAKKIDI-M-01-M2-KIDBIL
               MOVE FAKKIDI-IO-AREA (5:6)  TO FAKKIDI-M-01-M1-KIDREF
           END-EVALUATE.
 
       RESFILE-GET SECTION.
       RESFILE-GET-P.
           IF  RESFILE-EOF-OFF
               READ RESFILE
               AT END
                   SET RESFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESFILE-FLDSET SECTION.
       RESFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESFILE-IO-AREA (3:3)  TO RESFIR (1:3)
               MOVE RESFILE-IO-AREA (6:6)  TO RESRES (1:6)
               MOVE RESFILE-IO-AREA (30:6) TO RESBIL (1:6)
               MOVE RESFILE-IO-AREA (36:6) TO RESREF (1:6)
           END-EVALUATE.
 
       RESFILE-IDSET SECTION.
       RESFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       RESFILE-MATCH-SET SECTION.
       RESFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESFILE-IO-AREA (3:3)  TO RESFILE-M-02-M4-RESFIR
               MOVE RESFILE-IO-AREA (6:6)  TO RESFILE-M-02-M3-RESRES
               MOVE RESFILE-IO-AREA (30:6) TO RESFILE-M-02-M2-RESBIL
               MOVE RESFILE-IO-AREA (36:6) TO RESFILE-M-02-M1-RESREF
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
           IF  FAKKIDI-EOF
               MOVE HIGH-VALUES            TO FAKKIDI-MC
                                              FAKKIDI-MP
           END-IF
           IF  RESFILE-EOF
               MOVE HIGH-VALUES            TO RESFILE-MC
                                              RESFILE-MP
           END-IF
           IF  FAKKIDI-MC < FAKKIDI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESFILE-MC < RESFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FAKKIDI-MC < RESFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKKIDI-PROCESS     TO TRUE
                   MOVE FAKKIDI-MC         TO FAKKIDI-MP
                   IF  FAKKIDI-MC = RESFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESFILE-MC < FAKKIDI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESFILE-PROCESS     TO TRUE
                   MOVE RESFILE-MC         TO RESFILE-MP
                   IF  RESFILE-MC = FAKKIDI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKKIDI-MC = RESFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKKIDI-PROCESS     TO TRUE
                   MOVE FAKKIDI-MC         TO FAKKIDI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
           OR  (I-01 AND I-98)
               MOVE SPACES TO FAKKIDO-IO-AREA
               INITIALIZE FAKKIDO-IO-AREA
               MOVE FAKREC                 TO FAKKIDO-IO-AREA (1:40)
               WRITE FAKKIDO-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV FAKTKID.             ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VSAMUKE'          TO KLISTEO-IO-AREA (85:11)
               MOVE 'PROGRAM=REG112'       TO KLISTEO-IO-AREA (97:14)
      *       D  1     01 MR
      *      OR        01 98
      *                        FAKREC    40
      *                98NMR             60 "<<<SISTE OMG>>>"
      *       D  1     01NMRN98
      *                        FAKREC    40
      *                                  60 "<<< SLETTES >>>"
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
               MOVE 'AV FAKTKID.             ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VSAMUKE'          TO KLISTEO-IO-AREA (85:11)
               MOVE 'PROGRAM=REG112'       TO KLISTEO-IO-AREA (97:14)
      *       D  1     01 MR
      *      OR        01 98
      *                        FAKREC    40
      *                98NMR             60 "<<<SISTE OMG>>>"
      *       D  1     01NMRN98
      *                        FAKREC    40
      *                                  60 "<<< SLETTES >>>"
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ GAMMEL FAKTKID' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTGML                 TO XO-60YN9R
               MOVE XO-60YN9R              TO KLISTEO-IO-AREA (34:7)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL FJERNET          ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTSAN                 TO XO-60YN9R
               MOVE XO-60YN9R              TO KLISTEO-IO-AREA (34:7)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ NY FAKTKID    ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTKID                 TO XO-60YN9R
               MOVE XO-60YN9R              TO KLISTEO-IO-AREA (34:7)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF
           IF  (I-U8 AND I-02)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'DUMMY'                TO KLISTEO-IO-AREA (76:5)
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE FAKKIDI-DATA-FIELDS
           SET FAKKIDI-EOF-OFF             TO TRUE
           SET FAKKIDI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FAKKIDI-MC
                                              FAKKIDI-MP
           OPEN INPUT FAKKIDI
           INITIALIZE RESFILE-DATA-FIELDS
           SET RESFILE-EOF-OFF             TO TRUE
           SET RESFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESFILE-MC
                                              RESFILE-MP
           OPEN INPUT RESFILE
           OPEN OUTPUT FAKKIDO
           OPEN OUTPUT KLISTEO
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKKIDI
           CLOSE RESFILE
           CLOSE FAKKIDO
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
