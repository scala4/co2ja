       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOS014R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: KOS014, SANERING AV KASSE.OPPGJOR            *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VSA.KASSOPP                                  *
      *  LAGET DATO....: 14.03.12                                     *
      *  E 17.07.12....: LESER INN ANTALL MÅNEDER RECS SKAL LAGRES OG *
      *                  SANERER I HHT TIL DET.                       *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KOS014.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT KASSOPI
               ASSIGN TO UT-S-KASSOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KASSOPI-STATUS.
           SELECT FIRMAKT
               ASSIGN TO UT-S-FIRMAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRMAKT-STATUS.
           SELECT KASSOPO
               ASSIGN TO UT-S-KASSOPO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KASSOPO-STATUS.
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
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD KASSOPI
               BLOCK CONTAINS 4000
               RECORD CONTAINS 400.
       01  KASSOPI-IO-AREA.
           05  KASSOPI-IO-AREA-X           PICTURE X(400).
       FD FIRMAKT
               BLOCK CONTAINS 40
               RECORD CONTAINS 4.
       01  FIRMAKT-IO-AREA.
           05  FIRMAKT-IO-AREA-X           PICTURE X(4).
       FD KASSOPO
               BLOCK CONTAINS 4000
               RECORD CONTAINS 400.
       01  KASSOPO-IO-AREA.
           05  KASSOPO-IO-AREA-X           PICTURE X(400).
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
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  KASSOPI-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAKT-STATUS              PICTURE 99 VALUE 0.
           10  KASSOPO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
           10  BUGFILO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPI-EOF-OFF         VALUE '0'.
               88  KASSOPI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPI-READ-OFF        VALUE '0'.
               88  KASSOPI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPI-PROCESS-OFF     VALUE '0'.
               88  KASSOPI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KASSOPI-LEVEL-INIT-OFF  VALUE '0'.
               88  KASSOPI-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-EOF-OFF         VALUE '0'.
               88  FIRMAKT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-READ-OFF        VALUE '0'.
               88  FIRMAKT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-PROCESS-OFF     VALUE '0'.
               88  FIRMAKT-PROCESS         VALUE '1'.
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
           05  PARAM-DATA-FIELDS.
               10  MNDANT-IO.
                   15  MNDANT              PICTURE S9(2).
           05  KASSOPI-LEVEL-01.
               10  KASSOPI-01-L1.
                   15  KASSOPI-01-L1-FIRKEY PICTURE X(3).
           05  KASSOPI-DATA-FIELDS.
               10  FIRKEY                  PICTURE X(3).
               10  DATO                    PICTURE X(8).
               10  BOKFKD                  PICTURE X(1).
               10  REC120                  PICTURE X(120).
               10  REC200                  PICTURE X(200).
               10  REC400                  PICTURE X(200).
           05  KASSOPI-MP                  PICTURE X(3).
           05  KASSOPI-MC                  PICTURE X(3).
           05  KASSOPI-M-01            REDEFINES KASSOPI-MC.
               10  KASSOPI-M-01-M1.
                   15  KASSOPI-M-01-M1-FIRKEY-G.
                       20  KASSOPI-M-01-M1-FIRKEY PICTURE X(3).
           05  FIRMAKT-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  FIRMAKT-MP                  PICTURE X(3).
           05  FIRMAKT-MC                  PICTURE X(3).
           05  FIRMAKT-M-02            REDEFINES FIRMAKT-MC.
               10  FIRMAKT-M-02-M1.
                   15  FIRMAKT-M-02-M1-FIRKEY-G.
                       20  FIRMAKT-M-02-M1-FIRKEY PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  DATO4                   PICTURE X(4).
               10  YY-IO.
                   15  YY                  PICTURE S9(2).
               10  MM-IO.
                   15  MM                  PICTURE S9(2).
               10  SANMM-IO.
                   15  SANMM               PICTURE S9(2).
               10  SANYY-IO.
                   15  SANYY               PICTURE S9(2).
               10  SANDTO                  PICTURE X(8).
               10  BUGFL1                  PICTURE X(8).
               10  KLOKKE-IO.
                   15  KLOKKE              PICTURE S9(6).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(7).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KASSOPI-PROCESS
               SET KASSOPI-PROCESS-OFF     TO TRUE
               SET KASSOPI-READ            TO TRUE
           END-IF
 
           IF  KASSOPI-READ
               PERFORM KASSOPI-GET
               SET KASSOPI-READ-OFF        TO TRUE
               IF  NOT KASSOPI-EOF
                   PERFORM KASSOPI-MATCH-SET
               END-IF
           END-IF
 
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  KASSOPI-PROCESS
               PERFORM KASSOPI-IDSET
           END-IF
 
           IF  FIRMAKT-PROCESS
               PERFORM FIRMAKT-IDSET
           END-IF
 
           IF  KASSOPI-PROCESS
               PERFORM KASSOPI-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  KASSOPI-PROCESS
               PERFORM KASSOPI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KASSOPI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-12)
               SET NOT-I-12                TO TRUE
      * SETTER ANT MÅNEDER KASSOPP-RECS SKAL LAGRES
      * N11                Z-ADD3         MNDANT  20
           END-IF
           IF  (NOT-I-11)
               SET I-12                    TO TRUE
               MOVE '20'                   TO DATO4 (1:2)
               MOVE UYEAR                  TO YY-IO
               MOVE UMONTH                 TO MM-IO
               SET NOT-I-13                TO TRUE
               IF  MM NOT > MNDANT
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-11 AND I-13)
               ADD 12                      TO MM
           END-IF
           IF  (NOT-I-11)
               SUBTRACT MNDANT FROM MM GIVING SANMM
               MOVE MM                     TO DATO4 (3:2)
           END-IF
           IF  (NOT-I-11 AND I-13)
               SUBTRACT 1 FROM YY      GIVING SANYY
           END-IF
           IF  (NOT-I-11 AND NOT-I-13)
               SUBTRACT 0 FROM YY      GIVING SANYY
           END-IF
           IF  (NOT-I-11)
               MOVE SANYY                  TO DATO4 (3:2)
               MOVE DATO4                  TO SANDTO (1:4)
               MOVE SANMM                  TO DATO4 (1:2)
               MOVE UDAY                   TO DATO4 (3:2)
               MOVE DATO4                  TO SANDTO (5:4)
           END-IF
           IF  (NOT-I-11 AND I-U3)
               MOVE 'SANDTO  '             TO BUGFL1
           END-IF
           IF  (NOT-I-11)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO KLOKKE (1:6)
               SET I-11                    TO TRUE
           END-IF
           IF  (I-03)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  (I-01)
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-01 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  BOKFKD = 'J'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-10)
               SET NOT-I-10                TO TRUE
               IF  DATO < SANDTO
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-MR)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-01 AND I-10)
               ADD 1                       TO ANTSAN
      *  01N10 U3          MOVE "REC120  "BUGFL1  8        LEDETXT DEBUG
      *  01N10 U3BUGFL1    DEBUGBUGFILO   REC120           VIS FELT/IND
           END-IF
           IF  (I-01 AND NOT-I-10)
               ADD 1                       TO ANTUT
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARAM-IO-AREA (47:2)   TO MNDANT-IO
               INSPECT MNDANT-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-03                        TO TRUE.
 
       KASSOPI-GET SECTION.
       KASSOPI-GET-P.
           IF  KASSOPI-EOF-OFF
               READ KASSOPI
               AT END
                   SET KASSOPI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KASSOPI-FLDSET SECTION.
       KASSOPI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KASSOPI-IO-AREA (1:3)  TO FIRKEY (1:3)
               MOVE KASSOPI-IO-AREA (7:8)  TO DATO (1:8)
               MOVE KASSOPI-IO-AREA (123:1) TO BOKFKD (1:1)
               MOVE KASSOPI-IO-AREA (1:120) TO REC120 (1:120)
               MOVE KASSOPI-IO-AREA (1:200) TO REC200 (1:200)
               MOVE KASSOPI-IO-AREA (201:200) TO REC400 (1:200)
           END-EVALUATE.
 
       KASSOPI-IDSET SECTION.
       KASSOPI-IDSET-P.
           SET I-01                        TO TRUE.
 
       KASSOPI-CHK-LEVEL SECTION.
       KASSOPI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KASSOPI-LEVEL-01
               MOVE KASSOPI-IO-AREA (1:3)  TO KASSOPI-01-L1-FIRKEY
               IF  KASSOPI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KASSOPI-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KASSOPI-01-L1         TO THE-PRIOR-L1
               SET KASSOPI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KASSOPI-MATCH-SET SECTION.
       KASSOPI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KASSOPI-IO-AREA (1:3)  TO KASSOPI-M-01-M1-FIRKEY
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
 
       FIRMAKT-IDSET SECTION.
       FIRMAKT-IDSET-P.
           SET I-02                        TO TRUE.
 
       FIRMAKT-MATCH-SET SECTION.
       FIRMAKT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAKT-IO-AREA (1:3)  TO FIRMAKT-M-02-M1-FIRKEY
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
           IF  KASSOPI-EOF
               MOVE HIGH-VALUES            TO KASSOPI-MC
                                              KASSOPI-MP
           END-IF
           IF  FIRMAKT-EOF
               MOVE HIGH-VALUES            TO FIRMAKT-MC
                                              FIRMAKT-MP
           END-IF
           IF  KASSOPI-MC < KASSOPI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FIRMAKT-MC < FIRMAKT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KASSOPI-MC < FIRMAKT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KASSOPI-PROCESS     TO TRUE
                   MOVE KASSOPI-MC         TO KASSOPI-MP
                   IF  KASSOPI-MC = FIRMAKT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FIRMAKT-MC < KASSOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRMAKT-PROCESS     TO TRUE
                   MOVE FIRMAKT-MC         TO FIRMAKT-MP
                   IF  FIRMAKT-MC = KASSOPI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KASSOPI-MC = FIRMAKT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KASSOPI-PROCESS     TO TRUE
                   MOVE KASSOPI-MC         TO KASSOPI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO KASSOPO-IO-AREA
               INITIALIZE KASSOPO-IO-AREA
               MOVE REC200                 TO KASSOPO-IO-AREA (1:200)
               MOVE REC400                 TO KASSOPO-IO-AREA (201:200)
               WRITE KASSOPO-IO-AREA
           END-IF
           IF  (I-01 AND I-10 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SANERT:'              TO KLISTEO-IO-AREA (1:7)
               MOVE REC120                 TO KLISTEO-IO-AREA (10:120)
               MOVE BOKFKD                 TO KLISTEO-IO-AREA (131:1)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-10 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'LAGRET:'              TO KLISTEO-IO-AREA (1:7)
               MOVE REC120                 TO KLISTEO-IO-AREA (10:120)
               MOVE BOKFKD                 TO KLISTEO-IO-AREA (131:1)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-12)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV KASSE.OPPGJOR        ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO  :'              TO KLISTEO-IO-AREA (49:7)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (56:8)
               MOVE 'JOBB   : KASSOPP'     TO KLISTEO-IO-AREA (65:16)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KLOKKE:'              TO KLISTEO-IO-AREA (49:7)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (56:8)
               MOVE 'PROGRAM: KOS014 '     TO KLISTEO-IO-AREA (65:16)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SANERER OPPGJØR ELDRE EN' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'N: '                  TO KLISTEO-IO-AREA (25:3)
               MOVE SANDTO                 TO KLISTEO-IO-AREA (28:8)
               MOVE '  ANTALL MÅNEDER RECS SK' TO KLISTEO-IO-AREA
                                                               (36:24)
               MOVE 'AL LAGRES: '          TO KLISTEO-IO-AREA (60:11)
               MOVE MNDANT-IO              TO KLISTEO-IO-AREA (71:2)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
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
               MOVE 'AV KASSE.OPPGJOR        ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO  :'              TO KLISTEO-IO-AREA (49:7)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (56:8)
               MOVE 'JOBB   : KASSOPP'     TO KLISTEO-IO-AREA (65:16)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KLOKKE:'              TO KLISTEO-IO-AREA (49:7)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (56:8)
               MOVE 'PROGRAM: KOS014 '     TO KLISTEO-IO-AREA (65:16)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SANERER OPPGJØR ELDRE EN' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'N: '                  TO KLISTEO-IO-AREA (25:3)
               MOVE SANDTO                 TO KLISTEO-IO-AREA (28:8)
               MOVE '  ANTALL MÅNEDER RECS SK' TO KLISTEO-IO-AREA
                                                               (36:24)
               MOVE 'AL LAGRES: '          TO KLISTEO-IO-AREA (60:11)
               MOVE MNDANT-IO              TO KLISTEO-IO-AREA (71:2)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL FØR SANERING    :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL SANERT          :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTSAN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL ETTER SANERING  :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-LR AND I-02)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE ' '                    TO KLISTEO-IO-AREA (24:1)
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET KASSOPI-LEVEL-INIT          TO TRUE
           INITIALIZE KASSOPI-DATA-FIELDS
           SET KASSOPI-EOF-OFF             TO TRUE
           SET KASSOPI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KASSOPI-MC
                                              KASSOPI-MP
           OPEN INPUT KASSOPI
           SET FIRMAKT-EOF-OFF             TO TRUE
           SET FIRMAKT-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FIRMAKT-MC
                                              FIRMAKT-MP
           OPEN INPUT FIRMAKT
           OPEN OUTPUT KASSOPO
           IF I-U1
               OPEN OUTPUT KLISTEO
           END-IF
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES
           IF I-U3
               OPEN OUTPUT BUGFILO
           END-IF
           INITIALIZE BUGFILO-IO-AREA
           INITIALIZE BUGFILO-DATA-FIELDS
           MOVE 57                         TO BUGFILO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE KASSOPI
           CLOSE FIRMAKT
           CLOSE KASSOPO
           IF I-U1
               CLOSE KLISTEO
           END-IF
           IF I-U3
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
