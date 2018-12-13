       IDENTIFICATION DIVISION.
       PROGRAM-ID. STY002R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET ENDRER  LAGERSTYRING PARAMETERFILE  UKENTLIG     *    STY002
      *  KVITTERINGSLISTE PRINTES DERSOM UPSI-1 STÅR PÅ.             *    STY002
      *                                                              *    STY002
      *  PARAMETERE MED STJERNE I KOLONNE 1 BEHANDLES IKKE           *    STY002
      ****************************************************************    STY002
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STY002.rpg
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
           SELECT KORTF
               ASSIGN TO UT-S-KORTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTF-STATUS.
           SELECT LAGPAR
               ASSIGN TO UT-S-LAGPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LAGPAR-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORTF
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KORTF-IO-AREA.
           05  KORTF-IO-AREA-X             PICTURE X(80).
       FD LAGPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  LAGPAR-IO-AREA.
           05  LAGPAR-IO-AREA-X            PICTURE X(200).
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
           10  KORTF-STATUS                PICTURE 99 VALUE 0.
           10  LAGPAR-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTF-EOF-OFF           VALUE '0'.
               88  KORTF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTF-READ-OFF          VALUE '0'.
               88  KORTF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTF-PROCESS-OFF       VALUE '0'.
               88  KORTF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-EOF-OFF          VALUE '0'.
               88  LAGPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-READ-OFF         VALUE '0'.
               88  LAGPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-PROCESS-OFF      VALUE '0'.
               88  LAGPAR-PROCESS          VALUE '1'.
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
           05  KORTF-DATA-FIELDS.
               10  AAR                     PICTURE X(2).
               10  UKE                     PICTURE X(2).
               10  PERDAT-IO.
                   15  PERDAT              PICTURE S9(6).
               10  GRUPPE                  PICTURE X(1).
               10  AVIK                    PICTURE X(1).
               10  MNDNVN                  PICTURE X(36).
               10  NAVN1                   PICTURE X(54).
               10  NAVN2                   PICTURE X(54).
      *                                                                   STY002
           05  LAGPAR-DATA-FIELDS.
               10  PDATO-IO.
                   15  PDATO               PICTURE S9(6).
               10  PGRUPP                  PICTURE X(1).
               10  PAVIK                   PICTURE X(1).
               10  PA-ELGR                 PICTURE X(2).
               10  PUKE                    PICTURE X(2).
               10  PMNDN                   PICTURE X(36).
               10  PNAVN                   PICTURE X(108).
      *                                                                   STY002
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KORTF-PROCESS
               SET KORTF-PROCESS-OFF       TO TRUE
               SET KORTF-READ              TO TRUE
           END-IF
 
           IF  KORTF-READ
           AND RECORD-SELECTED-OFF
               PERFORM KORTF-GET
               SET KORTF-READ-OFF          TO TRUE
               IF  NOT KORTF-EOF
                   PERFORM KORTF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET KORTF-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LAGPAR-PROCESS
               SET LAGPAR-PROCESS-OFF      TO TRUE
               SET LAGPAR-READ             TO TRUE
           END-IF
 
           IF  LAGPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM LAGPAR-GET
               SET LAGPAR-READ-OFF         TO TRUE
               IF  NOT LAGPAR-EOF
                   SET LAGPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KORTF-PROCESS
               PERFORM KORTF-IDSET
           END-IF
 
           IF  LAGPAR-PROCESS
               PERFORM LAGPAR-IDSET
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
 
           IF  KORTF-PROCESS
               PERFORM KORTF-FLDSET
           END-IF
 
           IF  LAGPAR-PROCESS
               PERFORM LAGPAR-FLDSET
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
           IF  (I-01)
               SET I-11                    TO TRUE
           END-IF
           IF  (I-02)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-03)
               SET I-13                    TO TRUE
           END-IF
           IF  (I-04)
               SET I-14                    TO TRUE
           END-IF
           IF  (I-05)
               SET I-15                    TO TRUE
      *                                                                   STY002
           END-IF
           .
 
       KORTF-GET SECTION.
       KORTF-GET-P.
           IF  KORTF-EOF-OFF
               READ KORTF
               AT END
                   SET KORTF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORTF-FLDSET SECTION.
       KORTF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '0' )
               MOVE KORTF-IO-AREA (22:2)   TO AAR (1:2)
               MOVE KORTF-IO-AREA (32:2)   TO UKE (1:2)
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '1' )
               MOVE KORTF-IO-AREA (23:6)   TO PERDAT-IO
               INSPECT PERDAT-IO REPLACING ALL ' ' BY '0'
               MOVE KORTF-IO-AREA (39:1)   TO GRUPPE (1:1)
               MOVE KORTF-IO-AREA (56:1)   TO AVIK (1:1)
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '2' )
               MOVE KORTF-IO-AREA (6:36)   TO MNDNVN (1:36)
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '3' )
               MOVE KORTF-IO-AREA (6:54)   TO NAVN1 (1:54)
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '4' )
               MOVE KORTF-IO-AREA (6:54)   TO NAVN2 (1:54)
           END-EVALUATE.
 
       KORTF-IDCHK SECTION.
       KORTF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '0' )
             OR ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '1' )
             OR ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '2' )
             OR ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '3' )
             OR ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '4' )
             OR ( KORTF-IO-AREA (1:1) NOT = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KORTF-IDSET SECTION.
       KORTF-IDSET-P.
           EVALUATE TRUE
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '2' )
               SET I-03                    TO TRUE
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '3' )
               SET I-04                    TO TRUE
           WHEN ( KORTF-IO-AREA (1:1) = '9'
            AND   KORTF-IO-AREA (2:1) = '4' )
               SET I-05                    TO TRUE
           WHEN ( KORTF-IO-AREA (1:1) NOT = '9' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       LAGPAR-GET SECTION.
       LAGPAR-GET-P.
           IF  LAGPAR-EOF-OFF
               READ LAGPAR
               AT END
                   SET LAGPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LAGPAR-FLDSET SECTION.
       LAGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LAGPAR-IO-AREA (3:6)   TO PDATO-IO
               INSPECT PDATO-IO REPLACING ALL ' ' BY '0'
               MOVE LAGPAR-IO-AREA (9:1)   TO PGRUPP (1:1)
               MOVE LAGPAR-IO-AREA (10:1)  TO PAVIK (1:1)
               MOVE LAGPAR-IO-AREA (11:2)  TO PA-ELGR (1:2)
               MOVE LAGPAR-IO-AREA (13:2)  TO PUKE (1:2)
               MOVE LAGPAR-IO-AREA (21:36) TO PMNDN (1:36)
               MOVE LAGPAR-IO-AREA (57:108) TO PNAVN (1:108)
           END-EVALUATE.
 
       LAGPAR-IDSET SECTION.
       LAGPAR-IDSET-P.
           SET I-08                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-08)
               IF  (I-11)
                   MOVE AAR                TO LAGPAR-IO-AREA (11:2)
               END-IF
               IF  (I-11)
                   MOVE UKE                TO LAGPAR-IO-AREA (13:2)
               END-IF
               IF  (I-12)
                   MOVE PERDAT-IO          TO LAGPAR-IO-AREA (3:6)
               END-IF
               IF  (I-12)
                   MOVE GRUPPE             TO LAGPAR-IO-AREA (9:1)
               END-IF
               IF  (I-12)
                   MOVE AVIK               TO LAGPAR-IO-AREA (10:1)
               END-IF
               IF  (I-13)
                   MOVE MNDNVN             TO LAGPAR-IO-AREA (21:36)
               END-IF
               IF  (I-14)
                   MOVE NAVN1              TO LAGPAR-IO-AREA (57:54)
               END-IF
               IF  (I-15)
                   MOVE NAVN2              TO LAGPAR-IO-AREA (111:54)
               END-IF
               REWRITE LAGPAR-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '******     L A G E R S T' TO LISTE-IO-AREA (1:24)
               MOVE ' Y R I N G S - P A R A M' TO LISTE-IO-AREA (25:24)
               MOVE ' E T E R             ***' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DAGENS DATO    '      TO LISTE-IO-AREA (2:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (18:8)
               MOVE 'GML KJØRE ÅR/UKE'     TO LISTE-IO-AREA (36:16)
               MOVE PA-ELGR                TO LISTE-IO-AREA (53:2)
               MOVE '/'                    TO LISTE-IO-AREA (55:1)
               MOVE PUKE                   TO LISTE-IO-AREA (56:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND I-11)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NY KJØRE ÅR/UKE '     TO LISTE-IO-AREA (36:16)
               MOVE AAR                    TO LISTE-IO-AREA (53:2)
               MOVE '/'                    TO LISTE-IO-AREA (55:1)
               MOVE UKE                    TO LISTE-IO-AREA (56:2)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND NOT-I-11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GML PERIODEDATO'      TO LISTE-IO-AREA (2:15)
               MOVE PDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (18:8)
               MOVE 'FAKTURA-GRUPPE  '     TO LISTE-IO-AREA (36:16)
               MOVE PGRUPP                 TO LISTE-IO-AREA (53:1)
               MOVE 'ÅRLIG AVIK'           TO LISTE-IO-AREA (66:10)
               MOVE PAVIK                  TO LISTE-IO-AREA (77:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND I-12)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NY PERIODEDATO '      TO LISTE-IO-AREA (2:15)
               MOVE PERDAT                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (18:8)
               MOVE 'FAKTURA-GRUPPE  '     TO LISTE-IO-AREA (36:16)
               MOVE GRUPPE                 TO LISTE-IO-AREA (53:1)
               MOVE 'ÅRLIG AVIK'           TO LISTE-IO-AREA (66:10)
               MOVE AVIK                   TO LISTE-IO-AREA (77:1)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND NOT-I-12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MÅNEDSNAVN FORKORTET' TO LISTE-IO-AREA (1:20)
               MOVE PMNDN                  TO LISTE-IO-AREA (25:36)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND I-13)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MÅNEDSNAVN ENDRET TIL' TO LISTE-IO-AREA (1:21)
               MOVE MNDNVN                 TO LISTE-IO-AREA (25:36)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND NOT-I-13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FULLT MÅNEDSNAVN     ' TO LISTE-IO-AREA (1:21)
               MOVE PNAVN                  TO LISTE-IO-AREA (23:108)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND I-14)
           OR  (I-LR AND I-U1 AND I-15)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MÅNEDSNAVN ENDRET TIL' TO LISTE-IO-AREA (1:21)
               IF  (I-14)
                   MOVE NAVN1              TO LISTE-IO-AREA (23:54)
               END-IF
               IF  (I-15)
                   MOVE NAVN2              TO LISTE-IO-AREA (77:54)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '******     DENNE KONTROL' TO LISTE-IO-AREA (1:24)
               MOVE 'L AV PARAMETERET SKAL LE' TO LISTE-IO-AREA (25:24)
               MOVE 'VERES -TORMOD-LEILA  ***' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 01                     TO LISTE-AFTER-SKIP
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
           INITIALIZE KORTF-DATA-FIELDS
           SET KORTF-EOF-OFF               TO TRUE
           SET KORTF-PROCESS               TO TRUE
           OPEN INPUT KORTF
           INITIALIZE LAGPAR-DATA-FIELDS
           SET LAGPAR-EOF-OFF              TO TRUE
           SET LAGPAR-PROCESS              TO TRUE
           OPEN I-O LAGPAR
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KORTF
           CLOSE LAGPAR
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
