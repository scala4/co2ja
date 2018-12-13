       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR101R.
      ******************************************* :   Z-WIN-RPG2P    **
      * PROGRAMMET DANNER OVERSIKT OVER RECORDS SOM ER NYE    SIDEN *
      * FORRIGE KJØRING                                             *
      ***************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR101.rpg
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
           SELECT VAREBUP
               ASSIGN TO UT-S-VAREBUP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREBUP-STATUS.
           SELECT VAREMAS
               ASSIGN TO UT-S-VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREBUP
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VAREBUP-IO-AREA.
           05  VAREBUP-IO-AREA-X           PICTURE X(200).
       FD VAREMAS
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD VAREREC
               BLOCK CONTAINS 9400
               RECORD CONTAINS 200.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(200).
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
           10  VAREBUP-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-EOF-OFF         VALUE '0'.
               88  VAREBUP-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-READ-OFF        VALUE '0'.
               88  VAREBUP-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-PROCESS-OFF     VALUE '0'.
               88  VAREBUP-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
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
           05  VAREBUP-DATA-FIELDS.
               10  EDBKEY                  PICTURE X(7).
               10  BUPREC                  PICTURE X(200).
           05  VAREBUP-MP                  PICTURE X(7).
           05  VAREBUP-MC                  PICTURE X(7).
           05  VAREBUP-M-01            REDEFINES VAREBUP-MC.
               10  VAREBUP-M-01-M1.
                   15  VAREBUP-M-01-M1-EDBKEY-G.
                       20  VAREBUP-M-01-M1-EDBKEY PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  NYREC                   PICTURE X(200).
      *
           05  VAREMAS-MP                  PICTURE X(7).
           05  VAREMAS-MC                  PICTURE X(7).
           05  VAREMAS-M-02            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-02-M1.
                   15  VAREMAS-M-02-M1-EDBKEY-G.
                       20  VAREMAS-M-02-M1-EDBKEY PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  DAT1                    PICTURE X(4).
               10  DAT2                    PICTURE X(6).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  EDATO-IO.
                   15  EDATO               PICTURE S9(7).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(9).
               10  NULL7-IO.
                   15  NULL7               PICTURE S9(7).
               10  NULL5-IO.
                   15  NULL5               PICTURE S9(5).
               10  ANTE1-IO.
                   15  ANTE1               PICTURE S9(6).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(6).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(6).
               10  TOTNMR-IO.
                   15  TOTNMR              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
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
           IF  VAREBUP-PROCESS
               SET VAREBUP-PROCESS-OFF     TO TRUE
               SET VAREBUP-READ            TO TRUE
           END-IF
 
           IF  VAREBUP-READ
               PERFORM VAREBUP-GET
               SET VAREBUP-READ-OFF        TO TRUE
               IF  NOT VAREBUP-EOF
                   PERFORM VAREBUP-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   PERFORM VAREMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  VAREBUP-PROCESS
               PERFORM VAREBUP-IDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VAREBUP-PROCESS
               PERFORM VAREBUP-FLDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDSET
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
           MOVE UMONTH                     TO MND-IO
           MOVE UYEAR                      TO AAR-IO
           MOVE UDAY                       TO DAT1 (1:2)
           MOVE UMONTH                     TO DAT1 (3:2)
           MOVE DAT1                       TO DAT2 (1:4)
           MOVE UYEAR                      TO DAT2 (5:2)
           MOVE DAT2                       TO DATO-IO
           ADD DATO TO ZERO            GIVING EDATO
           MOVE 0                          TO NULL9
           MOVE 0                          TO NULL7
           MOVE 0                          TO NULL5
      *
           IF  (I-02 AND I-MR)
               SET NOT-I-10                TO TRUE
               IF  BUPREC = NYREC
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-10)
               ADD 1                       TO ANTE1
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO ANTMR
           END-IF
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNMR
               ADD 1                       TO TOTNMR
           END-IF.
 
       VAREBUP-GET SECTION.
       VAREBUP-GET-P.
           IF  VAREBUP-EOF-OFF
               READ VAREBUP
               AT END
                   SET VAREBUP-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREBUP-FLDSET SECTION.
       VAREBUP-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREBUP-IO-AREA (6:7)  TO EDBKEY (1:7)
               MOVE VAREBUP-IO-AREA (1:200) TO BUPREC (1:200)
           END-EVALUATE.
 
       VAREBUP-IDSET SECTION.
       VAREBUP-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREBUP-MATCH-SET SECTION.
       VAREBUP-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREBUP-IO-AREA (6:7)  TO VAREBUP-M-01-M1-EDBKEY
           END-EVALUATE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBKEY (1:7)
               MOVE VAREMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREMAS-IO-AREA (1:200) TO NYREC (1:200)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-M-02-M1-EDBKEY
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
           IF  VAREBUP-EOF
               MOVE HIGH-VALUES            TO VAREBUP-MC
                                              VAREBUP-MP
           END-IF
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  VAREBUP-MC < VAREBUP-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREBUP-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREBUP-PROCESS     TO TRUE
                   MOVE VAREBUP-MC         TO VAREBUP-MP
                   IF  VAREBUP-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC < VAREBUP-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = VAREBUP-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREBUP-MC = VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREBUP-PROCESS     TO TRUE
                   MOVE VAREBUP-MC         TO VAREBUP-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE NYREC                  TO VAREREC-IO-AREA (1:200)
               MOVE '962'                  TO VAREREC-IO-AREA (3:3)
               MOVE EDATO                  TO XO-70P
               MOVE XO-70P-EF              TO VAREREC-IO-AREA (84:4)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO VAREREC-IO-AREA (97:5)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO VAREREC-IO-AREA (102:5)
               MOVE AAR-IO                 TO VAREREC-IO-AREA (171:2)
               MOVE MND-IO                 TO VAREREC-IO-AREA (173:2)
      *                                 174 "0509"
               MOVE NULL7                  TO XO-70P
               MOVE XO-70P-EF              TO VAREREC-IO-AREA (175:4)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VAREREC-IO-AREA (179:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VAREREC-IO-AREA (182:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VAREREC-IO-AREA (185:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VAREREC-IO-AREA (188:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VAREREC-IO-AREA (191:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VAREREC-IO-AREA (194:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VAREREC-IO-AREA (197:3)
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MERGE VAREMASTER    ' TO LISTE-IO-AREA (1:20)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (23:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANR.'             TO LISTE-IO-AREA (3:8)
               MOVE 'ANT. MR.'             TO LISTE-IO-AREA (13:8)
               MOVE 'ANT.NMR.'             TO LISTE-IO-AREA (23:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MERGE VAREMASTER    ' TO LISTE-IO-AREA (1:20)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (23:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (8:3)
               MOVE ANTMR                  TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (14:7)
               MOVE ANTNMR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTNMR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'RECORDS ER KOPIERT.     ' TO LISTE-IO-AREA (32:24)
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE VAREBUP-DATA-FIELDS
           SET VAREBUP-EOF-OFF             TO TRUE
           SET VAREBUP-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREBUP-MC
                                              VAREBUP-MP
           OPEN INPUT VAREBUP
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN INPUT VAREMAS
           OPEN OUTPUT VAREREC
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREBUP
           CLOSE VAREMAS
           CLOSE VAREREC
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
