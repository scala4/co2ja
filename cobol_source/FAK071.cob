       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK071R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM: FAK071                                    *
      * PROGRAMMET FJERNER NULL-ORDRE                      *
      * DISSE ORDRENR BLE OPPDATERT SOM NULL-ORDRE         *
      * PÅ ORDRENRFILE I PROGRAM ORD110.                   *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK071.rpg
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
           SELECT NULLORD
               ASSIGN TO UT-S-NULLORD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NULLORD-STATUS.
           SELECT FAKTIN
               ASSIGN TO UT-S-FAKTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTIN-STATUS.
           SELECT FAKTUT
               ASSIGN TO UT-S-FAKTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUT-STATUS.
           SELECT PRF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD NULLORD
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  NULLORD-IO-AREA.
           05  NULLORD-IO-AREA-X           PICTURE X(40).
       FD FAKTIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKTIN-IO-AREA.
           05  FAKTIN-IO-AREA-X            PICTURE X(200).
       FD FAKTUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKTUT-IO-AREA.
           05  FAKTUT-IO-AREA-X            PICTURE X(200).
       FD PRF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRF-IO-PRINT.
           05  PRF-IO-AREA-CONTROL         PICTURE X VALUE ' '.
        02 PRF-IO-AREA.
           05  PRF-IO-AREA-X               PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  NULLORD-STATUS              PICTURE 99 VALUE 0.
           10  FAKTIN-STATUS               PICTURE 99 VALUE 0.
           10  FAKTUT-STATUS               PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  NULLORD-EOF-OFF         VALUE '0'.
               88  NULLORD-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NULLORD-READ-OFF        VALUE '0'.
               88  NULLORD-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NULLORD-PROCESS-OFF     VALUE '0'.
               88  NULLORD-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-EOF-OFF          VALUE '0'.
               88  FAKTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-READ-OFF         VALUE '0'.
               88  FAKTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-PROCESS-OFF      VALUE '0'.
               88  FAKTIN-PROCESS          VALUE '1'.
           05  PRF-DATA-FIELDS.
               10  PRF-AFTER-SPACE         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-AFTER-SKIP          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SPACE        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SKIP         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-MAX-LINES           PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-LINE-COUNT          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-CLR-IO              PICTURE X VALUE 'Y'.
           05  NULLORD-DATA-FIELDS.
               10  FAKAY1                  PICTURE X(15).
               10  BRKOD1-IO.
                   15  BRKOD1              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  FORDNR                  PICTURE X(6).
               10  UKODE                   PICTURE X(1).
           05  NULLORD-MP                  PICTURE X(24).
           05  NULLORD-MC                  PICTURE X(24).
           05  NULLORD-M-02            REDEFINES NULLORD-MC.
               10  NULLORD-M-02-M3.
                   15  NULLORD-M-02-M3-FAKAY1-G.
                       20  NULLORD-M-02-M3-FAKAY1 PICTURE X(15).
               10  NULLORD-M-02-M2.
                   15  NULLORD-M-02-M2-BRKOD1-G.
                       20  NULLORD-M-02-M2-BRKOD1 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  NULLORD-M-02-M1.
                   15  NULLORD-M-02-M1-FORDNR-G.
                       20  NULLORD-M-02-M1-FORDNR PICTURE X(6).
           05  FAKTIN-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  FAKEY2                  PICTURE X(15).
               10  FAKREC                  PICTURE X(200).
               10  KNRFAK                  PICTURE X(6).
               10  FAKTYP                  PICTURE X(1).
               10  FAKART                  PICTURE X(1).
               10  FAKMTE                  PICTURE X(1).
               10  FRITT                   PICTURE X(1).
               10  BETB                    PICTURE X(2).
               10  BRKODE-IO.
                   15  BRKODE              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ORDNR                   PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  VAREA                   PICTURE X(30).
               10  ARTNR                   PICTURE X(20).
               10  VAREB                   PICTURE X(30).
               10  LEVENH-IO.
                   15  LEVENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  ENHPR-IO.
                   15  ENHPR               PICTURE S9(7)V9(2).
      *****************************************************************
      *  HOVEDRUTINE.                                                 *
      *****************************************************************
           05  FAKTIN-MP                   PICTURE X(24).
           05  FAKTIN-MC                   PICTURE X(24).
           05  FAKTIN-M-03             REDEFINES FAKTIN-MC.
               10  FAKTIN-M-03-M3.
                   15  FAKTIN-M-03-M3-FAKEY2-G.
                       20  FAKTIN-M-03-M3-FAKEY2 PICTURE X(15).
               10  FAKTIN-M-03-M2.
                   15  FAKTIN-M-03-M2-BRKODE-G.
                       20  FAKTIN-M-03-M2-BRKODE PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  FAKTIN-M-03-M1.
                   15  FAKTIN-M-03-M1-ORDNR-G.
                       20  FAKTIN-M-03-M1-ORDNR PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANTFJ-IO.
                   15  ANTFJ               PICTURE S9(7).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  NULLORD-PROCESS
               SET NULLORD-PROCESS-OFF     TO TRUE
               SET NULLORD-READ            TO TRUE
           END-IF
 
           IF  NULLORD-READ
               PERFORM NULLORD-GET
               SET NULLORD-READ-OFF        TO TRUE
               IF  NOT NULLORD-EOF
                   PERFORM NULLORD-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKTIN-PROCESS
               SET FAKTIN-PROCESS-OFF      TO TRUE
               SET FAKTIN-READ             TO TRUE
           END-IF
 
           IF  FAKTIN-READ
               PERFORM FAKTIN-GET
               SET FAKTIN-READ-OFF         TO TRUE
               IF  NOT FAKTIN-EOF
                   PERFORM FAKTIN-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  NULLORD-PROCESS
               PERFORM NULLORD-IDSET
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-IDSET
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
 
           IF  NULLORD-PROCESS
               PERFORM NULLORD-FLDOFF
               PERFORM NULLORD-FLDSET
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-FLDSET
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
           SET NOT-I-11                    TO TRUE
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-22                TO TRUE
               IF  RECART = 'A'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-MR AND NOT-I-21)
               SET I-11                    TO TRUE
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-03 AND I-11)
               ADD 1                       TO ANTFJ
           END-IF
           IF  (I-03 AND NOT-I-11)
               ADD 1                       TO ANTUT
           END-IF.
 
       SLUTT-T.
      *******************************************************
           CONTINUE.
 
       NULLORD-GET SECTION.
       NULLORD-GET-P.
           IF  NULLORD-EOF-OFF
               READ NULLORD
               AT END
                   SET NULLORD-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NULLORD-FLDOFF SECTION.
       NULLORD-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-21                TO TRUE
           END-EVALUATE.
 
       NULLORD-FLDSET SECTION.
       NULLORD-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NULLORD-IO-AREA (1:15) TO FAKAY1 (1:15)
               MOVE NULLORD-IO-AREA (16:3) TO BRKOD1-IO
               MOVE NULLORD-IO-AREA (19:6) TO FORDNR (1:6)
               MOVE NULLORD-IO-AREA (25:1) TO UKODE (1:1)
               IF  UKODE = SPACES
                   SET I-21                TO TRUE
               END-IF
           END-EVALUATE.
 
       NULLORD-IDSET SECTION.
       NULLORD-IDSET-P.
           SET I-02                        TO TRUE.
 
       NULLORD-MATCH-SET SECTION.
       NULLORD-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE NULLORD-IO-AREA (1:15) TO NULLORD-M-02-M3-FAKAY1
               MOVE NULLORD-IO-AREA (16:3) TO NULLORD-M-02-M2-BRKOD1-G
               MOVE NULLORD-IO-AREA (19:6) TO NULLORD-M-02-M1-FORDNR
           END-EVALUATE.
 
       FAKTIN-GET SECTION.
       FAKTIN-GET-P.
           IF  FAKTIN-EOF-OFF
               READ FAKTIN
               AT END
                   SET FAKTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKTIN-FLDSET SECTION.
       FAKTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTIN-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKTIN-IO-AREA (1:15)  TO FAKEY2 (1:15)
               MOVE FAKTIN-IO-AREA (1:200) TO FAKREC (1:200)
               MOVE FAKTIN-IO-AREA (4:6)   TO KNRFAK (1:6)
               MOVE FAKTIN-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKTIN-IO-AREA (11:1)  TO FAKART (1:1)
               MOVE FAKTIN-IO-AREA (12:1)  TO FAKMTE (1:1)
               MOVE FAKTIN-IO-AREA (13:1)  TO FRITT (1:1)
               MOVE FAKTIN-IO-AREA (14:2)  TO BETB (1:2)
               MOVE FAKTIN-IO-AREA (16:3)  TO BRKODE-IO
               MOVE FAKTIN-IO-AREA (19:6)  TO ORDNR (1:6)
               MOVE FAKTIN-IO-AREA (25:1)  TO RECART (1:1)
               MOVE FAKTIN-IO-AREA (26:4)  TO SEQNR-IO
               INSPECT SEQNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (41:30) TO VAREA (1:30)
               MOVE FAKTIN-IO-AREA (82:20) TO ARTNR (1:20)
               MOVE FAKTIN-IO-AREA (102:30) TO VAREB (1:30)
               MOVE FAKTIN-IO-AREA (137:4) TO LEVENH-IO
               MOVE FAKTIN-IO-AREA (141:7) TO EDBNR (1:7)
               MOVE FAKTIN-IO-AREA (157:9) TO ENHPR-IO
               INSPECT ENHPR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKTIN-IDSET SECTION.
       FAKTIN-IDSET-P.
           SET I-03                        TO TRUE.
 
       FAKTIN-MATCH-SET SECTION.
       FAKTIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTIN-IO-AREA (1:15)  TO FAKTIN-M-03-M3-FAKEY2
               MOVE FAKTIN-IO-AREA (16:3)  TO FAKTIN-M-03-M2-BRKODE-G
               MOVE FAKTIN-IO-AREA (19:6)  TO FAKTIN-M-03-M1-ORDNR
           END-EVALUATE.
 
       PRF-PRINT-LINE SECTION.
       PRF-PRINT-LINE-P.
           IF  PRF-BEFORE-SKIP > 0
               PERFORM PRF-SKIP-BEFORE
           END-IF
           IF  PRF-BEFORE-SPACE > 0
               PERFORM PRF-SPACE-BEFORE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               IF  PRF-AFTER-SPACE > 0
                   PERFORM PRF-SPACE-AFTER
               END-IF
           ELSE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               PERFORM PRF-SPACE-AFTER
           END-IF
           IF  PRF-LINE-COUNT NOT < PRF-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       PRF-SKIP-BEFORE SECTION.
       PRF-SKIP-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-BEFORE-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-BEFORE SECTION.
       PRF-SPACE-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER PRF-BEFORE-SPACE LINES
           ADD PRF-BEFORE-SPACE            TO PRF-LINE-COUNT
           MOVE SPACES TO PRF-IO-AREA
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-BEFORE-SPACE.
 
       PRF-SKIP-AFTER SECTION.
       PRF-SKIP-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-AFTER-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-AFTER SECTION.
       PRF-SPACE-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE PRF-AFTER-SPACE LINES
           ADD PRF-AFTER-SPACE             TO PRF-LINE-COUNT
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  NULLORD-EOF
               MOVE HIGH-VALUES            TO NULLORD-MC
                                              NULLORD-MP
           END-IF
           IF  FAKTIN-EOF
               MOVE HIGH-VALUES            TO FAKTIN-MC
                                              FAKTIN-MP
           END-IF
           IF  NULLORD-MC < NULLORD-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FAKTIN-MC < FAKTIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  NULLORD-MC < FAKTIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET NULLORD-PROCESS     TO TRUE
                   MOVE NULLORD-MC         TO NULLORD-MP
                   IF  NULLORD-MC = FAKTIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKTIN-MC < NULLORD-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKTIN-PROCESS      TO TRUE
                   MOVE FAKTIN-MC          TO FAKTIN-MP
                   IF  FAKTIN-MC = NULLORD-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  NULLORD-MC = FAKTIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET NULLORD-PROCESS     TO TRUE
                   MOVE NULLORD-MC         TO NULLORD-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-11)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE FIRMNR                 TO PRF-IO-AREA (1:3)
               MOVE KNRFAK                 TO PRF-IO-AREA (5:6)
               MOVE FAKTYP                 TO PRF-IO-AREA (12:1)
               MOVE FAKART                 TO PRF-IO-AREA (14:1)
               MOVE FAKMTE                 TO PRF-IO-AREA (16:1)
               MOVE FRITT                  TO PRF-IO-AREA (18:1)
               MOVE BETB                   TO PRF-IO-AREA (19:2)
               MOVE BRKODE                 TO XO-50YNZ
               MOVE XO-50YNZ               TO PRF-IO-AREA (21:5)
               MOVE ORDNR                  TO PRF-IO-AREA (27:6)
               MOVE RECART                 TO PRF-IO-AREA (34:1)
               MOVE SEQNR-IO               TO PRF-IO-AREA (36:4)
               IF  (NOT-I-22)
                   MOVE ARTNR              TO PRF-IO-AREA (41:20)
               END-IF
               IF  (NOT-I-22)
                   MOVE VAREB              TO PRF-IO-AREA (62:30)
               END-IF
               IF  (I-22)
                   MOVE VAREA              TO PRF-IO-AREA (62:30)
               END-IF
               IF  (NOT-I-22)
                   MOVE LEVENH             TO XO-52YN9
                   MOVE XO-52YN9           TO PRF-IO-AREA (93:8)
               END-IF
               IF  (NOT-I-22)
                   MOVE EDBNR              TO PRF-IO-AREA (103:7)
               END-IF
               IF  (NOT-I-22)
                   MOVE ENHPR              TO XO-72YN9
                   MOVE XO-72YN9           TO PRF-IO-AREA (111:10)
               END-IF
               IF  (I-11)
                   MOVE 'REC.FJERNET'      TO PRF-IO-AREA (122:11)
               END-IF
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-03 AND NOT-I-11)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               WRITE FAKTUT-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'FØLGENDE ORDRE ER'    TO PRF-IO-AREA (10:17)
               MOVE 'PLUKKET UT FØR'       TO PRF-IO-AREA (28:14)
               MOVE 'FAKTURERING.'         TO PRF-IO-AREA (43:12)
               MOVE 'PROGRAM FAK071'       TO PRF-IO-AREA (62:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (78:8)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'FNR RESKNR T A M FBM ' TO PRF-IO-AREA (1:21)
               MOVE 'BRD ORDREN R SEQN'    TO PRF-IO-AREA (23:17)
               MOVE 'ARTIKKELNR'           TO PRF-IO-AREA (41:10)
               MOVE 'VAREBET./ VAREADR.'   TO PRF-IO-AREA (62:18)
               MOVE 'ANTALL  EDB-NR'       TO PRF-IO-AREA (95:14)
               MOVE 'BELØP'                TO PRF-IO-AREA (116:5)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'FØLGENDE ORDRE ER'    TO PRF-IO-AREA (10:17)
               MOVE 'PLUKKET UT FØR'       TO PRF-IO-AREA (28:14)
               MOVE 'FAKTURERING.'         TO PRF-IO-AREA (43:12)
               MOVE 'PROGRAM FAK071'       TO PRF-IO-AREA (62:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (78:8)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'FNR RESKNR T A M FBM ' TO PRF-IO-AREA (1:21)
               MOVE 'BRD ORDREN R SEQN'    TO PRF-IO-AREA (23:17)
               MOVE 'ARTIKKELNR'           TO PRF-IO-AREA (41:10)
               MOVE 'VAREBET./ VAREADR.'   TO PRF-IO-AREA (62:18)
               MOVE 'ANTALL  EDB-NR'       TO PRF-IO-AREA (95:14)
               MOVE 'BELØP'                TO PRF-IO-AREA (116:5)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO PRF-IO-AREA (1:24)
               MOVE '    --- FAK071 ---   ***' TO PRF-IO-AREA (25:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 3                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTINN                 TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'ANT. FAKTURAREC. LEST   ' TO PRF-IO-AREA (15:24)
               MOVE 1                      TO PRF-BEFORE-SPACE
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTFJ                  TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'ANT. FAKTURAREC. FJERNET' TO PRF-IO-AREA (15:24)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTUT                  TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'ANT. FAKTURAREC. BEHOLT ' TO PRF-IO-AREA (15:24)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '       KJØRT'         TO PRF-IO-AREA (7:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (20:8)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
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
           INITIALIZE NULLORD-DATA-FIELDS
           SET NULLORD-EOF-OFF             TO TRUE
           SET NULLORD-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO NULLORD-MC
                                              NULLORD-MP
           OPEN INPUT NULLORD
           INITIALIZE FAKTIN-DATA-FIELDS
           SET FAKTIN-EOF-OFF              TO TRUE
           SET FAKTIN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FAKTIN-MC
                                              FAKTIN-MP
           OPEN INPUT FAKTIN
           OPEN OUTPUT FAKTUT
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE NULLORD
           CLOSE FAKTIN
           CLOSE FAKTUT
           IF PRF-IO-AREA NOT = SPACES
             WRITE PRF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRF-IO-AREA
           END-IF
           CLOSE PRF.
 
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
