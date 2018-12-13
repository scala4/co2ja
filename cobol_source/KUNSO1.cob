       IDENTIFICATION DIVISION.
       PROGRAM-ID. KUNSO1R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: KUNSO1                                       *
      * DANNER KUNDEALFA-FILE I REORGANISERING AV KUNDEMASTER.        *
      *E 18.05.95 LEVERANDØR HOS AUTOUTSTYR ELVERUM, 953, KAN HA LEVE-*
      *           RANDØRNR SOM STARTER PÅ 8.                          *
      *E 21.05.97 TATT MED 906 I HELLA-AVVIK.                         *
      *E 27.10.98 NY SØKEKEY PÅ ALTERNATIVT RESKONTRONR.              *
      *E 20.03.01 NY SØKEKEY PÅ KJEDE-ALFA.                           *
      *E 14.05.01 LEGGER RESK.GRP 7 SOM LEVERANDØR.                   *
      *E 07.11.02 LEGGER RESK.GRP 8 SOM LEVERANDØR.                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KUNSO1.rpg
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
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT UTFIL
               ASSIGN TO UT-S-UTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFIL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1.
                   15  KUNDEMA-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1.
                   15  KUNDEMX-KEY1N       PICTURE S9(10).
               10  FILLER                  PICTURE X(190).
       FD UTFIL
               BLOCK CONTAINS 7954
               RECORD CONTAINS 97.
       01  UTFIL-IO-AREA.
           05  UTFIL-IO-AREA-X             PICTURE X(97).
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
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  UTFIL-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-EOF-OFF         VALUE '0'.
               88  KUNDEMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-READ-OFF        VALUE '0'.
               88  KUNDEMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-PROCESS-OFF     VALUE '0'.
               88  KUNDEMA-PROCESS         VALUE '1'.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-EOF-OFF         VALUE '0'.
               88  KUNDEMX-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-READ-OFF        VALUE '0'.
               88  KUNDEMX-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-PROCESS-OFF     VALUE '0'.
               88  KUNDEMX-PROCESS         VALUE '1'.
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
           05  KUNDEMA-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  KNR1                    PICTURE X(1).
               10  KNR                     PICTURE X(6).
               10  ALFA                    PICTURE X(4).
               10  NAVN1                   PICTURE X(30).
               10  PNR                     PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  BETM                    PICTURE X(2).
               10  ALTRSK                  PICTURE X(6).
      *                                     189 189 SLETT
      *MERGER REC TYPE 3 I KUNDEMX.
           05  KUNDEMA-MP                  PICTURE X(9).
           05  KUNDEMA-MC                  PICTURE X(9).
           05  KUNDEMA-M-01            REDEFINES KUNDEMA-MC.
               10  KUNDEMA-M-01-M2.
                   15  KUNDEMA-M-01-M2-FNR-G.
                       20  KUNDEMA-M-01-M2-FNR PICTURE X(3).
               10  KUNDEMA-M-01-M1.
                   15  KUNDEMA-M-01-M1-KNR-G.
                       20  KUNDEMA-M-01-M1-KNR PICTURE X(6).
           05  KUNDEMX-DATA-FIELDS.
               10  KALFA                   PICTURE X(4).
      *                                     155 162 KALFAL          17
           05  KUNDEMX-MP                  PICTURE X(9).
           05  KUNDEMX-MC                  PICTURE X(9).
           05  KUNDEMX-M-02            REDEFINES KUNDEMX-MC.
               10  KUNDEMX-M-02-M2.
                   15  KUNDEMX-M-02-M2-FNR-G.
                       20  KUNDEMX-M-02-M2-FNR PICTURE X(3).
               10  KUNDEMX-M-02-M1.
                   15  KUNDEMX-M-02-M1-KNR-G.
                       20  KUNDEMX-M-02-M1-KNR PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  TELL1-IO.
                   15  TELL1               PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KUNDEMA-PROCESS
               SET KUNDEMA-PROCESS-OFF     TO TRUE
               SET KUNDEMA-READ            TO TRUE
           END-IF
 
           IF  KUNDEMA-READ
               PERFORM KUNDEMA-GET
               SET KUNDEMA-READ-OFF        TO TRUE
               IF  NOT KUNDEMA-EOF
                   PERFORM KUNDEMA-MATCH-SET
               END-IF
           END-IF
 
           IF  KUNDEMX-PROCESS
               SET KUNDEMX-PROCESS-OFF     TO TRUE
               SET KUNDEMX-READ            TO TRUE
           END-IF
 
           IF  KUNDEMX-READ
               PERFORM KUNDEMX-GET
               SET KUNDEMX-READ-OFF        TO TRUE
               IF  NOT KUNDEMX-EOF
                   PERFORM KUNDEMX-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM KUNDEMX-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-IDSET
           END-IF
 
           IF  KUNDEMX-PROCESS
               PERFORM KUNDEMX-IDSET
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
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-FLDOFF
               PERFORM KUNDEMA-FLDSET
           END-IF
 
           IF  KUNDEMX-PROCESS
               PERFORM KUNDEMX-FLDOFF
               PERFORM KUNDEMX-FLDSET
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
           SET NOT-I-10                    TO TRUE
           IF  KNR1 = '9'
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  KNR1 = '8'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  KNR1 = '7'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-16)
               ADD 1                       TO TELL1
           END-IF.
 
       KUNDEMA-GET SECTION.
       KUNDEMA-GET-P.
           IF  KUNDEMA-EOF-OFF
               READ KUNDEMA
               AT END
                   SET KUNDEMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:3)  TO FNR (1:3)
               MOVE KUNDEMA-IO-AREA (6:1)  TO KNR1 (1:1)
               MOVE KUNDEMA-IO-AREA (6:6)  TO KNR (1:6)
               MOVE KUNDEMA-IO-AREA (12:4) TO ALFA (1:4)
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (121:4) TO PNR (1:4)
               MOVE KUNDEMA-IO-AREA (106:15) TO PSTED (1:15)
               MOVE KUNDEMA-IO-AREA (127:2) TO BETM (1:2)
               MOVE KUNDEMA-IO-AREA (172:6) TO ALTRSK (1:6)
               IF  ALTRSK = SPACES
                   SET I-15                TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-01                        TO TRUE.
 
       KUNDEMA-MATCH-SET SECTION.
       KUNDEMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:3)  TO KUNDEMA-M-01-M2-FNR
               MOVE KUNDEMA-IO-AREA (6:6)  TO KUNDEMA-M-01-M1-KNR
           END-EVALUATE.
 
       KUNDEMX-GET SECTION.
       KUNDEMX-GET-P.
           IF  KUNDEMX-EOF-OFF
               READ KUNDEMX
               AT END
                   SET KUNDEMX-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMX-FLDOFF SECTION.
       KUNDEMX-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KUNDEMX-IO-AREA (10:1) = '3' )
               SET NOT-I-16                TO TRUE
           END-EVALUATE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMX-IO-AREA (10:1) = '3' )
               MOVE KUNDEMX-IO-AREA (1:3)  TO FNR (1:3)
               MOVE KUNDEMX-IO-AREA (4:6)  TO KNR (1:6)
               MOVE KUNDEMX-IO-AREA (151:4) TO KALFA (1:4)
               IF  KALFA = SPACES
                   SET I-16                TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDEMX-IDCHK SECTION.
       KUNDEMX-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KUNDEMX-IO-AREA (10:1) = '3' )
             OR ( KUNDEMX-IO-AREA (10:1) NOT = '3' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMX-IO-AREA (10:1) = '3' )
               SET I-02                    TO TRUE
           WHEN ( KUNDEMX-IO-AREA (10:1) NOT = '3' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       KUNDEMX-MATCH-SET SECTION.
       KUNDEMX-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( KUNDEMX-IO-AREA (10:1) = '3' )
               MOVE KUNDEMX-IO-AREA (1:3)  TO KUNDEMX-M-02-M2-FNR
               MOVE KUNDEMX-IO-AREA (4:6)  TO KUNDEMX-M-02-M1-KNR
           WHEN ( KUNDEMX-IO-AREA (10:1) NOT = '3' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
           IF  KUNDEMA-EOF
               MOVE HIGH-VALUES            TO KUNDEMA-MC
                                              KUNDEMA-MP
           END-IF
           IF  KUNDEMX-EOF
               MOVE HIGH-VALUES            TO KUNDEMX-MC
                                              KUNDEMX-MP
           END-IF
           IF  KUNDEMA-MC < KUNDEMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KUNDEMX-MC < KUNDEMX-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KUNDEMA-MC < KUNDEMX-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   IF  KUNDEMA-MC = KUNDEMX-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEMX-MC < KUNDEMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMX-PROCESS     TO TRUE
                   MOVE KUNDEMX-MC         TO KUNDEMX-MP
                   IF  KUNDEMX-MC = KUNDEMA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEMA-MC = KUNDEMX-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE '99'                   TO UTFIL-IO-AREA (1:2)
               MOVE FNR                    TO UTFIL-IO-AREA (3:3)
               MOVE '1'                    TO UTFIL-IO-AREA (6:1)
               IF  (I-10)
                   MOVE '2'                TO UTFIL-IO-AREA (6:1)
               END-IF
               MOVE PNR                    TO UTFIL-IO-AREA (7:4)
               MOVE ALFA                   TO UTFIL-IO-AREA (11:4)
               MOVE '                      ' TO UTFIL-IO-AREA (15:22)
               MOVE KNR                    TO UTFIL-IO-AREA (37:6)
               MOVE NAVN1                  TO UTFIL-IO-AREA (43:30)
               MOVE PSTED                  TO UTFIL-IO-AREA (73:15)
               MOVE BETM                   TO UTFIL-IO-AREA (88:2)
               MOVE PNR                    TO UTFIL-IO-AREA (90:4)
               WRITE UTFIL-IO-AREA
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE '99'                   TO UTFIL-IO-AREA (1:2)
               MOVE FNR                    TO UTFIL-IO-AREA (3:3)
               MOVE '1'                    TO UTFIL-IO-AREA (6:1)
               IF  (I-10)
                   MOVE '2'                TO UTFIL-IO-AREA (6:1)
               END-IF
               MOVE ALFA                   TO UTFIL-IO-AREA (7:4)
               MOVE '    '                 TO UTFIL-IO-AREA (11:4)
               MOVE '                      ' TO UTFIL-IO-AREA (15:22)
               MOVE KNR                    TO UTFIL-IO-AREA (37:6)
               MOVE NAVN1                  TO UTFIL-IO-AREA (43:30)
               MOVE PSTED                  TO UTFIL-IO-AREA (73:15)
               MOVE BETM                   TO UTFIL-IO-AREA (88:2)
               MOVE PNR                    TO UTFIL-IO-AREA (90:4)
               WRITE UTFIL-IO-AREA
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE '99'                   TO UTFIL-IO-AREA (1:2)
               MOVE FNR                    TO UTFIL-IO-AREA (3:3)
               MOVE '3'                    TO UTFIL-IO-AREA (6:1)
               IF  (I-10)
                   MOVE '4'                TO UTFIL-IO-AREA (6:1)
               END-IF
               MOVE NAVN1                  TO UTFIL-IO-AREA (7:30)
               MOVE KNR                    TO UTFIL-IO-AREA (37:6)
               MOVE NAVN1                  TO UTFIL-IO-AREA (43:30)
               MOVE PSTED                  TO UTFIL-IO-AREA (73:15)
               MOVE BETM                   TO UTFIL-IO-AREA (88:2)
               MOVE PNR                    TO UTFIL-IO-AREA (90:4)
               WRITE UTFIL-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-15)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE '99'                   TO UTFIL-IO-AREA (1:2)
               MOVE FNR                    TO UTFIL-IO-AREA (3:3)
               MOVE '1'                    TO UTFIL-IO-AREA (6:1)
               IF  (I-10)
                   MOVE '2'                TO UTFIL-IO-AREA (6:1)
               END-IF
               MOVE 'A'                    TO UTFIL-IO-AREA (7:1)
               MOVE ALTRSK                 TO UTFIL-IO-AREA (8:6)
               MOVE ' '                    TO UTFIL-IO-AREA (14:1)
               MOVE '                      ' TO UTFIL-IO-AREA (15:22)
               MOVE KNR                    TO UTFIL-IO-AREA (37:6)
               MOVE NAVN1                  TO UTFIL-IO-AREA (43:30)
               MOVE PSTED                  TO UTFIL-IO-AREA (73:15)
               MOVE BETM                   TO UTFIL-IO-AREA (88:2)
               MOVE PNR                    TO UTFIL-IO-AREA (90:4)
               WRITE UTFIL-IO-AREA
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-16)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE '99'                   TO UTFIL-IO-AREA (1:2)
               MOVE FNR                    TO UTFIL-IO-AREA (3:3)
               MOVE '1'                    TO UTFIL-IO-AREA (6:1)
               IF  (I-10)
                   MOVE '2'                TO UTFIL-IO-AREA (6:1)
               END-IF
               MOVE KALFA                  TO UTFIL-IO-AREA (7:4)
               MOVE ALFA                   TO UTFIL-IO-AREA (11:4)
               MOVE '                      ' TO UTFIL-IO-AREA (15:22)
               MOVE KNR                    TO UTFIL-IO-AREA (37:6)
               MOVE NAVN1                  TO UTFIL-IO-AREA (43:30)
               MOVE PSTED                  TO UTFIL-IO-AREA (73:15)
               MOVE BETM                   TO UTFIL-IO-AREA (88:2)
               MOVE PNR                    TO UTFIL-IO-AREA (90:4)
               WRITE UTFIL-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ANTALL ='      TO LISTE-IO-AREA (7:15)
               MOVE TELL1                  TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (25:11)
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
           INITIALIZE KUNDEMA-DATA-FIELDS
           SET KUNDEMA-EOF-OFF             TO TRUE
           SET KUNDEMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEMA-MC
                                              KUNDEMA-MP
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           SET KUNDEMX-EOF-OFF             TO TRUE
           SET KUNDEMX-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEMX-MC
                                              KUNDEMX-MP
           OPEN INPUT KUNDEMX
           OPEN OUTPUT UTFIL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE UTFIL
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
