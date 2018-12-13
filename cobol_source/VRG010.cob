       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG010R.
      **********************************************  Z-WIN-RPG2      *
      *********************************************************************
      *  MERGE  KOPI FRA BUP48UB - SAMT VARETILLEGGSFILE.                 *
      *  DANNE NYE RECORDS TIL VARETILLEGGSFILE MED LEVERANDØRNR          *
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG010.rpg
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
           SELECT KOPI
               ASSIGN TO UT-S-KOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPI-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT IKKEMR
               ASSIGN TO UT-S-IKKEMR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS IKKEMR-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KOPI
               BLOCK CONTAINS 8400
               RECORD CONTAINS 140.
       01  KOPI-IO-AREA.
           05  KOPI-IO-AREA-X              PICTURE X(140).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1.
                   15  VARETIL-KEY1N       PICTURE S9(12).
               10  FILLER                  PICTURE X(188).
       FD IKKEMR
               BLOCK CONTAINS 9000
               RECORD CONTAINS 200.
       01  IKKEMR-IO-AREA.
           05  IKKEMR-IO-AREA-X            PICTURE X(200).
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
           10  KOPI-STATUS                 PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  IKKEMR-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-EOF-OFF            VALUE '0'.
               88  KOPI-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-READ-OFF           VALUE '0'.
               88  KOPI-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-PROCESS-OFF        VALUE '0'.
               88  KOPI-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOPI-LEVEL-INIT-OFF     VALUE '0'.
               88  KOPI-LEVEL-INIT         VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-EOF-OFF         VALUE '0'.
               88  VARETIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-READ-OFF        VALUE '0'.
               88  VARETIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-PROCESS-OFF     VALUE '0'.
               88  VARETIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARETIL-LEVEL-INIT-OFF  VALUE '0'.
               88  VARETIL-LEVEL-INIT      VALUE '1'.
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
           05  KOPI-LEVEL-02.
               10  KOPI-02-L1.
                   15  KOPI-02-L1-FIRMA    PICTURE X(3).
           05  KOPI-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(6).
           05  KOPI-MP                     PICTURE X(10).
           05  KOPI-MC                     PICTURE X(10).
           05  KOPI-M-02               REDEFINES KOPI-MC.
               10  KOPI-M-02-M2.
                   15  KOPI-M-02-M2-FIRMA-G.
                       20  KOPI-M-02-M2-FIRMA PICTURE X(3).
               10  KOPI-M-02-M1.
                   15  KOPI-M-02-M1-EDBNR-G.
                       20  KOPI-M-02-M1-EDBNR PICTURE X(7).
           05  VARETIL-LEVEL-03.
               10  VARETIL-03-L1.
                   15  VARETIL-03-L1-FNR   PICTURE X(3).
           05  VARETIL-DATA-FIELDS.
               10  ART                     PICTURE X(2).
               10  FNR                     PICTURE X(3).
               10  EDB                     PICTURE X(7).
               10  GLEV-IO.
                   15  GLEV                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  VARETIL-MP                  PICTURE X(10).
           05  VARETIL-MC                  PICTURE X(10).
           05  VARETIL-M-03            REDEFINES VARETIL-MC.
               10  VARETIL-M-03-M2.
                   15  VARETIL-M-03-M2-FNR-G.
                       20  VARETIL-M-03-M2-FNR PICTURE X(3).
               10  VARETIL-M-03-M1.
                   15  VARETIL-M-03-M1-EDB-G.
                       20  VARETIL-M-03-M1-EDB PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  GML-IO.
                   15  GML                 PICTURE S9(6).
               10  LEV-IO.
                   15  LEV                 PICTURE S9(7).
               10  NULL2-IO.
                   15  NULL2               PICTURE S9(2).
               10  NULL5-IO.
                   15  NULL5               PICTURE S9(5).
               10  NULL7-IO.
                   15  NULL7               PICTURE S9(7).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(9).
               10  NULL6-IO.
                   15  NULL6               PICTURE S9(6).
               10  ANTNM-IO.
                   15  ANTNM               PICTURE S9(8).
               10  ULNM-IO.
                   15  ULNM                PICTURE S9(8).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60YNZ                PICTURE ZZZZZZ.
               10  XO-70YNZ                PICTURE ZZZZZZZ.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KOPI-PROCESS
               SET KOPI-PROCESS-OFF        TO TRUE
               SET KOPI-READ               TO TRUE
           END-IF
 
           IF  KOPI-READ
               PERFORM KOPI-GET
               SET KOPI-READ-OFF           TO TRUE
               IF  NOT KOPI-EOF
                   PERFORM KOPI-MATCH-SET
               END-IF
           END-IF
 
           IF  VARETIL-PROCESS
               SET VARETIL-PROCESS-OFF     TO TRUE
               SET VARETIL-READ            TO TRUE
           END-IF
 
           IF  VARETIL-READ
               PERFORM VARETIL-GET
               SET VARETIL-READ-OFF        TO TRUE
               IF  NOT VARETIL-EOF
                   PERFORM VARETIL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM VARETIL-MATCH-SET
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
 
           IF  KOPI-PROCESS
               PERFORM KOPI-IDSET
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-IDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-CHK-LEVEL
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-CHK-LEVEL
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
 
           IF  KOPI-PROCESS
               PERFORM KOPI-FLDOFF
               PERFORM KOPI-FLDSET
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KOPI-PROCESS
           OR  VARETIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-15                    TO TRUE
           SET NOT-I-40                    TO TRUE
           IF  (I-04)
               OR  (I-05)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET NOT-I-50                TO TRUE
               IF  FIRMA = '950'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-MR)
               ADD GLEV TO ZERO        GIVING GML
               SET NOT-I-40                TO TRUE
               IF  GML = LEVNR
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               GO TO SLUTT-T
           END-IF
           ADD GLEV TO ZERO            GIVING LEV
           MOVE 0                          TO NULL2
           MOVE 0                          TO NULL5
           MOVE 0                          TO NULL7
           MOVE 0                          TO NULL9
           MOVE 0                          TO NULL6
           SET I-15                        TO TRUE
           IF  (I-02 AND NOT-I-MR AND NOT-I-10)
               ADD 1                       TO ANTNM
           END-IF
           IF  (I-02 AND NOT-I-MR AND I-10)
               ADD 1                       TO ULNM
           END-IF
           IF  (I-03 AND I-MR AND NOT-I-10)
               ADD 1                       TO ANT
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       KOPI-GET SECTION.
       KOPI-GET-P.
           IF  KOPI-EOF-OFF
               READ KOPI
               AT END
                   SET KOPI-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KOPI-FLDOFF SECTION.
       KOPI-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-10                TO TRUE
           END-EVALUATE.
 
       KOPI-FLDSET SECTION.
       KOPI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPI-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE KOPI-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE KOPI-IO-AREA (114:6)   TO LEVNR-IO
               INSPECT LEVNR-IO REPLACING ALL ' ' BY '0'
               IF  LEVNR = ZERO
                   SET I-10                TO TRUE
               END-IF
           END-EVALUATE.
 
       KOPI-IDSET SECTION.
       KOPI-IDSET-P.
           SET I-02                        TO TRUE.
 
       KOPI-CHK-LEVEL SECTION.
       KOPI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KOPI-LEVEL-02
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-02-L1-FIRMA
               IF  KOPI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOPI-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOPI-02-L1            TO THE-PRIOR-L1
               SET KOPI-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       KOPI-MATCH-SET SECTION.
       KOPI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-M-02-M2-FIRMA
               MOVE KOPI-IO-AREA (6:7)     TO KOPI-M-02-M1-EDBNR
           END-EVALUATE.
 
       VARETIL-GET SECTION.
       VARETIL-GET-P.
           IF  VARETIL-EOF-OFF
               READ VARETIL
               AT END
                   SET VARETIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
               MOVE VARETIL-IO-AREA (1:2)  TO ART (1:2)
               MOVE VARETIL-IO-AREA (3:3)  TO FNR (1:3)
               MOVE VARETIL-IO-AREA (6:7)  TO EDB (1:7)
               MOVE VARETIL-IO-AREA (68:4) TO GLEV-IO
           END-EVALUATE.
 
       VARETIL-IDCHK SECTION.
       VARETIL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
             OR ( VARETIL-IO-AREA (1:1) NOT = '8' )
             OR ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) NOT = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
               SET I-03                    TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) NOT = '8' )
               SET I-04                    TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) NOT = '0' )
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       VARETIL-CHK-LEVEL SECTION.
       VARETIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
               MOVE LOW-VALUES             TO VARETIL-LEVEL-03
               MOVE VARETIL-IO-AREA (3:3)  TO VARETIL-03-L1-FNR
               IF  VARETIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARETIL-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARETIL-03-L1         TO THE-PRIOR-L1
               SET VARETIL-LEVEL-INIT      TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) NOT = '8' )
               CONTINUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) NOT = '0' )
               CONTINUE
           END-EVALUATE.
 
       VARETIL-MATCH-SET SECTION.
       VARETIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
               MOVE VARETIL-IO-AREA (3:3)  TO VARETIL-M-03-M2-FNR
               MOVE VARETIL-IO-AREA (6:7)  TO VARETIL-M-03-M1-EDB
           WHEN ( VARETIL-IO-AREA (1:1) NOT = '8' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) NOT = '0' )
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
           IF  KOPI-EOF
               MOVE HIGH-VALUES            TO KOPI-MC
                                              KOPI-MP
           END-IF
           IF  VARETIL-EOF
               MOVE HIGH-VALUES            TO VARETIL-MC
                                              VARETIL-MP
           END-IF
           IF  KOPI-MC < KOPI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VARETIL-MC < VARETIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KOPI-MC < VARETIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KOPI-PROCESS        TO TRUE
                   MOVE KOPI-MC            TO KOPI-MP
                   IF  KOPI-MC = VARETIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARETIL-MC < KOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARETIL-PROCESS     TO TRUE
                   MOVE VARETIL-MC         TO VARETIL-MP
                   IF  VARETIL-MC = KOPI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KOPI-MC = VARETIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KOPI-PROCESS        TO TRUE
                   MOVE KOPI-MC            TO KOPI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-MR AND I-15)
           AND (NOT-I-10)
               MOVE LEVNR                  TO XO-60P
               MOVE XO-60P-EF              TO VARETIL-IO-AREA (68:4)
               REWRITE VARETIL-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV LEV.NR.' TO LISTE-IO-AREA (26:22)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '----------------------' TO LISTE-IO-AREA (26:22)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-MR AND I-15)
           AND (I-50 AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE EDBNR                  TO LISTE-IO-AREA (9:7)
               MOVE LEVNR                  TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (20:6)
               MOVE 'NY RECORD TIL VARETIL' TO LISTE-IO-AREA (35:21)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-MR AND I-15)
           AND (I-50 AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE EDBNR                  TO LISTE-IO-AREA (9:7)
               MOVE LEVNR                  TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (20:6)
               MOVE LEV                    TO XO-70YNZ
               MOVE XO-70YNZ               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-MR AND I-15)
               MOVE SPACES TO IKKEMR-IO-AREA
               INITIALIZE IKKEMR-IO-AREA
               MOVE '80'                   TO IKKEMR-IO-AREA (1:2)
               MOVE FIRMA                  TO IKKEMR-IO-AREA (3:3)
               MOVE EDBNR                  TO IKKEMR-IO-AREA (6:7)
               MOVE '                    ' TO IKKEMR-IO-AREA (13:20)
               MOVE '                    ' TO IKKEMR-IO-AREA (33:20)
               MOVE '             '        TO IKKEMR-IO-AREA (53:13)
               MOVE NULL2-IO               TO IKKEMR-IO-AREA (66:2)
               MOVE LEVNR                  TO XO-60P
               MOVE XO-60P-EF              TO IKKEMR-IO-AREA (68:4)
               IF  (NOT-I-10)
                   MOVE LEVNR              TO XO-60P
                   MOVE XO-60P-EF          TO IKKEMR-IO-AREA (68:4)
               END-IF
               IF  (I-10)
                   MOVE NULL7              TO XO-70P
                   MOVE XO-70P-EF          TO IKKEMR-IO-AREA (68:4)
               END-IF
               MOVE NULL7                  TO XO-70P
               MOVE XO-70P-EF              TO IKKEMR-IO-AREA (72:4)
               MOVE NULL7                  TO XO-70P
               MOVE XO-70P-EF              TO IKKEMR-IO-AREA (76:4)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (80:3)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO IKKEMR-IO-AREA (83:5)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO IKKEMR-IO-AREA (88:5)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO IKKEMR-IO-AREA (93:5)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (98:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (101:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (104:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (107:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (110:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (113:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (116:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (119:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (122:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (125:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (128:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO IKKEMR-IO-AREA (131:3)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO IKKEMR-IO-AREA (134:5)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO IKKEMR-IO-AREA (139:5)
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO IKKEMR-IO-AREA (144:4)
               MOVE '                    ' TO IKKEMR-IO-AREA (148:20)
               MOVE '                    ' TO IKKEMR-IO-AREA (168:20)
               MOVE '             '        TO IKKEMR-IO-AREA (188:13)
               WRITE IKKEMR-IO-AREA
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV LEV.NR.' TO LISTE-IO-AREA (26:22)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '----------------------' TO LISTE-IO-AREA (26:22)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERT'            TO LISTE-IO-AREA (2:9)
               MOVE ANT                    TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (12:10)
               MOVE 'NYE REC.M/LEVNR'      TO LISTE-IO-AREA (26:15)
               MOVE ANTNM                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (42:10)
               MOVE 'NYE REC.U/LEVNR'      TO LISTE-IO-AREA (56:15)
               MOVE ULNM                   TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (72:10)
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
           SET KOPI-LEVEL-INIT             TO TRUE
           INITIALIZE KOPI-DATA-FIELDS
           SET KOPI-EOF-OFF                TO TRUE
           SET KOPI-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO KOPI-MC
                                              KOPI-MP
           OPEN INPUT KOPI
           SET VARETIL-LEVEL-INIT          TO TRUE
           INITIALIZE VARETIL-DATA-FIELDS
           SET VARETIL-EOF-OFF             TO TRUE
           SET VARETIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VARETIL-MC
                                              VARETIL-MP
           OPEN I-O VARETIL
           OPEN OUTPUT IKKEMR
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KOPI
           CLOSE VARETIL
           CLOSE IKKEMR
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
