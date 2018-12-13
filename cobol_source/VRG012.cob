       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG012R.
      **********************************************  Z-WIN-RPG2      *
      *  DANNE TABELLER FOR KOPIERING AV VAREARKIV FRA FIRMA TIL FIRMA    *
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG012.rpg
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
           SELECT KORTFIL
               ASSIGN TO UT-S-KORTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTFIL-STATUS.
           SELECT KORTFIL-TMP
               ASSIGN TO UT-S-KORTFIL-TMP
               ACCESS MODE  IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTFIL-STATUS.
           SELECT SORT-WK1
               ASSIGN TO UT-S-SORT-WK1.
           SELECT FNRFIL
               ASSIGN TO UT-S-FNRFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FNRFIL-STATUS.
           SELECT FRAFIL
               ASSIGN TO UT-S-FRAFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRAFIL-STATUS.
           SELECT TILFIL
               ASSIGN TO UT-S-TILFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILFIL-STATUS.
           SELECT LIMFIL
               ASSIGN TO UT-S-LIMFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LIMFIL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORTFIL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KORTFIL-IO-AREA.
           05  KORTFIL-IO-AREA-X           PICTURE X(80).
       SD SORT-WK1
           DATA RECORD IS SORT-REC1.
       01  SORT-REC1.
           05  FILLER                  PIC X(2).
           05  SORT-WK1-K5             PIC X(3).
           05  FILLER                  PIC X(3).
           05  SORT-WK1-K4             PIC X(5).
           05  FILLER                  PIC X(3).
           05  SORT-WK1-K3             PIC X(3).
           05  FILLER                  PIC X(3).
           05  SORT-WK1-K2             PIC X(1).
           05  FILLER                  PIC X(4).
           05  SORT-WK1-K1             PIC X(3).
           05  FILLER                  PIC X(55).
       FD FNRFIL
               BLOCK CONTAINS 60
               RECORD CONTAINS 3.
       01  FNRFIL-IO-AREA.
           05  FNRFIL-IO-AREA-X            PICTURE X(3).
       FD FRAFIL
               BLOCK CONTAINS 140
               RECORD CONTAINS 14.
       01  FRAFIL-IO-AREA.
           05  FRAFIL-IO-AREA-X            PICTURE X(14).
       FD TILFIL
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  TILFIL-IO-AREA.
           05  TILFIL-IO-AREA-X            PICTURE X(60).
       FD LIMFIL
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  LIMFIL-IO-AREA.
           05  LIMFIL-IO-AREA-X            PICTURE X(80).
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
           10  KORTFIL-STATUS              PICTURE 99 VALUE 0.
           10  FNRFIL-STATUS               PICTURE 99 VALUE 0.
           10  FRAFIL-STATUS               PICTURE 99 VALUE 0.
           10  TILFIL-STATUS               PICTURE 99 VALUE 0.
           10  LIMFIL-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTFIL-EOF-OFF         VALUE '0'.
               88  KORTFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTFIL-READ-OFF        VALUE '0'.
               88  KORTFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTFIL-PROCESS-OFF     VALUE '0'.
               88  KORTFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KORTFIL-LEVEL-INIT-OFF  VALUE '0'.
               88  KORTFIL-LEVEL-INIT      VALUE '1'.
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
           05  KORTFIL-LEVEL-01.
               10  KORTFIL-01-L3.
                   15  KORTFIL-01-L3-FRAFNR PICTURE X(3).
               10  KORTFIL-01-L2.
                   15  KORTFIL-01-L2-FVGR  PICTURE X(5).
                   15  KORTFIL-01-L2-FALFA PICTURE X(3).
               10  KORTFIL-01-L1.
                   15  KORTFIL-01-L1-TILFNR PICTURE X(3).
           05  KORTFIL-DATA-FIELDS.
               10  FRAFNR                  PICTURE X(3).
               10  FVGR                    PICTURE X(5).
               10  FALFA                   PICTURE X(3).
               10  FPT                     PICTURE X(1).
               10  TILFNR                  PICTURE X(3).
               10  TVGR                    PICTURE X(5).
               10  TALFA                   PICTURE X(3).
               10  TPT                     PICTURE X(1).
               10  SVSKR                   PICTURE X(3).
               10  SVSO-ELGRE              PICTURE X(3).
               10  UTSKR                   PICTURE X(3).
               10  UTSO-ELGRE              PICTURE X(3).
               10  VALGMU                  PICTURE X(18).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(8).
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  SEQ-IO.
                   15  SEQ                 PICTURE S9(3).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(3).
               10  ANTL1-IO.
                   15  ANTL1               PICTURE S9(5).
               10  ANTL1F-IO.
                   15  ANTL1F              PICTURE S9(5).
               10  ANTL1T-IO.
                   15  ANTL1T              PICTURE S9(3).
               10  ANTL1M-IO.
                   15  ANTL1M              PICTURE S9(3).
               10  ANTL2-IO.
                   15  ANTL2               PICTURE S9(5).
               10  ANTL3-IO.
                   15  ANTL3               PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-30P-EF.
                 15  XO-30P                PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-30YY9                PICTURE ZZ9.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KORTFIL-PROCESS
               SET KORTFIL-PROCESS-OFF     TO TRUE
               SET KORTFIL-READ            TO TRUE
           END-IF
 
           IF  KORTFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM KORTFIL-GET
               SET KORTFIL-READ-OFF        TO TRUE
               IF  NOT KORTFIL-EOF
                   PERFORM KORTFIL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET KORTFIL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KORTFIL-PROCESS
               PERFORM KORTFIL-IDSET
           END-IF
 
           IF  KORTFIL-PROCESS
               PERFORM KORTFIL-CHK-LEVEL
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
 
           IF  KORTFIL-PROCESS
               PERFORM KORTFIL-FLDOFF
               PERFORM KORTFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KORTFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2 AND I-01)
               MOVE 0                      TO SEQ
           END-IF
           IF  (I-L1 AND I-01)
               ADD 1                       TO SEQ
               ADD 1                       TO ANT
           END-IF
           IF  (I-L1)
               ADD 1                       TO ANTL1
               ADD 1                       TO ANTL1F
               ADD 1                       TO ANTL1T
           END-IF
           IF  (I-L2)
               SET NOT-I-91                TO TRUE
               IF  ANTL1T > ANTL1M
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-91)
               MOVE ANTL1T                 TO ANTL1M-IO
           END-IF
           IF  (I-L2)
               SUBTRACT ANTL1T             FROM ANTL1T
               ADD 1                       TO ANTL2
           END-IF
           IF  (I-L3)
               ADD 1                       TO ANTL3
           END-IF.
 
       KORTFIL-GET SECTION.
       KORTFIL-GET-P.
           IF  KORTFIL-EOF-OFF
               READ KORTFIL-TMP
               AT END
                   SET KORTFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORTFIL-FLDOFF SECTION.
       KORTFIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KORTFIL-IO-AREA (1:1) = '2' )
               SET NOT-I-05                TO TRUE
               SET NOT-I-06                TO TRUE
           END-EVALUATE.
 
       KORTFIL-FLDSET SECTION.
       KORTFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KORTFIL-IO-AREA (1:1) = '2' )
               MOVE KORTFIL-IO-AREA (3:3)  TO FRAFNR (1:3)
               MOVE KORTFIL-IO-AREA (8:5)  TO FVGR (1:5)
               MOVE KORTFIL-IO-AREA (15:3) TO FALFA (1:3)
               MOVE KORTFIL-IO-AREA (20:1) TO FPT (1:1)
               MOVE KORTFIL-IO-AREA (24:3) TO TILFNR (1:3)
               MOVE KORTFIL-IO-AREA (29:5) TO TVGR (1:5)
               IF  TVGR = SPACES
                   SET I-05                TO TRUE
               END-IF
               MOVE KORTFIL-IO-AREA (36:3) TO TALFA (1:3)
               IF  TALFA = SPACES
                   SET I-06                TO TRUE
               END-IF
               MOVE KORTFIL-IO-AREA (41:1) TO TPT (1:1)
               MOVE KORTFIL-IO-AREA (44:3) TO SVSKR (1:3)
               MOVE KORTFIL-IO-AREA (48:3) TO SVSO-ELGRE (1:3)
               MOVE KORTFIL-IO-AREA (55:3) TO UTSKR (1:3)
               MOVE KORTFIL-IO-AREA (59:3) TO UTSO-ELGRE (1:3)
               MOVE KORTFIL-IO-AREA (63:18) TO VALGMU (1:18)
           END-EVALUATE.
 
       KORTFIL-IDCHK SECTION.
       KORTFIL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KORTFIL-IO-AREA (1:1) = '2' )
             OR ( KORTFIL-IO-AREA (1:1) NOT = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KORTFIL-IDSET SECTION.
       KORTFIL-IDSET-P.
           EVALUATE TRUE
           WHEN ( KORTFIL-IO-AREA (1:1) = '2' )
               SET I-01                    TO TRUE
           WHEN ( KORTFIL-IO-AREA (1:1) NOT = '2' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       KORTFIL-CHK-LEVEL SECTION.
       KORTFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KORTFIL-IO-AREA (1:1) = '2' )
               MOVE LOW-VALUES             TO KORTFIL-LEVEL-01
               MOVE KORTFIL-IO-AREA (3:3)  TO KORTFIL-01-L3-FRAFNR
               MOVE KORTFIL-IO-AREA (8:5)  TO KORTFIL-01-L2-FVGR
               MOVE KORTFIL-IO-AREA (15:3) TO KORTFIL-01-L2-FALFA
               MOVE KORTFIL-IO-AREA (24:3) TO KORTFIL-01-L1-TILFNR
               IF  KORTFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KORTFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  KORTFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KORTFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KORTFIL-01-L3         TO THE-PRIOR-L3
               MOVE  KORTFIL-01-L2         TO THE-PRIOR-L2
               MOVE  KORTFIL-01-L1         TO THE-PRIOR-L1
               SET KORTFIL-LEVEL-INIT      TO TRUE
           WHEN ( KORTFIL-IO-AREA (1:1) NOT = '2' )
               CONTINUE
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TABELLTOTALER VRG012' TO LISTE-IO-AREA (1:20)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (22:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO TILFIL-IO-AREA
               INITIALIZE TILFIL-IO-AREA
               MOVE FRAFNR                 TO TILFIL-IO-AREA (1:3)
               MOVE FVGR                   TO TILFIL-IO-AREA (4:5)
               MOVE FALFA                  TO TILFIL-IO-AREA (9:3)
               MOVE FPT                    TO TILFIL-IO-AREA (12:1)
               MOVE SEQ                    TO XO-30P
               MOVE XO-30P-EF              TO TILFIL-IO-AREA (13:2)
               MOVE TILFNR                 TO TILFIL-IO-AREA (15:3)
               MOVE SVSKR                  TO TILFIL-IO-AREA (18:3)
               MOVE SVSO-ELGRE             TO TILFIL-IO-AREA (21:3)
               MOVE UTSKR                  TO TILFIL-IO-AREA (24:3)
               MOVE UTSO-ELGRE             TO TILFIL-IO-AREA (27:3)
               MOVE TVGR                   TO TILFIL-IO-AREA (30:5)
               MOVE TALFA                  TO TILFIL-IO-AREA (35:3)
               IF  (I-05)
                   MOVE FVGR               TO TILFIL-IO-AREA (30:5)
               END-IF
               IF  (I-06)
                   MOVE FALFA              TO TILFIL-IO-AREA (35:3)
               END-IF
               MOVE TPT                    TO TILFIL-IO-AREA (38:1)
               MOVE VALGMU                 TO TILFIL-IO-AREA (43:18)
               WRITE TILFIL-IO-AREA
           END-IF
           IF  (I-L2)
               MOVE SPACES TO FRAFIL-IO-AREA
               INITIALIZE FRAFIL-IO-AREA
               MOVE FRAFNR                 TO FRAFIL-IO-AREA (1:3)
               MOVE FVGR                   TO FRAFIL-IO-AREA (4:5)
               MOVE FALFA                  TO FRAFIL-IO-AREA (9:3)
               MOVE FPT                    TO FRAFIL-IO-AREA (12:1)
               MOVE ANT-IO                 TO FRAFIL-IO-AREA (12:3)
               INITIALIZE ANT-IO
               WRITE FRAFIL-IO-AREA
           END-IF
           IF  (I-L3)
               MOVE SPACES TO FNRFIL-IO-AREA
               INITIALIZE FNRFIL-IO-AREA
               MOVE FRAFNR                 TO FNRFIL-IO-AREA (1:3)
               WRITE FNRFIL-IO-AREA
           END-IF
           IF  (I-L3)
               MOVE SPACES TO LIMFIL-IO-AREA
               INITIALIZE LIMFIL-IO-AREA
               MOVE FRAFNR                 TO LIMFIL-IO-AREA (1:3)
               MOVE '0000001'              TO LIMFIL-IO-AREA (4:7)
               MOVE FRAFNR                 TO LIMFIL-IO-AREA (11:3)
               MOVE '8999999'              TO LIMFIL-IO-AREA (14:7)
               WRITE LIMFIL-IO-AREA
           END-IF
           IF  (I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTL1F                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               INITIALIZE ANTL1F
               MOVE 'FRA FIRMA '           TO LISTE-IO-AREA (8:10)
               MOVE FRAFNR                 TO LISTE-IO-AREA (19:3)
               MOVE ANTL1                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (25:6)
               MOVE 'FRA AKK.'             TO LISTE-IO-AREA (33:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTL1                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               MOVE 'ANT. TIL FIRMA TABELL' TO LISTE-IO-AREA (8:21)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTL1M                 TO XO-30YY9
               MOVE XO-30YY9               TO LISTE-IO-AREA (4:3)
               MOVE 'MAX TIL FIRMA PR. GRP' TO LISTE-IO-AREA (8:21)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTL2                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               MOVE 'ANT. FRA FIRMA TABELL' TO LISTE-IO-AREA (8:21)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTL3                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               MOVE 'ANT. FIRMANUMMER-TABELL' TO LISTE-IO-AREA (8:23)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTL3                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               MOVE 'ANT. FIRMANR.LIMIT.FILE' TO LISTE-IO-AREA (8:23)
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           MOVE 1                          TO LR-CHECK
           SET KORTFIL-LEVEL-INIT          TO TRUE
           INITIALIZE KORTFIL-DATA-FIELDS
           SET KORTFIL-EOF-OFF             TO TRUE
           SET KORTFIL-PROCESS             TO TRUE
           SORT SORT-WK1
               ASCENDING  SORT-WK1-K1
               ASCENDING  SORT-WK1-K2
               ASCENDING  SORT-WK1-K3
               ASCENDING  SORT-WK1-K4
               ASCENDING  SORT-WK1-K5
               USING  KORTFIL
               GIVING KORTFIL-TMP.
           OPEN INPUT  KORTFIL-TMP.
           OPEN OUTPUT FNRFIL
           OPEN OUTPUT FRAFIL
           OPEN OUTPUT TILFIL
           OPEN OUTPUT LIMFIL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KORTFIL-TMP
           CLOSE FNRFIL
           CLOSE FRAFIL
           CLOSE TILFIL
           CLOSE LIMFIL
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
