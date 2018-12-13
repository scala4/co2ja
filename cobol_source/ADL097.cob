       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADL097R.
      ****************************************************************
      **** XADL17B ****************************** :   Z-WIN-RPG2   **** 000002
      *  PROGRAM: ADL097                         XX2000XXIRXXMT       *
      *  UTSKRIFT AV ANSATTOPPGAVE 1,2 OG 3.                         *
      *    U1 1  =  ANSATTOPPGAVE 1.                                 *
      *    U2 1  =  ANSATTOPPGAVE 2.  (PR. AVDELING)                 *
      *    U1 0  =                                                   *
      *  + U2 0  =  ANSATTOPPGAVE 3.  (TELEFONLISTE)                 *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ADL097.rpg
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
           SELECT LONNMAS
               ASSIGN TO UT-S-LONNMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LONNMAS-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD LONNMAS
               BLOCK CONTAINS 4100
               RECORD CONTAINS 1025.
       01  LONNMAS-IO-AREA.
           05  LONNMAS-IO-AREA-X           PICTURE X(1025).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  ARF-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE S9(8)V9(2).
           05  ARF-TABLE.
               10  ARF-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARF-I
                                                      ARF-S.
                   15  ARF                 PICTURE S9(8)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  LONNMAS-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  LONNMAS-EOF-OFF         VALUE '0'.
               88  LONNMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LONNMAS-READ-OFF        VALUE '0'.
               88  LONNMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LONNMAS-PROCESS-OFF     VALUE '0'.
               88  LONNMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  LONNMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  LONNMAS-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  LONNMAS-LEVEL-01.
               10  LONNMAS-01-L2.
                   15  LONNMAS-01-L2-FNR   PICTURE X(3).
               10  LONNMAS-01-L1.
                   15  LONNMAS-01-L1-AVD   PICTURE X(4).
           05  LONNMAS-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  ANR                     PICTURE X(3).
               10  AVD                     PICTURE X(4).
               10  NAVN                    PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  POST                    PICTURE X(20).
               10  FDATO                   PICTURE X(6).
               10  PERSNR                  PICTURE X(5).
               10  ANSATT-IO.
                   15  ANSATT              PICTURE S9(6).
               10  STILL                   PICTURE X(20).
               10  KOMMNR                  PICTURE X(4).
               10  TELEF                   PICTURE X(8).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM3-IO.
                   15  SUM3                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM4-IO.
                   15  SUM4                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM5-IO.
                   15  SUM5                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM6-IO.
                   15  SUM6                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM7-IO.
                   15  SUM7                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM8-IO.
                   15  SUM8                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM9-IO.
                   15  SUM9                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM10-IO.
                   15  SUM10               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM11-IO.
                   15  SUM11               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUM12-IO.
                   15  SUM12               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LK1                     PICTURE X(3).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LK2                     PICTURE X(3).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LK3                     PICTURE X(3).
               10  BEL3-IO.
                   15  BEL3                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  FLONN-IO.
                   15  FLONN               PICTURE S9(5)V9(2).
               10  OPPGNR                  PICTURE X(6).
               10  FIRMNR                  PICTURE X(3).
               10  FIRMNA                  PICTURE X(30).
           05  EDITTING-FIELDS.
               10  XO-52YYZ                PICTURE ZZ.ZZZ,ZZ.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
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
           IF  LONNMAS-PROCESS
               SET LONNMAS-PROCESS-OFF     TO TRUE
               SET LONNMAS-READ            TO TRUE
           END-IF
 
           IF  LONNMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM LONNMAS-GET
               SET LONNMAS-READ-OFF        TO TRUE
               IF  NOT LONNMAS-EOF
                   PERFORM LONNMAS-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET LONNMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  LONNMAS-PROCESS
               PERFORM LONNMAS-IDSET
           END-IF
 
           IF  LONNMAS-PROCESS
               PERFORM LONNMAS-CHK-LEVEL
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
 
           IF  LONNMAS-PROCESS
               PERFORM LONNMAS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  LONNMAS-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           SET NOT-I-51                    TO TRUE
      ********************************************
      *  TEST OM DETTE FIRMAET SKAL PRINTES.     *
      ********************************************
           IF  (I-U1)
               OR  (I-U2)
               SET I-51                    TO TRUE
           END-IF
           SET I-50                        TO TRUE
      ********************************************
      *    OPPSLAG VED LEVELBRUDD.               *
      ********************************************
           IF  (I-L2)
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               PERFORM SIDE1-S
      ********************************************
      * NULLSTILLE TOTALER.                      *
      ********************************************
           END-IF
           IF  (NOT-I-U1 AND NOT-I-U2)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               PERFORM VARYING ARF-I FROM 1 BY 1
                         UNTIL ARF-I > ARF-MAX
                   MOVE 0                  TO ARF (ARF-I)
               END-PERFORM
               SET ARF-I                   TO 1
           END-IF
           IF  (I-L1 AND I-U2)
               PERFORM VARYING ARA-I FROM 1 BY 1
                         UNTIL ARA-I > ARA-MAX
                   MOVE 0                  TO ARA (ARA-I)
               END-PERFORM
               SET ARA-I                   TO 1
      ********************************************
      *  FINNE FAST LØNN.                        *
      ********************************************
           END-IF
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-21                    TO TRUE
           IF  LK1 = '01J'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               SET NOT-I-22                TO TRUE
               IF  LK2 = '01J'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-21 AND NOT-I-22)
               SET NOT-I-23                TO TRUE
               IF  LK3 = '01J'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-21)
               ADD BEL1 TO ZERO        GIVING FLONN
           END-IF
           IF  (I-22)
               ADD BEL2 TO ZERO        GIVING FLONN
           END-IF
           IF  (I-23)
               ADD BEL3 TO ZERO        GIVING FLONN
      *********************************************
      *  SUMMERING PR FIRMA.                      *
      *********************************************
           END-IF
           ADD SUM1                        TO ARF (1)
           ADD SUM2                        TO ARF (2)
           ADD SUM3                        TO ARF (3)
           ADD SUM4                        TO ARF (4)
           ADD SUM5                        TO ARF (5)
           ADD SUM6                        TO ARF (6)
           ADD SUM7                        TO ARF (7)
           ADD SUM8                        TO ARF (8)
           ADD SUM9                        TO ARF (9)
           ADD SUM10                       TO ARF (10)
           ADD SUM11                       TO ARF (11)
           ADD SUM12                       TO ARF (12)
      *********************************************
      *  SUMMERING PR. AVDELING.                  *
      *********************************************
           IF  (NOT-I-U2)
               GO TO ENDAVD-T
           END-IF
           ADD SUM1                        TO ARA (1)
           ADD SUM2                        TO ARA (2)
           ADD SUM3                        TO ARA (3)
           ADD SUM4                        TO ARA (4)
           ADD SUM5                        TO ARA (5)
           ADD SUM6                        TO ARA (6)
           ADD SUM7                        TO ARA (7)
           ADD SUM8                        TO ARA (8)
           ADD SUM9                        TO ARA (9)
           ADD SUM10                       TO ARA (10)
           ADD SUM11                       TO ARA (11)
           ADD SUM12                       TO ARA (12).
 
       ENDAVD-T.
      **********************************************
           CONTINUE.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR PRINTING AV SIDE 1 PR. FIRMA      *
      ******************************************************
           CONTINUE.
 
       SIDE1-S SECTION.
       SIDE1-S-P.
           SET I-86                        TO TRUE
           MOVE 'ADL097'                   TO OPPGNR
           MOVE FNR                        TO FIRMNR
           MOVE FINAVN                     TO FIRMNA
           PERFORM EXCEPTION-OUTPUT
           SET NOT-I-86                    TO TRUE.
 
       LONNMAS-GET SECTION.
       LONNMAS-GET-P.
           IF  LONNMAS-EOF-OFF
               READ LONNMAS
               AT END
                   SET LONNMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LONNMAS-FLDSET SECTION.
       LONNMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ( LONNMAS-IO-AREA (1:1) = 'L' )
               MOVE LONNMAS-IO-AREA (2:3)  TO FNR (1:3)
               MOVE LONNMAS-IO-AREA (5:3)  TO ANR (1:3)
               MOVE LONNMAS-IO-AREA (8:4)  TO AVD (1:4)
               MOVE LONNMAS-IO-AREA (12:30) TO NAVN (1:30)
               MOVE LONNMAS-IO-AREA (42:30) TO ADR (1:30)
               MOVE LONNMAS-IO-AREA (72:20) TO POST (1:20)
               MOVE LONNMAS-IO-AREA (101:6) TO FDATO (1:6)
               MOVE LONNMAS-IO-AREA (107:5) TO PERSNR (1:5)
               MOVE LONNMAS-IO-AREA (112:6) TO ANSATT-IO
               INSPECT ANSATT-IO REPLACING ALL ' ' BY '0'
               MOVE LONNMAS-IO-AREA (135:20) TO STILL (1:20)
               MOVE LONNMAS-IO-AREA (164:4) TO KOMMNR (1:4)
               MOVE LONNMAS-IO-AREA (193:8) TO TELEF (1:8)
               MOVE LONNMAS-IO-AREA (271:5) TO SUM1-IO
               MOVE LONNMAS-IO-AREA (276:5) TO SUM2-IO
               MOVE LONNMAS-IO-AREA (281:5) TO SUM3-IO
               MOVE LONNMAS-IO-AREA (286:5) TO SUM4-IO
               MOVE LONNMAS-IO-AREA (291:5) TO SUM5-IO
               MOVE LONNMAS-IO-AREA (296:5) TO SUM6-IO
               MOVE LONNMAS-IO-AREA (301:5) TO SUM7-IO
               MOVE LONNMAS-IO-AREA (306:5) TO SUM8-IO
               MOVE LONNMAS-IO-AREA (311:5) TO SUM9-IO
               MOVE LONNMAS-IO-AREA (316:5) TO SUM10-IO
               MOVE LONNMAS-IO-AREA (325:5) TO SUM11-IO
               MOVE LONNMAS-IO-AREA (330:5) TO SUM12-IO
               MOVE LONNMAS-IO-AREA (341:3) TO LK1 (1:3)
               MOVE LONNMAS-IO-AREA (355:5) TO BEL1-IO
               MOVE LONNMAS-IO-AREA (379:3) TO LK2 (1:3)
               MOVE LONNMAS-IO-AREA (393:5) TO BEL2-IO
               MOVE LONNMAS-IO-AREA (417:3) TO LK3 (1:3)
               MOVE LONNMAS-IO-AREA (431:5) TO BEL3-IO
           END-EVALUATE.
 
       LONNMAS-IDCHK SECTION.
       LONNMAS-IDCHK-P.
           EVALUATE TRUE
           WHEN ( LONNMAS-IO-AREA (1:1) = 'L' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       LONNMAS-IDSET SECTION.
       LONNMAS-IDSET-P.
           EVALUATE TRUE
           WHEN ( LONNMAS-IO-AREA (1:1) = 'L' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       LONNMAS-CHK-LEVEL SECTION.
       LONNMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( LONNMAS-IO-AREA (1:1) = 'L' )
               MOVE LOW-VALUES             TO LONNMAS-LEVEL-01
               MOVE LONNMAS-IO-AREA (2:3)  TO LONNMAS-01-L2-FNR
               MOVE LONNMAS-IO-AREA (8:4)  TO LONNMAS-01-L1-AVD
               IF  LONNMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  LONNMAS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  LONNMAS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  LONNMAS-01-L2         TO THE-PRIOR-L2
               MOVE  LONNMAS-01-L1         TO THE-PRIOR-L1
               SET LONNMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE NAVN                   TO LISTE-IO-AREA (1:30)
               MOVE STILL                  TO LISTE-IO-AREA (32:20)
               MOVE TELEF                  TO LISTE-IO-AREA (53:8)
               IF  (I-51)
                   MOVE FDATO              TO LISTE-IO-AREA (63:6)
               END-IF
               IF  (I-51)
                   MOVE '.'                TO LISTE-IO-AREA (69:1)
               END-IF
               IF  (I-51)
                   MOVE PERSNR             TO LISTE-IO-AREA (70:5)
               END-IF
               IF  (I-51)
                   MOVE ANSATT             TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO LISTE-IO-AREA (76:8)
               END-IF
               IF  (I-51)
                   MOVE FLONN              TO XO-52YYZ
                   MOVE XO-52YYZ           TO LISTE-IO-AREA (85:9)
                   INITIALIZE FLONN
               END-IF
               IF  (I-51)
                   MOVE AVD                TO LISTE-IO-AREA (96:4)
               END-IF
               IF  (I-51)
                   MOVE KOMMNR             TO LISTE-IO-AREA (102:4)
               END-IF
               IF  (I-51)
                   MOVE ANR                TO LISTE-IO-AREA (108:3)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ADR                    TO LISTE-IO-AREA (1:30)
               MOVE POST                   TO LISTE-IO-AREA (32:20)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '******************'   TO LISTE-IO-AREA (41:18)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'OPPGAVENR. L24     PROG.' TO LISTE-IO-AREA (23:24)
               IF  (I-U1)
                   MOVE 'OPPGAVENR. L25     PROG.' TO LISTE-IO-AREA
                                                               (23:24)
               END-IF
               IF  (I-U2)
                   MOVE 'OPPGAVENR. L26     PROG.' TO LISTE-IO-AREA
                                                               (23:24)
               END-IF
               MOVE OPPGNR                 TO LISTE-IO-AREA (47:6)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'ANSATTOPPGAVE 3.        ' TO LISTE-IO-AREA (23:24)
               IF  (I-U1)
                   MOVE 'ANSATTOPPGAVE 1.        ' TO LISTE-IO-AREA
                                                               (23:24)
               END-IF
               IF  (I-U2)
                   MOVE 'ANSATTOPPGAVE 2.        ' TO LISTE-IO-AREA
                                                               (23:24)
               END-IF
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'TELEFONLISTE.           ' TO LISTE-IO-AREA (23:24)
               IF  (I-U1)
                   MOVE '                        ' TO LISTE-IO-AREA
                                                               (23:24)
               END-IF
               IF  (I-U2)
                   MOVE 'PR. AVDELING.           ' TO LISTE-IO-AREA
                                                               (23:24)
               END-IF
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     DENNE OPPGAVE'  TO LISTE-IO-AREA (17:19)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     FREMSTILT'      TO LISTE-IO-AREA (17:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (33:8)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     TILHØRER FIRMA' TO LISTE-IO-AREA (17:20)
               MOVE FIRMNR                 TO LISTE-IO-AREA (38:3)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE FIRMNA                 TO LISTE-IO-AREA (23:30)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '******************'   TO LISTE-IO-AREA (41:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND I-50)
           OR  (I-L1 AND I-50 AND I-U2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-10)
                   MOVE FINAVN             TO LISTE-IO-AREA (1:30)
               END-IF
               MOVE 'A N S A T T'          TO LISTE-IO-AREA (35:11)
               MOVE 'O P P G A V E'        TO LISTE-IO-AREA (47:13)
               MOVE '3'                    TO LISTE-IO-AREA (63:1)
               IF  (I-U1)
                   MOVE '1'                TO LISTE-IO-AREA (63:1)
               END-IF
               IF  (I-U2)
                   MOVE '2'                TO LISTE-IO-AREA (63:1)
               END-IF
               IF  (I-U2)
                   MOVE 'AVD.'             TO LISTE-IO-AREA (70:4)
               END-IF
               IF  (I-U2)
                   MOVE AVD                TO LISTE-IO-AREA (75:4)
               END-IF
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NAVN OG ADRESSE'      TO LISTE-IO-AREA (2:15)
               MOVE 'STILLING'             TO LISTE-IO-AREA (32:8)
               MOVE 'TELEFON '             TO LISTE-IO-AREA (53:8)
               IF  (I-51)
                   MOVE 'PERSONNR.'        TO LISTE-IO-AREA (63:9)
               END-IF
               IF  (I-51)
                   MOVE 'ANSATT'           TO LISTE-IO-AREA (78:6)
               END-IF
               IF  (I-51)
                   MOVE 'FAST LØNN'        TO LISTE-IO-AREA (85:9)
               END-IF
               IF  (I-51)
                   MOVE 'AVD.  KOMM  A.NR' TO LISTE-IO-AREA (96:16)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-10)
                   MOVE FINAVN             TO LISTE-IO-AREA (1:30)
               END-IF
               MOVE 'A N S A T T'          TO LISTE-IO-AREA (35:11)
               MOVE 'O P P G A V E'        TO LISTE-IO-AREA (47:13)
               MOVE '3'                    TO LISTE-IO-AREA (63:1)
               IF  (I-U1)
                   MOVE '1'                TO LISTE-IO-AREA (63:1)
               END-IF
               IF  (I-U2)
                   MOVE '2'                TO LISTE-IO-AREA (63:1)
               END-IF
               IF  (I-U2)
                   MOVE 'AVD.'             TO LISTE-IO-AREA (70:4)
               END-IF
               IF  (I-U2)
                   MOVE AVD                TO LISTE-IO-AREA (75:4)
               END-IF
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NAVN OG ADRESSE'      TO LISTE-IO-AREA (2:15)
               MOVE 'STILLING'             TO LISTE-IO-AREA (32:8)
               MOVE 'TELEFON '             TO LISTE-IO-AREA (53:8)
               IF  (I-51)
                   MOVE 'PERSONNR.'        TO LISTE-IO-AREA (63:9)
               END-IF
               IF  (I-51)
                   MOVE 'ANSATT'           TO LISTE-IO-AREA (78:6)
               END-IF
               IF  (I-51)
                   MOVE 'FAST LØNN'        TO LISTE-IO-AREA (85:9)
               END-IF
               IF  (I-51)
                   MOVE 'AVD.  KOMM  A.NR' TO LISTE-IO-AREA (96:16)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-50 AND I-U2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-10)
                   MOVE FINAVN             TO LISTE-IO-AREA (1:30)
               END-IF
               MOVE 'A N S A T T'          TO LISTE-IO-AREA (35:11)
               MOVE 'O P P G A V E'        TO LISTE-IO-AREA (47:13)
               MOVE '3'                    TO LISTE-IO-AREA (63:1)
               IF  (I-U1)
                   MOVE '1'                TO LISTE-IO-AREA (63:1)
               END-IF
               IF  (I-U2)
                   MOVE '2'                TO LISTE-IO-AREA (63:1)
               END-IF
               IF  (I-U2)
                   MOVE 'AVD.'             TO LISTE-IO-AREA (70:4)
               END-IF
               IF  (I-U2)
                   MOVE AVD                TO LISTE-IO-AREA (75:4)
               END-IF
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVDELINGSTOTALER'     TO LISTE-IO-AREA (10:16)
               MOVE 'HITTIL I ÅR.'         TO LISTE-IO-AREA (27:12)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BRUTTO LØNN.............' TO LISTE-IO-AREA (10:24)
               MOVE ARA (1)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SKATTETREKK.............' TO LISTE-IO-AREA (10:24)
               MOVE ARA (2)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PÅLEGGSTREKK............' TO LISTE-IO-AREA (10:24)
               MOVE ARA (3)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BIDRAGSTREKK............' TO LISTE-IO-AREA (10:24)
               MOVE ARA (4)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PENSJON.................' TO LISTE-IO-AREA (10:24)
               MOVE ARA (5)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONTIGENT...............' TO LISTE-IO-AREA (10:24)
               MOVE ARA (6)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SMUSSTILLEGG............' TO LISTE-IO-AREA (10:24)
               MOVE ARA (7)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BILGODTGJØRELSE.........' TO LISTE-IO-AREA (10:24)
               MOVE ARA (8)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL KILOMETER........' TO LISTE-IO-AREA (10:24)
               MOVE ARA (9)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRUNNLAG FERIEPENGER....' TO LISTE-IO-AREA (10:24)
               MOVE ARA (10)               TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SYKELØNN................' TO LISTE-IO-AREA (10:24)
               MOVE ARA (11)               TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FERIEPENGER TIL UTBET...' TO LISTE-IO-AREA (10:24)
               MOVE ARA (12)               TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-50 AND I-51)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-10)
                   MOVE FINAVN             TO LISTE-IO-AREA (1:30)
               END-IF
               MOVE 'A N S A T T'          TO LISTE-IO-AREA (35:11)
               MOVE 'O P P G A V E'        TO LISTE-IO-AREA (47:13)
               MOVE '3'                    TO LISTE-IO-AREA (63:1)
               IF  (I-U1)
                   MOVE '1'                TO LISTE-IO-AREA (63:1)
               END-IF
               IF  (I-U2)
                   MOVE '2'                TO LISTE-IO-AREA (63:1)
               END-IF
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '    FIRMATOTALER'     TO LISTE-IO-AREA (10:16)
               MOVE 'HITTIL I ÅR.'         TO LISTE-IO-AREA (27:12)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BRUTTO LØNN.............' TO LISTE-IO-AREA (10:24)
               MOVE ARF (1)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SKATTETREKK.............' TO LISTE-IO-AREA (10:24)
               MOVE ARF (2)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PÅLEGGSTREKK............' TO LISTE-IO-AREA (10:24)
               MOVE ARF (3)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BIDRAGSTREKK............' TO LISTE-IO-AREA (10:24)
               MOVE ARF (4)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PENSJON.................' TO LISTE-IO-AREA (10:24)
               MOVE ARF (5)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONTIGENT...............' TO LISTE-IO-AREA (10:24)
               MOVE ARF (6)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SMUSSTILLEGG............' TO LISTE-IO-AREA (10:24)
               MOVE ARF (7)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BILGODTGJØRELSE.........' TO LISTE-IO-AREA (10:24)
               MOVE ARF (8)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL KILOMETER........' TO LISTE-IO-AREA (10:24)
               MOVE ARF (9)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRUNNLAG FERIEPENGER....' TO LISTE-IO-AREA (10:24)
               MOVE ARF (10)               TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SYKELØNN................' TO LISTE-IO-AREA (10:24)
               MOVE ARF (11)               TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FERIEPENGER TIL UTBET...' TO LISTE-IO-AREA (10:24)
               MOVE ARF (12)               TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (34:14)
      *******************************************
      *  PRINTRUTINE FOR FØRSTESIDE PR. FIRMA.  *
      *******************************************
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
           MOVE 1                          TO LR-CHECK
           SET LONNMAS-LEVEL-INIT          TO TRUE
           INITIALIZE LONNMAS-DATA-FIELDS
           SET LONNMAS-EOF-OFF             TO TRUE
           SET LONNMAS-PROCESS             TO TRUE
           OPEN INPUT LONNMAS
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           PERFORM VARYING ARF-I FROM 1 BY 1
                     UNTIL ARF-I > ARF-MAX
               INITIALIZE ARF (ARF-I)
           END-PERFORM
           SET ARF-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE LONNMAS
           CLOSE FIRMAF
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
