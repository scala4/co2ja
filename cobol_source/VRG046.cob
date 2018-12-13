       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG046R.
      **********************************************  Z-WIN-RPG2      *
      * PROGRAM............ VRG046               XX2000XXIRXXEN       *
      * PROGRAMMERER....... ESPEN LARSEN - JCL=XVSAM34X               *
      * PROGRAMERT......... 10/3-1992                                 *
      * PROGRAMMET MERGER VARE.OVERF.ALTERNATIV.FILE MED VAREMASTER   *
      * PÅ ALTERNATIV EDB-NR FOR Å FINNE ALTERTIV ARTIKKELNR.         *
      * DETTE ARTIKKELNR ENDRES TIL OPPSLAGSNUMMER.                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG046.rpg
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
           SELECT VARE
               ASSIGN TO UT-S-VARE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARE-STATUS.
           SELECT VOAINN
               ASSIGN TO UT-S-VOAINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VOAINN-STATUS.
           SELECT VOAUT
               ASSIGN TO UT-S-VOAUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VOAUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  VARE-IO-AREA.
           05  VARE-IO-AREA-X              PICTURE X(200).
       FD VOAINN
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VOAINN-IO-AREA.
           05  VOAINN-IO-AREA-X            PICTURE X(60).
       FD VOAUT
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VOAUT-IO-AREA.
           05  VOAUT-IO-AREA-X             PICTURE X(60).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       77  ARO-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(1).
           05  ARO-TABLE.
               10  ARO-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARO-I
                                                      ARO-S.
                   15  ARO                 PICTURE X(1).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VARE-STATUS                 PICTURE 99 VALUE 0.
           10  VOAINN-STATUS               PICTURE 99 VALUE 0.
           10  VOAUT-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-EOF-OFF            VALUE '0'.
               88  VARE-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-READ-OFF           VALUE '0'.
               88  VARE-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-PROCESS-OFF        VALUE '0'.
               88  VARE-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARE-LEVEL-INIT-OFF     VALUE '0'.
               88  VARE-LEVEL-INIT         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-EOF-OFF          VALUE '0'.
               88  VOAINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-READ-OFF         VALUE '0'.
               88  VOAINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-PROCESS-OFF      VALUE '0'.
               88  VOAINN-PROCESS          VALUE '1'.
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
           05  VARE-LEVEL-01.
               10  VARE-01-L2.
                   15  VARE-01-L2-FIRMA    PICTURE X(3).
               10  VARE-01-L1.
                   15  VARE-01-L1-EDBNR    PICTURE X(7).
           05  VARE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  ALF                     PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ART5                    PICTURE X(15).
               10  ART10                   PICTURE X(10).
               10  ART15                   PICTURE X(5).
           05  VARE-MP                     PICTURE X(10).
           05  VARE-MC                     PICTURE X(10).
           05  VARE-M-01               REDEFINES VARE-MC.
               10  VARE-M-01-M2.
                   15  VARE-M-01-M2-FIRMA-G.
                       20  VARE-M-01-M2-FIRMA PICTURE X(3).
               10  VARE-M-01-M1.
                   15  VARE-M-01-M1-EDBNR-G.
                       20  VARE-M-01-M1-EDBNR PICTURE X(7).
           05  VOAINN-DATA-FIELDS.
               10  ALTEDB                  PICTURE X(7).
               10  EDB                     PICTURE X(7).
               10  VOAREC                  PICTURE X(60).
           05  VOAINN-MP                   PICTURE X(10).
           05  VOAINN-MC                   PICTURE X(10).
           05  VOAINN-M-02             REDEFINES VOAINN-MC.
               10  VOAINN-M-02-M2.
                   15  VOAINN-M-02-M2-FIRMA-G.
                       20  VOAINN-M-02-M2-FIRMA PICTURE X(3).
               10  VOAINN-M-02-M1.
                   15  VOAINN-M-02-M1-ALTEDB-G.
                       20  VOAINN-M-02-M1-ALTEDB PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(7).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(7).
               10  ANTLIK-IO.
                   15  ANTLIK              PICTURE S9(7).
               10  MAXANT-IO.
                   15  MAXANT              PICTURE S9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  Z-IO.
                   15  Z                   PICTURE S9(2).
               10  ANR                     PICTURE X(1).
               10  ANTOK-IO.
                   15  ANTOK               PICTURE S9(7).
           05  EDITTING-FIELDS.
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
           05  MOVEA-COUNT                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE1                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA1                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE2                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA2                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-LENGTH                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-OFFSET                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-TEMP                  PICTURE X(256).
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
           IF  VARE-PROCESS
               SET VARE-PROCESS-OFF        TO TRUE
               SET VARE-READ               TO TRUE
           END-IF
 
           IF  VARE-READ
               PERFORM VARE-GET
               SET VARE-READ-OFF           TO TRUE
               IF  NOT VARE-EOF
                   PERFORM VARE-MATCH-SET
               END-IF
           END-IF
 
           IF  VOAINN-PROCESS
               SET VOAINN-PROCESS-OFF      TO TRUE
               SET VOAINN-READ             TO TRUE
           END-IF
 
           IF  VOAINN-READ
               PERFORM VOAINN-GET
               SET VOAINN-READ-OFF         TO TRUE
               IF  NOT VOAINN-EOF
                   PERFORM VOAINN-MATCH-SET
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
 
           IF  VARE-PROCESS
               PERFORM VARE-IDSET
           END-IF
 
           IF  VOAINN-PROCESS
               PERFORM VOAINN-IDSET
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-CHK-LEVEL
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
 
           IF  VARE-PROCESS
               PERFORM VARE-FLDOFF
               PERFORM VARE-FLDSET
           END-IF
 
           IF  VOAINN-PROCESS
               PERFORM VOAINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARE-PROCESS
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
           SET NOT-I-52                    TO TRUE
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT01
           END-IF
           IF  (I-02 AND NOT-I-MR)
               SET I-51                    TO TRUE
               ADD 1                       TO ANTNMR
               GO TO SLUTT-T
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  EDB = ALTEDB
               SET I-52                    TO TRUE
           END-IF
           IF  (I-02 AND I-52)
               ADD 1                       TO ANTLIK
               GO TO SLUTT-T
      ****************************************************
      *     RUTINE FOR Å LAGE OPPSLAGSNUMMER.            *
      ****************************************************
           END-IF
           MOVE 20                         TO MAXANT
           IF  (I-07)
               MOVE 15                     TO MAXANT
           END-IF
           IF  (I-06)
               MOVE 10                     TO MAXANT
           END-IF
           IF  (I-05)
               MOVE 5                      TO MAXANT
      *
           END-IF
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               MOVE ' '                    TO ARO (ARO-I)
           END-PERFORM
           MOVE 1                          TO MOVEA-SA1 MOVEA-SA2
           MOVE 20                         TO MOVEA-SIZE1
           MULTIPLY ARA-MAX BY 1 GIVING MOVEA-SIZE2
           IF  MOVEA-SIZE1 > MOVEA-SIZE2
               MOVE MOVEA-SIZE2            TO MOVEA-SIZE1
           END-IF
           MOVE ARTNR
                    TO ARA-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           SUBTRACT X                      FROM X
           SUBTRACT Z                      FROM Z
      *
           .
 
       RUTA-T.
           ADD 1                           TO X
           SET NOT-I-21                    TO TRUE
           IF  X > MAXANT
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO PRINT-X-T
           END-IF
           MOVE ARA (X)                    TO ANR
           SET NOT-I-40                    TO TRUE
           IF  ANR = '.'
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '""""
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '&'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ','
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '+'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '-'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '/'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ')'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '('
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '*'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ' '
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               GO TO RUTA-T
      **********************************************************
      *      RUTINE FOR OPPBYGGING AV OPPSLAGSNUMMER           *
      **********************************************************
           END-IF
           ADD 1                           TO Z
           SET NOT-I-22                    TO TRUE
           IF  Z > MAXANT
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               GO TO PRINT-X-T
           END-IF
           MOVE ANR                        TO ARO (Z)
           GO TO RUTA-T
      **********************************************************
           .
 
       PRINT-X-T.
           SET I-50                        TO TRUE
           IF  (I-02 AND I-50)
               ADD 1                       TO ANTOK
           END-IF.
 
       SLUTT-T.
      ******************************************************
           CONTINUE.
 
       VARE-GET SECTION.
       VARE-GET-P.
           IF  VARE-EOF-OFF
               READ VARE
               AT END
                   SET VARE-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARE-FLDOFF SECTION.
       VARE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-05                TO TRUE
               SET NOT-I-06                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       VARE-FLDSET SECTION.
       VARE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE VARE-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE VARE-IO-AREA (13:3)    TO ALF (1:3)
               MOVE VARE-IO-AREA (16:20)   TO ARTNR (1:20)
               MOVE VARE-IO-AREA (21:15)   TO ART5 (1:15)
               IF  ART5 = SPACES
                   SET I-05                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (26:10)   TO ART10 (1:10)
               IF  ART10 = SPACES
                   SET I-06                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (31:5)    TO ART15 (1:5)
               IF  ART15 = SPACES
                   SET I-07                TO TRUE
               END-IF
           END-EVALUATE.
 
       VARE-IDSET SECTION.
       VARE-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARE-CHK-LEVEL SECTION.
       VARE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARE-LEVEL-01
               MOVE VARE-IO-AREA (3:3)     TO VARE-01-L2-FIRMA
               MOVE VARE-IO-AREA (6:7)     TO VARE-01-L1-EDBNR
               IF  VARE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARE-01-L2            TO THE-PRIOR-L2
               MOVE  VARE-01-L1            TO THE-PRIOR-L1
               SET VARE-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       VARE-MATCH-SET SECTION.
       VARE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO VARE-M-01-M2-FIRMA
               MOVE VARE-IO-AREA (6:7)     TO VARE-M-01-M1-EDBNR
           END-EVALUATE.
 
       VOAINN-GET SECTION.
       VOAINN-GET-P.
           IF  VOAINN-EOF-OFF
               READ VOAINN
               AT END
                   SET VOAINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VOAINN-FLDSET SECTION.
       VOAINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VOAINN-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE VOAINN-IO-AREA (11:7)  TO ALTEDB (1:7)
               MOVE VOAINN-IO-AREA (4:7)   TO EDB (1:7)
               MOVE VOAINN-IO-AREA (1:60)  TO VOAREC (1:60)
           END-EVALUATE.
 
       VOAINN-IDSET SECTION.
       VOAINN-IDSET-P.
           SET I-02                        TO TRUE.
 
       VOAINN-MATCH-SET SECTION.
       VOAINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VOAINN-IO-AREA (1:3)   TO VOAINN-M-02-M2-FIRMA
               MOVE VOAINN-IO-AREA (11:7)  TO VOAINN-M-02-M1-ALTEDB
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VARE-EOF
               MOVE HIGH-VALUES            TO VARE-MC
                                              VARE-MP
           END-IF
           IF  VOAINN-EOF
               MOVE HIGH-VALUES            TO VOAINN-MC
                                              VOAINN-MP
           END-IF
           IF  VARE-MC < VARE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VOAINN-MC < VOAINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VARE-MC < VOAINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARE-PROCESS        TO TRUE
                   MOVE VARE-MC            TO VARE-MP
                   IF  VARE-MC = VOAINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VOAINN-MC < VARE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VOAINN-PROCESS      TO TRUE
                   MOVE VOAINN-MC          TO VOAINN-MP
                   IF  VOAINN-MC = VARE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARE-MC = VOAINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARE-PROCESS        TO TRUE
                   MOVE VARE-MC            TO VARE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO VOAUT-IO-AREA
               INITIALIZE VOAUT-IO-AREA
               MOVE VOAREC                 TO VOAUT-IO-AREA (1:60)
               IF  (I-50)
                   MOVE ALF                TO VOAUT-IO-AREA (18:3)
               END-IF
               IF  (I-50)
                   MOVE 42                 TO BW-A
                   PERFORM VARYING ARO-I FROM ARO-MAX BY -1
                             UNTIL ARO-I < 1
                       SUBTRACT 1        FROM BW-A
                       MOVE ARO-ENTRY (ARO-I) TO VOAUT-IO-AREA (BW-A:1)
                   END-PERFORM
               END-IF
               IF  (I-50)
                   MOVE 'A'                TO VOAUT-IO-AREA (59:1)
               END-IF
               IF  (I-51)
                   MOVE 'B'                TO VOAUT-IO-AREA (59:1)
               END-IF
               IF  (I-52)
                   MOVE 'E'                TO VOAUT-IO-AREA (59:1)
               END-IF
               WRITE VOAUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG046 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT RECORDS FRA M/ALT' TO LISTE-IO-AREA (1:24)
               MOVE ' LEST.  '             TO LISTE-IO-AREA (25:8)
               MOVE ANT01                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT REC. MED FEIL ALT' TO LISTE-IO-AREA (1:24)
               MOVE '. EDBNR.'             TO LISTE-IO-AREA (25:8)
               MOVE ANTNMR                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT REC. MED SAMME ED' TO LISTE-IO-AREA (1:24)
               MOVE 'BNR.    '             TO LISTE-IO-AREA (25:8)
               MOVE ANTLIK                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT RECORDS OPPDAT. M' TO LISTE-IO-AREA (1:24)
               MOVE 'ED ALT.ART'           TO LISTE-IO-AREA (25:10)
               MOVE ANTOK                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           SET VARE-LEVEL-INIT             TO TRUE
           INITIALIZE VARE-DATA-FIELDS
           SET VARE-EOF-OFF                TO TRUE
           SET VARE-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO VARE-MC
                                              VARE-MP
           OPEN INPUT VARE
           INITIALIZE VOAINN-DATA-FIELDS
           SET VOAINN-EOF-OFF              TO TRUE
           SET VOAINN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO VOAINN-MC
                                              VOAINN-MP
           OPEN INPUT VOAINN
           OPEN OUTPUT VOAUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               INITIALIZE ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARE
           CLOSE VOAINN
           CLOSE VOAUT
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
