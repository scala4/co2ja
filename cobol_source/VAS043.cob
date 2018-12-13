       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS043R.
      ******************************************* :   Z-WIN-RPG2P    **
      * PROGRAMMET DANNER AJOURHOLDSRECORD"S TIL VARE.STAT.MASTER  *
      * INPUT = FAKTURA VAREREC.  SORTERT PR. FIRMA,EDBNR OG       *
      *         SALGS ÅR,MND,DAG (SISTE DATO SIST PR. EDBNR.)      *
      *    SUMMERING PÅ ANTALL OG NETTO FAKTURA SUM.               *
      *    SKAFFEVARENR. (EDB.NR 9XXXXXX ER IKKE MED)              *
      *    PRISTILEGG/PANT HAR LAGERKODE "PT" (ANTALL SUMMERS IKKE)*
      *    14.03.97  FIRMA 923 HISTORIKK PR LAGERSTED              *
      *              LAGER 10 = 00  LAGER 15 = 15  STEIN SANDVOLD  *
      *    21.06.00  FIRMA 626 HISTORIKK PR LAGERSTED              *
      *    21.06.00  FIRMA 658 HISTORIKK PR LAGERSTED              *
      *    21.06.00  SKAFFEVARE TAS MED
      *    16.08.01  FIRMA 938 (627) HISTORIKK PR. LAGERSTED
      *    01.01.002 FIRMA 956 HISTORIKK PR. LAGERSTED
      *    23.01.002 FIRMA 975 HISTORIKK PR. LAGERSTED
      *    21.10.009 lagt fakturapar. år mnd i output
      **************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS043.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FAKREC
               ASSIGN TO UT-S-FAKREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKREC-STATUS.
           SELECT VARSTAA
               ASSIGN TO UT-S-VARSTAA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTAA-STATUS.
           SELECT PRF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FAKREC
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKREC-IO-AREA.
           05  FAKREC-IO-AREA-X            PICTURE X(200).
       FD VARSTAA
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  VARSTAA-IO-AREA.
           05  VARSTAA-IO-AREA-X           PICTURE X(40).
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
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FAKREC-STATUS               PICTURE 99 VALUE 0.
           10  VARSTAA-STATUS              PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-EOF-OFF          VALUE '0'.
               88  FAKREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-READ-OFF         VALUE '0'.
               88  FAKREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-PROCESS-OFF      VALUE '0'.
               88  FAKREC-PROCESS          VALUE '1'.
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
           05  FAKPAR-DATA-FIELDS.
               10  PA-ELGR                 PICTURE X(2).
               10  PMND                    PICTURE X(2).
           05  FAKREC-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  FAKT                    PICTURE X(1).
               10  LAGERK                  PICTURE X(1).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  MND                     PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  VGR                     PICTURE X(5).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  RABA-IO.
                   15  RABA                PICTURE S9(2)V9(1).
               10  RABB-IO.
                   15  RABB                PICTURE S9(2)V9(1).
               10  RABC-IO.
                   15  RABC                PICTURE S9(2)V9(1).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  KRTYPE                  PICTURE X(1).
               10  ALF                     PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  ANTL1-IO.
                   15  ANTL1               PICTURE S9(7)V9(2).
               10  SUML1-IO.
                   15  SUML1               PICTURE S9(7)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(9)V9(2).
               10  SUMA-IO.
                   15  SUMA                PICTURE S9(9)V9(2).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(7)V9(2).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(6).
               10  F6-IO.
                   15  F6                  PICTURE S9(6).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  AAMMDD-IO.
                   15  AAMMDD              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKPAR-PROCESS
               SET FAKPAR-PROCESS-OFF      TO TRUE
               SET FAKPAR-READ             TO TRUE
           END-IF
 
           IF  FAKPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKPAR-GET
               SET FAKPAR-READ-OFF         TO TRUE
               IF  NOT FAKPAR-EOF
                   SET FAKPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FAKREC-PROCESS
               SET FAKREC-PROCESS-OFF      TO TRUE
               SET FAKREC-READ             TO TRUE
           END-IF
 
           IF  FAKREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKREC-GET
               SET FAKREC-READ-OFF         TO TRUE
               IF  NOT FAKREC-EOF
                   PERFORM FAKREC-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET FAKREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
           END-IF
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-IDSET
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
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
           END-IF
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-FLDOFF
               PERFORM FAKREC-FLDSET
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
           SET NOT-I-60                    TO TRUE
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           IF  (I-49)
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO ANT
           SET NOT-I-31                    TO TRUE
           IF  FIRMA = '915'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               SET NOT-I-31                TO TRUE
               IF  LAGERK = '15'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  FIRMA = '626'
               SET I-32                    TO TRUE
           END-IF
      * N32      FIRMA     COMP "938"                    32  AMFIBIUM
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '627'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '975'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-32)
               SET NOT-I-32                TO TRUE
               IF  LAGERK = '13'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  FIRMA = '956'
               SET I-33                    TO TRUE
           END-IF
           IF  (I-33)
               SET NOT-I-34                TO TRUE
               IF  LAGERK = '13'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  LAGERK = '15'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'VEN'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'GAB'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'TIL'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'BOS'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'CAT'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND I-35)
               SET NOT-I-34                TO TRUE
      ** SKAFFEVARER SKAL MED HERETTER 20.06.00
      *          EDBNR     COMP "8999999"            90      SKAFFEVARER
      *  90                GOTO SLUTT
           END-IF
           MOVE 0                          TO ANTL1
           MOVE 0                          TO SUML1
           MOVE 0                          TO SUM-X
           MOVE 0                          TO SUMA
           ADD ANTUT TO ZERO           GIVING ANTALL
           SET NOT-I-99                    TO TRUE
           IF  LAGERK = 'PT'
               SET I-99                    TO TRUE
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  FAKT = 'F'
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-12                TO TRUE
               IF  KRTYPE = '2'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-12)
               MULTIPLY -1 BY ANTALL   GIVING ANTALL
           END-IF
           IF  (NOT-I-10 AND NOT-I-12)
               OR  (I-99)
               MOVE 0,00                   TO ANTALL
           END-IF
           ADD ANTALL                      TO ANTL1
           SET I-60                        TO TRUE
      *    RUTINE FOR Å BEREGNE NETTO SALGSSUM.
           IF  (I-77)
               ADD PRIS TO ZERO        GIVING SUM-X
           END-IF
           IF  (NOT-I-77)
               MULTIPLY PRIS BY ANTUT  GIVING SUM-X
           END-IF
           IF  (NOT-I-50)
               MULTIPLY RABA BY SUM-X  GIVING SUMA ROUNDED
               DIVIDE SUMA BY 100      GIVING SUMA ROUNDED
               SUBTRACT SUMA               FROM SUM-X
           END-IF
           IF  (NOT-I-51)
               MULTIPLY RABB BY SUM-X  GIVING SUMA ROUNDED
               DIVIDE SUMA BY 100      GIVING SUMA ROUNDED
               SUBTRACT SUMA               FROM SUM-X
           END-IF
           IF  (NOT-I-52)
               MULTIPLY RABC BY SUM-X  GIVING SUMA ROUNDED
               DIVIDE SUMA BY 100      GIVING SUMA ROUNDED
               SUBTRACT SUMA               FROM SUM-X
           END-IF
           IF  (I-10)
               ADD SUM-X                   TO SUML1
           END-IF
           IF  (NOT-I-10)
               SUBTRACT SUM-X              FROM SUML1
           END-IF
           IF  (I-60)
               ADD 1                       TO ANTNYE
      *****************************************************
      * RUTINE FOR Å SNU DATO FOR SISTE SALG TIL ÅÅMMDD   *
      *****************************************************
           END-IF
           IF  (I-60)
               MOVE DATO                   TO F6-IO
               MOVE F6 (1:2)               TO DD
               MOVE F6 (5:2)               TO AA-IO
               MOVE F6                     TO AAMMDD-IO
               MOVE AA                     TO AAMMDD (1:2)
               MOVE DD                     TO AAMMDD-IO (5:2)
      *****************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       FAKPAR-GET SECTION.
       FAKPAR-GET-P.
           IF  FAKPAR-EOF-OFF
               READ FAKPAR
               AT END
                   SET FAKPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (6:2)   TO PA-ELGR (1:2)
               MOVE FAKPAR-IO-AREA (8:2)   TO PMND (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-04                        TO TRUE.
 
       FAKREC-GET SECTION.
       FAKREC-GET-P.
           IF  FAKREC-EOF-OFF
               READ FAKREC
               AT END
                   SET FAKREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKREC-FLDOFF SECTION.
       FAKREC-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( FAKREC-IO-AREA (25:1) = 'L' )
               SET NOT-I-77                TO TRUE
               SET NOT-I-49                TO TRUE
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-52                TO TRUE
           END-EVALUATE.
 
       FAKREC-FLDSET SECTION.
       FAKREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FAKREC-IO-AREA (25:1) = 'L' )
               MOVE FAKREC-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE FAKREC-IO-AREA (10:1)  TO FAKT (1:1)
               MOVE FAKREC-IO-AREA (63:1)  TO LAGERK (1:1)
               MOVE FAKREC-IO-AREA (71:6)  TO DATO-IO
               INSPECT DATO-IO REPLACING ALL ' ' BY '0'
               MOVE FAKREC-IO-AREA (73:2)  TO MND (1:2)
               MOVE FAKREC-IO-AREA (75:2)  TO AAR (1:2)
               MOVE FAKREC-IO-AREA (77:5)  TO VGR (1:5)
               MOVE FAKREC-IO-AREA (137:4) TO ANTUT-IO
               IF  ANTUT = ZERO
                   SET I-77                TO TRUE
               END-IF
               MOVE FAKREC-IO-AREA (141:7) TO EDBNR (1:7)
               IF  EDBNR = SPACES
                   SET I-49                TO TRUE
               END-IF
               MOVE FAKREC-IO-AREA (148:3) TO RABA-IO
               INSPECT RABA-IO REPLACING ALL ' ' BY '0'
               IF  RABA = ZERO
                   SET I-50                TO TRUE
               END-IF
               MOVE FAKREC-IO-AREA (151:3) TO RABB-IO
               INSPECT RABB-IO REPLACING ALL ' ' BY '0'
               IF  RABB = ZERO
                   SET I-51                TO TRUE
               END-IF
               MOVE FAKREC-IO-AREA (154:3) TO RABC-IO
               INSPECT RABC-IO REPLACING ALL ' ' BY '0'
               IF  RABC = ZERO
                   SET I-52                TO TRUE
               END-IF
               MOVE FAKREC-IO-AREA (157:9) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE FAKREC-IO-AREA (179:1) TO KRTYPE (1:1)
               MOVE FAKREC-IO-AREA (190:3) TO ALF (1:3)
           END-EVALUATE.
 
       FAKREC-IDCHK SECTION.
       FAKREC-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FAKREC-IO-AREA (25:1) = 'L' )
             OR ( FAKREC-IO-AREA (25:1) NOT = 'L' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FAKREC-IDSET SECTION.
       FAKREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( FAKREC-IO-AREA (25:1) = 'L' )
               SET I-01                    TO TRUE
           WHEN ( FAKREC-IO-AREA (25:1) NOT = 'L' )
               SET I-02                    TO TRUE
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
               MOVE 7                      TO PRF-AFTER-SKIP
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-60)
               MOVE SPACES TO VARSTAA-IO-AREA
               INITIALIZE VARSTAA-IO-AREA
               MOVE 'E'                    TO VARSTAA-IO-AREA (1:1)
               MOVE FIRMA                  TO VARSTAA-IO-AREA (2:3)
               MOVE EDBNR                  TO VARSTAA-IO-AREA (5:7)
               MOVE '00'                   TO VARSTAA-IO-AREA (12:2)
               IF  (I-31)
                   MOVE LAGERK             TO VARSTAA-IO-AREA (13:1)
               END-IF
               IF  (I-32)
                   MOVE LAGERK             TO VARSTAA-IO-AREA (13:1)
               END-IF
               IF  (I-33 AND I-34)
                   MOVE LAGERK             TO VARSTAA-IO-AREA (13:1)
               END-IF
               MOVE VGR                    TO VARSTAA-IO-AREA (17:5)
               MOVE AAMMDD                 TO XO-60P
               MOVE XO-60P-EF              TO VARSTAA-IO-AREA (22:4)
               MOVE PA-ELGR                TO VARSTAA-IO-AREA (26:2)
               MOVE PMND                   TO VARSTAA-IO-AREA (28:2)
               MOVE ANTL1                  TO XO-72P
               MOVE XO-72P-EF              TO VARSTAA-IO-AREA (31:5)
               MOVE SUML1                  TO XO-72P
               MOVE XO-72P-EF              TO VARSTAA-IO-AREA (36:5)
               WRITE VARSTAA-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (4:7)
               MOVE 'FAKT.RECORDS LEST'    TO PRF-IO-AREA (13:17)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTNYE                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (4:7)
               MOVE 'STAT.AJOURHOLDRECORDS' TO PRF-IO-AREA (12:21)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '*** VAS043 ***  KJØRT' TO PRF-IO-AREA (12:21)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (33:8)
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
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           INITIALIZE FAKREC-DATA-FIELDS
           SET FAKREC-EOF-OFF              TO TRUE
           SET FAKREC-PROCESS              TO TRUE
           OPEN INPUT FAKREC
           OPEN OUTPUT VARSTAA
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKPAR
           CLOSE FAKREC
           CLOSE VARSTAA
           IF PRF-IO-AREA NOT = SPACES
             WRITE PRF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRF-IO-AREA
           END-IF
           CLOSE PRF.
 
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
