       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS105R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET DANNER AJOURHOLDSRECORD"S TIL VARE.STAT.MASTER  *
      * INPUT = FAKTURA VAREREC.  SORTERT PR. FIRMA,EDBNR OG       *
      *         SALGS ÅR,MND,DAG (SISTE DATO SIST PR. EDBNR.)      *
      *    SUMMERING PÅ ANTALL OG NETTO FAKTURA SUM.               *
      *    SKAFFEVARENR. (EDB.NR 9XXXXXX ER IKKE MED)              *
      *    PRISTILEGG/PANT HAR LAGERKODE "PT" (ANTALL SUMMERS IKKE)*
      *    14.03.97  FIRMA 923 HISTORIKK PR LAGERSTED              *    000009
      *              LAGER 10 = 00  LAGER 15 = 15  STEIN SANDVOLD  *    000010
      *    21.06.00  FIRMA 626 HISTORIKK PR LAGERSTED              *    000011
      *    21.06.00  FIRMA 658 HISTORIKK PR LAGERSTED              *    000012
      *    21.06.00  SKAFFEVARE TAS MED                                 000013
      *    16.08.01  FIRMA 938 HISTORIKK PR. LAGERSTED
      *    01.01.002 FIRMA 956 HISTORIKK PR. LAGERSTED
      *    23.01.002 FIRMA 975 HISTORIKK PR. LAGERSTED
      *    28.02.002 KOPI AV VAS005 KUN ALFA VEN FOR 956
      **************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS105.rpg
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
           SELECT FAKVARE
               ASSIGN TO UT-S-FAKVARE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKVARE-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
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
       FD FAKVARE
               BLOCK CONTAINS 820
               RECORD CONTAINS 82.
       01  FAKVARE-IO-AREA.
           05  FAKVARE-IO-AREA-X           PICTURE X(82).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD VARSTAA
               BLOCK CONTAINS 400
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
           10  FAKVARE-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARSTAA-STATUS              PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-EOF-OFF         VALUE '0'.
               88  FAKVARE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-READ-OFF        VALUE '0'.
               88  FAKVARE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-PROCESS-OFF     VALUE '0'.
               88  FAKVARE-PROCESS         VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  FAKVARE-DATA-FIELDS.
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
               10  FAKT                    PICTURE X(1).
               10  MND                     PICTURE X(2).
               10  RNR1                    PICTURE X(1).
               10  AAR                     PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  KRTYPE                  PICTURE X(1).
               10  LAGERK                  PICTURE X(2).
               10  DATO-IO.
                   15  DATO                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  VAREMAS-DATA-FIELDS.
               10  ALFAK                   PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  KEY-X                   PICTURE X(10).
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
               10  DATO-N-IO.
                   15  DATO-N              PICTURE S9(7).
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKVARE-PROCESS
               SET FAKVARE-PROCESS-OFF     TO TRUE
               SET FAKVARE-READ            TO TRUE
           END-IF
 
           IF  FAKVARE-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKVARE-GET
               SET FAKVARE-READ-OFF        TO TRUE
               IF  NOT FAKVARE-EOF
                   SET FAKVARE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKVARE-PROCESS
               PERFORM FAKVARE-IDSET
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
 
           IF  FAKVARE-PROCESS
               PERFORM FAKVARE-FLDOFF
               PERFORM FAKVARE-FLDSET
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
           ADD 1                           TO ANT
           IF  (I-01)
               SET NOT-I-33                TO TRUE
               IF  FIRMA = '956'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-33)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  LAGERK = '13'
               SET I-34                    TO TRUE
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  LAGERK = '15'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           MOVE FIRMA                      TO KEY-X (1:3)
           MOVE EDBNR                      TO KEY-X (4:7)
           MOVE KEY-X                      TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-31                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-31                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (I-31)
               MOVE '   '                  TO ALFAK
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  ALFAK = 'VEN'
               SET I-32                    TO TRUE
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  ALFAK = 'GAB'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  ALFAK = 'TIL'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  ALFAK = 'BOS'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  ALFAK = 'CAT'
                   SET I-32                TO TRUE
               END-IF
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
           IF  FAKT = '1'
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
           END-IF.
 
       SLUTT-T.
           IF  (I-60)
               ADD 1                       TO ANTNYE
      *****************************************************
      * RUTINE FOR Å SNU DATO FOR SISTE SALG TIL ÅÅMMDD   *
      *****************************************************
           END-IF
           IF  (I-60)
               MOVE DATO                   TO DATO-N
               MOVE DATO-N-IO (2:6)        TO F6-IO
               MOVE F6 (1:2)               TO DD
               MOVE F6 (5:2)               TO AA-IO
               MOVE F6                     TO AAMMDD-IO
               MOVE AA                     TO AAMMDD (1:2)
               MOVE DD                     TO AAMMDD-IO (5:2)
      *****************************************************
           END-IF
           .
 
       FAKVARE-GET SECTION.
       FAKVARE-GET-P.
           IF  FAKVARE-EOF-OFF
               READ FAKVARE
               AT END
                   SET FAKVARE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKVARE-FLDOFF SECTION.
       FAKVARE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-77                TO TRUE
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-52                TO TRUE
           END-EVALUATE.
 
       FAKVARE-FLDSET SECTION.
       FAKVARE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKVARE-IO-AREA (12:4) TO ANTUT-IO
               IF  ANTUT = ZERO
                   SET I-77                TO TRUE
               END-IF
               MOVE FAKVARE-IO-AREA (16:7) TO EDBNR (1:7)
               MOVE FAKVARE-IO-AREA (23:3) TO RABA-IO
               INSPECT RABA-IO REPLACING ALL ' ' BY '0'
               IF  RABA = ZERO
                   SET I-50                TO TRUE
               END-IF
               MOVE FAKVARE-IO-AREA (26:3) TO RABB-IO
               INSPECT RABB-IO REPLACING ALL ' ' BY '0'
               IF  RABB = ZERO
                   SET I-51                TO TRUE
               END-IF
               MOVE FAKVARE-IO-AREA (29:3) TO RABC-IO
               INSPECT RABC-IO REPLACING ALL ' ' BY '0'
               IF  RABC = ZERO
                   SET I-52                TO TRUE
               END-IF
               MOVE FAKVARE-IO-AREA (32:9) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE FAKVARE-IO-AREA (41:1) TO FAKT (1:1)
               MOVE FAKVARE-IO-AREA (42:2) TO MND (1:2)
               MOVE FAKVARE-IO-AREA (45:1) TO RNR1 (1:1)
               MOVE FAKVARE-IO-AREA (58:2) TO AAR (1:2)
               MOVE FAKVARE-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE FAKVARE-IO-AREA (60:5) TO VGR (1:5)
               MOVE FAKVARE-IO-AREA (66:1) TO KRTYPE (1:1)
               MOVE FAKVARE-IO-AREA (69:2) TO LAGERK (1:2)
               MOVE FAKVARE-IO-AREA (77:4) TO DATO-IO
           END-EVALUATE.
 
       FAKVARE-IDSET SECTION.
       FAKVARE-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFAK (1:3)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
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
               IF  (I-34 AND NOT-I-32)
                   MOVE LAGERK             TO VARSTAA-IO-AREA (12:2)
               END-IF
               MOVE VGR                    TO VARSTAA-IO-AREA (17:5)
               MOVE AAMMDD                 TO XO-60P
               MOVE XO-60P-EF              TO VARSTAA-IO-AREA (22:4)
               MOVE AAR                    TO VARSTAA-IO-AREA (26:2)
               MOVE MND                    TO VARSTAA-IO-AREA (28:2)
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
               MOVE '*** VAS105 ***  KJØRT' TO PRF-IO-AREA (12:21)
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE FAKVARE-DATA-FIELDS
           SET FAKVARE-EOF-OFF             TO TRUE
           SET FAKVARE-PROCESS             TO TRUE
           OPEN INPUT FAKVARE
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT VARSTAA
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKVARE
           CLOSE VAREMAS
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
