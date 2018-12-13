       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK781R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: FAK781  (KOPI AV FAK771 MED SEQ. FILE)          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMERT.: 22.01.04                                        *
      *  RETTET    .: 13.04.04                                        *
      *                                                               *
      *  OPPDATERE KUNDEKAT I SEQ.FAKTURA.SALGDATA FRA KUNDE.MASTER   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK781.rpg
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
           SELECT FAKSALG
               ASSIGN TO UT-S-FAKSALG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKSALG-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKSALG
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  FAKSALG-IO-AREA.
           05  FAKSALG-IO-AREA-X           PICTURE X(160).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD TOTALER
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKSALG-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-EOF-OFF         VALUE '0'.
               88  FAKSALG-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-READ-OFF        VALUE '0'.
               88  FAKSALG-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-PROCESS-OFF     VALUE '0'.
               88  FAKSALG-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKSALG-LEVEL-INIT-OFF  VALUE '0'.
               88  FAKSALG-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  TOTALER-DATA-FIELDS.
               10  TOTALER-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-CLR-IO          PICTURE X VALUE 'Y'.
           05  FAKSALG-LEVEL-01.
               10  FAKSALG-01-L2.
                   15  FAKSALG-01-L2-KONS  PICTURE X(3).
               10  FAKSALG-01-L1.
                   15  FAKSALG-01-L1-KUNDE PICTURE X(6).
           05  FAKSALG-DATA-FIELDS.
               10  KONS                    PICTURE X(3).
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
      *                                      56  58 FKKAT
           05  KUNDEMA-DATA-FIELDS.
               10  KKAT-IO.
                   15  KKAT                PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
           05  KUNDEMX-DATA-FIELDS.
               10  KKAT1                   PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTR-IO.
                   15  ANTR                PICTURE S9(8).
               10  FNRKNR                  PICTURE X(9).
               10  ANTU-IO.
                   15  ANTU                PICTURE S9(8).
               10  NYKKAT                  PICTURE X(3).
               10  KATNUM-IO.
                   15  KATNUM              PICTURE S9(3).
               10  KKAT-N-IO.
                   15  KKAT-N              PICTURE S9(3).
               10  ANTO-IO.
                   15  ANTO                PICTURE S9(8).
               10  ANTK-IO.
                   15  ANTK                PICTURE S9(8).
               10  KXKEY                   PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
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
           SET NOT-I-09                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKSALG-PROCESS
               SET FAKSALG-PROCESS-OFF     TO TRUE
               SET FAKSALG-READ            TO TRUE
           END-IF
 
           IF  FAKSALG-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKSALG-GET
               SET FAKSALG-READ-OFF        TO TRUE
               IF  NOT FAKSALG-EOF
                   SET FAKSALG-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-IDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-CHK-LEVEL
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
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKSALG-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-25                TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTR
      *****************************************************************
      * RUTINE FOR Å HENT KUNDEKAT FRA KUNDE.MASTER.                  *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-51                TO TRUE
               IF  KONS = '923'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51)
               GO TO KXRUT-T
           END-IF
           IF  (I-L2)
               SET NOT-I-50                TO TRUE
               IF  KONS = '970'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-50)
               GO TO KXRUT-T
           END-IF
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               IF  FIRMA = '970'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-50)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO FNRKNR (1:3)
               MOVE KUNDE                  TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-18                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-18            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-18)
               ADD 1                       TO ANTU
           END-IF
           IF  (I-L1 AND NOT-I-18)
               SET NOT-I-19                TO TRUE
               IF  KKAT > 000
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               MOVE '   '                  TO NYKKAT
           END-IF
           IF  (I-L1 AND NOT-I-18 AND I-19)
               MOVE KKAT                   TO KKAT-N
               MOVE KKAT-N-IO              TO KATNUM-IO
               MOVE KATNUM                 TO NYKKAT
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE NYKKAT (3:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO NYKKAT (3:1)
           END-IF
           IF  (I-L1)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTO
           END-IF
           IF  (I-01 AND NOT-I-18 AND I-19)
               ADD 1                       TO ANTK
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR Å HENT KUNDEKAT FRA KUNDE.XTRA.                    *
      *****************************************************************
           END-IF
           .
 
       KXRUT-T.
           IF  (I-L2)
               SET NOT-I-50                TO TRUE
               IF  KONS = '923'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-50)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               IF  FIRMA = '923'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-50)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO FNRKNR (1:3)
               MOVE KUNDE                  TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KXKEY (1:9)
               MOVE '1'                    TO KXKEY (10:1)
               MOVE KXKEY                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-18                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-18            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-18)
               ADD 1                       TO ANTU
           END-IF
           IF  (I-L1 AND NOT-I-18)
               SET NOT-I-19                TO TRUE
               IF  KKAT1 > '   '
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               MOVE '   '                  TO NYKKAT
           END-IF
           IF  (I-L1 AND NOT-I-18 AND I-19)
               MOVE KKAT1                  TO NYKKAT
           END-IF
           IF  (I-L1)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTO
           END-IF
           IF  (I-01 AND NOT-I-18 AND I-19)
               ADD 1                       TO ANTK
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       FAKSALG-GET SECTION.
       FAKSALG-GET-P.
           IF  FAKSALG-EOF-OFF
               READ FAKSALG
               AT END
                   SET FAKSALG-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKSALG-FLDSET SECTION.
       FAKSALG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKSALG-IO-AREA (1:3)  TO KONS (1:3)
               MOVE FAKSALG-IO-AREA (39:3) TO FIRMA (1:3)
               MOVE FAKSALG-IO-AREA (10:6) TO KUNDE (1:6)
           END-EVALUATE.
 
       FAKSALG-IDSET SECTION.
       FAKSALG-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKSALG-CHK-LEVEL SECTION.
       FAKSALG-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKSALG-LEVEL-01
               MOVE FAKSALG-IO-AREA (1:3)  TO FAKSALG-01-L2-KONS
               MOVE FAKSALG-IO-AREA (10:6) TO FAKSALG-01-L1-KUNDE
               IF  FAKSALG-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKSALG-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKSALG-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKSALG-01-L2         TO THE-PRIOR-L2
               MOVE  FAKSALG-01-L1         TO THE-PRIOR-L1
               SET FAKSALG-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (162:2) TO KKAT-IO
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-09                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (132:3) TO KKAT1 (1:3)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-08                        TO TRUE.
 
       TOTALER-PRINT-LINE SECTION.
       TOTALER-PRINT-LINE-P.
           IF  TOTALER-BEFORE-SKIP > 0
               PERFORM TOTALER-SKIP-BEFORE
           END-IF
           IF  TOTALER-BEFORE-SPACE > 0
               PERFORM TOTALER-SPACE-BEFORE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               IF  TOTALER-AFTER-SPACE > 0
                   PERFORM TOTALER-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               PERFORM TOTALER-SPACE-AFTER
           END-IF
           IF  TOTALER-LINE-COUNT NOT < TOTALER-MAX-LINES
               MOVE 7                      TO TOTALER-AFTER-SKIP
           END-IF.
 
       TOTALER-SKIP-BEFORE SECTION.
       TOTALER-SKIP-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-BEFORE-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-BEFORE SECTION.
       TOTALER-SPACE-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER TOTALER-BEFORE-SPACE
                                                                 LINES
           ADD TOTALER-BEFORE-SPACE        TO TOTALER-LINE-COUNT
           MOVE SPACES TO TOTALER-IO-AREA
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-BEFORE-SPACE.
 
       TOTALER-SKIP-AFTER SECTION.
       TOTALER-SKIP-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-AFTER-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-AFTER SECTION.
       TOTALER-SPACE-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE TOTALER-AFTER-SPACE LINES
           ADD TOTALER-AFTER-SPACE         TO TOTALER-LINE-COUNT
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-25)
               MOVE NYKKAT                 TO FAKSALG-IO-AREA (56:3)
               REWRITE FAKSALG-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTR                   TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'RECORDS PÅ FAKTURA.SA' TO TOTALER-IO-AREA (14:21)
               MOVE 'LGDATA  '             TO TOTALER-IO-AREA (35:8)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 3                      TO TOTALER-BEFORE-SPACE
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTO                   TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'BLE KORRIGERT NÅ.    ' TO TOTALER-IO-AREA (14:21)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTU                   TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'IKKE I KUNDE.MASTER. ' TO TOTALER-IO-AREA (14:21)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTK                   TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'BLE TILDELT KUNDEKAT.' TO TOTALER-IO-AREA (14:21)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'KJØREDATO ER'         TO TOTALER-IO-AREA (12:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (25:8)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
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
           SET FAKSALG-LEVEL-INIT          TO TRUE
           INITIALIZE FAKSALG-DATA-FIELDS
           SET FAKSALG-EOF-OFF             TO TRUE
           SET FAKSALG-PROCESS             TO TRUE
           OPEN I-O FAKSALG
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKSALG
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
