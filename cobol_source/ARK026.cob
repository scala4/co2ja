       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARK026R.
      **********************************************  Z-WIN-RPG2   ****
      *       BRUKES I VSA.KONSFKJ                                        ARK026
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ARK026.rpg
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
           SELECT KOKJFIR
               ASSIGN TO KOKJFIR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KOKJFIR-STATUS
               RECORD KEY IS KOKJFIR-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KOKJFIR
               RECORD CONTAINS 80.
       01  KOKJFIR-IO-AREA.
           05  KOKJFIR-IO-AREA-X.
               10  KOKJFIR-KEY1.
                   15  KOKJFIR-KEY1N       PICTURE S9(6).
               10  FILLER                  PICTURE X(74).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
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
           10  KOKJFIR-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KOKJFIR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KOKJFIR-EOF-OFF         VALUE '0'.
               88  KOKJFIR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOKJFIR-READ-OFF        VALUE '0'.
               88  KOKJFIR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOKJFIR-PROCESS-OFF     VALUE '0'.
               88  KOKJFIR-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOKJFIR-LEVEL-INIT-OFF  VALUE '0'.
               88  KOKJFIR-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  KOKJFIR-LEVEL-01.
               10  KOKJFIR-01-L1.
                   15  KOKJFIR-01-L1-FIRMA PICTURE X(3).
           05  KOKJFIR-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  REC                     PICTURE X(80).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTR-IO.
                   15  ANTR                PICTURE S9(5).
               10  ANTS-IO.
                   15  ANTS                PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KOKJFIR-PROCESS
               SET KOKJFIR-PROCESS-OFF     TO TRUE
               SET KOKJFIR-READ            TO TRUE
           END-IF
 
           IF  KOKJFIR-READ
           AND RECORD-SELECTED-OFF
               PERFORM KOKJFIR-GET
               SET KOKJFIR-READ-OFF        TO TRUE
               IF  NOT KOKJFIR-EOF
                   SET KOKJFIR-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KOKJFIR-PROCESS
               PERFORM KOKJFIR-IDSET
           END-IF
 
           IF  KOKJFIR-PROCESS
               PERFORM KOKJFIR-CHK-LEVEL
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
 
           IF  KOKJFIR-PROCESS
               PERFORM KOKJFIR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KOKJFIR-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
           IF  (I-L1)
               SET NOT-I-10                TO TRUE
               SET NOT-I-98                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  FIRMA = '000'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-20)
               PERFORM FISLET-S
           END-IF
           IF  (I-01 AND I-98)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-98)
               ADD 1                       TO ANTR
           END-IF
           IF  (I-01 AND I-98)
               ADD 1                       TO ANTS
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
           END-IF
           .
 
       FISLET-S SECTION.
       FISLET-S-P.
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF.
      ******************************************************
 
       KOKJFIR-GET SECTION.
       KOKJFIR-GET-P.
           IF  KOKJFIR-EOF-OFF
               READ KOKJFIR
               AT END
                   SET KOKJFIR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KOKJFIR-FLDSET SECTION.
       KOKJFIR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KOKJFIR-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE KOKJFIR-IO-AREA (1:80) TO REC (1:80)
           END-EVALUATE.
 
       KOKJFIR-IDSET SECTION.
       KOKJFIR-IDSET-P.
           SET I-01                        TO TRUE.
 
       KOKJFIR-CHK-LEVEL SECTION.
       KOKJFIR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KOKJFIR-LEVEL-01
               MOVE KOKJFIR-IO-AREA (2:3)  TO KOKJFIR-01-L1-FIRMA
               IF  KOKJFIR-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOKJFIR-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOKJFIR-01-L1         TO THE-PRIOR-L1
               SET KOKJFIR-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
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
           IF  (I-01 AND I-10 AND NOT-I-U1)
               DELETE KOKJFIR RECORD
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTR                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'RECORDS PÅ KOKJFIR     ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 3                      TO TOTALER-BEFORE-SPACE
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTS                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'BLE NÅ SLETTET.'      TO TOTALER-IO-AREA (12:15)
               MOVE 3                      TO TOTALER-AFTER-SPACE
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
           SET KOKJFIR-LEVEL-INIT          TO TRUE
           INITIALIZE KOKJFIR-DATA-FIELDS
           SET KOKJFIR-EOF-OFF             TO TRUE
           SET KOKJFIR-PROCESS             TO TRUE
           OPEN I-O KOKJFIR
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KOKJFIR
           CLOSE FIRMAF
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
