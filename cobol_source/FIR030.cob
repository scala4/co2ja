       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIR030R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FIR030.rpg
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
           SELECT FNRREF
               ASSIGN TO FNRREF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FNRREF-STATUS
               RECORD KEY IS FNRREF-KEY1.
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
       FD FNRREF
               RECORD CONTAINS 132.
       01  FNRREF-IO-AREA.
           05  FNRREF-IO-AREA-X.
               10  FNRREF-KEY1.
                   15  FNRREF-KEY1N        PICTURE S9(6).
               10  FILLER                  PICTURE X(126).
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
           10  FNRREF-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FNRREF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRREF-EOF-OFF          VALUE '0'.
               88  FNRREF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRREF-READ-OFF         VALUE '0'.
               88  FNRREF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRREF-PROCESS-OFF      VALUE '0'.
               88  FNRREF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FNRREF-LEVEL-INIT-OFF   VALUE '0'.
               88  FNRREF-LEVEL-INIT       VALUE '1'.
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
           05  FNRREF-LEVEL-01.
               10  FNRREF-01-L1.
                   15  FNRREF-01-L1-FIRMA  PICTURE X(3).
           05  FNRREF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  FIRTIL                  PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  BRKODE                  PICTURE X(1).
               10  LK1                     PICTURE X(2).
               10  LK2                     PICTURE X(2).
               10  LK3                     PICTURE X(2).
               10  LK4                     PICTURE X(2).
               10  LEVNAV                  PICTURE X(9).
               10  LEVKOD                  PICTURE X(1).
               10  LEVNR                   PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTR-IO.
                   15  ANTR                PICTURE S9(5).
               10  ANTS-IO.
                   15  ANTS                PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-40D                  PICTURE S9(4).
               10  XO-40U                  PICTURE 9(4).
               10  XO-50YY9                PICTURE ZZ.ZZ9.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FNRREF-PROCESS
               SET FNRREF-PROCESS-OFF      TO TRUE
               SET FNRREF-READ             TO TRUE
           END-IF
 
           IF  FNRREF-READ
           AND RECORD-SELECTED-OFF
               PERFORM FNRREF-GET
               SET FNRREF-READ-OFF         TO TRUE
               IF  NOT FNRREF-EOF
                   SET FNRREF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FNRREF-PROCESS
               PERFORM FNRREF-IDSET
           END-IF
 
           IF  FNRREF-PROCESS
               PERFORM FNRREF-CHK-LEVEL
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
 
           IF  FNRREF-PROCESS
               PERFORM FNRREF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FNRREF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               SET NOT-I-10                TO TRUE
           END-IF
           IF  (I-L1)
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
           SET NOT-I-98                    TO TRUE
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
 
       FNRREF-GET SECTION.
       FNRREF-GET-P.
           IF  FNRREF-EOF-OFF
               READ FNRREF
               AT END
                   SET FNRREF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FNRREF-FLDSET SECTION.
       FNRREF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FNRREF-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE FNRREF-IO-AREA (4:3)   TO FIRTIL (1:3)
               MOVE FNRREF-IO-AREA (7:6)   TO KNR (1:6)
               MOVE FNRREF-IO-AREA (13:1)  TO BRKODE (1:1)
               MOVE FNRREF-IO-AREA (14:2)  TO LK1 (1:2)
               MOVE FNRREF-IO-AREA (16:2)  TO LK2 (1:2)
               MOVE FNRREF-IO-AREA (18:2)  TO LK3 (1:2)
               MOVE FNRREF-IO-AREA (20:2)  TO LK4 (1:2)
               MOVE FNRREF-IO-AREA (22:9)  TO LEVNAV (1:9)
               MOVE FNRREF-IO-AREA (31:1)  TO LEVKOD (1:1)
               MOVE FNRREF-IO-AREA (32:6)  TO LEVNR (1:6)
           END-EVALUATE.
 
       FNRREF-IDSET SECTION.
       FNRREF-IDSET-P.
           SET I-01                        TO TRUE.
 
       FNRREF-CHK-LEVEL SECTION.
       FNRREF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FNRREF-LEVEL-01
               MOVE FNRREF-IO-AREA (1:3)   TO FNRREF-01-L1-FIRMA
               IF  FNRREF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FNRREF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FNRREF-01-L1          TO THE-PRIOR-L1
               SET FNRREF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
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
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
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
               DELETE FNRREF RECORD
           END-IF
           IF  (I-01 AND I-L1 AND I-U2)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMA                  TO TOTALER-IO-AREA (1:3)
               MOVE FINAVN                 TO TOTALER-IO-AREA (5:30)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-01 AND I-U2)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMA                  TO TOTALER-IO-AREA (1:3)
               MOVE FIRTIL                 TO TOTALER-IO-AREA (5:3)
               MOVE KNR                    TO TOTALER-IO-AREA (9:6)
               MOVE BRKODE                 TO TOTALER-IO-AREA (16:1)
               MOVE LK1                    TO TOTALER-IO-AREA (18:2)
               MOVE LK2                    TO TOTALER-IO-AREA (21:2)
               MOVE LK3                    TO TOTALER-IO-AREA (24:2)
               MOVE LK4                    TO TOTALER-IO-AREA (27:2)
               MOVE LEVNAV                 TO TOTALER-IO-AREA (30:9)
               MOVE LEVKOD                 TO TOTALER-IO-AREA (40:1)
               MOVE LEVNR                  TO TOTALER-IO-AREA (42:6)
               IF  (NOT-I-U1 AND I-10)
                   MOVE 'SLETTET'          TO TOTALER-IO-AREA (49:7)
               END-IF
               IF  (I-U1 AND I-10)
                   MOVE 'SLETTES'          TO TOTALER-IO-AREA (49:7)
               END-IF
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P AND I-U2)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '* * *  FIRMA FRA TIL FIL' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'E  * * *'             TO TOTALER-IO-AREA (25:8)
               MOVE 'KJØREDATO:'           TO TOTALER-IO-AREA (41:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (51:8)
               MOVE 'SIDE'                 TO TOTALER-IO-AREA (61:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO TOTALER-IO-AREA (65:4)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FRA'                  TO TOTALER-IO-AREA (1:3)
               MOVE 'TIL'                  TO TOTALER-IO-AREA (5:3)
               MOVE 'KUNDNR'               TO TOTALER-IO-AREA (9:6)
               MOVE 'T'                    TO TOTALER-IO-AREA (16:1)
               MOVE 'LAGERKODER'           TO TOTALER-IO-AREA (18:10)
               MOVE 'LEVERANDØR'           TO TOTALER-IO-AREA (29:10)
               MOVE 'B'                    TO TOTALER-IO-AREA (40:1)
               MOVE 'LEV.NR'               TO TOTALER-IO-AREA (42:6)
               MOVE 'MERKADER'             TO TOTALER-IO-AREA (49:8)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (49:24)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '* * *  FIRMA FRA TIL FIL' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'E  * * *'             TO TOTALER-IO-AREA (25:8)
               MOVE 'KJØREDATO:'           TO TOTALER-IO-AREA (41:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (51:8)
               MOVE 'SIDE'                 TO TOTALER-IO-AREA (61:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO TOTALER-IO-AREA (65:4)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FRA'                  TO TOTALER-IO-AREA (1:3)
               MOVE 'TIL'                  TO TOTALER-IO-AREA (5:3)
               MOVE 'KUNDNR'               TO TOTALER-IO-AREA (9:6)
               MOVE 'T'                    TO TOTALER-IO-AREA (16:1)
               MOVE 'LAGERKODER'           TO TOTALER-IO-AREA (18:10)
               MOVE 'LEVERANDØR'           TO TOTALER-IO-AREA (29:10)
               MOVE 'B'                    TO TOTALER-IO-AREA (40:1)
               MOVE 'LEV.NR'               TO TOTALER-IO-AREA (42:6)
               MOVE 'MERKADER'             TO TOTALER-IO-AREA (49:8)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (49:24)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTR                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'RECORDS PÅ FIRMAFRA-FIL' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 'E.    '               TO TOTALER-IO-AREA (35:6)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTS                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               IF  (NOT-I-U1)
                   MOVE 'BLE NÅ SLETTET.'  TO TOTALER-IO-AREA (12:15)
               END-IF
               IF  (I-U1)
                   MOVE 'SKAL SLETTES.  '  TO TOTALER-IO-AREA (12:15)
               END-IF
               MOVE 1                      TO TOTALER-AFTER-SPACE
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
           SET FNRREF-LEVEL-INIT           TO TRUE
           INITIALIZE FNRREF-DATA-FIELDS
           SET FNRREF-EOF-OFF              TO TRUE
           SET FNRREF-PROCESS              TO TRUE
           OPEN I-O FNRREF
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FNRREF
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
