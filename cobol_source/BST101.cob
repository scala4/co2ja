       IDENTIFICATION DIVISION.
       PROGRAM-ID. BST101R.
      **********************************************  Z-WIN-RPG2   ****
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BST101.rpg
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
           SELECT NYEBEST
               ASSIGN TO NYEBEST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS NYEBEST-STATUS
               RECORD KEY IS NYEBEST-KEY1.
           SELECT BEST
               ASSIGN TO UT-S-BEST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BEST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD NYEBEST
               RECORD CONTAINS 150.
       01  NYEBEST-IO-AREA.
           05  NYEBEST-IO-AREA-X.
               10  NYEBEST-KEY1.
                   15  NYEBEST-KEY1N       PICTURE S9(16).
               10  FILLER                  PICTURE X(134).
       FD BEST
               BLOCK CONTAINS 4080
               RECORD CONTAINS 120.
       01  BEST-IO-AREA.
           05  BEST-IO-AREA-X              PICTURE X(120).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  NYEBEST-STATUS              PICTURE 99 VALUE 0.
           10  BEST-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  NYEBEST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-EOF-OFF         VALUE '0'.
               88  NYEBEST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-READ-OFF        VALUE '0'.
               88  NYEBEST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-PROCESS-OFF     VALUE '0'.
               88  NYEBEST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  NYEBEST-LEVEL-INIT-OFF  VALUE '0'.
               88  NYEBEST-LEVEL-INIT      VALUE '1'.
           05  NYEBEST-LEVEL-08.
               10  NYEBEST-08-L3.
                   15  NYEBEST-08-L3-FIRMA PICTURE X(3).
               10  NYEBEST-08-L2.
                   15  NYEBEST-08-L2-BESNR PICTURE X(5).
               10  NYEBEST-08-L1.
                   15  NYEBEST-08-L1-POSNR PICTURE X(4).
           05  NYEBEST-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  BESNR                   PICTURE X(5).
               10  POSNR                   PICTURE X(4).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BANT-IO.
                   15  BANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LAAR-IO.
                   15  LAAR                PICTURE S9(2).
               10  LUKE-IO.
                   15  LUKE                PICTURE S9(2).
               10  LANT-IO.
                   15  LANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TEKST                   PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  FABR                    PICTURE X(1).
               10  RECART                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(5).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  FAB                     PICTURE X(1).
               10  EDB-IO.
                   15  EDB                 PICTURE S9(7).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  ANTB-IO.
                   15  ANTB                PICTURE S9(6).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  UKE-IO.
                   15  UKE                 PICTURE S9(2).
               10  AVD-IO.
                   15  AVD                 PICTURE S9(1).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
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
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  NYEBEST-PROCESS
               SET NYEBEST-PROCESS-OFF     TO TRUE
               SET NYEBEST-READ            TO TRUE
           END-IF
 
           IF  NYEBEST-READ
           AND RECORD-SELECTED-OFF
               PERFORM NYEBEST-GET
               SET NYEBEST-READ-OFF        TO TRUE
               IF  NOT NYEBEST-EOF
                   SET NYEBEST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-IDSET
           END-IF
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-CHK-LEVEL
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
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  NYEBEST-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-04                    TO TRUE
           SET NOT-I-02                    TO TRUE
           IF  RECART = '4'
               SET I-02                    TO TRUE
           END-IF
           IF  (NOT-I-02)
               SET NOT-I-02                TO TRUE
               IF  RECART = '5'
                   SET I-02                TO TRUE
               END-IF
           END-IF
           SET NOT-I-03                    TO TRUE
           IF  RECART = '1'
               SET I-03                    TO TRUE
           END-IF
           IF  (NOT-I-02)
               AND (NOT-I-03)
               SET I-04                    TO TRUE
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  (I-L2)
               SET NOT-I-40                TO TRUE
               SET NOT-I-41                TO TRUE
           END-IF
           IF  (I-03)
               SET NOT-I-40                TO TRUE
               IF  STATUS-X = 'A'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  FABR = 'F'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  FABR = 'G'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-41)
               MOVE FABR                   TO FAB
           END-IF
           IF  (I-03 AND NOT-I-41)
               MOVE ' '                    TO FAB
           END-IF
           IF  (I-03)
               OR  (I-04)
               OR  (I-40)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  TEKST = 'T'
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  TEKST = 'S'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               MOVE EDBNR                  TO EDBNR-N
               MOVE EDBNR-N-IO             TO EDB-IO
               ADD BANT TO ZERO        GIVING ANTB ROUNDED
               MOVE LAAR                   TO AAR-IO
               MOVE LUKE                   TO UKE-IO
               SUBTRACT LANT               FROM LANT
               MOVE STATUS-X               TO AVD-IO
           END-IF
           SET I-45                        TO TRUE.
 
       SLUTT-T.
      *
           CONTINUE.
 
       NYEBEST-GET SECTION.
       NYEBEST-GET-P.
           IF  NYEBEST-EOF-OFF
               READ NYEBEST
               AT END
                   SET NYEBEST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYEBEST-FLDSET SECTION.
       NYEBEST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEBEST-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE NYEBEST-IO-AREA (5:5)  TO BESNR (1:5)
               MOVE NYEBEST-IO-AREA (11:4) TO POSNR (1:4)
               MOVE NYEBEST-IO-AREA (75:4) TO EDBNR-IO
               MOVE NYEBEST-IO-AREA (89:5) TO BANT-IO
               MOVE NYEBEST-IO-AREA (18:2) TO LAAR-IO
               INSPECT LAAR-IO REPLACING ALL ' ' BY '0'
               MOVE NYEBEST-IO-AREA (20:2) TO LUKE-IO
               INSPECT LUKE-IO REPLACING ALL ' ' BY '0'
               MOVE NYEBEST-IO-AREA (99:5) TO LANT-IO
               MOVE NYEBEST-IO-AREA (124:1) TO TEKST (1:1)
               MOVE NYEBEST-IO-AREA (17:1) TO STATUS-X (1:1)
               MOVE NYEBEST-IO-AREA (106:1) TO FABR (1:1)
               MOVE NYEBEST-IO-AREA (150:1) TO RECART (1:1)
           END-EVALUATE.
 
       NYEBEST-IDSET SECTION.
       NYEBEST-IDSET-P.
           SET I-08                        TO TRUE.
 
       NYEBEST-CHK-LEVEL SECTION.
       NYEBEST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO NYEBEST-LEVEL-08
               MOVE NYEBEST-IO-AREA (2:3)  TO NYEBEST-08-L3-FIRMA
               MOVE NYEBEST-IO-AREA (5:5)  TO NYEBEST-08-L2-BESNR
               MOVE NYEBEST-IO-AREA (11:4) TO NYEBEST-08-L1-POSNR
               IF  NYEBEST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYEBEST-08-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  NYEBEST-08-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  NYEBEST-08-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYEBEST-08-L3         TO THE-PRIOR-L3
               MOVE  NYEBEST-08-L2         TO THE-PRIOR-L2
               MOVE  NYEBEST-08-L1         TO THE-PRIOR-L1
               SET NYEBEST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L1 AND I-U1 AND I-45)
               MOVE LANT                   TO XO-72P
               MOVE XO-72P-EF              TO NYEBEST-IO-AREA (99:5)
               MOVE ' '                    TO NYEBEST-IO-AREA (148:1)
               MOVE ' '                    TO NYEBEST-IO-AREA (149:1)
               REWRITE NYEBEST-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-33 AND I-45)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE '4'                    TO BEST-IO-AREA (1:1)
               MOVE FIRMA                  TO BEST-IO-AREA (2:3)
               IF  (I-41)
                   MOVE FAB                TO BEST-IO-AREA (5:1)
               END-IF
               MOVE AVD-IO                 TO BEST-IO-AREA (6:1)
               MOVE AAR-IO                 TO BEST-IO-AREA (23:2)
               MOVE UKE-IO                 TO BEST-IO-AREA (25:2)
               MOVE EDB                    TO XO-70P
               MOVE XO-70P-EF              TO BEST-IO-AREA (87:4)
               INITIALIZE EDB
               MOVE ANTB                   TO XO-60P
               MOVE XO-60P-EF              TO BEST-IO-AREA (101:4)
               INITIALIZE ANTB
               WRITE BEST-IO-AREA
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
           SET NYEBEST-LEVEL-INIT          TO TRUE
           INITIALIZE NYEBEST-DATA-FIELDS
           SET NYEBEST-EOF-OFF             TO TRUE
           SET NYEBEST-PROCESS             TO TRUE
           OPEN I-O NYEBEST
           OPEN OUTPUT BEST.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE NYEBEST
           CLOSE BEST.
 
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
