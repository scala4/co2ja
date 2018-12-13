       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD142R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK142  OPPDATERING AV VENTE.ORDRE.FILE.             *
      *    LEGGER INN NYE ORDRE, FJERNER ORDRE SOM ER FULLFØRT.       *
      * PROGRAMMERT AV ESPEN LARSEN 26.03.2004                        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD142.rpg
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
           SELECT GMLVFIL
               ASSIGN TO UT-S-GMLVFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLVFIL-STATUS.
           SELECT NYEVFIL
               ASSIGN TO UT-S-NYEVFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYEVFIL-STATUS.
           SELECT VORDFIL
               ASSIGN TO VORDFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VORDFIL-STATUS
               RECORD KEY IS VORDFIL-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD GMLVFIL
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  GMLVFIL-IO-AREA.
           05  GMLVFIL-IO-AREA-X           PICTURE X(80).
       FD NYEVFIL
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  NYEVFIL-IO-AREA.
           05  NYEVFIL-IO-AREA-X           PICTURE X(80).
      * TSUM  O   F 132 132     OF     PRINTERSYSLST
       FD VORDFIL
               RECORD CONTAINS 80.
       01  VORDFIL-IO-AREA.
           05  VORDFIL-IO-AREA-X.
               10  VORDFIL-KEY1.
                   15  VORDFIL-KEY1N       PICTURE S9(10).
               10  FILLER                  PICTURE X(70).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  GMLVFIL-STATUS              PICTURE 99 VALUE 0.
           10  NYEVFIL-STATUS              PICTURE 99 VALUE 0.
           10  VORDFIL-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLVFIL-EOF-OFF         VALUE '0'.
               88  GMLVFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLVFIL-READ-OFF        VALUE '0'.
               88  GMLVFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLVFIL-PROCESS-OFF     VALUE '0'.
               88  GMLVFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEVFIL-EOF-OFF         VALUE '0'.
               88  NYEVFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEVFIL-READ-OFF        VALUE '0'.
               88  NYEVFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEVFIL-PROCESS-OFF     VALUE '0'.
               88  NYEVFIL-PROCESS         VALUE '1'.
           05  VORDFIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  GMLVFIL-DATA-FIELDS.
               10  GMLREC                  PICTURE X(80).
               10  FNR                     PICTURE X(3).
               10  ONR                     PICTURE X(6).
           05  GMLVFIL-MP                  PICTURE X(9).
           05  GMLVFIL-MC                  PICTURE X(9).
           05  GMLVFIL-M-01            REDEFINES GMLVFIL-MC.
               10  GMLVFIL-M-01-M2.
                   15  GMLVFIL-M-01-M2-FNR-G.
                       20  GMLVFIL-M-01-M2-FNR PICTURE X(3).
               10  GMLVFIL-M-01-M1.
                   15  GMLVFIL-M-01-M1-ONR-G.
                       20  GMLVFIL-M-01-M1-ONR PICTURE X(6).
           05  NYEVFIL-DATA-FIELDS.
               10  NYEREC                  PICTURE X(80).
           05  NYEVFIL-MP                  PICTURE X(9).
           05  NYEVFIL-MC                  PICTURE X(9).
           05  NYEVFIL-M-02            REDEFINES NYEVFIL-MC.
               10  NYEVFIL-M-02-M2.
                   15  NYEVFIL-M-02-M2-FNR-G.
                       20  NYEVFIL-M-02-M2-FNR PICTURE X(3).
               10  NYEVFIL-M-02-M1.
                   15  NYEVFIL-M-02-M1-ONR-G.
                       20  NYEVFIL-M-02-M1-ONR PICTURE X(6).
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
           IF  GMLVFIL-PROCESS
               SET GMLVFIL-PROCESS-OFF     TO TRUE
               SET GMLVFIL-READ            TO TRUE
           END-IF
 
           IF  GMLVFIL-READ
               PERFORM GMLVFIL-GET
               SET GMLVFIL-READ-OFF        TO TRUE
               IF  NOT GMLVFIL-EOF
                   PERFORM GMLVFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  NYEVFIL-PROCESS
               SET NYEVFIL-PROCESS-OFF     TO TRUE
               SET NYEVFIL-READ            TO TRUE
           END-IF
 
           IF  NYEVFIL-READ
               PERFORM NYEVFIL-GET
               SET NYEVFIL-READ-OFF        TO TRUE
               IF  NOT NYEVFIL-EOF
                   PERFORM NYEVFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  GMLVFIL-PROCESS
               PERFORM GMLVFIL-IDSET
           END-IF
 
           IF  NYEVFIL-PROCESS
               PERFORM NYEVFIL-IDSET
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  GMLVFIL-PROCESS
               PERFORM GMLVFIL-FLDSET
           END-IF
 
           IF  NYEVFIL-PROCESS
               PERFORM NYEVFIL-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       GMLVFIL-GET SECTION.
       GMLVFIL-GET-P.
           IF  GMLVFIL-EOF-OFF
               READ GMLVFIL
               AT END
                   SET GMLVFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLVFIL-FLDSET SECTION.
       GMLVFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLVFIL-IO-AREA (1:80) TO GMLREC (1:80)
               MOVE GMLVFIL-IO-AREA (1:3)  TO FNR (1:3)
               MOVE GMLVFIL-IO-AREA (4:6)  TO ONR (1:6)
           END-EVALUATE.
 
       GMLVFIL-IDSET SECTION.
       GMLVFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLVFIL-MATCH-SET SECTION.
       GMLVFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLVFIL-IO-AREA (1:3)  TO GMLVFIL-M-01-M2-FNR
               MOVE GMLVFIL-IO-AREA (4:6)  TO GMLVFIL-M-01-M1-ONR
           END-EVALUATE.
 
       NYEVFIL-GET SECTION.
       NYEVFIL-GET-P.
           IF  NYEVFIL-EOF-OFF
               READ NYEVFIL
               AT END
                   SET NYEVFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYEVFIL-FLDSET SECTION.
       NYEVFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEVFIL-IO-AREA (1:80) TO NYEREC (1:80)
               MOVE NYEVFIL-IO-AREA (1:3)  TO FNR (1:3)
               MOVE NYEVFIL-IO-AREA (4:6)  TO ONR (1:6)
           END-EVALUATE.
 
       NYEVFIL-IDSET SECTION.
       NYEVFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       NYEVFIL-MATCH-SET SECTION.
       NYEVFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEVFIL-IO-AREA (1:3)  TO NYEVFIL-M-02-M2-FNR
               MOVE NYEVFIL-IO-AREA (4:6)  TO NYEVFIL-M-02-M1-ONR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  GMLVFIL-EOF
               MOVE HIGH-VALUES            TO GMLVFIL-MC
                                              GMLVFIL-MP
           END-IF
           IF  NYEVFIL-EOF
               MOVE HIGH-VALUES            TO NYEVFIL-MC
                                              NYEVFIL-MP
           END-IF
           IF  GMLVFIL-MC < GMLVFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  NYEVFIL-MC < NYEVFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLVFIL-MC < NYEVFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLVFIL-PROCESS     TO TRUE
                   MOVE GMLVFIL-MC         TO GMLVFIL-MP
                   IF  GMLVFIL-MC = NYEVFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  NYEVFIL-MC < GMLVFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET NYEVFIL-PROCESS     TO TRUE
                   MOVE NYEVFIL-MC         TO NYEVFIL-MP
                   IF  NYEVFIL-MC = GMLVFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLVFIL-MC = NYEVFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLVFIL-PROCESS     TO TRUE
                   MOVE GMLVFIL-MC         TO GMLVFIL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE SPACES TO VORDFIL-IO-AREA
               INITIALIZE VORDFIL-IO-AREA
               MOVE GMLREC                 TO VORDFIL-IO-AREA (1:80)
               WRITE VORDFIL-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO VORDFIL-IO-AREA
               INITIALIZE VORDFIL-IO-AREA
               MOVE NYEREC                 TO VORDFIL-IO-AREA (1:80)
               WRITE VORDFIL-IO-AREA
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
           INITIALIZE GMLVFIL-DATA-FIELDS
           SET GMLVFIL-EOF-OFF             TO TRUE
           SET GMLVFIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO GMLVFIL-MC
                                              GMLVFIL-MP
           OPEN INPUT GMLVFIL
           INITIALIZE NYEVFIL-DATA-FIELDS
           SET NYEVFIL-EOF-OFF             TO TRUE
           SET NYEVFIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO NYEVFIL-MC
                                              NYEVFIL-MP
           OPEN INPUT NYEVFIL
           OPEN OUTPUT VORDFIL.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLVFIL
           CLOSE NYEVFIL
           CLOSE VORDFIL.
 
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
