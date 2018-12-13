       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR240R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM..: ROR040                                *
      * DANNE NY REST.TEXT.FILE  REORGANISERING.         *
      * FJERNE RECORDS MED ORDRENR SOM IKKE LENGERE ER   *
      *        I REST.ORDRE.MASTER.                      *
      ****************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR240.rpg
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
           SELECT RORDNR
               ASSIGN TO UT-S-RORDNR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RORDNR-STATUS.
           SELECT RTEXTS
               ASSIGN TO UT-S-RTEXTS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RTEXTS-STATUS.
           SELECT RESTEXT
               ASSIGN TO RESTEXT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESTEXT-STATUS
               RECORD KEY IS RESTEXT-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD RORDNR
               BLOCK CONTAINS 20
               RECORD CONTAINS 10.
       01  RORDNR-IO-AREA.
           05  RORDNR-IO-AREA-X            PICTURE X(10).
       FD RTEXTS
               BLOCK CONTAINS 260
               RECORD CONTAINS 130.
       01  RTEXTS-IO-AREA.
           05  RTEXTS-IO-AREA-X            PICTURE X(130).
       FD RESTEXT
               RECORD CONTAINS 130.
       01  RESTEXT-IO-AREA.
           05  RESTEXT-IO-AREA-X.
               10  RESTEXT-KEY1.
                   15  RESTEXT-KEY1N       PICTURE S9(13).
               10  FILLER                  PICTURE X(117).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RORDNR-STATUS               PICTURE 99 VALUE 0.
           10  RTEXTS-STATUS               PICTURE 99 VALUE 0.
           10  RESTEXT-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RORDNR-EOF-OFF          VALUE '0'.
               88  RORDNR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RORDNR-READ-OFF         VALUE '0'.
               88  RORDNR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RORDNR-PROCESS-OFF      VALUE '0'.
               88  RORDNR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RTEXTS-EOF-OFF          VALUE '0'.
               88  RTEXTS-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RTEXTS-READ-OFF         VALUE '0'.
               88  RTEXTS-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RTEXTS-PROCESS-OFF      VALUE '0'.
               88  RTEXTS-PROCESS          VALUE '1'.
           05  RESTEXT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RORDNR-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  RTYPE                   PICTURE X(1).
               10  RORDNR-XX               PICTURE X(6).
           05  RORDNR-MP                   PICTURE X(10).
           05  RORDNR-MC                   PICTURE X(10).
           05  RORDNR-M-01             REDEFINES RORDNR-MC.
               10  RORDNR-M-01-M3.
                   15  RORDNR-M-01-M3-FNR-G.
                       20  RORDNR-M-01-M3-FNR PICTURE X(3).
               10  RORDNR-M-01-M2.
                   15  RORDNR-M-01-M2-RTYPE-G.
                       20  RORDNR-M-01-M2-RTYPE PICTURE X(1).
               10  RORDNR-M-01-M1.
                   15  RORDNR-M-01-M1-RORDNR-XX-G.
                       20  RORDNR-M-01-M1-RORDNR-XX PICTURE X(6).
           05  RTEXTS-DATA-FIELDS.
               10  RTREC                   PICTURE X(130).
           05  RTEXTS-MP                   PICTURE X(10).
           05  RTEXTS-MC                   PICTURE X(10).
           05  RTEXTS-M-02             REDEFINES RTEXTS-MC.
               10  RTEXTS-M-02-M3.
                   15  RTEXTS-M-02-M3-FNR-G.
                       20  RTEXTS-M-02-M3-FNR PICTURE X(3).
               10  RTEXTS-M-02-M2.
                   15  RTEXTS-M-02-M2-RTYPE-G.
                       20  RTEXTS-M-02-M2-RTYPE PICTURE X(1).
               10  RTEXTS-M-02-M1.
                   15  RTEXTS-M-02-M1-RORDNR-XX-G.
                       20  RTEXTS-M-02-M1-RORDNR-XX PICTURE X(6).
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
           IF  RORDNR-PROCESS
               SET RORDNR-PROCESS-OFF      TO TRUE
               SET RORDNR-READ             TO TRUE
           END-IF
 
           IF  RORDNR-READ
               PERFORM RORDNR-GET
               SET RORDNR-READ-OFF         TO TRUE
               IF  NOT RORDNR-EOF
                   PERFORM RORDNR-MATCH-SET
               END-IF
           END-IF
 
           IF  RTEXTS-PROCESS
               SET RTEXTS-PROCESS-OFF      TO TRUE
               SET RTEXTS-READ             TO TRUE
           END-IF
 
           IF  RTEXTS-READ
               PERFORM RTEXTS-GET
               SET RTEXTS-READ-OFF         TO TRUE
               IF  NOT RTEXTS-EOF
                   PERFORM RTEXTS-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  RORDNR-PROCESS
               PERFORM RORDNR-IDSET
           END-IF
 
           IF  RTEXTS-PROCESS
               PERFORM RTEXTS-IDSET
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
 
           IF  RORDNR-PROCESS
               PERFORM RORDNR-FLDSET
           END-IF
 
           IF  RTEXTS-PROCESS
               PERFORM RTEXTS-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       RORDNR-GET SECTION.
       RORDNR-GET-P.
           IF  RORDNR-EOF-OFF
               READ RORDNR
               AT END
                   SET RORDNR-EOF          TO TRUE
               END-READ
           END-IF.
 
       RORDNR-FLDSET SECTION.
       RORDNR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RORDNR-IO-AREA (1:3)   TO FNR (1:3)
               MOVE RORDNR-IO-AREA (4:1)   TO RTYPE (1:1)
               MOVE RORDNR-IO-AREA (5:6)   TO RORDNR-XX (1:6)
           END-EVALUATE.
 
       RORDNR-IDSET SECTION.
       RORDNR-IDSET-P.
           SET I-01                        TO TRUE.
 
       RORDNR-MATCH-SET SECTION.
       RORDNR-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RORDNR-IO-AREA (1:3)   TO RORDNR-M-01-M3-FNR
               MOVE RORDNR-IO-AREA (4:1)   TO RORDNR-M-01-M2-RTYPE
               MOVE RORDNR-IO-AREA (5:6)   TO RORDNR-M-01-M1-RORDNR-XX
           END-EVALUATE.
 
       RTEXTS-GET SECTION.
       RTEXTS-GET-P.
           IF  RTEXTS-EOF-OFF
               READ RTEXTS
               AT END
                   SET RTEXTS-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RTEXTS-FLDSET SECTION.
       RTEXTS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RTEXTS-IO-AREA (1:3)   TO FNR (1:3)
               MOVE RTEXTS-IO-AREA (4:1)   TO RTYPE (1:1)
               MOVE RTEXTS-IO-AREA (5:6)   TO RORDNR-XX (1:6)
               MOVE RTEXTS-IO-AREA (1:130) TO RTREC (1:130)
           END-EVALUATE.
 
       RTEXTS-IDSET SECTION.
       RTEXTS-IDSET-P.
           SET I-02                        TO TRUE.
 
       RTEXTS-MATCH-SET SECTION.
       RTEXTS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RTEXTS-IO-AREA (1:3)   TO RTEXTS-M-02-M3-FNR
               MOVE RTEXTS-IO-AREA (4:1)   TO RTEXTS-M-02-M2-RTYPE
               MOVE RTEXTS-IO-AREA (5:6)   TO RTEXTS-M-02-M1-RORDNR-XX
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  RORDNR-EOF
               MOVE HIGH-VALUES            TO RORDNR-MC
                                              RORDNR-MP
           END-IF
           IF  RTEXTS-EOF
               MOVE HIGH-VALUES            TO RTEXTS-MC
                                              RTEXTS-MP
           END-IF
           IF  RORDNR-MC < RORDNR-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RTEXTS-MC < RTEXTS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RORDNR-MC < RTEXTS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RORDNR-PROCESS      TO TRUE
                   MOVE RORDNR-MC          TO RORDNR-MP
                   IF  RORDNR-MC = RTEXTS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RTEXTS-MC < RORDNR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RTEXTS-PROCESS      TO TRUE
                   MOVE RTEXTS-MC          TO RTEXTS-MP
                   IF  RTEXTS-MC = RORDNR-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RORDNR-MC = RTEXTS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RORDNR-PROCESS      TO TRUE
                   MOVE RORDNR-MC          TO RORDNR-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR)
               MOVE SPACES TO RESTEXT-IO-AREA
               INITIALIZE RESTEXT-IO-AREA
               MOVE RTREC                  TO RESTEXT-IO-AREA (1:130)
               WRITE RESTEXT-IO-AREA
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
           INITIALIZE RORDNR-DATA-FIELDS
           SET RORDNR-EOF-OFF              TO TRUE
           SET RORDNR-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO RORDNR-MC
                                              RORDNR-MP
           OPEN INPUT RORDNR
           INITIALIZE RTEXTS-DATA-FIELDS
           SET RTEXTS-EOF-OFF              TO TRUE
           SET RTEXTS-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO RTEXTS-MC
                                              RTEXTS-MP
           OPEN INPUT RTEXTS
           OPEN OUTPUT RESTEXT.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RORDNR
           CLOSE RTEXTS
           CLOSE RESTEXT.
 
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
