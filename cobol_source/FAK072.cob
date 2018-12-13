       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK072R.
      ******************************************* :   Z-WIN-RPG2     **
      * OPPDATERE KUNDEMASTER MED TOTALT FAKTURERT PR. KUNDE *
      * HITTIL I ÅR.                                         *
      * UPSI 1 = TILBAKEFØRING AV OPPDATERING.               *
      * FØRSTE FAKTURERING PR. ÅR NULLSTILLES KUNDEMASTER.   *
      * 04.02.00 KONSERNFIRMANR LEGGES INN FRA PROG FAK075.  *
      * 22.05.00 UPSI 6,7,8, NULLSTILLER KJØP I ÅR.          *
      ********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK072.rpg
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
           SELECT SUMFILE
               ASSIGN TO UT-S-SUMFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFILE-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SUMFILE
               BLOCK CONTAINS 9440
               RECORD CONTAINS 20.
       01  SUMFILE-IO-AREA.
           05  SUMFILE-IO-AREA-X           PICTURE X(20).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1.
                   15  KUNDEMA-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(190).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  SUMFILE-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFILE-EOF-OFF         VALUE '0'.
               88  SUMFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFILE-READ-OFF        VALUE '0'.
               88  SUMFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFILE-PROCESS-OFF     VALUE '0'.
               88  SUMFILE-PROCESS         VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-EOF-OFF         VALUE '0'.
               88  KUNDEMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-READ-OFF        VALUE '0'.
               88  KUNDEMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-PROCESS-OFF     VALUE '0'.
               88  KUNDEMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  SUMFILE-DATA-FIELDS.
               10  KEY-X                   PICTURE X(9).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  SUMFILE-MP                  PICTURE X(9).
           05  SUMFILE-MC                  PICTURE X(9).
           05  SUMFILE-M-01            REDEFINES SUMFILE-MC.
               10  SUMFILE-M-01-M1.
                   15  SUMFILE-M-01-M1-KEY-X-G.
                       20  SUMFILE-M-01-M1-KEY-X PICTURE X(9).
           05  KUNDEMA-DATA-FIELDS.
               10  RESK1                   PICTURE X(1).
               10  FAKSUM-IO.
                   15  FAKSUM              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  KUNDEMA-MP                  PICTURE X(9).
           05  KUNDEMA-MC                  PICTURE X(9).
           05  KUNDEMA-M-02            REDEFINES KUNDEMA-MC.
               10  KUNDEMA-M-02-M1.
                   15  KUNDEMA-M-02-M1-KEY-X-G.
                       20  KUNDEMA-M-02-M1-KEY-X PICTURE X(9).
           05  FAKPAR-DATA-FIELDS.
               10  FAKONR                  PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  SUMFILE-PROCESS
               SET SUMFILE-PROCESS-OFF     TO TRUE
               SET SUMFILE-READ            TO TRUE
           END-IF
 
           IF  SUMFILE-READ
               PERFORM SUMFILE-GET
               SET SUMFILE-READ-OFF        TO TRUE
               IF  NOT SUMFILE-EOF
                   PERFORM SUMFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  KUNDEMA-PROCESS
               SET KUNDEMA-PROCESS-OFF     TO TRUE
               SET KUNDEMA-READ            TO TRUE
           END-IF
 
           IF  KUNDEMA-READ
               PERFORM KUNDEMA-GET
               SET KUNDEMA-READ-OFF        TO TRUE
               IF  NOT KUNDEMA-EOF
                   PERFORM KUNDEMA-MATCH-SET
               END-IF
           END-IF
 
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
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  SUMFILE-PROCESS
               PERFORM SUMFILE-IDSET
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-IDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
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
 
           IF  SUMFILE-PROCESS
               PERFORM SUMFILE-FLDSET
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-FLDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
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
           IF  (I-03)
               SET NOT-I-11                TO TRUE
               IF  FAKONR = '01'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-U6 AND I-U7 AND I-U8)
               SET I-11                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-U1)
               ADD FAKBEL                  TO BEL
           END-IF
           IF  (I-01 AND I-U1)
               SUBTRACT FAKBEL             FROM BEL
           END-IF
           IF  (I-02)
               SET NOT-I-10                TO TRUE
               IF  RESK1 = '9'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               SUBTRACT FAKSUM             FROM FAKSUM
           END-IF
           IF  (I-02 AND I-MR)
               ADD BEL                     TO FAKSUM
           END-IF
           IF  (I-02)
               SUBTRACT BEL                FROM BEL
           END-IF.
 
       SUMFILE-GET SECTION.
       SUMFILE-GET-P.
           IF  SUMFILE-EOF-OFF
               READ SUMFILE
               AT END
                   SET SUMFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SUMFILE-FLDSET SECTION.
       SUMFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SUMFILE-IO-AREA (2:9)  TO KEY-X (1:9)
               MOVE SUMFILE-IO-AREA (11:5) TO FAKBEL-IO
           END-EVALUATE.
 
       SUMFILE-IDSET SECTION.
       SUMFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       SUMFILE-MATCH-SET SECTION.
       SUMFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SUMFILE-IO-AREA (2:9)  TO SUMFILE-M-01-M1-KEY-X
           END-EVALUATE.
 
       KUNDEMA-GET SECTION.
       KUNDEMA-GET-P.
           IF  KUNDEMA-EOF-OFF
               READ KUNDEMA
               AT END
                   SET KUNDEMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:9)  TO KEY-X (1:9)
               MOVE KUNDEMA-IO-AREA (6:1)  TO RESK1 (1:1)
               MOVE KUNDEMA-IO-AREA (150:6) TO FAKSUM-IO
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMA-MATCH-SET SECTION.
       KUNDEMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:9)  TO KUNDEMA-M-02-M1-KEY-X
           END-EVALUATE.
 
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
               MOVE FAKPAR-IO-AREA (10:2)  TO FAKONR (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  SUMFILE-EOF
               MOVE HIGH-VALUES            TO SUMFILE-MC
                                              SUMFILE-MP
           END-IF
           IF  KUNDEMA-EOF
               MOVE HIGH-VALUES            TO KUNDEMA-MC
                                              KUNDEMA-MP
           END-IF
           IF  SUMFILE-MC < SUMFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KUNDEMA-MC < KUNDEMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  SUMFILE-MC < KUNDEMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SUMFILE-PROCESS     TO TRUE
                   MOVE SUMFILE-MC         TO SUMFILE-MP
                   IF  SUMFILE-MC = KUNDEMA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEMA-MC < SUMFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   IF  KUNDEMA-MC = SUMFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SUMFILE-MC = KUNDEMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SUMFILE-PROCESS     TO TRUE
                   MOVE SUMFILE-MC         TO SUMFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR AND NOT-I-10)
           OR  (I-02 AND I-11 AND NOT-I-10)
               MOVE FAKSUM                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDEMA-IO-AREA (150:6)
               REWRITE KUNDEMA-IO-AREA
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
           MOVE 3                          TO LR-CHECK
           INITIALIZE SUMFILE-DATA-FIELDS
           SET SUMFILE-EOF-OFF             TO TRUE
           SET SUMFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO SUMFILE-MC
                                              SUMFILE-MP
           OPEN INPUT SUMFILE
           INITIALIZE KUNDEMA-DATA-FIELDS
           SET KUNDEMA-EOF-OFF             TO TRUE
           SET KUNDEMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEMA-MC
                                              KUNDEMA-MP
           OPEN I-O KUNDEMA
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE SUMFILE
           CLOSE KUNDEMA
           CLOSE FAKPAR.
 
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
