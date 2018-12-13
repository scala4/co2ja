       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBS090R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: BBS090, VIDERSENDING AV OCR-FIL              *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: BBS02A.                                      *
      *  LAGET DATO....: 09.02.07                                     *
      *  ENDRET........: 28.04.10 TATT UT HARDKODING AV FIRMANR,      *
      *                           LESER FRA SYSIPT                    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BBS090.rpg
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
           SELECT FIRMAT
               ASSIGN TO UT-S-FIRMAT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRMAT-STATUS.
           SELECT BBSTRAI
               ASSIGN TO UT-S-BBSTRAI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSTRAI-STATUS.
           SELECT BANKGF
               ASSIGN TO BANKGF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS BANKGF-STATUS
               RECORD KEY IS BANKGF-KEY1.
           SELECT BBSREC
               ASSIGN TO UT-S-BBSREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSREC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRMAT
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FIRMAT-IO-AREA.
           05  FIRMAT-IO-AREA-X            PICTURE X(80).
       FD BBSTRAI
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  BBSTRAI-IO-AREA.
           05  BBSTRAI-IO-AREA-X           PICTURE X(80).
       FD BANKGF
               RECORD CONTAINS 80.
       01  BANKGF-IO-AREA.
           05  BANKGF-IO-AREA-X.
               10  BANKGF-KEY1             PICTURE X(11).
               10  FILLER                  PICTURE X(69).
      *FLISTEO O   F  80  80            PRINTERSYSLST
       FD BBSREC
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  BBSREC-IO-AREA.
           05  BBSREC-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  TABFNR-MAX   VALUE 1            PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFNR-TABLE.
               10  TABFNR-ENTRY
                                           OCCURS 1 TIMES
                                           INDEXED BY TABFNR-I
                                                      TABFNR-S.
                   15  TABFNR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FIRMAT-STATUS               PICTURE 99 VALUE 0.
           10  BBSTRAI-STATUS              PICTURE 99 VALUE 0.
           10  BANKGF-STATUS               PICTURE 99 VALUE 0.
           10  BBSREC-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAT-EOF-OFF          VALUE '0'.
               88  FIRMAT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSTRAI-EOF-OFF         VALUE '0'.
               88  BBSTRAI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSTRAI-READ-OFF        VALUE '0'.
               88  BBSTRAI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSTRAI-PROCESS-OFF     VALUE '0'.
               88  BBSTRAI-PROCESS         VALUE '1'.
           05  BANKGF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  BBSTRAI-DATA-FIELDS.
               10  REC080                  PICTURE X(80).
               10  RECTYP                  PICTURE X(2).
               10  BBSKTO                  PICTURE X(11).
           05  BANKGF-DATA-FIELDS.
               10  BGFFNR                  PICTURE X(3).
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
           IF  BBSTRAI-PROCESS
               SET BBSTRAI-PROCESS-OFF     TO TRUE
               SET BBSTRAI-READ            TO TRUE
           END-IF
 
           IF  BBSTRAI-READ
           AND RECORD-SELECTED-OFF
               PERFORM BBSTRAI-GET
               SET BBSTRAI-READ-OFF        TO TRUE
               IF  NOT BBSTRAI-EOF
                   SET BBSTRAI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BBSTRAI-PROCESS
               PERFORM BBSTRAI-IDSET
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
 
           IF  BBSTRAI-PROCESS
               PERFORM BBSTRAI-FLDSET
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
           SET NOT-I-10                    TO TRUE
           SET NOT-I-11                    TO TRUE
           IF  (I-12 AND I-13)
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  RECTYP = '20'
               SET I-10                    TO TRUE
           END-IF
           IF  (I-10)
               MOVE BBSKTO                 TO BANKGF-KEY1
               READ BANKGF RECORD KEY IS BANKGF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM BANKGF-FLDSET
                   PERFORM BANKGF-IDSET
               END-READ
      * N11      BGFFNR    COMP "172"                    12             ROLL
           END-IF
           IF  (NOT-I-11)
               SET NOT-I-12                TO TRUE
               SET TABFNR-S                TO TABFNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFNR-I FROM 1 BY 1
                         UNTIL TABFNR-I >= TABFNR-MAX
                            OR I-12
                   IF  BGFFNR = TABFNR (TABFNR-I)
                       SET I-12            TO TRUE
                       SET TABFNR-S        TO TABFNR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-12)
               SET NOT-I-13                TO TRUE
               IF  RECTYP = '88'
                   SET I-13                TO TRUE
               END-IF
      *                    MOVE "BBSKTO  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGF         BBSKTO           VIS INDIKATOR
           END-IF
           .
 
       BBSTRAI-GET SECTION.
       BBSTRAI-GET-P.
           IF  BBSTRAI-EOF-OFF
               READ BBSTRAI
               AT END
                   SET BBSTRAI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BBSTRAI-FLDSET SECTION.
       BBSTRAI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BBSTRAI-IO-AREA (1:80) TO REC080 (1:80)
               MOVE BBSTRAI-IO-AREA (7:2)  TO RECTYP (1:2)
               MOVE BBSTRAI-IO-AREA (25:11) TO BBSKTO (1:11)
           END-EVALUATE.
 
       BBSTRAI-IDSET SECTION.
       BBSTRAI-IDSET-P.
           SET I-01                        TO TRUE.
 
       BANKGF-FLDSET SECTION.
       BANKGF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BANKGF-IO-AREA (12:3)  TO BGFFNR (1:3)
           END-EVALUATE.
 
       BANKGF-IDSET SECTION.
       BANKGF-IDSET-P.
           SET I-02                        TO TRUE.
 
       FIRMAT-LOAD SECTION.
       FIRMAT-LOAD-P.
           OPEN INPUT FIRMAT
           SET TABFNR-I                    TO 1
           PERFORM UNTIL FIRMAT-EOF
               READ FIRMAT
               AT END
                   SET FIRMAT-EOF          TO TRUE
               NOT AT END
                   MOVE FIRMAT-IO-AREA (1:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FIRMAT.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-12)
               MOVE SPACES TO BBSREC-IO-AREA
               INITIALIZE BBSREC-IO-AREA
               MOVE REC080                 TO BBSREC-IO-AREA (1:80)
               WRITE BBSREC-IO-AREA
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
           PERFORM FIRMAT-LOAD
           INITIALIZE BBSTRAI-DATA-FIELDS
           SET BBSTRAI-EOF-OFF             TO TRUE
           SET BBSTRAI-PROCESS             TO TRUE
           OPEN INPUT BBSTRAI
           INITIALIZE BANKGF-DATA-FIELDS
           OPEN INPUT BANKGF
           OPEN OUTPUT BBSREC.
           SET TABFNR-I                    TO 1.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BBSTRAI
           CLOSE BANKGF
           CLOSE BBSREC.
 
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
