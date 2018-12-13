       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK060R.
      ******************************************* :   Z-WIN-RPG2     **
      *  PROGRAM: FAK060                                              *
      *           ESPEN LARSEN 18.07.2001                             *
      *  LEGGER UT KUNDEREF. I EGET SORTOMERÅDE PÅ ALLE RECORDS PÅ    *
      *  KUNDER SOM SKAL HA SAMLEFAKTURA PR. KUNDEREF.                *
      *  DISSE RECORD ER MERKET I POS. 38 MED KODE R I PROGRAM FAK035.*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK060.rpg
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
           SELECT FAKTIN
               ASSIGN TO UT-S-FAKTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTIN-STATUS.
           SELECT FAKTUT
               ASSIGN TO UT-S-FAKTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKTIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKTIN-IO-AREA.
           05  FAKTIN-IO-AREA-X            PICTURE X(200).
       FD FAKTUT
               BLOCK CONTAINS 430
               RECORD CONTAINS 215.
       01  FAKTUT-IO-AREA.
           05  FAKTUT-IO-AREA-X            PICTURE X(215).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKTIN-STATUS               PICTURE 99 VALUE 0.
           10  FAKTUT-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-EOF-OFF          VALUE '0'.
               88  FAKTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-READ-OFF         VALUE '0'.
               88  FAKTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-PROCESS-OFF      VALUE '0'.
               88  FAKTIN-PROCESS          VALUE '1'.
           05  FAKTIN-DATA-FIELDS.
               10  FAKREC                  PICTURE X(200).
               10  SAMFKR                  PICTURE X(1).
               10  KUNREF                  PICTURE X(15).
               10  ORDKNR                  PICTURE X(6).
      *****************************************************************
      * HOVEDRUTINE: SKAL DET LEGGES UT KUNDEREF. I SORTFELT ?        *
      *              ELLER SKAL DET LEGGES UT ORDREKUNDENR.  ?        *
      *****************************************************************
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKTIN-PROCESS
               SET FAKTIN-PROCESS-OFF      TO TRUE
               SET FAKTIN-READ             TO TRUE
           END-IF
 
           IF  FAKTIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKTIN-GET
               SET FAKTIN-READ-OFF         TO TRUE
               IF  NOT FAKTIN-EOF
                   SET FAKTIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-IDSET
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
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-FLDSET
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
           SET NOT-I-50                    TO TRUE
           SET NOT-I-20                    TO TRUE
           IF  SAMFKR = 'R'
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  SAMFKR = 'B'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-20 AND NOT-I-21)
               GO TO SLUTT-T
           END-IF
           SET I-50                        TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
       FAKTIN-GET SECTION.
       FAKTIN-GET-P.
           IF  FAKTIN-EOF-OFF
               READ FAKTIN
               AT END
                   SET FAKTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKTIN-FLDSET SECTION.
       FAKTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTIN-IO-AREA (1:200) TO FAKREC (1:200)
               MOVE FAKTIN-IO-AREA (38:1)  TO SAMFKR (1:1)
               MOVE FAKTIN-IO-AREA (41:15) TO KUNREF (1:15)
               MOVE FAKTIN-IO-AREA (184:6) TO ORDKNR (1:6)
           END-EVALUATE.
 
       FAKTIN-IDSET SECTION.
       FAKTIN-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               IF  (NOT-I-50)
                   MOVE '               '  TO FAKTUT-IO-AREA (201:15)
               END-IF
               IF  (I-50 AND I-20)
                   MOVE KUNREF             TO FAKTUT-IO-AREA (201:15)
               END-IF
               IF  (I-50 AND I-21)
                   MOVE ORDKNR             TO FAKTUT-IO-AREA (210:6)
               END-IF
               WRITE FAKTUT-IO-AREA
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
           INITIALIZE FAKTIN-DATA-FIELDS
           SET FAKTIN-EOF-OFF              TO TRUE
           SET FAKTIN-PROCESS              TO TRUE
           OPEN INPUT FAKTIN
           OPEN OUTPUT FAKTUT.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKTIN
           CLOSE FAKTUT.
 
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
