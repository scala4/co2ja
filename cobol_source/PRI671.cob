       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI671R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET MERGER UTPLUKK FRA VAREARKIVET MED    **
      *  UTPLUKK FRA OPPSLAGSARKIVET OG HENTER KORTNUMMER.**
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI671.rpg
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
           SELECT INF1
               ASSIGN TO UT-S-INF1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF1-STATUS.
           SELECT INF2
               ASSIGN TO UT-S-INF2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF2-STATUS.
           SELECT PRTFILE
               ASSIGN TO UT-S-PRTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INF1
               BLOCK CONTAINS 2670
               RECORD CONTAINS 30.
       01  INF1-IO-AREA.
           05  INF1-IO-AREA-X              PICTURE X(30).
       FD INF2
               BLOCK CONTAINS 9000
               RECORD CONTAINS 250.
       01  INF2-IO-AREA.
           05  INF2-IO-AREA-X              PICTURE X(250).
       FD PRTFILE
               BLOCK CONTAINS 4067
               RECORD CONTAINS 83.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(83).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INF1-STATUS                 PICTURE 99 VALUE 0.
           10  INF2-STATUS                 PICTURE 99 VALUE 0.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INF1-EOF-OFF            VALUE '0'.
               88  INF1-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF1-READ-OFF           VALUE '0'.
               88  INF1-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF1-PROCESS-OFF        VALUE '0'.
               88  INF1-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF2-EOF-OFF            VALUE '0'.
               88  INF2-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF2-READ-OFF           VALUE '0'.
               88  INF2-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF2-PROCESS-OFF        VALUE '0'.
               88  INF2-PROCESS            VALUE '1'.
           05  INF1-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  KORTNR                  PICTURE X(5).
           05  INF1-MP                     PICTURE X(10).
           05  INF1-MC                     PICTURE X(10).
           05  INF1-M-04               REDEFINES INF1-MC.
               10  INF1-M-04-M2.
                   15  INF1-M-04-M2-FNR-G.
                       20  INF1-M-04-M2-FNR PICTURE X(3).
               10  INF1-M-04-M1.
                   15  INF1-M-04-M1-EDBNR-G.
                       20  INF1-M-04-M1-EDBNR PICTURE X(7).
           05  INF2-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VAREN                   PICTURE X(30).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  PT                      PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  PTILG-IO.
                   15  PTILG               PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  INF2-MP                     PICTURE X(10).
           05  INF2-MC                     PICTURE X(10).
           05  INF2-M-03               REDEFINES INF2-MC.
               10  INF2-M-03-M2.
                   15  INF2-M-03-M2-FNR-G.
                       20  INF2-M-03-M2-FNR PICTURE X(3).
               10  INF2-M-03-M1.
                   15  INF2-M-03-M1-EDBNR-G.
                       20  INF2-M-03-M1-EDBNR PICTURE X(7).
           05  EDITTING-FIELDS.
               10  XO-52D                  PICTURE S9(5)V9(2).
               10  XO-52U                  PICTURE 9(5)V9(2).
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INF1-PROCESS
               SET INF1-PROCESS-OFF        TO TRUE
               SET INF1-READ               TO TRUE
           END-IF
 
           IF  INF1-READ
               PERFORM INF1-GET
               SET INF1-READ-OFF           TO TRUE
               IF  NOT INF1-EOF
                   PERFORM INF1-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM INF1-MATCH-SET
               END-IF
           END-IF
 
           IF  INF2-PROCESS
               SET INF2-PROCESS-OFF        TO TRUE
               SET INF2-READ               TO TRUE
           END-IF
 
           IF  INF2-READ
               PERFORM INF2-GET
               SET INF2-READ-OFF           TO TRUE
               IF  NOT INF2-EOF
                   PERFORM INF2-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM INF2-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  INF1-PROCESS
               PERFORM INF1-IDSET
           END-IF
 
           IF  INF2-PROCESS
               PERFORM INF2-IDSET
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
 
           IF  INF1-PROCESS
               PERFORM INF1-FLDSET
           END-IF
 
           IF  INF2-PROCESS
               PERFORM INF2-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       INF1-GET SECTION.
       INF1-GET-P.
           IF  INF1-EOF-OFF
               READ INF1
               AT END
                   SET INF1-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF1-FLDSET SECTION.
       INF1-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INF1-IO-AREA (1:1) = 'A' )
               MOVE INF1-IO-AREA (2:3)     TO FNR (1:3)
               MOVE INF1-IO-AREA (23:7)    TO EDBNR (1:7)
               MOVE INF1-IO-AREA (9:5)     TO KORTNR (1:5)
           END-EVALUATE.
 
       INF1-IDCHK SECTION.
       INF1-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INF1-IO-AREA (1:1) = 'A' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INF1-IDSET SECTION.
       INF1-IDSET-P.
           EVALUATE TRUE
           WHEN ( INF1-IO-AREA (1:1) = 'A' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       INF1-MATCH-SET SECTION.
       INF1-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( INF1-IO-AREA (1:1) = 'A' )
               MOVE INF1-IO-AREA (2:3)     TO INF1-M-04-M2-FNR
               MOVE INF1-IO-AREA (23:7)    TO INF1-M-04-M1-EDBNR
           END-EVALUATE.
 
       INF2-GET SECTION.
       INF2-GET-P.
           IF  INF2-EOF-OFF
               READ INF2
               AT END
                   SET INF2-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF2-FLDSET SECTION.
       INF2-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INF2-IO-AREA (1:1) = '7' )
               MOVE INF2-IO-AREA (3:3)     TO FNR (1:3)
               MOVE INF2-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE INF2-IO-AREA (13:3)    TO ALFA (1:3)
               MOVE INF2-IO-AREA (16:20)   TO ARTNR (1:20)
               MOVE INF2-IO-AREA (36:30)   TO VAREN (1:30)
               MOVE INF2-IO-AREA (75:9)    TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE INF2-IO-AREA (95:1)    TO PT (1:1)
               MOVE INF2-IO-AREA (118:5)   TO VGR (1:5)
               MOVE INF2-IO-AREA (161:4)   TO PTILG-IO
           END-EVALUATE.
 
       INF2-IDCHK SECTION.
       INF2-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INF2-IO-AREA (1:1) = '7' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INF2-IDSET SECTION.
       INF2-IDSET-P.
           EVALUATE TRUE
           WHEN ( INF2-IO-AREA (1:1) = '7' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       INF2-MATCH-SET SECTION.
       INF2-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( INF2-IO-AREA (1:1) = '7' )
               MOVE INF2-IO-AREA (3:3)     TO INF2-M-03-M2-FNR
               MOVE INF2-IO-AREA (6:7)     TO INF2-M-03-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  INF1-EOF
               MOVE HIGH-VALUES            TO INF1-MC
                                              INF1-MP
           END-IF
           IF  INF2-EOF
               MOVE HIGH-VALUES            TO INF2-MC
                                              INF2-MP
           END-IF
           IF  INF1-MC < INF1-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INF2-MC < INF2-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INF1-MC < INF2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INF1-PROCESS        TO TRUE
                   MOVE INF1-MC            TO INF1-MP
                   IF  INF1-MC = INF2-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INF2-MC < INF1-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INF2-PROCESS        TO TRUE
                   MOVE INF2-MC            TO INF2-MP
                   IF  INF2-MC = INF1-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INF1-MC = INF2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INF1-PROCESS        TO TRUE
                   MOVE INF1-MC            TO INF1-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FNR                    TO PRTFILE-IO-AREA (1:3)
               MOVE ALFA                   TO PRTFILE-IO-AREA (4:3)
               MOVE ARTNR                  TO PRTFILE-IO-AREA (7:20)
               IF  (I-MR)
                   MOVE KORTNR             TO PRTFILE-IO-AREA (27:5)
                   INITIALIZE KORTNR
               END-IF
               MOVE PRIS-IO                TO PRTFILE-IO-AREA (32:9)
               MOVE VGR                    TO PRTFILE-IO-AREA (41:5)
               MOVE VAREN                  TO PRTFILE-IO-AREA (46:30)
               MOVE PT                     TO PRTFILE-IO-AREA (76:1)
               MOVE PTILG                  TO XO-52U
               MOVE XO-52U (1:7)           TO PRTFILE-IO-AREA (77:7)
               WRITE PRTFILE-IO-AREA
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
           INITIALIZE INF1-DATA-FIELDS
           SET INF1-EOF-OFF                TO TRUE
           SET INF1-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO INF1-MC
                                              INF1-MP
           OPEN INPUT INF1
           INITIALIZE INF2-DATA-FIELDS
           SET INF2-EOF-OFF                TO TRUE
           SET INF2-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO INF2-MC
                                              INF2-MP
           OPEN INPUT INF2
           OPEN OUTPUT PRTFILE.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INF1
           CLOSE INF2
           CLOSE PRTFILE.
 
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
