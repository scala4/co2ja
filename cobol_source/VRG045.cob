       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG045R.
      **********************************************  Z-WIN-RPG2      *
      * PROGRAM........... VRG045                XX2000XXIRXXRE       *
      * PROGRAMERER....... ESPEN LARSEN                               *
      * PROGRAMERT........ 10/3-1992                                  *
      * PROGRAM ENDRET.... 10/3-1992                                  *
      * PROGRAMMET MERGER VARE.OVERF.ALTERNATIV.FILE MED FIRMA.EDBNR. *
      * REORG FOR Å OPPDATERE TIL EDB-NR                              *
      * HVIST IKKE RECORDEN MATCHER LEGGES DEN IKKE UT PÅ OUTPUT.     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG045.rpg
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
           SELECT EDBTAB
               ASSIGN TO UT-S-EDBTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDBTAB-STATUS.
           SELECT VOAINN
               ASSIGN TO UT-S-VOAINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VOAINN-STATUS.
           SELECT VOAUT
               ASSIGN TO UT-S-VOAUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VOAUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD EDBTAB
               BLOCK CONTAINS 200
               RECORD CONTAINS 20.
       01  EDBTAB-IO-AREA.
           05  EDBTAB-IO-AREA-X            PICTURE X(20).
       FD VOAINN
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VOAINN-IO-AREA.
           05  VOAINN-IO-AREA-X            PICTURE X(60).
       FD VOAUT
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VOAUT-IO-AREA.
           05  VOAUT-IO-AREA-X             PICTURE X(60).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  EDBTAB-STATUS               PICTURE 99 VALUE 0.
           10  VOAINN-STATUS               PICTURE 99 VALUE 0.
           10  VOAUT-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBTAB-EOF-OFF          VALUE '0'.
               88  EDBTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBTAB-READ-OFF         VALUE '0'.
               88  EDBTAB-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBTAB-PROCESS-OFF      VALUE '0'.
               88  EDBTAB-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-EOF-OFF          VALUE '0'.
               88  VOAINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-READ-OFF         VALUE '0'.
               88  VOAINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-PROCESS-OFF      VALUE '0'.
               88  VOAINN-PROCESS          VALUE '1'.
           05  EDBTAB-DATA-FIELDS.
               10  TFNR                    PICTURE X(3).
               10  FFNR                    PICTURE X(3).
               10  EDB                     PICTURE X(7).
               10  TEDB                    PICTURE X(7).
           05  EDBTAB-MP                   PICTURE X(13).
           05  EDBTAB-MC                   PICTURE X(13).
           05  EDBTAB-M-01             REDEFINES EDBTAB-MC.
               10  EDBTAB-M-01-M3.
                   15  EDBTAB-M-01-M3-TFNR-G.
                       20  EDBTAB-M-01-M3-TFNR PICTURE X(3).
               10  EDBTAB-M-01-M2.
                   15  EDBTAB-M-01-M2-FFNR-G.
                       20  EDBTAB-M-01-M2-FFNR PICTURE X(3).
               10  EDBTAB-M-01-M1.
                   15  EDBTAB-M-01-M1-EDB-G.
                       20  EDBTAB-M-01-M1-EDB PICTURE X(7).
           05  VOAINN-DATA-FIELDS.
               10  TFIRM                   PICTURE X(3).
               10  FFIRM                   PICTURE X(3).
               10  FEDBNR                  PICTURE X(7).
               10  VOAREC                  PICTURE X(60).
           05  VOAINN-MP                   PICTURE X(13).
           05  VOAINN-MC                   PICTURE X(13).
           05  VOAINN-M-02             REDEFINES VOAINN-MC.
               10  VOAINN-M-02-M3.
                   15  VOAINN-M-02-M3-TFIRM-G.
                       20  VOAINN-M-02-M3-TFIRM PICTURE X(3).
               10  VOAINN-M-02-M2.
                   15  VOAINN-M-02-M2-FFIRM-G.
                       20  VOAINN-M-02-M2-FFIRM PICTURE X(3).
               10  VOAINN-M-02-M1.
                   15  VOAINN-M-02-M1-FEDBNR-G.
                       20  VOAINN-M-02-M1-FEDBNR PICTURE X(7).
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
           IF  EDBTAB-PROCESS
               SET EDBTAB-PROCESS-OFF      TO TRUE
               SET EDBTAB-READ             TO TRUE
           END-IF
 
           IF  EDBTAB-READ
               PERFORM EDBTAB-GET
               SET EDBTAB-READ-OFF         TO TRUE
               IF  NOT EDBTAB-EOF
                   PERFORM EDBTAB-MATCH-SET
               END-IF
           END-IF
 
           IF  VOAINN-PROCESS
               SET VOAINN-PROCESS-OFF      TO TRUE
               SET VOAINN-READ             TO TRUE
           END-IF
 
           IF  VOAINN-READ
               PERFORM VOAINN-GET
               SET VOAINN-READ-OFF         TO TRUE
               IF  NOT VOAINN-EOF
                   PERFORM VOAINN-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  EDBTAB-PROCESS
               PERFORM EDBTAB-IDSET
           END-IF
 
           IF  VOAINN-PROCESS
               PERFORM VOAINN-IDSET
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
 
           IF  EDBTAB-PROCESS
               PERFORM EDBTAB-FLDSET
           END-IF
 
           IF  VOAINN-PROCESS
               PERFORM VOAINN-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       EDBTAB-GET SECTION.
       EDBTAB-GET-P.
           IF  EDBTAB-EOF-OFF
               READ EDBTAB
               AT END
                   SET EDBTAB-EOF          TO TRUE
               END-READ
           END-IF.
 
       EDBTAB-FLDSET SECTION.
       EDBTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE EDBTAB-IO-AREA (1:3)   TO TFNR (1:3)
               MOVE EDBTAB-IO-AREA (4:3)   TO FFNR (1:3)
               MOVE EDBTAB-IO-AREA (7:7)   TO EDB (1:7)
               MOVE EDBTAB-IO-AREA (14:7)  TO TEDB (1:7)
           END-EVALUATE.
 
       EDBTAB-IDSET SECTION.
       EDBTAB-IDSET-P.
           SET I-01                        TO TRUE.
 
       EDBTAB-MATCH-SET SECTION.
       EDBTAB-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE EDBTAB-IO-AREA (1:3)   TO EDBTAB-M-01-M3-TFNR
               MOVE EDBTAB-IO-AREA (4:3)   TO EDBTAB-M-01-M2-FFNR
               MOVE EDBTAB-IO-AREA (7:7)   TO EDBTAB-M-01-M1-EDB
           END-EVALUATE.
 
       VOAINN-GET SECTION.
       VOAINN-GET-P.
           IF  VOAINN-EOF-OFF
               READ VOAINN
               AT END
                   SET VOAINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VOAINN-FLDSET SECTION.
       VOAINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VOAINN-IO-AREA (42:3)  TO TFIRM (1:3)
               MOVE VOAINN-IO-AREA (1:3)   TO FFIRM (1:3)
               MOVE VOAINN-IO-AREA (4:7)   TO FEDBNR (1:7)
               MOVE VOAINN-IO-AREA (1:60)  TO VOAREC (1:60)
           END-EVALUATE.
 
       VOAINN-IDSET SECTION.
       VOAINN-IDSET-P.
           SET I-02                        TO TRUE.
 
       VOAINN-MATCH-SET SECTION.
       VOAINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VOAINN-IO-AREA (42:3)  TO VOAINN-M-02-M3-TFIRM
               MOVE VOAINN-IO-AREA (1:3)   TO VOAINN-M-02-M2-FFIRM
               MOVE VOAINN-IO-AREA (4:7)   TO VOAINN-M-02-M1-FEDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  EDBTAB-EOF
               MOVE HIGH-VALUES            TO EDBTAB-MC
                                              EDBTAB-MP
           END-IF
           IF  VOAINN-EOF
               MOVE HIGH-VALUES            TO VOAINN-MC
                                              VOAINN-MP
           END-IF
           IF  EDBTAB-MC < EDBTAB-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VOAINN-MC < VOAINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  EDBTAB-MC < VOAINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET EDBTAB-PROCESS      TO TRUE
                   MOVE EDBTAB-MC          TO EDBTAB-MP
                   IF  EDBTAB-MC = VOAINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VOAINN-MC < EDBTAB-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VOAINN-PROCESS      TO TRUE
                   MOVE VOAINN-MC          TO VOAINN-MP
                   IF  VOAINN-MC = EDBTAB-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  EDBTAB-MC = VOAINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET EDBTAB-PROCESS      TO TRUE
                   MOVE EDBTAB-MC          TO EDBTAB-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR)
               MOVE SPACES TO VOAUT-IO-AREA
               INITIALIZE VOAUT-IO-AREA
               MOVE VOAREC                 TO VOAUT-IO-AREA (1:60)
               MOVE TEDB                   TO VOAUT-IO-AREA (45:7)
               WRITE VOAUT-IO-AREA
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
           INITIALIZE EDBTAB-DATA-FIELDS
           SET EDBTAB-EOF-OFF              TO TRUE
           SET EDBTAB-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO EDBTAB-MC
                                              EDBTAB-MP
           OPEN INPUT EDBTAB
           INITIALIZE VOAINN-DATA-FIELDS
           SET VOAINN-EOF-OFF              TO TRUE
           SET VOAINN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO VOAINN-MC
                                              VOAINN-MP
           OPEN INPUT VOAINN
           OPEN OUTPUT VOAUT.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE EDBTAB
           CLOSE VOAINN
           CLOSE VOAUT.
 
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
