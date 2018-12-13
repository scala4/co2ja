       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR112R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMERT AV: ESPEN LARSEN  18.06.2004                       *
      * MERGER OPPSLAGSRECORD OG LEGGER UT RECORD SOM MERGER MED      *
      * ALTERNATIVT OPPSLAGSKEY OG EDB-NR TIL VIRKELIG ARTIKKEL.      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR112.rpg
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
           SELECT OPPSM
               ASSIGN TO UT-S-OPPSM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OPPSM-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD OPPSM
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  OPPSM-IO-AREA.
           05  OPPSM-IO-AREA-X             PICTURE X(30).
       FD INNPUT
               BLOCK CONTAINS 120
               RECORD CONTAINS 60.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(60).
       FD OUTPUT-X
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(30).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  OPPSM-STATUS                PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSM-EOF-OFF           VALUE '0'.
               88  OPPSM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSM-READ-OFF          VALUE '0'.
               88  OPPSM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSM-PROCESS-OFF       VALUE '0'.
               88  OPPSM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  OPPSM-DATA-FIELDS.
               10  OFIRMA                  PICTURE X(3).
               10  OALF                    PICTURE X(3).
               10  OART14                  PICTURE X(14).
               10  EDB                     PICTURE X(7).
           05  OPPSM-MP                    PICTURE X(20).
           05  OPPSM-MC                    PICTURE X(20).
           05  OPPSM-M-01              REDEFINES OPPSM-MC.
               10  OPPSM-M-01-M3.
                   15  OPPSM-M-01-M3-OFIRMA-G.
                       20  OPPSM-M-01-M3-OFIRMA PICTURE X(3).
               10  OPPSM-M-01-M2.
                   15  OPPSM-M-01-M2-OALF-G.
                       20  OPPSM-M-01-M2-OALF PICTURE X(3).
               10  OPPSM-M-01-M1.
                   15  OPPSM-M-01-M1-OART14-G.
                       20  OPPSM-M-01-M1-OART14 PICTURE X(14).
           05  INNPUT-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ALF                     PICTURE X(3).
               10  ART                     PICTURE X(14).
               10  ART14                   PICTURE X(14).
               10  VALF                    PICTURE X(3).
      *****************************************************************
      * BLANK I ALFAKODE SKAL IKKE OVERFØRES.                         *
      *****************************************************************
           05  INNPUT-MP                   PICTURE X(20).
           05  INNPUT-MC                   PICTURE X(20).
           05  INNPUT-M-02             REDEFINES INNPUT-MC.
               10  INNPUT-M-02-M3.
                   15  INNPUT-M-02-M3-FIRMA-G.
                       20  INNPUT-M-02-M3-FIRMA PICTURE X(3).
               10  INNPUT-M-02-M2.
                   15  INNPUT-M-02-M2-VALF-G.
                       20  INNPUT-M-02-M2-VALF PICTURE X(3).
               10  INNPUT-M-02-M1.
                   15  INNPUT-M-02-M1-ART14-G.
                       20  INNPUT-M-02-M1-ART14 PICTURE X(14).
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
           IF  OPPSM-PROCESS
               SET OPPSM-PROCESS-OFF       TO TRUE
               SET OPPSM-READ              TO TRUE
           END-IF
 
           IF  OPPSM-READ
               PERFORM OPPSM-GET
               SET OPPSM-READ-OFF          TO TRUE
               IF  NOT OPPSM-EOF
                   PERFORM OPPSM-MATCH-SET
               END-IF
           END-IF
 
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   PERFORM INNPUT-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  OPPSM-PROCESS
               PERFORM OPPSM-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
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
 
           IF  OPPSM-PROCESS
               PERFORM OPPSM-FLDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDOFF
               PERFORM INNPUT-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       OPPSM-GET SECTION.
       OPPSM-GET-P.
           IF  OPPSM-EOF-OFF
               READ OPPSM
               AT END
                   SET OPPSM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       OPPSM-FLDSET SECTION.
       OPPSM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSM-IO-AREA (2:3)    TO OFIRMA (1:3)
               MOVE OPPSM-IO-AREA (5:3)    TO OALF (1:3)
               MOVE OPPSM-IO-AREA (9:14)   TO OART14 (1:14)
               MOVE OPPSM-IO-AREA (23:7)   TO EDB (1:7)
           END-EVALUATE.
 
       OPPSM-IDSET SECTION.
       OPPSM-IDSET-P.
           SET I-01                        TO TRUE.
 
       OPPSM-MATCH-SET SECTION.
       OPPSM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSM-IO-AREA (2:3)    TO OPPSM-M-01-M3-OFIRMA
               MOVE OPPSM-IO-AREA (5:3)    TO OPPSM-M-01-M2-OALF
               MOVE OPPSM-IO-AREA (9:14)   TO OPPSM-M-01-M1-OART14
           END-EVALUATE.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT-FLDOFF SECTION.
       INNPUT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE INNPUT-IO-AREA (5:3)   TO ALF (1:3)
               IF  ALF = SPACES
                   SET I-09                TO TRUE
               END-IF
               MOVE INNPUT-IO-AREA (9:14)  TO ART (1:14)
               MOVE INNPUT-IO-AREA (31:14) TO ART14 (1:14)
               MOVE INNPUT-IO-AREA (51:3)  TO VALF (1:3)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-02                        TO TRUE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (2:3)   TO INNPUT-M-02-M3-FIRMA
               MOVE INNPUT-IO-AREA (51:3)  TO INNPUT-M-02-M2-VALF
               MOVE INNPUT-IO-AREA (31:14) TO INNPUT-M-02-M1-ART14
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  OPPSM-EOF
               MOVE HIGH-VALUES            TO OPPSM-MC
                                              OPPSM-MP
           END-IF
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  OPPSM-MC < OPPSM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  OPPSM-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET OPPSM-PROCESS       TO TRUE
                   MOVE OPPSM-MC           TO OPPSM-MP
                   IF  OPPSM-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC < OPPSM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = OPPSM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  OPPSM-MC = INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET OPPSM-PROCESS       TO TRUE
                   MOVE OPPSM-MC           TO OPPSM-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR AND NOT-I-09)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE 'A'                    TO OUTPUT-X-IO-AREA (1:1)
               MOVE OFIRMA                 TO OUTPUT-X-IO-AREA (2:3)
               MOVE ALF                    TO OUTPUT-X-IO-AREA (5:3)
               MOVE ' '                    TO OUTPUT-X-IO-AREA (8:1)
               MOVE ART                    TO OUTPUT-X-IO-AREA (9:14)
               MOVE EDB                    TO OUTPUT-X-IO-AREA (23:7)
               WRITE OUTPUT-X-IO-AREA
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
           INITIALIZE OPPSM-DATA-FIELDS
           SET OPPSM-EOF-OFF               TO TRUE
           SET OPPSM-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO OPPSM-MC
                                              OPPSM-MP
           OPEN INPUT OPPSM
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           OPEN OUTPUT OUTPUT-X.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE OPPSM
           CLOSE INNPUT
           CLOSE OUTPUT-X.
 
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
