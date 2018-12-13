       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADK113R.
      **********************************************  Z-WIN-RPG2   ****
      ***************************************    XX2000XXIRXXEN       *
      *  PROGRAM....: ADK113                                          *
      *  PROGRAMERER: RUNE ERSVIK                                     *
      *  PROGRAMERT.: 12.12.97                                        *
      *  RETTET.....: 08.01.98                                        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ADK113.rpg
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
           SELECT KKSTAT
               ASSIGN TO KKSTAT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KKSTAT-STATUS.
           SELECT ORGNRF
               ASSIGN TO UT-S-ORGNRF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORGNRF-STATUS.
           SELECT UTFIL
               ASSIGN TO UT-S-UTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KKSTAT
               RECORD CONTAINS 1000.
       01  KKSTAT-IO-AREA.
           05  KKSTAT-IO-AREA-X            PICTURE X(1000).
       FD ORGNRF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  ORGNRF-IO-AREA.
           05  ORGNRF-IO-AREA-X            PICTURE X(200).
       FD UTFIL
               BLOCK CONTAINS 150
               RECORD CONTAINS 75.
       01  UTFIL-IO-AREA.
           05  UTFIL-IO-AREA-X             PICTURE X(75).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KKSTAT-STATUS               PICTURE 99 VALUE 0.
           10  ORGNRF-STATUS               PICTURE 99 VALUE 0.
           10  UTFIL-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KKSTAT-EOF-OFF          VALUE '0'.
               88  KKSTAT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KKSTAT-READ-OFF         VALUE '0'.
               88  KKSTAT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KKSTAT-PROCESS-OFF      VALUE '0'.
               88  KKSTAT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KKSTAT-LEVEL-INIT-OFF   VALUE '0'.
               88  KKSTAT-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORGNRF-EOF-OFF          VALUE '0'.
               88  ORGNRF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORGNRF-READ-OFF         VALUE '0'.
               88  ORGNRF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORGNRF-PROCESS-OFF      VALUE '0'.
               88  ORGNRF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORGNRF-LEVEL-INIT-OFF   VALUE '0'.
               88  ORGNRF-LEVEL-INIT       VALUE '1'.
           05  KKSTAT-LEVEL-02.
               10  KKSTAT-02-L1.
                   15  KKSTAT-02-L1-ORGNR2 PICTURE X(5).
           05  KKSTAT-DATA-FIELDS.
               10  ORGNR2                  PICTURE X(5).
               10  ANTJAN-IO.
                   15  ANTJAN              PICTURE S9(5).
               10  ANTFEB-IO.
                   15  ANTFEB              PICTURE S9(5).
               10  ANTMAR-IO.
                   15  ANTMAR              PICTURE S9(5).
               10  ANTAPR-IO.
                   15  ANTAPR              PICTURE S9(5).
               10  ANTMAI-IO.
                   15  ANTMAI              PICTURE S9(5).
               10  ANTJUN-IO.
                   15  ANTJUN              PICTURE S9(5).
               10  ANTJUL-IO.
                   15  ANTJUL              PICTURE S9(5).
               10  ANTAUG-IO.
                   15  ANTAUG              PICTURE S9(5).
               10  ANTSEP-IO.
                   15  ANTSEP              PICTURE S9(5).
               10  ANTOKT-IO.
                   15  ANTOKT              PICTURE S9(5).
               10  ANTNOV-IO.
                   15  ANTNOV              PICTURE S9(5).
               10  ANTDES-IO.
                   15  ANTDES              PICTURE S9(5).
           05  KKSTAT-MP                   PICTURE X(5).
           05  KKSTAT-MC                   PICTURE X(5).
           05  KKSTAT-M-02             REDEFINES KKSTAT-MC.
               10  KKSTAT-M-02-M1.
                   15  KKSTAT-M-02-M1-ORGNR2-G.
                       20  KKSTAT-M-02-M1-ORGNR2 PICTURE X(5).
           05  ORGNRF-LEVEL-01.
               10  ORGNRF-01-L1.
                   15  ORGNRF-01-L1-ORGNR  PICTURE X(5).
           05  ORGNRF-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  STATUS-X                PICTURE X(1).
               10  ORGNR                   PICTURE X(5).
               10  KNR                     PICTURE X(6).
      *****************************************************************
      *                                                               *
      *****************************************************************
           05  ORGNRF-MP                   PICTURE X(5).
           05  ORGNRF-MC                   PICTURE X(5).
           05  ORGNRF-M-01             REDEFINES ORGNRF-MC.
               10  ORGNRF-M-01-M1.
                   15  ORGNRF-M-01-M1-ORGNR-G.
                       20  ORGNRF-M-01-M1-ORGNR PICTURE X(5).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(5).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KKSTAT-PROCESS
               SET KKSTAT-PROCESS-OFF      TO TRUE
               SET KKSTAT-READ             TO TRUE
           END-IF
 
           IF  KKSTAT-READ
               PERFORM KKSTAT-GET
               SET KKSTAT-READ-OFF         TO TRUE
               IF  NOT KKSTAT-EOF
                   PERFORM KKSTAT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM KKSTAT-MATCH-SET
               END-IF
           END-IF
 
           IF  ORGNRF-PROCESS
               SET ORGNRF-PROCESS-OFF      TO TRUE
               SET ORGNRF-READ             TO TRUE
           END-IF
 
           IF  ORGNRF-READ
               PERFORM ORGNRF-GET
               SET ORGNRF-READ-OFF         TO TRUE
               IF  NOT ORGNRF-EOF
                   PERFORM ORGNRF-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  KKSTAT-PROCESS
               PERFORM KKSTAT-IDSET
           END-IF
 
           IF  ORGNRF-PROCESS
               PERFORM ORGNRF-IDSET
           END-IF
 
           IF  KKSTAT-PROCESS
               PERFORM KKSTAT-CHK-LEVEL
           END-IF
 
           IF  ORGNRF-PROCESS
               PERFORM ORGNRF-CHK-LEVEL
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
 
           IF  KKSTAT-PROCESS
               PERFORM KKSTAT-FLDSET
           END-IF
 
           IF  ORGNRF-PROCESS
               PERFORM ORGNRF-FLDSET
           END-IF
 
           IF  KKSTAT-PROCESS
           OR  ORGNRF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       KKSTAT-GET SECTION.
       KKSTAT-GET-P.
           IF  KKSTAT-EOF-OFF
               READ KKSTAT
               AT END
                   SET KKSTAT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KKSTAT-FLDSET SECTION.
       KKSTAT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KKSTAT-IO-AREA (1:1) = 'B' )
               MOVE KKSTAT-IO-AREA (2:5)   TO ORGNR2 (1:5)
               MOVE KKSTAT-IO-AREA (42:5)  TO ANTJAN-IO
               INSPECT ANTJAN-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (47:5)  TO ANTFEB-IO
               INSPECT ANTFEB-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (52:5)  TO ANTMAR-IO
               INSPECT ANTMAR-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (57:5)  TO ANTAPR-IO
               INSPECT ANTAPR-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (62:5)  TO ANTMAI-IO
               INSPECT ANTMAI-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (67:5)  TO ANTJUN-IO
               INSPECT ANTJUN-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (72:5)  TO ANTJUL-IO
               INSPECT ANTJUL-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (77:5)  TO ANTAUG-IO
               INSPECT ANTAUG-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (82:5)  TO ANTSEP-IO
               INSPECT ANTSEP-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (87:5)  TO ANTOKT-IO
               INSPECT ANTOKT-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (92:5)  TO ANTNOV-IO
               INSPECT ANTNOV-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT-IO-AREA (97:5)  TO ANTDES-IO
               INSPECT ANTDES-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KKSTAT-IDCHK SECTION.
       KKSTAT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KKSTAT-IO-AREA (1:1) = 'B' )
             OR ( KKSTAT-IO-AREA (1:1) NOT = 'B' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KKSTAT-IDSET SECTION.
       KKSTAT-IDSET-P.
           EVALUATE TRUE
           WHEN ( KKSTAT-IO-AREA (1:1) = 'B' )
               SET I-02                    TO TRUE
           WHEN ( KKSTAT-IO-AREA (1:1) NOT = 'B' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       KKSTAT-CHK-LEVEL SECTION.
       KKSTAT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KKSTAT-IO-AREA (1:1) = 'B' )
               MOVE LOW-VALUES             TO KKSTAT-LEVEL-02
               MOVE KKSTAT-IO-AREA (2:5)   TO KKSTAT-02-L1-ORGNR2
               IF  KKSTAT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KKSTAT-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KKSTAT-02-L1          TO THE-PRIOR-L1
               SET KKSTAT-LEVEL-INIT       TO TRUE
           WHEN ( KKSTAT-IO-AREA (1:1) NOT = 'B' )
               CONTINUE
           END-EVALUATE.
 
       KKSTAT-MATCH-SET SECTION.
       KKSTAT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( KKSTAT-IO-AREA (1:1) = 'B' )
               MOVE KKSTAT-IO-AREA (2:5)   TO KKSTAT-M-02-M1-ORGNR2
           WHEN ( KKSTAT-IO-AREA (1:1) NOT = 'B' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           END-EVALUATE.
 
       ORGNRF-GET SECTION.
       ORGNRF-GET-P.
           IF  ORGNRF-EOF-OFF
               READ ORGNRF
               AT END
                   SET ORGNRF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORGNRF-FLDSET SECTION.
       ORGNRF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORGNRF-IO-AREA (2:3)   TO FNR (1:3)
               MOVE ORGNRF-IO-AREA (17:1)  TO STATUS-X (1:1)
               MOVE ORGNRF-IO-AREA (18:5)  TO ORGNR (1:5)
               MOVE ORGNRF-IO-AREA (25:6)  TO KNR (1:6)
           END-EVALUATE.
 
       ORGNRF-IDSET SECTION.
       ORGNRF-IDSET-P.
           SET I-01                        TO TRUE.
 
       ORGNRF-CHK-LEVEL SECTION.
       ORGNRF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORGNRF-LEVEL-01
               MOVE ORGNRF-IO-AREA (18:5)  TO ORGNRF-01-L1-ORGNR
               IF  ORGNRF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORGNRF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORGNRF-01-L1          TO THE-PRIOR-L1
               SET ORGNRF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       ORGNRF-MATCH-SET SECTION.
       ORGNRF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ORGNRF-IO-AREA (18:5)  TO ORGNRF-M-01-M1-ORGNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  KKSTAT-EOF
               MOVE HIGH-VALUES            TO KKSTAT-MC
                                              KKSTAT-MP
           END-IF
           IF  ORGNRF-EOF
               MOVE HIGH-VALUES            TO ORGNRF-MC
                                              ORGNRF-MP
           END-IF
           IF  KKSTAT-MC < KKSTAT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORGNRF-MC < ORGNRF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KKSTAT-MC < ORGNRF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KKSTAT-PROCESS      TO TRUE
                   MOVE KKSTAT-MC          TO KKSTAT-MP
                   IF  KKSTAT-MC = ORGNRF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORGNRF-MC < KKSTAT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORGNRF-PROCESS      TO TRUE
                   MOVE ORGNRF-MC          TO ORGNRF-MP
                   IF  ORGNRF-MC = KKSTAT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KKSTAT-MC = ORGNRF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KKSTAT-PROCESS      TO TRUE
                   MOVE KKSTAT-MC          TO KKSTAT-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE KNR                    TO UTFIL-IO-AREA (1:6)
               MOVE ORGNR                  TO UTFIL-IO-AREA (7:5)
               MOVE FNR                    TO UTFIL-IO-AREA (12:3)
               MOVE ANTJAN-IO              TO UTFIL-IO-AREA (15:5)
               MOVE ANTFEB-IO              TO UTFIL-IO-AREA (20:5)
               MOVE ANTMAR-IO              TO UTFIL-IO-AREA (25:5)
               MOVE ANTAPR-IO              TO UTFIL-IO-AREA (30:5)
               MOVE ANTMAI-IO              TO UTFIL-IO-AREA (35:5)
               MOVE ANTJUN-IO              TO UTFIL-IO-AREA (40:5)
               MOVE ANTJUL-IO              TO UTFIL-IO-AREA (45:5)
               MOVE ANTAUG-IO              TO UTFIL-IO-AREA (50:5)
               MOVE ANTSEP-IO              TO UTFIL-IO-AREA (55:5)
               MOVE ANTOKT-IO              TO UTFIL-IO-AREA (60:5)
               MOVE ANTNOV-IO              TO UTFIL-IO-AREA (65:5)
               MOVE ANTDES-IO              TO UTFIL-IO-AREA (70:5)
               MOVE STATUS-X               TO UTFIL-IO-AREA (75:1)
               WRITE UTFIL-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE KNR                    TO UTFIL-IO-AREA (1:6)
               MOVE ORGNR                  TO UTFIL-IO-AREA (7:5)
               MOVE FNR                    TO UTFIL-IO-AREA (12:3)
               MOVE '00000'                TO UTFIL-IO-AREA (15:5)
               MOVE '00000'                TO UTFIL-IO-AREA (20:5)
               MOVE '00000'                TO UTFIL-IO-AREA (25:5)
               MOVE '00000'                TO UTFIL-IO-AREA (30:5)
               MOVE '00000'                TO UTFIL-IO-AREA (35:5)
               MOVE '00000'                TO UTFIL-IO-AREA (40:5)
               MOVE '00000'                TO UTFIL-IO-AREA (45:5)
               MOVE '00000'                TO UTFIL-IO-AREA (50:5)
               MOVE '00000'                TO UTFIL-IO-AREA (55:5)
               MOVE '00000'                TO UTFIL-IO-AREA (60:5)
               MOVE '00000'                TO UTFIL-IO-AREA (65:5)
               MOVE '00000'                TO UTFIL-IO-AREA (70:5)
               MOVE STATUS-X               TO UTFIL-IO-AREA (75:1)
               WRITE UTFIL-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR)
      *                        KNR        6
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE ORGNR2                 TO UTFIL-IO-AREA (7:5)
      *                        FNR       14
               MOVE ANTJAN-IO              TO UTFIL-IO-AREA (15:5)
               MOVE ANTFEB-IO              TO UTFIL-IO-AREA (20:5)
               MOVE ANTMAR-IO              TO UTFIL-IO-AREA (25:5)
               MOVE ANTAPR-IO              TO UTFIL-IO-AREA (30:5)
               MOVE ANTMAI-IO              TO UTFIL-IO-AREA (35:5)
               MOVE ANTJUN-IO              TO UTFIL-IO-AREA (40:5)
               MOVE ANTJUL-IO              TO UTFIL-IO-AREA (45:5)
               MOVE ANTAUG-IO              TO UTFIL-IO-AREA (50:5)
               MOVE ANTSEP-IO              TO UTFIL-IO-AREA (55:5)
               MOVE ANTOKT-IO              TO UTFIL-IO-AREA (60:5)
               MOVE ANTNOV-IO              TO UTFIL-IO-AREA (65:5)
               MOVE ANTDES-IO              TO UTFIL-IO-AREA (70:5)
               MOVE STATUS-X               TO UTFIL-IO-AREA (75:1)
               WRITE UTFIL-IO-AREA
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
           SET KKSTAT-LEVEL-INIT           TO TRUE
           INITIALIZE KKSTAT-DATA-FIELDS
           SET KKSTAT-EOF-OFF              TO TRUE
           SET KKSTAT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO KKSTAT-MC
                                              KKSTAT-MP
           OPEN INPUT KKSTAT
           SET ORGNRF-LEVEL-INIT           TO TRUE
           INITIALIZE ORGNRF-DATA-FIELDS
           SET ORGNRF-EOF-OFF              TO TRUE
           SET ORGNRF-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO ORGNRF-MC
                                              ORGNRF-MP
           OPEN INPUT ORGNRF
           OPEN OUTPUT UTFIL.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KKSTAT
           CLOSE ORGNRF
           CLOSE UTFIL.
 
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
