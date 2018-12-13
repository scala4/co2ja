       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR100R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM VAR100                                              *
      *   OPPDATERING AV VAREARKIVET MED BESTILT ANTALL OG FORVENTET  *
      *   LEVERINGSUKE OG ÅR, SAMT ANTALL I RESTORDRE.                *
      *   INPUT FRA PROGRAM VAR090.                                   *
      *   12.10.94  ESPEN LARSEN                                      *
      *   31.05.95        HELLANOR SKAL IKKE OPPDATERES.              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR100.rpg
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
           SELECT UPDFILE
               ASSIGN TO UT-S-UPDFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UPDFILE-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD UPDFILE
               BLOCK CONTAINS 4000
               RECORD CONTAINS 40.
       01  UPDFILE-IO-AREA.
           05  UPDFILE-IO-AREA-X           PICTURE X(40).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  UPDFILE-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  UPDFILE-EOF-OFF         VALUE '0'.
               88  UPDFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UPDFILE-READ-OFF        VALUE '0'.
               88  UPDFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UPDFILE-PROCESS-OFF     VALUE '0'.
               88  UPDFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  UPDFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  UPDFILE-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREMAS-LEVEL-INIT      VALUE '1'.
           05  UPDFILE-LEVEL-01.
               10  UPDFILE-01-L2.
                   15  UPDFILE-01-L2-FIRMA PICTURE X(3).
               10  UPDFILE-01-L1.
                   15  UPDFILE-01-L1-EDBNR PICTURE X(7).
           05  UPDFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  IREST-IO.
                   15  IREST               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IBEST-IO.
                   15  IBEST               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  LEVA-ELGR-IO.
                   15  LEVA-ELGR           PICTURE S9(2).
               10  LEVUKE-IO.
                   15  LEVUKE              PICTURE S9(2).
               10  FAB                     PICTURE X(1).
           05  UPDFILE-MP                  PICTURE X(10).
           05  UPDFILE-MC                  PICTURE X(10).
           05  UPDFILE-M-01            REDEFINES UPDFILE-MC.
               10  UPDFILE-M-01-M2.
                   15  UPDFILE-M-01-M2-FIRMA-G.
                       20  UPDFILE-M-01-M2-FIRMA PICTURE X(3).
               10  UPDFILE-M-01-M1.
                   15  UPDFILE-M-01-M1-EDBNR-G.
                       20  UPDFILE-M-01-M1-EDBNR PICTURE X(7).
           05  VAREMAS-LEVEL-02.
               10  VAREMAS-02-L2.
                   15  VAREMAS-02-L2-FIRMA PICTURE X(3).
               10  VAREMAS-02-L1.
                   15  VAREMAS-02-L1-EDBNR PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
      **************************************************************
      * VAREARKIVET"S KONKURERENDE PRISER HAR EGET FIRMANR.        *
      *    DISSE SKAL IKKE OPPDATERES, DA DETTE ER ANDRE DATA.     *
      **************************************************************
               10  FILLER                  PICTURE X.
           05  VAREMAS-MP                  PICTURE X(10).
           05  VAREMAS-MC                  PICTURE X(10).
           05  VAREMAS-M-02            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-02-M2.
                   15  VAREMAS-M-02-M2-FIRMA-G.
                       20  VAREMAS-M-02-M2-FIRMA PICTURE X(3).
               10  VAREMAS-M-02-M1.
                   15  VAREMAS-M-02-M1-EDBNR-G.
                       20  VAREMAS-M-02-M1-EDBNR PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  VREST-IO.
                   15  VREST               PICTURE S9(5).
               10  VBEST-IO.
                   15  VBEST               PICTURE S9(7).
               10  TILA-ELGR-IO.
                   15  TILA-ELGR           PICTURE S9(2).
               10  TILUKE-IO.
                   15  TILUKE              PICTURE S9(2).
               10  VFAB                    PICTURE X(1).
               10  IREST-N-IO.
                   15  IREST-N             PICTURE S9(5).
               10  IBEST-N-IO.
                   15  IBEST-N             PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  UPDFILE-PROCESS
               SET UPDFILE-PROCESS-OFF     TO TRUE
               SET UPDFILE-READ            TO TRUE
           END-IF
 
           IF  UPDFILE-READ
               PERFORM UPDFILE-GET
               SET UPDFILE-READ-OFF        TO TRUE
               IF  NOT UPDFILE-EOF
                   PERFORM UPDFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   PERFORM VAREMAS-MATCH-SET
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
 
           IF  UPDFILE-PROCESS
               PERFORM UPDFILE-IDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
           END-IF
 
           IF  UPDFILE-PROCESS
               PERFORM UPDFILE-CHK-LEVEL
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-CHK-LEVEL
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
 
           IF  UPDFILE-PROCESS
               PERFORM UPDFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  UPDFILE-PROCESS
           OR  VAREMAS-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '018'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '019'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '075'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '099'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '989'
                   SET I-18                TO TRUE
               END-IF
      *****************************************************************
      * NULLSTILLING AV ALLE FELT VED NYTT EDB-NR.                    *
      *****************************************************************
           END-IF
           IF  (I-18)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               MOVE 0                      TO VREST
               MOVE 0                      TO VBEST
               MOVE 0                      TO TILA-ELGR
               MOVE 0                      TO TILUKE
               MOVE ' '                    TO VFAB
      *****************************************************************
      * FLYTTE DATA TIL VAREMASTER.                                   *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE IREST                  TO IREST-N
               MOVE IREST-N-IO             TO VREST-IO
               MOVE IBEST                  TO IBEST-N
               MOVE IBEST-N-IO             TO VBEST-IO
               MOVE LEVA-ELGR              TO TILA-ELGR-IO
               MOVE LEVUKE                 TO TILUKE-IO
               MOVE FAB                    TO VFAB
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       UPDFILE-GET SECTION.
       UPDFILE-GET-P.
           IF  UPDFILE-EOF-OFF
               READ UPDFILE
               AT END
                   SET UPDFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       UPDFILE-FLDSET SECTION.
       UPDFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE UPDFILE-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE UPDFILE-IO-AREA (4:7)  TO EDBNR (1:7)
               MOVE UPDFILE-IO-AREA (11:3) TO IREST-IO
               MOVE UPDFILE-IO-AREA (14:4) TO IBEST-IO
               MOVE UPDFILE-IO-AREA (18:2) TO LEVA-ELGR-IO
               INSPECT LEVA-ELGR-IO REPLACING ALL ' ' BY '0'
               MOVE UPDFILE-IO-AREA (20:2) TO LEVUKE-IO
               INSPECT LEVUKE-IO REPLACING ALL ' ' BY '0'
               MOVE UPDFILE-IO-AREA (22:1) TO FAB (1:1)
           END-EVALUATE.
 
       UPDFILE-IDSET SECTION.
       UPDFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       UPDFILE-CHK-LEVEL SECTION.
       UPDFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO UPDFILE-LEVEL-01
               MOVE UPDFILE-IO-AREA (1:3)  TO UPDFILE-01-L2-FIRMA
               MOVE UPDFILE-IO-AREA (4:7)  TO UPDFILE-01-L1-EDBNR
               IF  UPDFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  UPDFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  UPDFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  UPDFILE-01-L2         TO THE-PRIOR-L2
               MOVE  UPDFILE-01-L1         TO THE-PRIOR-L1
               SET UPDFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       UPDFILE-MATCH-SET SECTION.
       UPDFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE UPDFILE-IO-AREA (1:3)  TO UPDFILE-M-01-M2-FIRMA
               MOVE UPDFILE-IO-AREA (4:7)  TO UPDFILE-M-01-M1-EDBNR
           END-EVALUATE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
               END-READ
           END-IF.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-CHK-LEVEL SECTION.
       VAREMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREMAS-LEVEL-02
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-02-L2-FIRMA
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-02-L1-EDBNR
               IF  VAREMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREMAS-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREMAS-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREMAS-02-L2         TO THE-PRIOR-L2
               MOVE  VAREMAS-02-L1         TO THE-PRIOR-L1
               SET VAREMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-M-02-M2-FIRMA
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  UPDFILE-EOF
               MOVE HIGH-VALUES            TO UPDFILE-MC
                                              UPDFILE-MP
           END-IF
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  UPDFILE-MC < UPDFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  UPDFILE-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET UPDFILE-PROCESS     TO TRUE
                   MOVE UPDFILE-MC         TO UPDFILE-MP
                   IF  UPDFILE-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC < UPDFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = UPDFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  UPDFILE-MC = VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET UPDFILE-PROCESS     TO TRUE
                   MOVE UPDFILE-MC         TO UPDFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-18)
               MOVE VREST                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (114:3)
               MOVE VBEST-IO               TO VAREMAS-IO-AREA (129:7)
               MOVE TILA-ELGR-IO           TO VAREMAS-IO-AREA (136:2)
               MOVE TILUKE-IO              TO VAREMAS-IO-AREA (138:2)
               MOVE VFAB                   TO VAREMAS-IO-AREA (155:1)
               REWRITE VAREMAS-IO-AREA
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
           SET UPDFILE-LEVEL-INIT          TO TRUE
           INITIALIZE UPDFILE-DATA-FIELDS
           SET UPDFILE-EOF-OFF             TO TRUE
           SET UPDFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO UPDFILE-MC
                                              UPDFILE-MP
           OPEN INPUT UPDFILE
           SET VAREMAS-LEVEL-INIT          TO TRUE
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN I-O VAREMAS.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE UPDFILE
           CLOSE VAREMAS.
 
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
