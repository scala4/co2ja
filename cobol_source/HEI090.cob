       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEI090R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: HEI090    PROGRAMERT AV: ESPEN LARSEN               *
      *  DAGLIG REORGANISERING AV ORDLOGI FILEN.                      *
      *  ORDRE MED STATUS-KODE "A" (DAGENS RECORD) BLIR ENDRET TIL B  *
      *    (GÅRSDAGENS RECORD) OG DE SOM HAR B BLIR FJERNET.          *
      *  LESER INPUT KSDS-FILE OG DANNER SEQ.FILE.                    *
      *  PROGRAMMERT:  3.10.2003                                      *
      *  ENDRET.....: 13.05.2005                                      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: HEI090.rpg
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
           SELECT ORDLOGI
               ASSIGN TO ORDLOGI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDLOGI-STATUS
               RECORD KEY IS ORDLOGI-KEY1.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDLOGI
               RECORD CONTAINS 240.
       01  ORDLOGI-IO-AREA.
           05  ORDLOGI-IO-AREA-X.
               10  ORDLOGI-KEY1.
                   15  ORDLOGI-KEY1N       PICTURE S9(13).
               10  FILLER                  PICTURE X(227).
       FD UTFILE
               BLOCK CONTAINS 480
               RECORD CONTAINS 240.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(240).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDLOGI-STATUS              PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ORDLOGI-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDLOGI-EOF-OFF         VALUE '0'.
               88  ORDLOGI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDLOGI-READ-OFF        VALUE '0'.
               88  ORDLOGI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDLOGI-PROCESS-OFF     VALUE '0'.
               88  ORDLOGI-PROCESS         VALUE '1'.
           05  ORDLOGI-DATA-FIELDS.
               10  LRECA                   PICTURE X(240).
               10  STATUS-X                PICTURE X(1).
               10  LRECB                   PICTURE X(240).
               10  LRECC                   PICTURE X(240).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDLOGI-PROCESS
               SET ORDLOGI-PROCESS-OFF     TO TRUE
               SET ORDLOGI-READ            TO TRUE
           END-IF
 
           IF  ORDLOGI-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDLOGI-GET
               SET ORDLOGI-READ-OFF        TO TRUE
               IF  NOT ORDLOGI-EOF
                   SET ORDLOGI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDLOGI-PROCESS
               PERFORM ORDLOGI-IDSET
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
 
           IF  ORDLOGI-PROCESS
               PERFORM ORDLOGI-FLDSET
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
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  STATUS-X = 'A'
               SET I-50                    TO TRUE
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       ORDLOGI-GET SECTION.
       ORDLOGI-GET-P.
           IF  ORDLOGI-EOF-OFF
               READ ORDLOGI
               AT END
                   SET ORDLOGI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDLOGI-FLDSET SECTION.
       ORDLOGI-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDLOGI-IO-AREA (1:1) = 'A' )
               MOVE ORDLOGI-IO-AREA (1:240) TO LRECA (1:240)
               MOVE ORDLOGI-IO-AREA (14:1) TO STATUS-X (1:1)
           WHEN ( ORDLOGI-IO-AREA (1:1) = 'B' )
               MOVE ORDLOGI-IO-AREA (1:240) TO LRECB (1:240)
               MOVE ORDLOGI-IO-AREA (14:1) TO STATUS-X (1:1)
           WHEN ( ORDLOGI-IO-AREA (1:1) = 'C' )
               MOVE ORDLOGI-IO-AREA (1:240) TO LRECC (1:240)
               MOVE ORDLOGI-IO-AREA (14:1) TO STATUS-X (1:1)
           END-EVALUATE.
 
       ORDLOGI-IDSET SECTION.
       ORDLOGI-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDLOGI-IO-AREA (1:1) = 'A' )
               SET I-01                    TO TRUE
           WHEN ( ORDLOGI-IO-AREA (1:1) = 'B' )
               SET I-02                    TO TRUE
           WHEN ( ORDLOGI-IO-AREA (1:1) = 'C' )
               SET I-03                    TO TRUE
           WHEN  OTHER
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE LRECA                  TO UTFILE-IO-AREA (1:240)
               MOVE 'B'                    TO UTFILE-IO-AREA (14:1)
               WRITE UTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-50)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE LRECB                  TO UTFILE-IO-AREA (1:240)
               MOVE 'B'                    TO UTFILE-IO-AREA (14:1)
               WRITE UTFILE-IO-AREA
           END-IF
           IF  (I-03 AND I-50)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE LRECC                  TO UTFILE-IO-AREA (1:240)
               MOVE 'B'                    TO UTFILE-IO-AREA (14:1)
               WRITE UTFILE-IO-AREA
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
           INITIALIZE ORDLOGI-DATA-FIELDS
           SET ORDLOGI-EOF-OFF             TO TRUE
           SET ORDLOGI-PROCESS             TO TRUE
           OPEN INPUT ORDLOGI
           OPEN OUTPUT UTFILE.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDLOGI
           CLOSE UTFILE.
 
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
