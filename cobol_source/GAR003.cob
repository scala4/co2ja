       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAR003R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET LESER SEQ. GARANTI-MASTER OG DANNER    *
      *  KEY-FILE PÅ NAVN OG FORHANDLERNR.                 *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: GAR003.rpg
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
           SELECT GBANKS
               ASSIGN TO UT-S-GBANKS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GBANKS-STATUS.
           SELECT ALFFIL
               ASSIGN TO UT-S-ALFFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ALFFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GBANKS
               BLOCK CONTAINS 4000
               RECORD CONTAINS 200.
       01  GBANKS-IO-AREA.
           05  GBANKS-IO-AREA-X            PICTURE X(200).
       FD ALFFIL
               BLOCK CONTAINS 4080
               RECORD CONTAINS 68.
       01  ALFFIL-IO-AREA.
           05  ALFFIL-IO-AREA-X            PICTURE X(68).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  GBANKS-STATUS               PICTURE 99 VALUE 0.
           10  ALFFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GBANKS-EOF-OFF          VALUE '0'.
               88  GBANKS-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GBANKS-READ-OFF         VALUE '0'.
               88  GBANKS-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GBANKS-PROCESS-OFF      VALUE '0'.
               88  GBANKS-PROCESS          VALUE '1'.
           05  GBANKS-DATA-FIELDS.
               10  KEY-X                   PICTURE X(27).
               10  FNR                     PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  FHND                    PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  SEQ2-IO.
                   15  SEQ2                PICTURE S9(4).
               10  UTHND                   PICTURE X(30).
               10  FFHND                   PICTURE X(6).
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  GBANKS-PROCESS
               SET GBANKS-PROCESS-OFF      TO TRUE
               SET GBANKS-READ             TO TRUE
           END-IF
 
           IF  GBANKS-READ
           AND RECORD-SELECTED-OFF
               PERFORM GBANKS-GET
               SET GBANKS-READ-OFF         TO TRUE
               IF  NOT GBANKS-EOF
                   SET GBANKS-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  GBANKS-PROCESS
               PERFORM GBANKS-IDSET
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
 
           IF  GBANKS-PROCESS
               PERFORM GBANKS-FLDSET
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
           SET NOT-I-11                    TO TRUE
           IF  FHND = FFHND
               SET I-11                    TO TRUE
           END-IF
      *
           IF  (NOT-I-11)
               MOVE 0                      TO SEQ2
           END-IF
           ADD 1                           TO SEQ2
           MOVE FHND                       TO UTHND (1:6)
           MOVE FHND                       TO FFHND.
 
       GBANKS-GET SECTION.
       GBANKS-GET-P.
           IF  GBANKS-EOF-OFF
               READ GBANKS
               AT END
                   SET GBANKS-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GBANKS-FLDSET SECTION.
       GBANKS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GBANKS-IO-AREA (1:27)  TO KEY-X (1:27)
               MOVE GBANKS-IO-AREA (1:3)   TO FNR (1:3)
               MOVE GBANKS-IO-AREA (4:3)   TO ALFA (1:3)
               MOVE GBANKS-IO-AREA (171:6) TO FHND (1:6)
           END-EVALUATE.
 
       GBANKS-IDSET SECTION.
       GBANKS-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO ALFFIL-IO-AREA
               INITIALIZE ALFFIL-IO-AREA
               MOVE FNR                    TO ALFFIL-IO-AREA (1:3)
               MOVE ALFA                   TO ALFFIL-IO-AREA (4:3)
               MOVE '3'                    TO ALFFIL-IO-AREA (7:1)
               MOVE UTHND                  TO ALFFIL-IO-AREA (8:30)
               MOVE SEQ2-IO                TO ALFFIL-IO-AREA (38:4)
               MOVE KEY-X                  TO ALFFIL-IO-AREA (42:27)
               WRITE ALFFIL-IO-AREA
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
           INITIALIZE GBANKS-DATA-FIELDS
           SET GBANKS-EOF-OFF              TO TRUE
           SET GBANKS-PROCESS              TO TRUE
           OPEN INPUT GBANKS
           OPEN OUTPUT ALFFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GBANKS
           CLOSE ALFFIL.
 
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
