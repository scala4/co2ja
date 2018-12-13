       IDENTIFICATION DIVISION.
       PROGRAM-ID. RBS109R.
      **********************************************  Z-WIN-RPG2   ****
      *****************************************************************
      *       P R O G R A M                           R B S 1 0 9     *
      *       ----------------------------------------------------    *
      *  1. KOPIERER DATA I POS 1 TIL 29 OM DETTE ER BLANKT PÅ REC X. *
      *                                                               *
      *  4. STND. INNPUT ER:                                          *
      *      -  PRINTLINE  POS.   1-132.                              *
      *      -  FIRMANR    POS. 133-135.                              *
      *      -  SPACEKODE  POS. 136-136.                              *
      *                    F = SPACE TIL NY SIDE FØRST.               *
      *                    E = SPACE TIL NY SIDE ETTER.               *
      *                    G = HOPP TIL KANAL 02 FØRST.               *
      *                    H = HOPP TIL KANAL 02 ETTER.               *
      *                    I = HOPP TIL KANAL 03 FØRST.               *
      *                    J = HOPP TIL KANAL 03 ETTER.               *
      *                    K = HOPP TIL KANAL 04 FØRST.               *
      *                    L = HOPP TIL KANAL 04 ETTER.               *
      *      -  SKIPKODE   POS. 137-137.                              *
      *                    0 = SKIP 0 LINER.                          *
      *                    1 = SKIP 1 LINE  FØRST.                    *
      *                    2 = SKIP 1 LINER FØRST.                    *
      *                    3 = SKIP 1 LINER FØRST.                    *
      *                    6 = SKIP 1 LINE  ETTER.                    *
      *                    7 = SKIP 2 LINER ETTER.                    *
      *                    8 = SKIP 3 LINER ETTER.                    *
      *      -  OPPGAVENR  POS. 138-142.                              *
      *      -  PROGRAMNR  POS. 143-148.                              *
      *      -  LINE TYPE  POS. 150-150.                              *
      *                    A = HEADINGLINJE 1.       DENNE SAVES.     *
      *                    B = HEADINGLINJE 2.       DENNE SAVES.     *
      *                    C = HEADINGLINJE 3.       DENNE SAVES.     *
      *                    D = HEADINGLINJE 4.       DENNE SAVES.     *
      *                    E = HEADINGLINJE 5.       DENNE SAVES.     *
      *                    X = DETALJ OG TOTALLINJER.                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RBS109.rpg
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
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(150).
       FD UTFILE
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(150).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  INFILE-DATA-FIELDS.
               10  LINJE                   PICTURE X(150).
               10  LINJEX                  PICTURE X(150).
               10  F29                     PICTURE X(29).
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           05  TEMPORARY-FIELDS.
               10  XF29                    PICTURE X(29).
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
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   PERFORM INFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET INFILE-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDOFF
               PERFORM INFILE-FLDSET
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
           IF  (I-06 AND NOT-I-09)
               MOVE F29                    TO XF29
      *********************************************************************
           END-IF
           .
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDOFF SECTION.
       INFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X' )
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) NOT = 'X' )
               MOVE INFILE-IO-AREA (1:150) TO LINJE (1:150)
           WHEN ( INFILE-IO-AREA (150:1) = 'X' )
               MOVE INFILE-IO-AREA (1:150) TO LINJEX (1:150)
               MOVE INFILE-IO-AREA (1:29)  TO F29 (1:29)
               IF  F29 = SPACES
                   SET I-09                TO TRUE
               END-IF
           END-EVALUATE.
 
       INFILE-IDCHK SECTION.
       INFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) NOT = 'X' )
             OR ( INFILE-IO-AREA (150:1) = 'X' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) NOT = 'X' )
               SET I-01                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE LINJE                  TO UTFILE-IO-AREA (1:150)
               WRITE UTFILE-IO-AREA
           END-IF
           IF  (I-06)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE LINJEX                 TO UTFILE-IO-AREA (1:150)
               IF  (I-09)
                   MOVE XF29               TO UTFILE-IO-AREA (1:29)
               END-IF
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
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           OPEN OUTPUT UTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
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
