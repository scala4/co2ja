       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK020R.
      ******************************************* :   Z-WIN-RPG2     **
      *    JCL    : XFAK10AU                                    ******
      *    PROGRAM: FAK020  PROGRAMMERER: ESPEN LARSEN DATO:03.10.00 *
      *    DANNE SORTERINGSKEY SOM SKAL BENYTTES I FAKTURA UTSKRIFT. *
      *    SORTERINGSKODE HENTES FRA KUNDEMX. (KAJ2)                 *
      *    BLANK/P ER SAMME REKKEFØLGE SOM INPUT. (ORDRE.MASTER)     *
      *    R       ER REGISTRERINGSREKKEFØLGE = POS.NR.              *
      *    A       ER ALFA/ARTIKKELNUMMER.                           *
      ****************************************************************
      * 21.10.05 : HENTER BESØKSFREK FRA KUNDEMASTER. BRUKES FOR Å   *
      *            FJERNE FRAKT PÅ ORDRE OVER 5000 FOR FIRMA 855.    *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK020.rpg
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
           SELECT ORDFAKT
               ASSIGN TO UT-S-ORDFAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDFAKT-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDFAKT
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDFAKT-IO-AREA.
           05  ORDFAKT-IO-AREA-X           PICTURE X(164).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD OUTF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDFAKT-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFAKT-EOF-OFF         VALUE '0'.
               88  ORDFAKT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFAKT-READ-OFF        VALUE '0'.
               88  ORDFAKT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFAKT-PROCESS-OFF     VALUE '0'.
               88  ORDFAKT-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDFAKT-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDFAKT-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  ORDFAKT-LEVEL-01.
               10  ORDFAKT-01-L2.
                   15  ORDFAKT-01-L2-FIRMNR PICTURE X(3).
               10  ORDFAKT-01-L1.
                   15  ORDFAKT-01-L1-ONR   PICTURE X(6).
           05  ORDFAKT-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  ONR                     PICTURE X(6).
               10  KNR1                    PICTURE X(6).
               10  RECH1                   PICTURE X(164).
               10  RECH2                   PICTURE X(164).
               10  RECH3                   PICTURE X(164).
               10  RECV1                   PICTURE X(164).
      *                                      11  16 LAGLOC
               10  POSNR                   PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ARTN8F                  PICTURE X(8).
           05  KUNDEMA-DATA-FIELDS.
               10  BSFREK                  PICTURE X(1).
           05  KUNDEMX-DATA-FIELDS.
               10  FAKSEQ                  PICTURE X(1).
      *****************************************************************
      * RUTINE FOR Å FINNE SORTERINGSREKKEFØLGE.                      *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  FNRKNR                  PICTURE X(9).
               10  KXKEY                   PICTURE X(10).
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDFAKT-PROCESS
               SET ORDFAKT-PROCESS-OFF     TO TRUE
               SET ORDFAKT-READ            TO TRUE
           END-IF
 
           IF  ORDFAKT-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDFAKT-GET
               SET ORDFAKT-READ-OFF        TO TRUE
               IF  NOT ORDFAKT-EOF
                   PERFORM ORDFAKT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORDFAKT-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDFAKT-PROCESS
               PERFORM ORDFAKT-IDSET
           END-IF
 
           IF  ORDFAKT-PROCESS
               PERFORM ORDFAKT-CHK-LEVEL
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
 
           IF  ORDFAKT-PROCESS
               PERFORM ORDFAKT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDFAKT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
               SUBTRACT SEQNR              FROM SEQNR
               MOVE FIRMNR                 TO FNRKNR (1:3)
               MOVE KNR1                   TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KXKEY (1:9)
               MOVE '1'                    TO KXKEY (10:1)
               MOVE KXKEY                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-16)
               SET I-11                    TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-16)
               SET NOT-I-11                TO TRUE
               IF  FAKSEQ = 'P'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  FAKSEQ = 'R'
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-13                TO TRUE
               IF  FAKSEQ = 'A'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-16 AND NOT-I-11)
               AND (NOT-I-12 AND NOT-I-13)
               SET I-11                    TO TRUE
           END-IF
           IF  (I-04)
               ADD 1                       TO SEQNR
      *****************************************************************
      * RUTINE FOR Å FINNE FRAKTFRIKODE FOR FIRMA 855 KCL.            *
      * TESTER OGSÅ PÅ OM DET ER FRAKTRECORD. DENNE MÅ LIGGE SIST OG  *
      *        TILDELES DERFOR ALFAKODE 999 I SORTERINGSDELEN.        *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-85                TO TRUE
               SET NOT-I-85                TO TRUE
               IF  FIRMNR = '855'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-85)
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-15                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-15            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-04)
               SET NOT-I-86                TO TRUE
               IF  ARTN8F = 'FRAKT   '
                   SET I-86                TO TRUE
               END-IF
      *****************************************************************
      *  ORDRE HEADING RECORD 1.                                      *
           END-IF
           .
 
       ORDFAKT-GET SECTION.
       ORDFAKT-GET-P.
           IF  ORDFAKT-EOF-OFF
               READ ORDFAKT
               AT END
                   SET ORDFAKT-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDFAKT-FLDSET SECTION.
       ORDFAKT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
               MOVE ORDFAKT-IO-AREA (2:3)  TO FIRMNR (1:3)
               MOVE ORDFAKT-IO-AREA (5:6)  TO ONR (1:6)
               MOVE ORDFAKT-IO-AREA (21:6) TO KNR1 (1:6)
               MOVE ORDFAKT-IO-AREA (1:164) TO RECH1 (1:164)
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
               MOVE ORDFAKT-IO-AREA (1:164) TO RECH2 (1:164)
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
               MOVE ORDFAKT-IO-AREA (1:164) TO RECH3 (1:164)
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDFAKT-IO-AREA (1:164) TO RECV1 (1:164)
               MOVE ORDFAKT-IO-AREA (17:3) TO POSNR (1:3)
               MOVE ORDFAKT-IO-AREA (34:3) TO ALFA (1:3)
               MOVE ORDFAKT-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDFAKT-IO-AREA (37:8) TO ARTN8F (1:8)
           END-EVALUATE.
 
       ORDFAKT-IDCHK SECTION.
       ORDFAKT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
             OR ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
             OR ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
             OR ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDFAKT-IDSET SECTION.
       ORDFAKT-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDFAKT-CHK-LEVEL SECTION.
       ORDFAKT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDFAKT-LEVEL-01
               MOVE ORDFAKT-IO-AREA (2:3)  TO ORDFAKT-01-L2-FIRMNR
               MOVE ORDFAKT-IO-AREA (5:6)  TO ORDFAKT-01-L1-ONR
               IF  ORDFAKT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDFAKT-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDFAKT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDFAKT-01-L2         TO THE-PRIOR-L2
               MOVE  ORDFAKT-01-L1         TO THE-PRIOR-L1
               SET ORDFAKT-LEVEL-INIT      TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
               CONTINUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
               CONTINUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (166:1) TO BSFREK (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (42:1) TO FAKSEQ (1:1)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-06                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE RECH1                  TO OUTF-IO-AREA (1:164)
               IF  (I-85 AND NOT-I-15)
                   MOVE BSFREK             TO OUTF-IO-AREA (162:1)
               END-IF
               MOVE 'H1'                   TO OUTF-IO-AREA (171:2)
      *****************************************************************
      *  ORDRE HEADING RECORD 2.                                      *
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE RECH2                  TO OUTF-IO-AREA (1:164)
               MOVE 'H2'                   TO OUTF-IO-AREA (171:2)
      *****************************************************************
      *  ORDRE HEADING RECORD 3.                                      *
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE RECH3                  TO OUTF-IO-AREA (1:164)
               MOVE 'H3'                   TO OUTF-IO-AREA (171:2)
      *****************************************************************
      *  ORDRE VARELINJE/TEKSTLINJE.                                  *
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE RECV1                  TO OUTF-IO-AREA (1:164)
               MOVE 'V1'                   TO OUTF-IO-AREA (171:2)
               IF  (I-11)
                   MOVE 'P'                TO OUTF-IO-AREA (173:1)
               END-IF
               IF  (I-12)
                   MOVE 'R'                TO OUTF-IO-AREA (173:1)
               END-IF
               IF  (I-13)
                   MOVE 'A'                TO OUTF-IO-AREA (173:1)
               END-IF
               IF  (I-11)
                   MOVE SEQNR-IO           TO OUTF-IO-AREA (174:4)
               END-IF
               IF  (I-12)
                   MOVE POSNR              TO OUTF-IO-AREA (174:3)
               END-IF
               IF  (I-12)
                   MOVE SEQNR-IO           TO OUTF-IO-AREA (177:4)
               END-IF
               IF  (I-13 AND NOT-I-86)
                   MOVE ALFA               TO OUTF-IO-AREA (174:3)
               END-IF
               IF  (I-13 AND I-86)
                   MOVE '999'              TO OUTF-IO-AREA (174:3)
               END-IF
               IF  (I-13)
                   MOVE ARTNR              TO OUTF-IO-AREA (177:20)
               END-IF
               IF  (I-13)
                   MOVE SEQNR-IO           TO OUTF-IO-AREA (197:4)
               END-IF
               WRITE OUTF-IO-AREA
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
           SET ORDFAKT-LEVEL-INIT          TO TRUE
           INITIALIZE ORDFAKT-DATA-FIELDS
           SET ORDFAKT-EOF-OFF             TO TRUE
           SET ORDFAKT-PROCESS             TO TRUE
           OPEN INPUT ORDFAKT
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           OPEN OUTPUT OUTF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDFAKT
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE OUTF.
 
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
