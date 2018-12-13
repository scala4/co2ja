       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAR043R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMNAVN: PAR043, INNLEGGING AV DIV PARAM. I  AUTOPAR.     *
      * LAGET AV   : ESPEN LARSEN  (KOPI AV PAR042)                   *
      * LAGET DATO : 07.02.03                                         *
      * ENDRINGER  : 03.01.04                                         *
      * INPUT      : SYSIPT                                           *
      * GJØR       : LEGGER INN ÅRSTALL FOR BRUK I STAT.SS02-SS05     *
      * GJØR       : REC P01 KORRIGERER ÅRSTALL FOR ÅRETS SS02.       *
      * GJØR       : REC P02 KORRIGERER ÅRSTALL FOR HISTORISK SS04.   *
      * GJØR       : REC P03 KORRIGERER ÅRSTALL FOR ÅRETS SS03.       *
      * GJØR       : REC P04 KORRIGERER ÅRSTALL FOR HISTORISK SS05.   *
      * GJØR       : REC P05 KORRIGERER OM MAILSYSTEMER ER KLART.     *
      * GIR        : OPPDATERING AV PARAMETERFILE (AUTOPAR).          *
      * VED CANCEL : RETT INPUT OG KJØR OM.                           *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PAR043.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AUTOPAR-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X           PICTURE X(1000).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOPAR-EOF-OFF         VALUE '0'.
               88  AUTOPAR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOPAR-READ-OFF        VALUE '0'.
               88  AUTOPAR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOPAR-PROCESS-OFF     VALUE '0'.
               88  AUTOPAR-PROCESS         VALUE '1'.
           05  PARAM-DATA-FIELDS.
               10  OIAAR1                  PICTURE X(4).
               10  OIAAR2                  PICTURE X(4).
               10  OHAAR1                  PICTURE X(4).
               10  OHAAR2                  PICTURE X(4).
               10  FIAAR1                  PICTURE X(4).
               10  FIAAR2                  PICTURE X(4).
               10  FHAAR1                  PICTURE X(4).
               10  FHAAR2                  PICTURE X(4).
               10  MAILJN                  PICTURE X(1).
           05  AUTOPAR-DATA-FIELDS.
               10  FILLER                  PICTURE X.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  AUTOPAR-PROCESS
               SET AUTOPAR-PROCESS-OFF     TO TRUE
               SET AUTOPAR-READ            TO TRUE
           END-IF
 
           IF  AUTOPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM AUTOPAR-GET
               SET AUTOPAR-READ-OFF        TO TRUE
               IF  NOT AUTOPAR-EOF
                   SET AUTOPAR-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  AUTOPAR-PROCESS
               PERFORM AUTOPAR-IDSET
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
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
           IF  (I-02)
               SET I-11                    TO TRUE
           END-IF
           IF  (I-03)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-04)
               SET I-13                    TO TRUE
           END-IF
           IF  (I-05)
               SET I-14                    TO TRUE
           END-IF
           IF  (I-06)
               SET I-15                    TO TRUE
           END-IF.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '1' )
               MOVE PARAM-IO-AREA (41:4)   TO OIAAR1 (1:4)
               MOVE PARAM-IO-AREA (55:4)   TO OIAAR2 (1:4)
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '2' )
               MOVE PARAM-IO-AREA (41:4)   TO OHAAR1 (1:4)
               MOVE PARAM-IO-AREA (55:4)   TO OHAAR2 (1:4)
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '3' )
               MOVE PARAM-IO-AREA (41:4)   TO FIAAR1 (1:4)
               MOVE PARAM-IO-AREA (55:4)   TO FIAAR2 (1:4)
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '4' )
               MOVE PARAM-IO-AREA (41:4)   TO FHAAR1 (1:4)
               MOVE PARAM-IO-AREA (55:4)   TO FHAAR2 (1:4)
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '5' )
               MOVE PARAM-IO-AREA (41:1)   TO MAILJN (1:1)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '1' )
             OR ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '2' )
             OR ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '3' )
             OR ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '4' )
             OR ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '5' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '1' )
               SET I-02                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '2' )
               SET I-03                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '3' )
               SET I-04                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '4' )
               SET I-05                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '5' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       AUTOPAR-GET SECTION.
       AUTOPAR-GET-P.
           IF  AUTOPAR-EOF-OFF
               READ AUTOPAR
               AT END
                   SET AUTOPAR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( AUTOPAR-IO-AREA (1:1) = 'A'
            AND   AUTOPAR-IO-AREA (2:1) = '0'
            AND   AUTOPAR-IO-AREA (3:1) = '1' )
               SET I-01                    TO TRUE
           WHEN  OTHER
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               IF  (I-11)
                   MOVE OIAAR1             TO AUTOPAR-IO-AREA (245:4)
               END-IF
               IF  (I-11)
                   MOVE OIAAR2             TO AUTOPAR-IO-AREA (249:4)
               END-IF
               IF  (I-12)
                   MOVE OHAAR1             TO AUTOPAR-IO-AREA (253:4)
               END-IF
               IF  (I-12)
                   MOVE OHAAR2             TO AUTOPAR-IO-AREA (257:4)
               END-IF
               IF  (I-13)
                   MOVE FIAAR1             TO AUTOPAR-IO-AREA (261:4)
               END-IF
               IF  (I-13)
                   MOVE FIAAR2             TO AUTOPAR-IO-AREA (265:4)
               END-IF
               IF  (I-14)
                   MOVE FHAAR1             TO AUTOPAR-IO-AREA (269:4)
               END-IF
               IF  (I-14)
                   MOVE FHAAR2             TO AUTOPAR-IO-AREA (273:4)
               END-IF
               IF  (I-15)
                   MOVE MAILJN             TO AUTOPAR-IO-AREA (277:1)
               END-IF
               REWRITE AUTOPAR-IO-AREA
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET AUTOPAR-EOF-OFF             TO TRUE
           SET AUTOPAR-PROCESS             TO TRUE
           OPEN I-O AUTOPAR.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE AUTOPAR.
 
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
