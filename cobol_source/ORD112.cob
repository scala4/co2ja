       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD112R.
      **********************************************  Z-WIN-RPG2   ****
      *****************************************************************
      *  PROGRAM ORD112. LAGET AV ESPEN LARSEN 20.08.1998             *
      *                                                               *
      *  PROGRAMMET HENTER VAREBEHOLDNING FRA VARELAGER FOR FIRMA     *
      *  918. OG KORRIGERER FERDIGMELDINGSRECORDEN FRA ORD110.        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD112.rpg
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
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 8250
               RECORD CONTAINS 150.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(150).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
 
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
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INFILE-LEVEL-01.
               10  INFILE-01-L1.
                   15  INFILE-01-L1-FIRMA  PICTURE X(3).
           05  INFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  VAREMAS-DATA-FIELDS.
               10  VANTIN-IO.
                   15  VANTIN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VANTUT-IO.
                   15  VANTUT              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  UTGA-ELGR               PICTURE X(1).
               10  BLK13-IO.
                   15  BLK13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BLK93-IO.
                   15  BLK93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BLK15-IO.
                   15  BLK15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BLK17-IO.
                   15  BLK17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BLK92-IO.
                   15  BLK92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BLK18-IO.
                   15  BLK18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  NEDBNR                  PICTURE X(10).
               10  F70-IO.
                   15  F70                 PICTURE S9(7).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  REDBNR                  PICTURE X(7).
               10  ILAGER-IO.
                   15  ILAGER              PICTURE S9(7)V9(2).
               10  TOTA-IO.
                   15  TOTA                PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
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
           SET NOT-I-07                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
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
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               IF  FIRMA = '918'
                   SET I-50                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           IF  (I-05)
               GO TO SLUTT-T
           END-IF
           IF  (I-06)
               SET NOT-I-60                TO TRUE
           END-IF
           IF  (I-06 AND NOT-I-50)
               GO TO SLUTT-T
           END-IF
           IF  (I-07)
               GO TO SLUTT-T
      ******************************************************
      *  RUTINE FOR OPPSLAG MOT VAREMASTER.                *
      *     IKKE SKAFFEVARER.                              *
      ******************************************************
           END-IF
           MOVE FIRMA                      TO NEDBNR (1:3)
           MOVE EDBNR                      TO EDBNR-N
           MOVE EDBNR-N-IO                 TO F70-IO
           MOVE F70                        TO REDBNR
      ** MLLzo
           MOVE '0'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE REDBNR (7:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO REDBNR (7:1)
           SET NOT-I-09                    TO TRUE
           IF  REDBNR > '9000000'
               SET I-09                    TO TRUE
           END-IF
           IF  (I-09)
               GO TO SLUTT-T
           END-IF
           MOVE REDBNR                     TO NEDBNR (4:7)
           MOVE NEDBNR                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-10                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-10                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (NOT-I-10)
               PERFORM HBHRUT-S
           END-IF
           IF  (NOT-I-10)
               SET I-60                    TO TRUE
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * RUTINE FOR Å BEREGNE BEHOLDNING PÅ HOVEDLAGER.                *
      *****************************************************************
           CONTINUE.
 
       HBHRUT-S SECTION.
       HBHRUT-S-P.
           SUBTRACT VANTUT FROM VANTIN GIVING ILAGER
           SUBTRACT TOTA                   FROM TOTA
           ADD BLK13                       TO TOTA
           ADD BLK15                       TO TOTA
           ADD BLK17                       TO TOTA
           ADD BLK92                       TO TOTA
           ADD BLK18                       TO TOTA
           ADD BLK93                       TO TOTA
           SUBTRACT TOTA                   FROM ILAGER
           SET NOT-I-15                    TO TRUE
           IF  ILAGER > 0
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-15)
               SUBTRACT ILAGER             FROM ILAGER
           END-IF.
      *****************************************************************
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
               MOVE INFILE-IO-AREA (133:3) TO FIRMA (1:3)
           WHEN ( INFILE-IO-AREA (150:1) = 'X'
            AND   INFILE-IO-AREA (149:1) = 'V' )
               MOVE INFILE-IO-AREA (111:4) TO EDBNR-IO
           END-EVALUATE.
 
       INFILE-IDCHK SECTION.
       INFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
             OR ( INFILE-IO-AREA (150:1) = 'B' )
             OR ( INFILE-IO-AREA (150:1) = 'C' )
             OR ( INFILE-IO-AREA (150:1) = 'D' )
             OR ( INFILE-IO-AREA (150:1) = 'E' )
             OR ( INFILE-IO-AREA (150:1) = 'X'
            AND   INFILE-IO-AREA (149:1) = 'V' )
             OR ( INFILE-IO-AREA (150:1) = 'X'
            AND   INFILE-IO-AREA (149:1) NOT = 'V' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
               SET I-01                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'B' )
               SET I-02                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'C' )
               SET I-03                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'D' )
               SET I-04                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'E' )
               SET I-05                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X'
            AND   INFILE-IO-AREA (149:1) = 'V' )
               SET I-06                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X'
            AND   INFILE-IO-AREA (149:1) NOT = 'V' )
               SET I-07                    TO TRUE
           END-EVALUATE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (133:3) TO INFILE-01-L1-FIRMA
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'B' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'C' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'D' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'E' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X'
            AND   INFILE-IO-AREA (149:1) = 'V' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X'
            AND   INFILE-IO-AREA (149:1) NOT = 'V' )
               CONTINUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (97:5) TO VANTIN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO VANTUT-IO
               MOVE VAREMAS-IO-AREA (127:1) TO UTGA-ELGR (1:1)
               MOVE VAREMAS-IO-AREA (179:3) TO BLK13-IO
               MOVE VAREMAS-IO-AREA (182:3) TO BLK93-IO
               MOVE VAREMAS-IO-AREA (185:3) TO BLK15-IO
               MOVE VAREMAS-IO-AREA (188:3) TO BLK17-IO
               MOVE VAREMAS-IO-AREA (191:3) TO BLK92-IO
               MOVE VAREMAS-IO-AREA (194:3) TO BLK18-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-08                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-50)
               MOVE ' BEHOLDNING'          TO INFILE-IO-AREA (100:11)
               REWRITE INFILE-IO-AREA
           END-IF
           IF  (I-06)
               IF  (I-50)
                   MOVE '                ' TO INFILE-IO-AREA (95:16)
               END-IF
               IF  (I-50 AND I-60)
                   MOVE ILAGER             TO XO-72YY9
                   MOVE XO-72YY9           TO INFILE-IO-AREA (99:12)
               END-IF
               IF  (I-50 AND NOT-I-60)
                   MOVE '  IKKE LAGERVARE' TO INFILE-IO-AREA (95:16)
               END-IF
               MOVE '    '                 TO INFILE-IO-AREA (111:4)
               REWRITE INFILE-IO-AREA
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
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN I-O INFILE
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
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
