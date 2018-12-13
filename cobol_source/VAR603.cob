       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR603R.
      **********************************************  Z-WIN-RPG2      *
      * PROGRAM VAR603.  LAGET AV ESPEN LARSEN 11.10.2005             *
      * OPPDATERE VAREGRMASTER FRA FIRMA TIL FIRMA SOM ER I PARAMETER.*
      * NYE RECORD BLIR LAGT INN. GAMLE BLIR ENDRET.                  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR603.rpg
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
           SELECT VGRKOPI
               ASSIGN TO UT-S-VGRKOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VGRKOPI-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD VGRKOPI
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  VGRKOPI-IO-AREA.
           05  VGRKOPI-IO-AREA-X           PICTURE X(80).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  VGRKOPI-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
 
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
               88  VGRKOPI-EOF-OFF         VALUE '0'.
               88  VGRKOPI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VGRKOPI-READ-OFF        VALUE '0'.
               88  VGRKOPI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VGRKOPI-PROCESS-OFF     VALUE '0'.
               88  VGRKOPI-PROCESS         VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PARAM-DATA-FIELDS.
               10  FRAFNR                  PICTURE X(3).
               10  TILFNR                  PICTURE X(3).
           05  VGRKOPI-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KEY-X                   PICTURE X(8).
               10  VGREC1                  PICTURE X(46).
           05  VAGRMAS-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  TEMPORARY-FIELDS.
               10  F11-IO.
                   15  F11                 PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
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
 
           IF  VGRKOPI-PROCESS
               SET VGRKOPI-PROCESS-OFF     TO TRUE
               SET VGRKOPI-READ            TO TRUE
           END-IF
 
           IF  VGRKOPI-READ
           AND RECORD-SELECTED-OFF
               PERFORM VGRKOPI-GET
               SET VGRKOPI-READ-OFF        TO TRUE
               IF  NOT VGRKOPI-EOF
                   SET VGRKOPI-PROCESS     TO TRUE
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
 
           IF  VGRKOPI-PROCESS
               PERFORM VGRKOPI-IDSET
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
 
           IF  VGRKOPI-PROCESS
               PERFORM VGRKOPI-FLDSET
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
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-LR                    TO TRUE
           SET NOT-I-11                    TO TRUE
           IF  FIRMA > FRAFNR
               SET I-LR                    TO TRUE
           END-IF
           IF  FIRMA = FRAFNR
               SET I-11                    TO TRUE
           END-IF
           IF  (NOT-I-11)
               GO TO SLUTT-T
           END-IF
           MOVE TILFNR                     TO KEY-X (1:3)
           MOVE KEY-X                      TO VAGRMAS-KEY1
           READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
           INVALID KEY
               SET I-20                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-20                TO TRUE
               PERFORM VAGRMAS-IDSET
           END-READ
           IF  (I-20)
               MOVE 0,00                   TO F11
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
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
            AND   PARAM-IO-AREA (2:1) = 'A'
            AND   PARAM-IO-AREA (3:1) = 'R' )
               MOVE PARAM-IO-AREA (35:3)   TO FRAFNR (1:3)
               MOVE PARAM-IO-AREA (50:3)   TO TILFNR (1:3)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A'
            AND   PARAM-IO-AREA (3:1) = 'R' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A'
            AND   PARAM-IO-AREA (3:1) = 'R' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       VGRKOPI-GET SECTION.
       VGRKOPI-GET-P.
           IF  VGRKOPI-EOF-OFF
               READ VGRKOPI
               AT END
                   SET VGRKOPI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VGRKOPI-FLDSET SECTION.
       VGRKOPI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VGRKOPI-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VGRKOPI-IO-AREA (3:8)  TO KEY-X (1:8)
               MOVE VGRKOPI-IO-AREA (1:46) TO VGREC1 (1:46)
           END-EVALUATE.
 
       VGRKOPI-IDSET SECTION.
       VGRKOPI-IDSET-P.
           SET I-04                        TO TRUE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-04 AND I-11 AND NOT-I-20)
               MOVE VGREC1                 TO VAGRMAS-IO-AREA (1:46)
               MOVE TILFNR                 TO VAGRMAS-IO-AREA (3:3)
               REWRITE VAGRMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAGRMAS'
               END-REWRITE
           END-IF
           IF  (I-04 AND I-11 AND I-20)
               MOVE VGREC1                 TO VAGRMAS-IO-AREA (1:46)
               MOVE TILFNR                 TO VAGRMAS-IO-AREA (3:3)
               MOVE F11                    TO XO-92P
               MOVE XO-92P-EF              TO VAGRMAS-IO-AREA (57:6)
               MOVE F11                    TO XO-92P
               MOVE XO-92P-EF              TO VAGRMAS-IO-AREA (63:6)
               WRITE VAGRMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad WRITE - file = VAGRMAS'
               END-WRITE
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
           INITIALIZE VGRKOPI-DATA-FIELDS
           SET VGRKOPI-EOF-OFF             TO TRUE
           SET VGRKOPI-PROCESS             TO TRUE
           OPEN INPUT VGRKOPI
           OPEN I-O VAGRMAS.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE VGRKOPI
           CLOSE VAGRMAS.
 
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
