       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG415R.
      **********************************************  Z-WIN-RPG2
      *   PROGRAM  R E G 4 1 5    A U T O - P O S T E R I N G.                 *
      *   LESER DAGENS BILAGSNR-FILE FRA PROG. REG410.                         *
      *   HENTER SISTE BENYTTEDE BILAGSNR FRA FIRMAFILE, LEGGER TIL 1.         *
      *   DET BILAGSNUMMER BLIR LAGT INN I BILAGSNR-FILE.                      *
      *   SISTE BENYTTEDE BILAGSNR BLIR LAGT TILBAKE I FIRMAFILE.              *
      *   E 18.08.00 UTVIDET BILAGFI FRA 33 TIL 120                            *
      *   E 21.02.07 UTVIDET BILAGFI FRA 120 TIL 240                           *
      *   E 09.06.10 UTVIDET BILAGFI FRA 240 TIL 396                           *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG415.rpg
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
           SELECT BILAGFI
               ASSIGN TO UT-S-BILAGFI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BILAGFI-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD BILAGFI
               BLOCK CONTAINS 396
               RECORD CONTAINS 396.
       01  BILAGFI-IO-AREA.
           05  BILAGFI-IO-AREA-X           PICTURE X(396).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       WORKING-STORAGE SECTION.
       77  ARN-MAX   VALUE 9               PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARN-TABLE.
               10  ARN-ENTRY
                                           OCCURS 9 TIMES
                                           INDEXED BY ARN-I
                                                      ARN-S.
                   15  ARN                 PICTURE S9(7).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BILAGFI-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BILAGFI-EOF-OFF         VALUE '0'.
               88  BILAGFI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BILAGFI-READ-OFF        VALUE '0'.
               88  BILAGFI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BILAGFI-PROCESS-OFF     VALUE '0'.
               88  BILAGFI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BILAGFI-LEVEL-INIT-OFF  VALUE '0'.
               88  BILAGFI-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  BILAGFI-LEVEL-01.
               10  BILAGFI-01-L3.
                   15  BILAGFI-01-L3-FIRMA PICTURE X(3).
               10  BILAGFI-01-L2.
                   15  BILAGFI-01-L2-BILART PICTURE S9(1).
               10  BILAGFI-01-L1.
                   15  BILAGFI-01-L1-FRA   PICTURE X(10).
           05  BILAGFI-DATA-FIELDS.
               10  FRA                     PICTURE X(10).
               10  FIRMA                   PICTURE X(3).
               10  BILART-IO.
                   15  BILART              PICTURE S9(1).
           05  FIRMAF-DATA-FIELDS.
               10  FILLER                  PICTURE X.
               10  XI-ARN-GRP.
                   15  XI-ARN              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(10).
           05  TEMPORARY-FIELDS.
               10  X-IO.
                   15  X                   PICTURE S9(1).
               10  BILNR-IO.
                   15  BILNR               PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BILAGFI-PROCESS
               SET BILAGFI-PROCESS-OFF     TO TRUE
               SET BILAGFI-READ            TO TRUE
           END-IF
 
           IF  BILAGFI-READ
           AND RECORD-SELECTED-OFF
               PERFORM BILAGFI-GET
               SET BILAGFI-READ-OFF        TO TRUE
               IF  NOT BILAGFI-EOF
                   SET BILAGFI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BILAGFI-PROCESS
               PERFORM BILAGFI-IDSET
           END-IF
 
           IF  BILAGFI-PROCESS
               PERFORM BILAGFI-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  BILAGFI-PROCESS
               PERFORM BILAGFI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BILAGFI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L3)
               SET NOT-I-50                TO TRUE
      ****************************************************************
      *  HENT SISTE-BILAGSNR OG ØK MED 1 PR. BILAGSNR. PR. BILAGSART.*
      ****************************************************************
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-21)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-L2)
               ADD 1 TO BILART         GIVING X
           END-IF
           IF  (I-L1)
               ADD 1                       TO ARN (X)
               ADD ARN (X) TO ZERO     GIVING BILNR
      ******************************************************
           END-IF
           .
 
       BILAGFI-GET SECTION.
       BILAGFI-GET-P.
           IF  BILAGFI-EOF-OFF
               READ BILAGFI
               AT END
                   SET BILAGFI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BILAGFI-FLDSET SECTION.
       BILAGFI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BILAGFI-IO-AREA (1:10) TO FRA (1:10)
               MOVE BILAGFI-IO-AREA (11:3) TO FIRMA (1:3)
               MOVE BILAGFI-IO-AREA (14:1) TO BILART-IO
               INSPECT BILART-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       BILAGFI-IDSET SECTION.
       BILAGFI-IDSET-P.
           SET I-01                        TO TRUE.
 
       BILAGFI-CHK-LEVEL SECTION.
       BILAGFI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BILAGFI-LEVEL-01
               MOVE BILAGFI-IO-AREA (11:3) TO BILAGFI-01-L3-FIRMA
               MOVE BILAGFI-IO-AREA (14:1) TO BILAGFI-01-L2-BILART
               MOVE BILAGFI-IO-AREA (1:10) TO BILAGFI-01-L1-FRA
               IF  BILAGFI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BILAGFI-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  BILAGFI-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  BILAGFI-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BILAGFI-01-L3         TO THE-PRIOR-L3
               MOVE  BILAGFI-01-L2         TO THE-PRIOR-L2
               MOVE  BILAGFI-01-L1         TO THE-PRIOR-L1
               SET BILAGFI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE 741                    TO BW-A
               PERFORM VARYING ARN-I FROM ARN-MAX BY -1
                         UNTIL ARN-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE FIRMAF-IO-AREA (BW-A:4) TO XI-ARN-GRP
                   MOVE XI-ARN             TO ARN (ARN-I)
               END-PERFORM
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE BILNR-IO               TO BILAGFI-IO-AREA (15:6)
               REWRITE BILAGFI-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND I-50)
               MOVE ARN (1)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (705:4)
               MOVE ARN (2)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (709:4)
               MOVE ARN (3)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (713:4)
               MOVE ARN (4)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (717:4)
               MOVE ARN (5)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (721:4)
               MOVE ARN (6)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (725:4)
               MOVE ARN (7)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (729:4)
               MOVE ARN (8)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (733:4)
               MOVE ARN (9)                TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (737:4)
               REWRITE FIRMAF-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = FIRMAF'
               END-REWRITE
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
           SET BILAGFI-LEVEL-INIT          TO TRUE
           INITIALIZE BILAGFI-DATA-FIELDS
           SET BILAGFI-EOF-OFF             TO TRUE
           SET BILAGFI-PROCESS             TO TRUE
           OPEN I-O BILAGFI
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN I-O FIRMAF.
           PERFORM VARYING ARN-I FROM 1 BY 1
                     UNTIL ARN-I > ARN-MAX
               INITIALIZE ARN (ARN-I)
           END-PERFORM
           SET ARN-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BILAGFI
           CLOSE FIRMAF.
 
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
