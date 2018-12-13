       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBS053R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: BBS053                          XX2000XXIRXXMT       *
      * HENTER BILAGSNUMMER OG OPPDATERER.                            *
      * ENDR: 13.11.97 BRUKER HJELPEFELT TIL BILAGSNR DA              *
      *                BELØPSFELT BLE OVERSKREVET AV DET              *
      *                PAKKEDE BILAGSNR-FELTET.                       *
      * ENDR: 15.08.00 TAKLER AUTOGIRO.                               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BBS053.rpg
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
           SELECT BBSREC
               ASSIGN TO UT-S-BBSREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSREC-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD BBSREC
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  BBSREC-IO-AREA.
           05  BBSREC-IO-AREA-X            PICTURE X(80).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BBSREC-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-EOF-OFF          VALUE '0'.
               88  BBSREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-READ-OFF         VALUE '0'.
               88  BBSREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-PROCESS-OFF      VALUE '0'.
               88  BBSREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BBSREC-LEVEL-INIT-OFF   VALUE '0'.
               88  BBSREC-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  BBSREC-LEVEL-03.
               10  BBSREC-03-L3.
                   15  BBSREC-03-L3-FIRMA  PICTURE X(3).
               10  BBSREC-03-L2.
                   15  BBSREC-03-L2-REFNR  PICTURE X(10).
           05  BBSREC-LEVEL-04.
               10  BBSREC-04-L3.
                   15  BBSREC-04-L3-FIRMA  PICTURE X(3).
               10  BBSREC-04-L1.
                   15  BBSREC-04-L1-DATO   PICTURE X(6).
           05  BBSREC-DATA-FIELDS.
               10  REFNR                   PICTURE X(10).
               10  FIRMA                   PICTURE X(3).
               10  DATO                    PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  BILNR-IO.
                   15  BILNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(10).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  WBILNR-IO.
                   15  WBILNR              PICTURE S9(6).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BBSREC-PROCESS
               SET BBSREC-PROCESS-OFF      TO TRUE
               SET BBSREC-READ             TO TRUE
           END-IF
 
           IF  BBSREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM BBSREC-GET
               SET BBSREC-READ-OFF         TO TRUE
               IF  NOT BBSREC-EOF
                   SET BBSREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BBSREC-PROCESS
               PERFORM BBSREC-IDSET
           END-IF
 
           IF  BBSREC-PROCESS
               PERFORM BBSREC-CHK-LEVEL
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
 
           IF  BBSREC-PROCESS
               PERFORM BBSREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BBSREC-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-03 AND I-L2)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-03 AND I-L2 AND NOT-I-11)
               ADD BILNR TO ZERO       GIVING WBILNR
               ADD 1                       TO WBILNR
               ADD WBILNR TO ZERO      GIVING BILNR
           END-IF
           IF  (I-04 AND I-L1)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-04 AND I-L1 AND NOT-I-11)
               ADD BILNR TO ZERO       GIVING WBILNR
               ADD 1                       TO WBILNR
               ADD WBILNR TO ZERO      GIVING BILNR
           END-IF.
 
       BBSREC-GET SECTION.
       BBSREC-GET-P.
           IF  BBSREC-EOF-OFF
               READ BBSREC
               AT END
                   SET BBSREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BBSREC-FLDSET SECTION.
       BBSREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (4:1) = '9'
            AND   BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               MOVE BBSREC-IO-AREA (22:10) TO REFNR (1:10)
               MOVE BBSREC-IO-AREA (75:3)  TO FIRMA (1:3)
           WHEN ( BBSREC-IO-AREA (4:1) = '1'
            AND   BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               MOVE BBSREC-IO-AREA (16:6)  TO DATO (1:6)
               MOVE BBSREC-IO-AREA (75:3)  TO FIRMA (1:3)
           END-EVALUATE.
 
       BBSREC-IDSET SECTION.
       BBSREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (4:1) = '9'
            AND   BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               SET I-03                    TO TRUE
           WHEN ( BBSREC-IO-AREA (4:1) = '1'
            AND   BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               SET I-04                    TO TRUE
           WHEN  OTHER
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       BBSREC-CHK-LEVEL SECTION.
       BBSREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (4:1) = '9'
            AND   BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               MOVE LOW-VALUES             TO BBSREC-LEVEL-03
               MOVE BBSREC-IO-AREA (75:3)  TO BBSREC-03-L3-FIRMA
               MOVE BBSREC-IO-AREA (22:10) TO BBSREC-03-L2-REFNR
               IF  BBSREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BBSREC-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  BBSREC-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  BBSREC-03-L3          TO THE-PRIOR-L3
               MOVE  BBSREC-03-L2          TO THE-PRIOR-L2
               SET BBSREC-LEVEL-INIT       TO TRUE
           WHEN ( BBSREC-IO-AREA (4:1) = '1'
            AND   BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               MOVE LOW-VALUES             TO BBSREC-LEVEL-04
               MOVE BBSREC-IO-AREA (75:3)  TO BBSREC-04-L3-FIRMA
               MOVE BBSREC-IO-AREA (16:6)  TO BBSREC-04-L1-DATO
               IF  BBSREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BBSREC-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  BBSREC-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BBSREC-04-L3          TO THE-PRIOR-L3
               MOVE  BBSREC-04-L1          TO THE-PRIOR-L1
               SET BBSREC-LEVEL-INIT       TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (741:4) TO BILNR-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND NOT-I-11)
               MOVE WBILNR-IO              TO BBSREC-IO-AREA (50:6)
               REWRITE BBSREC-IO-AREA
           END-IF
           IF  (I-04 AND NOT-I-11)
               MOVE WBILNR-IO              TO BBSREC-IO-AREA (50:6)
               REWRITE BBSREC-IO-AREA
           END-IF
           IF  (I-L1 AND NOT-I-11)
               MOVE BILNR                  TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (741:4)
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
           SET BBSREC-LEVEL-INIT           TO TRUE
           INITIALIZE BBSREC-DATA-FIELDS
           SET BBSREC-EOF-OFF              TO TRUE
           SET BBSREC-PROCESS              TO TRUE
           OPEN I-O BBSREC
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN I-O FIRMAF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BBSREC
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
