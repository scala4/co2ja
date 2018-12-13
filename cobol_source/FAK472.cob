       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK472R.
      **********************************************  Z-WIN-RPG2   ****
      * PROG FAK472                                                  *
      * OPPDATERE KUNDEMASTER MED TOTALT FAKTURERT PR. KUNDE         *
      * HITTIL I ÅR.                                                 *
      * UPSI 1 = TILBAKEFØRING AV OPPDATERING.                       *
      * FØRSTE FAKTURERING PR. ÅR NULLSTILLES KUNDEMASTER.           *
      * 03.02.2000 KONSERNMODELL HENSYNTATT FOR KUNDE.MASTER         *
      * 22.05.2000 FEIL I KONSERNMODELL. RETTET.                     *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK472.rpg
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
           SELECT SUMFILE
               ASSIGN TO UT-S-SUMFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFILE-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD SUMFILE
               BLOCK CONTAINS 9440
               RECORD CONTAINS 20.
       01  SUMFILE-IO-AREA.
           05  SUMFILE-IO-AREA-X           PICTURE X(20).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  SUMFILE-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFILE-EOF-OFF         VALUE '0'.
               88  SUMFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFILE-READ-OFF        VALUE '0'.
               88  SUMFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFILE-PROCESS-OFF     VALUE '0'.
               88  SUMFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SUMFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  SUMFILE-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SUMFILE-LEVEL-01.
               10  SUMFILE-01-L1.
                   15  SUMFILE-01-L1-RESKEY PICTURE X(9).
           05  SUMFILE-DATA-FIELDS.
               10  RESKEY                  PICTURE X(9).
               10  RESK1                   PICTURE X(1).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  KUNDEMA-DATA-FIELDS.
               10  FAKSUM-IO.
                   15  FAKSUM              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  SUMFILE-PROCESS
               SET SUMFILE-PROCESS-OFF     TO TRUE
               SET SUMFILE-READ            TO TRUE
           END-IF
 
           IF  SUMFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM SUMFILE-GET
               SET SUMFILE-READ-OFF        TO TRUE
               IF  NOT SUMFILE-EOF
                   SET SUMFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  SUMFILE-PROCESS
               PERFORM SUMFILE-IDSET
           END-IF
 
           IF  SUMFILE-PROCESS
               PERFORM SUMFILE-CHK-LEVEL
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
 
           IF  SUMFILE-PROCESS
               PERFORM SUMFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SUMFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-25                    TO TRUE
           IF  (I-L1)
               SUBTRACT BEL                FROM BEL
           END-IF
           IF  (I-01)
               SET NOT-I-10                TO TRUE
               IF  RESK1 = '9'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND NOT-I-U1)
               ADD FAKBEL                  TO BEL
           END-IF
           IF  (I-01 AND I-U1)
               SUBTRACT FAKBEL             FROM BEL
           END-IF
           IF  (I-01)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD BEL                     TO FAKSUM
               SET I-25                    TO TRUE
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       SUMFILE-GET SECTION.
       SUMFILE-GET-P.
           IF  SUMFILE-EOF-OFF
               READ SUMFILE
               AT END
                   SET SUMFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SUMFILE-FLDSET SECTION.
       SUMFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SUMFILE-IO-AREA (2:9)  TO RESKEY (1:9)
               MOVE SUMFILE-IO-AREA (5:1)  TO RESK1 (1:1)
               MOVE SUMFILE-IO-AREA (11:5) TO FAKBEL-IO
           END-EVALUATE.
 
       SUMFILE-IDSET SECTION.
       SUMFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       SUMFILE-CHK-LEVEL SECTION.
       SUMFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SUMFILE-LEVEL-01
               MOVE SUMFILE-IO-AREA (2:9)  TO SUMFILE-01-L1-RESKEY
               IF  SUMFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SUMFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SUMFILE-01-L1         TO THE-PRIOR-L1
               SET SUMFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (150:6) TO FAKSUM-IO
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-25)
               MOVE FAKSUM                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDEMA-IO-AREA (150:6)
               REWRITE KUNDEMA-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = KUNDEMA'
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
           SET SUMFILE-LEVEL-INIT          TO TRUE
           INITIALIZE SUMFILE-DATA-FIELDS
           SET SUMFILE-EOF-OFF             TO TRUE
           SET SUMFILE-PROCESS             TO TRUE
           OPEN INPUT SUMFILE
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN I-O KUNDEMA.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE SUMFILE
           CLOSE KUNDEMA.
 
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
