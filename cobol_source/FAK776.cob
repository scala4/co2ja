       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK776R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: FAK776                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMT...: 23.10.2000                                      *
      *  RETTET.....: 23.10.2000                                      *
      *                                                               *
      *  PROGRAMMET MERKER SALGSDATA SOM HOVEDBOKSDATA OG IKKE SALG.  *
      *  RECORD"S UTEN SVS BEREGNES SVS FRA INNMELDT SVS I VGR.MASTER *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK776.rpg
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
           SELECT STAINN
               ASSIGN TO UT-S-STAINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STAINN-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD STAINN
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  STAINN-IO-AREA.
           05  STAINN-IO-AREA-X            PICTURE X(160).
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
           10  STAINN-STATUS               PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  STAINN-EOF-OFF          VALUE '0'.
               88  STAINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STAINN-READ-OFF         VALUE '0'.
               88  STAINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STAINN-PROCESS-OFF      VALUE '0'.
               88  STAINN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  STAINN-LEVEL-INIT-OFF   VALUE '0'.
               88  STAINN-LEVEL-INIT       VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  STAINN-LEVEL-01.
               10  STAINN-01-L2.
                   15  STAINN-01-L2-FIRMA  PICTURE X(3).
               10  STAINN-01-L1.
                   15  STAINN-01-L1-VGR    PICTURE X(5).
           05  STAINN-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  NTOAKK-IO.
                   15  NTOAKK              PICTURE S9(7)V9(2).
               10  SVSAKK-IO.
                   15  SVSAKK              PICTURE S9(7)V9(2).
           05  VAGRMAS-DATA-FIELDS.
               10  KONTO                   PICTURE X(4).
               10  BRFPR-IO.
                   15  BRFPR               PICTURE S9(3)V9(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  VGRKEY                  PICTURE X(8).
               10  PROS-IO.
                   15  PROS                PICTURE S9(3)V9(1).
               10  BSVS-IO.
                   15  BSVS                PICTURE S9(11)V9(1).
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
           IF  STAINN-PROCESS
               SET STAINN-PROCESS-OFF      TO TRUE
               SET STAINN-READ             TO TRUE
           END-IF
 
           IF  STAINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM STAINN-GET
               SET STAINN-READ-OFF         TO TRUE
               IF  NOT STAINN-EOF
                   SET STAINN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  STAINN-PROCESS
               PERFORM STAINN-IDSET
           END-IF
 
           IF  STAINN-PROCESS
               PERFORM STAINN-CHK-LEVEL
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
 
           IF  STAINN-PROCESS
               PERFORM STAINN-FLDOFF
               PERFORM STAINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  STAINN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
           SET NOT-I-60                    TO TRUE
      *****************************************************************
      * BEREGNING AV SOLGTE VARERS SELVKOST (SVS).                    *
      * PÅ VARERECORD SOM MANGLER SVS VED Å HENTE BRUTTOFJ. PROSENT   *
      * FRA VAREGRUPPE MASTER. ER IKKE DETTE INNMELDT BEREGNES 25 %   *
      *****************************************************************
           IF  (I-L1)
               MOVE FIRMA                  TO VGRKEY (1:3)
               MOVE VGR                    TO VGRKEY (4:5)
               MOVE VGRKEY                 TO VAGRMAS-KEY1
               READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
               INVALID KEY
                   SET I-50                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-50            TO TRUE
                   PERFORM VAGRMAS-FLDOFF
                   PERFORM VAGRMAS-FLDSET
                   PERFORM VAGRMAS-IDSET
               END-READ
           END-IF
           IF  (I-50)
               GO TO SVSRUT-T
           END-IF
           SET NOT-I-60                    TO TRUE
           IF  KONTO > '0000'
               SET I-60                    TO TRUE
           END-IF
           IF  (I-60)
               SET I-10                    TO TRUE
           END-IF.
 
       SVSRUT-T.
           IF  (NOT-I-07)
               GO TO SLUTT-T
           END-IF
           IF  (I-12)
               OR  (I-50)
               GO TO SVSNUL-T
           END-IF
           SUBTRACT BRFPR FROM 100     GIVING PROS
           MULTIPLY PROS BY NTOAKK     GIVING BSVS ROUNDED
           DIVIDE BSVS BY 100          GIVING SVSAKK ROUNDED.
 
       SVSNUL-T.
           IF  (I-12)
               OR  (I-50)
               ADD NTOAKK TO ZERO      GIVING SVSAKK
           END-IF
           SET I-10                        TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
       STAINN-GET SECTION.
       STAINN-GET-P.
           IF  STAINN-EOF-OFF
               READ STAINN
               AT END
                   SET STAINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       STAINN-FLDOFF SECTION.
       STAINN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       STAINN-FLDSET SECTION.
       STAINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STAINN-IO-AREA (39:3)  TO FIRMA (1:3)
               MOVE STAINN-IO-AREA (51:5)  TO VGR (1:5)
               MOVE STAINN-IO-AREA (122:9) TO NTOAKK-IO
               INSPECT NTOAKK-IO REPLACING ALL ' ' BY '0'
               MOVE STAINN-IO-AREA (131:9) TO SVSAKK-IO
               INSPECT SVSAKK-IO REPLACING ALL ' ' BY '0'
               IF  SVSAKK = ZERO
                   SET I-07                TO TRUE
               END-IF
           END-EVALUATE.
 
       STAINN-IDSET SECTION.
       STAINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       STAINN-CHK-LEVEL SECTION.
       STAINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO STAINN-LEVEL-01
               MOVE STAINN-IO-AREA (39:3)  TO STAINN-01-L2-FIRMA
               MOVE STAINN-IO-AREA (51:5)  TO STAINN-01-L1-VGR
               IF  STAINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  STAINN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  STAINN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  STAINN-01-L2          TO THE-PRIOR-L2
               MOVE  STAINN-01-L1          TO THE-PRIOR-L1
               SET STAINN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDOFF SECTION.
       VAGRMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-12                TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (69:4) TO KONTO (1:4)
               MOVE VAGRMAS-IO-AREA (77:4) TO BRFPR-IO
               INSPECT BRFPR-IO REPLACING ALL ' ' BY '0'
               IF  BRFPR = ZERO
                   SET I-12                TO TRUE
               END-IF
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-10)
               IF  (I-60)
                   MOVE 'K'                TO STAINN-IO-AREA (49:1)
               END-IF
               MOVE SVSAKK-IO              TO STAINN-IO-AREA (131:9)
               INITIALIZE SVSAKK-IO
               REWRITE STAINN-IO-AREA
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
           SET STAINN-LEVEL-INIT           TO TRUE
           INITIALIZE STAINN-DATA-FIELDS
           SET STAINN-EOF-OFF              TO TRUE
           SET STAINN-PROCESS              TO TRUE
           OPEN I-O STAINN
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE STAINN
           CLOSE VAGRMAS.
 
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
