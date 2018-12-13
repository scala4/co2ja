       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVS011R.
      **********************************************  Z-WIN-RPG2   ****
      *    BEREGNER SOLGTE VARERS SELVKOST ELLER UTSALGSPRIS.    *
      ************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: SVS011.rpg
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
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
           SELECT RECORDS-X
               ASSIGN TO UT-S-RECORDS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RECORDS-X-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT KUNDET
               ASSIGN TO UT-S-KUNDET
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KUNDET-STATUS.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1.
                   15  VAGRMAS-KEY1N       PICTURE S9(8).
               10  FILLER                  PICTURE X(71).
       FD RECORDS-X
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  RECORDS-X-IO-AREA.
           05  RECORDS-X-IO-AREA-X         PICTURE X(82).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD KUNDET
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KUNDET-IO-AREA.
           05  KUNDET-IO-AREA-X            PICTURE X(80).
       FD OUTF
               BLOCK CONTAINS 4680
               RECORD CONTAINS 65.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(65).
       WORKING-STORAGE SECTION.
       77  TABKEY-MAX   VALUE 50           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABKEY-TABLE.
               10  TABKEY-ENTRY
                                           OCCURS 50 TIMES
                                           INDEXED BY TABKEY-I
                                                      TABKEY-S.
                   15  TABKEY              PICTURE X(9).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  RECORDS-X-STATUS            PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  KUNDET-STATUS               PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VAGRMAS-EOF-OFF         VALUE '0'.
               88  VAGRMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAGRMAS-READ-OFF        VALUE '0'.
               88  VAGRMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAGRMAS-PROCESS-OFF     VALUE '0'.
               88  VAGRMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAGRMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  VAGRMAS-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RECORDS-X-EOF-OFF       VALUE '0'.
               88  RECORDS-X-EOF           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RECORDS-X-READ-OFF      VALUE '0'.
               88  RECORDS-X-READ          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RECORDS-X-PROCESS-OFF   VALUE '0'.
               88  RECORDS-X-PROCESS       VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RECORDS-X-LEVEL-INIT-OFF VALUE '0'.
               88  RECORDS-X-LEVEL-INIT    VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDET-EOF-OFF          VALUE '0'.
               88  KUNDET-EOF              VALUE '1'.
           05  VAGRMAS-LEVEL-01.
               10  VAGRMAS-01-L2.
                   15  VAGRMAS-01-L2-FIRMA PICTURE X(3).
               10  VAGRMAS-01-L1.
                   15  VAGRMAS-01-L1-VGR   PICTURE X(5).
           05  VAGRMAS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  NAVN                    PICTURE X(30).
               10  TILKTO                  PICTURE X(4).
               10  FPRO-IO.
                   15  FPRO                PICTURE S9(3)V9(1).
           05  VAGRMAS-MP                  PICTURE X(8).
           05  VAGRMAS-MC                  PICTURE X(8).
           05  VAGRMAS-M-01            REDEFINES VAGRMAS-MC.
               10  VAGRMAS-M-01-M2.
                   15  VAGRMAS-M-01-M2-FIRMA-G.
                       20  VAGRMAS-M-01-M2-FIRMA PICTURE X(3).
               10  VAGRMAS-M-01-M1.
                   15  VAGRMAS-M-01-M1-VGR-G.
                       20  VAGRMAS-M-01-M1-VGR PICTURE X(5).
           05  RECORDS-X-LEVEL-02.
               10  RECORDS-X-02-L2.
                   15  RECORDS-X-02-L2-FIRMA PICTURE X(3).
               10  RECORDS-X-02-L1.
                   15  RECORDS-X-02-L1-VGR PICTURE X(5).
           05  RECORDS-X-DATA-FIELDS.
               10  KOSTPR-IO.
                   15  KOSTPR              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LAGERK                  PICTURE X(2).
               10  LEVERT-IO.
                   15  LEVERT              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  FK                      PICTURE X(1).
               10  BK                      PICTURE X(1).
               10  KUNDE                   PICTURE X(6).
               10  PRITIL                  PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
           05  RECORDS-X-MP                PICTURE X(8).
           05  RECORDS-X-MC                PICTURE X(8).
           05  RECORDS-X-M-02          REDEFINES RECORDS-X-MC.
               10  RECORDS-X-M-02-M2.
                   15  RECORDS-X-M-02-M2-FIRMA-G.
                       20  RECORDS-X-M-02-M2-FIRMA PICTURE X(3).
               10  RECORDS-X-M-02-M1.
                   15  RECORDS-X-M-02-M1-VGR-G.
                       20  RECORDS-X-M-02-M1-VGR PICTURE X(5).
           05  VAREMAS-DATA-FIELDS.
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  SVS-IO.
                   15  SVS                 PICTURE S9(8)V9(2).
               10  SVU-IO.
                   15  SVU                 PICTURE S9(8)V9(2).
               10  VARKEY                  PICTURE X(10).
               10  KUNKEY                  PICTURE X(9).
               10  PRIS-N-IO.
                   15  PRIS-N              PICTURE S9(7)V9(2).
               10  BRUTTO-IO.
                   15  BRUTTO              PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(8)V9(3).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(7)V9(2).
               10  SVSBEL-IO.
                   15  SVSBEL              PICTURE S9(8)V9(2).
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
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAGRMAS-PROCESS
               SET VAGRMAS-PROCESS-OFF     TO TRUE
               SET VAGRMAS-READ            TO TRUE
           END-IF
 
           IF  VAGRMAS-READ
               PERFORM VAGRMAS-GET
               SET VAGRMAS-READ-OFF        TO TRUE
               IF  NOT VAGRMAS-EOF
                   PERFORM VAGRMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  RECORDS-X-PROCESS
               SET RECORDS-X-PROCESS-OFF   TO TRUE
               SET RECORDS-X-READ          TO TRUE
           END-IF
 
           IF  RECORDS-X-READ
               PERFORM RECORDS-X-GET
               SET RECORDS-X-READ-OFF      TO TRUE
               IF  NOT RECORDS-X-EOF
                   PERFORM RECORDS-X-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  VAGRMAS-PROCESS
               PERFORM VAGRMAS-IDSET
           END-IF
 
           IF  RECORDS-X-PROCESS
               PERFORM RECORDS-X-IDSET
           END-IF
 
           IF  VAGRMAS-PROCESS
               PERFORM VAGRMAS-CHK-LEVEL
           END-IF
 
           IF  RECORDS-X-PROCESS
               PERFORM RECORDS-X-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VAGRMAS-PROCESS
               PERFORM VAGRMAS-FLDSET
           END-IF
 
           IF  RECORDS-X-PROCESS
               PERFORM RECORDS-X-FLDOFF
               PERFORM RECORDS-X-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAGRMAS-PROCESS
           OR  RECORDS-X-PROCESS
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
               MOVE 0                      TO SVS
               MOVE 0                      TO SVU
               SET NOT-I-90                TO TRUE
               SET NOT-I-91                TO TRUE
      *
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO VARKEY (1:3)
               MOVE FIRMA                  TO KUNKEY (1:3)
      *
           END-IF
           IF  (I-01)
               SET NOT-I-91                TO TRUE
               IF  TILKTO > '    '
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               OR  (I-91)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET NOT-I-23                TO TRUE
               IF  PRITIL = 'P'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-23)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  FK = '1'
               SET I-22                    TO TRUE
           END-IF
      *
           SET NOT-I-14                    TO TRUE
           IF  BK = '4'
               SET I-14                    TO TRUE
           END-IF
           IF  (I-14)
               MOVE PRIS                   TO PRIS-N
               MOVE PRIS-N-IO              TO KOSTPR-IO
               GO TO SVSRUT-T
      ********************************************************
      * 18/11-89  FJERNET TEST PÅ NULL I FAKTURABELØP.       *
      *           IFØLGE B.KLÆSTAD OG T.SIMONSEN SKAL DET    *
      *           BEREGNES SVS AV NULLFAKTURA.               *
      *  06                GOTO SLUTT                       NULL I FAKT.PRIS.
      ********************************************************
           END-IF
           MOVE KUNDE                      TO KUNKEY (4:6)
           SET NOT-I-24                    TO TRUE
           SET TABKEY-S                    TO TABKEY-I
           PERFORM WITH TEST AFTER
                   VARYING TABKEY-I FROM 1 BY 1
                     UNTIL TABKEY-I >= TABKEY-MAX
                        OR I-24
               IF  KUNKEY = TABKEY (TABKEY-I)
                   SET I-24                TO TRUE
                   SET TABKEY-S            TO TABKEY-I
               END-IF
           END-PERFORM
           IF  (I-24)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-02 AND I-07)
               MOVE EDBNR                  TO VARKEY (4:7)
               MOVE VARKEY                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM VAREMAS-FLDOFF
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-21 AND I-07)
               GO TO SLUTT-T
      *
      ******************************************************
      * KREDITNOTARUTINE.  KUN KREDITYPE 2 (RETUR AV VARER *
      *                     OG KREDITYPE 5 (ERSTATNING.)   *
      *                    SKAL OPPDATERE SVS I VGR-MASTER *
      ******************************************************
           END-IF
           SET NOT-I-20                    TO TRUE
           SET NOT-I-20                    TO TRUE
           IF  KRETYP = '2'
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  KRETYP = '5'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  KRETYP = '6'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-09 AND I-20)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (NOT-I-22 AND NOT-I-20)
               GO TO SLUTT-T
      ******************************************************
      * TEST OM BEREGNING AV SVS ELLER SVU                 *
      ******************************************************
           END-IF
           IF  (I-09)
               GO TO SVURUT-T
           END-IF
           IF  (I-07 AND I-08)
               GO TO SVURUT-T
           END-IF
           GO TO SVSRUT-T
      ******************************************************
      *  RUTINE FOR BEREGNING SOLGTE VARERS UTSALGSPRIS.   *
      ******************************************************
           .
 
       SVURUT-T.
           SET I-90                        TO TRUE
           MULTIPLY LEVERT BY PRIS     GIVING BRUTTO ROUNDED
           IF  (I-09)
               ADD PRIS TO ZERO        GIVING BRUTTO
           END-IF
           IF  (NOT-I-10)
               MULTIPLY RAB1 BY BRUTTO GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BRUTTO
           END-IF
           IF  (NOT-I-11)
               MULTIPLY RAB2 BY BRUTTO GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BRUTTO
           END-IF
           IF  (NOT-I-12)
               MULTIPLY RAB3 BY BRUTTO GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BRUTTO
      *
           END-IF
           IF  (I-22)
               ADD BRUTTO                  TO SVU
           END-IF
           IF  (NOT-I-22)
               SUBTRACT BRUTTO             FROM SVU
           END-IF
           GO TO SLUTT-T
      ******************************************************
      *  RUTINE FOR BEREGNING AV SOLGTE VARERS KOSTPRIS.   *
      ******************************************************
           .
 
       SVSRUT-T.
           SET I-90                        TO TRUE
      *        RABATT MÅ TREKKES UT, FOR DET GJØRE PÅ SALGET.
           IF  (I-14)
               ADD PRIS TO ZERO        GIVING BRUTTO
           END-IF
           IF  (NOT-I-10 AND I-14)
               MULTIPLY RAB1 BY BRUTTO GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BRUTTO
           END-IF
           IF  (NOT-I-11 AND I-14)
               MULTIPLY RAB2 BY BRUTTO GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BRUTTO
           END-IF
           IF  (NOT-I-12 AND I-14)
               MULTIPLY RAB3 BY BRUTTO GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BRUTTO
           END-IF
           IF  (I-14)
               ADD BRUTTO TO ZERO      GIVING KOSTPR
           END-IF
           IF  (NOT-I-07)
               MULTIPLY LEVERT BY KOSTPR GIVING SVSBEL ROUNDED
           END-IF
           IF  (I-07 AND NOT-I-14)
               MULTIPLY LEVERT BY SELVK GIVING SVSBEL ROUNDED
      *  07 14             Z-ADDKOSTPR    SVSBEL              ????
           END-IF
           IF  (I-07 AND I-14)
               MULTIPLY LEVERT BY KOSTPR GIVING SVSBEL
           END-IF
           IF  (I-22)
               ADD SVSBEL                  TO SVS
           END-IF
           IF  (NOT-I-22)
               SUBTRACT SVSBEL             FROM SVS
      *
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       VAGRMAS-GET SECTION.
       VAGRMAS-GET-P.
           IF  VAGRMAS-EOF-OFF
               READ VAGRMAS
               AT END
                   SET VAGRMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAGRMAS-IO-AREA (6:5)  TO VGR (1:5)
               MOVE VAGRMAS-IO-AREA (11:30) TO NAVN (1:30)
               MOVE VAGRMAS-IO-AREA (69:4) TO TILKTO (1:4)
               MOVE VAGRMAS-IO-AREA (77:4) TO FPRO-IO
               INSPECT FPRO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAGRMAS-CHK-LEVEL SECTION.
       VAGRMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAGRMAS-LEVEL-01
               MOVE VAGRMAS-IO-AREA (3:3)  TO VAGRMAS-01-L2-FIRMA
               MOVE VAGRMAS-IO-AREA (6:5)  TO VAGRMAS-01-L1-VGR
               IF  VAGRMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAGRMAS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAGRMAS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAGRMAS-01-L2         TO THE-PRIOR-L2
               MOVE  VAGRMAS-01-L1         TO THE-PRIOR-L1
               SET VAGRMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAGRMAS-MATCH-SET SECTION.
       VAGRMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (3:3)  TO VAGRMAS-M-01-M2-FIRMA
               MOVE VAGRMAS-IO-AREA (6:5)  TO VAGRMAS-M-01-M1-VGR
           END-EVALUATE.
 
       RECORDS-X-GET SECTION.
       RECORDS-X-GET-P.
           IF  RECORDS-X-EOF-OFF
               READ RECORDS-X
               AT END
                   SET RECORDS-X-EOF       TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RECORDS-X-FLDOFF SECTION.
       RECORDS-X-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-07                TO TRUE
               SET NOT-I-09                TO TRUE
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-06                TO TRUE
           END-EVALUATE.
 
       RECORDS-X-FLDSET SECTION.
       RECORDS-X-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RECORDS-X-IO-AREA (3:5) TO KOSTPR-IO
               IF  KOSTPR = ZERO
                   SET I-07                TO TRUE
               END-IF
               MOVE RECORDS-X-IO-AREA (69:2) TO LAGERK (1:2)
               MOVE RECORDS-X-IO-AREA (12:4) TO LEVERT-IO
               IF  LEVERT = ZERO
                   SET I-09                TO TRUE
               END-IF
               MOVE RECORDS-X-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE RECORDS-X-IO-AREA (16:7) TO EDBNR (1:7)
               MOVE RECORDS-X-IO-AREA (23:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               IF  RAB1 = ZERO
                   SET I-10                TO TRUE
               END-IF
               MOVE RECORDS-X-IO-AREA (26:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               IF  RAB2 = ZERO
                   SET I-11                TO TRUE
               END-IF
               MOVE RECORDS-X-IO-AREA (29:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               IF  RAB3 = ZERO
                   SET I-12                TO TRUE
               END-IF
               MOVE RECORDS-X-IO-AREA (32:9) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               IF  PRIS = ZERO
                   SET I-06                TO TRUE
               END-IF
               MOVE RECORDS-X-IO-AREA (41:1) TO FK (1:1)
               MOVE RECORDS-X-IO-AREA (44:1) TO BK (1:1)
               MOVE RECORDS-X-IO-AREA (45:6) TO KUNDE (1:6)
               MOVE RECORDS-X-IO-AREA (60:5) TO VGR (1:5)
               MOVE RECORDS-X-IO-AREA (65:1) TO PRITIL (1:1)
               MOVE RECORDS-X-IO-AREA (66:1) TO KRETYP (1:1)
           END-EVALUATE.
 
       RECORDS-X-IDSET SECTION.
       RECORDS-X-IDSET-P.
           SET I-02                        TO TRUE.
 
       RECORDS-X-CHK-LEVEL SECTION.
       RECORDS-X-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RECORDS-X-LEVEL-02
               MOVE RECORDS-X-IO-AREA (51:3) TO RECORDS-X-02-L2-FIRMA
               MOVE RECORDS-X-IO-AREA (60:5) TO RECORDS-X-02-L1-VGR
               IF  RECORDS-X-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RECORDS-X-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RECORDS-X-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RECORDS-X-02-L2       TO THE-PRIOR-L2
               MOVE  RECORDS-X-02-L1       TO THE-PRIOR-L1
               SET RECORDS-X-LEVEL-INIT    TO TRUE
           END-EVALUATE.
 
       RECORDS-X-MATCH-SET SECTION.
       RECORDS-X-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RECORDS-X-IO-AREA (51:3) TO RECORDS-X-M-02-M2-FIRMA
               MOVE RECORDS-X-IO-AREA (60:5) TO RECORDS-X-M-02-M1-VGR
           END-EVALUATE.
 
       VAREMAS-FLDOFF SECTION.
       VAREMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               IF  SELVK = ZERO
                   SET I-08                TO TRUE
               END-IF
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VAGRMAS-EOF
               MOVE HIGH-VALUES            TO VAGRMAS-MC
                                              VAGRMAS-MP
           END-IF
           IF  RECORDS-X-EOF
               MOVE HIGH-VALUES            TO RECORDS-X-MC
                                              RECORDS-X-MP
           END-IF
           IF  VAGRMAS-MC < VAGRMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RECORDS-X-MC < RECORDS-X-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAGRMAS-MC < RECORDS-X-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAGRMAS-PROCESS     TO TRUE
                   MOVE VAGRMAS-MC         TO VAGRMAS-MP
                   IF  VAGRMAS-MC = RECORDS-X-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RECORDS-X-MC < VAGRMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RECORDS-X-PROCESS   TO TRUE
                   MOVE RECORDS-X-MC       TO RECORDS-X-MP
                   IF  RECORDS-X-MC = VAGRMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAGRMAS-MC = RECORDS-X-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAGRMAS-PROCESS     TO TRUE
                   MOVE VAGRMAS-MC         TO VAGRMAS-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDET-LOAD SECTION.
       KUNDET-LOAD-P.
           OPEN INPUT KUNDET
           SET TABKEY-I                    TO 1
           PERFORM UNTIL KUNDET-EOF
               READ KUNDET
               AT END
                   SET KUNDET-EOF          TO TRUE
               NOT AT END
                   MOVE KUNDET-IO-AREA (1:9) TO TABKEY-ENTRY (TABKEY-I)
                   SET TABKEY-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KUNDET.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-90)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE '4'                    TO OUTF-IO-AREA (1:1)
               MOVE FIRMA                  TO OUTF-IO-AREA (2:3)
               MOVE VGR                    TO OUTF-IO-AREA (5:5)
               MOVE SVS-IO                 TO OUTF-IO-AREA (10:10)
               MOVE SVU-IO                 TO OUTF-IO-AREA (20:10)
               IF  (I-MR)
                   MOVE FPRO-IO            TO OUTF-IO-AREA (30:4)
               END-IF
               IF  (I-MR)
                   MOVE NAVN               TO OUTF-IO-AREA (34:30)
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
           MOVE 2                          TO LR-CHECK
           SET VAGRMAS-LEVEL-INIT          TO TRUE
           INITIALIZE VAGRMAS-DATA-FIELDS
           SET VAGRMAS-EOF-OFF             TO TRUE
           SET VAGRMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAGRMAS-MC
                                              VAGRMAS-MP
           OPEN INPUT VAGRMAS
           SET RECORDS-X-LEVEL-INIT        TO TRUE
           INITIALIZE RECORDS-X-DATA-FIELDS
           SET RECORDS-X-EOF-OFF           TO TRUE
           SET RECORDS-X-PROCESS           TO TRUE
           MOVE LOW-VALUES                 TO RECORDS-X-MC
                                              RECORDS-X-MP
           OPEN INPUT RECORDS-X
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           PERFORM KUNDET-LOAD
           OPEN OUTPUT OUTF.
           SET TABKEY-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAGRMAS
           CLOSE RECORDS-X
           CLOSE VAREMAS
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
