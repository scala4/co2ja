       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD005R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET PLUKKER DAGENS ORDRE PÅ PAK1                      *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD005.rpg
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
           SELECT OPDAT
               ASSIGN TO UT-S-OPDAT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OPDAT-STATUS.
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT PAKKREC
               ASSIGN TO UT-S-PAKKREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PAKKREC-STATUS.
           SELECT TILUP
               ASSIGN TO UT-S-TILUP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILUP-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD OPDAT
               BLOCK CONTAINS 9440
               RECORD CONTAINS 20.
       01  OPDAT-IO-AREA.
           05  OPDAT-IO-AREA-X             PICTURE X(20).
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA-2.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD PAKKREC
               BLOCK CONTAINS 9000
               RECORD CONTAINS 300.
       01  PAKKREC-IO-AREA.
           05  PAKKREC-IO-AREA-X           PICTURE X(300).
       FD TILUP
               BLOCK CONTAINS 9440
               RECORD CONTAINS 20.
       01  TILUP-IO-AREA.
           05  TILUP-IO-AREA-X             PICTURE X(20).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  OPDAT-STATUS                PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  PAKKREC-STATUS              PICTURE 99 VALUE 0.
           10  TILUP-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  OPDAT-EOF-OFF           VALUE '0'.
               88  OPDAT-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPDAT-READ-OFF          VALUE '0'.
               88  OPDAT-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPDAT-PROCESS-OFF       VALUE '0'.
               88  OPDAT-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  OPDAT-LEVEL-INIT-OFF    VALUE '0'.
               88  OPDAT-LEVEL-INIT        VALUE '1'.
           05  ORDREM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-EOF-OFF          VALUE '0'.
               88  ORDREM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-READ-OFF         VALUE '0'.
               88  ORDREM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-PROCESS-OFF      VALUE '0'.
               88  ORDREM-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDREM-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDREM-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-AHEAD-EOF-OFF    VALUE '0'.
               88  ORDREM-AHEAD-EOF        VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-AHEAD-READ-OFF   VALUE '0'.
               88  ORDREM-AHEAD-READ       VALUE '1'.
           05  OPDAT-LEVEL-05.
               10  OPDAT-05-L2.
                   15  OPDAT-05-L2-FIRMA   PICTURE X(3).
               10  OPDAT-05-L1.
                   15  OPDAT-05-L1-ORDNR   PICTURE X(6).
           05  OPDAT-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
           05  OPDAT-MP                    PICTURE X(9).
           05  OPDAT-MC                    PICTURE X(9).
           05  OPDAT-M-05              REDEFINES OPDAT-MC.
               10  OPDAT-M-05-M2.
                   15  OPDAT-M-05-M2-FIRMA-G.
                       20  OPDAT-M-05-M2-FIRMA PICTURE X(3).
               10  OPDAT-M-05-M1.
                   15  OPDAT-M-05-M1-ORDNR-G.
                       20  OPDAT-M-05-M1-ORDNR PICTURE X(6).
           05  ORDREM-LEVEL-01.
               10  ORDREM-01-L2.
                   15  ORDREM-01-L2-FIRMA  PICTURE X(3).
               10  ORDREM-01-L1.
                   15  ORDREM-01-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-02.
               10  ORDREM-02-L2.
                   15  ORDREM-02-L2-FIRMA  PICTURE X(3).
               10  ORDREM-02-L1.
                   15  ORDREM-02-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-04.
               10  ORDREM-04-L2.
                   15  ORDREM-04-L2-FIRMA  PICTURE X(3).
               10  ORDREM-04-L1.
                   15  ORDREM-04-L1-ORDNR  PICTURE X(6).
           05  ORDREM-DATA-FIELDS.
               10  KUNDNR                  PICTURE X(6).
               10  NAVN1                   PICTURE X(30).
               10  NAVN2                   PICTURE X(30).
               10  PTYPE                   PICTURE X(1).
               10  PDATO                   PICTURE X(6).
               10  PTAKST                  PICTURE X(1).
               10  PVEKT                   PICTURE X(4).
               10  PPORTO-IO.
                   15  PPORTO              PICTURE S9(6).
               10  MERKE                   PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  ADR                     PICTURE X(30).
               10  PNR                     PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  VADR1                   PICTURE X(30).
               10  VADR2                   PICTURE X(30).
               10  VADR3                   PICTURE X(30).
               10  VADR4                   PICTURE X(20).
               10  NXTREC                  PICTURE X(2).
      *
           05  ORDREM-MP                   PICTURE X(9).
           05  ORDREM-MC                   PICTURE X(9).
           05  ORDREM-M-01             REDEFINES ORDREM-MC.
               10  ORDREM-M-01-M2.
                   15  ORDREM-M-01-M2-FIRMA-G.
                       20  ORDREM-M-01-M2-FIRMA PICTURE X(3).
               10  ORDREM-M-01-M1.
                   15  ORDREM-M-01-M1-ORDNR-G.
                       20  ORDREM-M-01-M1-ORDNR PICTURE X(6).
           05  ORDREM-M-02             REDEFINES ORDREM-MC.
               10  ORDREM-M-02-M2.
                   15  ORDREM-M-02-M2-FIRMA-G.
                       20  ORDREM-M-02-M2-FIRMA PICTURE X(3).
               10  ORDREM-M-02-M1.
                   15  ORDREM-M-02-M1-ORDNR-G.
                       20  ORDREM-M-02-M1-ORDNR PICTURE X(6).
           05  ORDREM-M-04             REDEFINES ORDREM-MC.
               10  ORDREM-M-04-M2.
                   15  ORDREM-M-04-M2-FIRMA-G.
                       20  ORDREM-M-04-M2-FIRMA PICTURE X(3).
               10  ORDREM-M-04-M1.
                   15  ORDREM-M-04-M1-ORDNR-G.
                       20  ORDREM-M-04-M1-ORDNR PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  ORDREM-IO-AREA.
               10  FILLER                  PICTURE X(164).
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  OPDAT-PROCESS
               SET OPDAT-PROCESS-OFF       TO TRUE
               SET OPDAT-READ              TO TRUE
           END-IF
 
           IF  OPDAT-READ
               PERFORM OPDAT-GET
               SET OPDAT-READ-OFF          TO TRUE
               IF  NOT OPDAT-EOF
                   PERFORM OPDAT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM OPDAT-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDREM-PROCESS
               SET ORDREM-PROCESS-OFF      TO TRUE
               SET ORDREM-READ             TO TRUE
           END-IF
 
           IF  ORDREM-READ
               PERFORM ORDREM-GET
               SET ORDREM-READ-OFF         TO TRUE
               IF  NOT ORDREM-EOF
                   PERFORM ORDREM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM ORDREM-MATCH-SET
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
 
           IF  OPDAT-PROCESS
               PERFORM OPDAT-IDSET
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-IDSET
           END-IF
 
           IF  OPDAT-PROCESS
               PERFORM OPDAT-CHK-LEVEL
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  OPDAT-PROCESS
               PERFORM OPDAT-FLDSET
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  OPDAT-PROCESS
           OR  ORDREM-PROCESS
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
               SET NOT-I-10                TO TRUE
               SET NOT-I-20                TO TRUE
               SET NOT-I-30                TO TRUE
           END-IF
           IF  (I-03)
               OR  (I-05)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-10                TO TRUE
               IF  MERKE = 'X'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-20                TO TRUE
               IF  PTYPE = '1'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  PTYPE = '2'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  PTYPE = '3'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET NOT-I-30                TO TRUE
               IF  NXTREC = ' 3'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-30)
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  (I-04)
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           SET NOT-I-81                    TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
       OPDAT-GET SECTION.
       OPDAT-GET-P.
           IF  OPDAT-EOF-OFF
               READ OPDAT
               AT END
                   SET OPDAT-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       OPDAT-FLDSET SECTION.
       OPDAT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( OPDAT-IO-AREA (1:1) = '1' )
               MOVE OPDAT-IO-AREA (2:3)    TO FIRMA (1:3)
               MOVE OPDAT-IO-AREA (5:6)    TO ORDNR (1:6)
           END-EVALUATE.
 
       OPDAT-IDCHK SECTION.
       OPDAT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( OPDAT-IO-AREA (1:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       OPDAT-IDSET SECTION.
       OPDAT-IDSET-P.
           EVALUATE TRUE
           WHEN ( OPDAT-IO-AREA (1:1) = '1' )
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       OPDAT-CHK-LEVEL SECTION.
       OPDAT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( OPDAT-IO-AREA (1:1) = '1' )
               MOVE LOW-VALUES             TO OPDAT-LEVEL-05
               MOVE OPDAT-IO-AREA (2:3)    TO OPDAT-05-L2-FIRMA
               MOVE OPDAT-IO-AREA (5:6)    TO OPDAT-05-L1-ORDNR
               IF  OPDAT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  OPDAT-05-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  OPDAT-05-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  OPDAT-05-L2           TO THE-PRIOR-L2
               MOVE  OPDAT-05-L1           TO THE-PRIOR-L1
               SET OPDAT-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       OPDAT-MATCH-SET SECTION.
       OPDAT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( OPDAT-IO-AREA (1:1) = '1' )
               MOVE OPDAT-IO-AREA (2:3)    TO OPDAT-M-05-M2-FIRMA
               MOVE OPDAT-IO-AREA (5:6)    TO OPDAT-M-05-M1-ORDNR
           END-EVALUATE.
 
       ORDREM-GET SECTION.
       ORDREM-GET-P.
           IF  ORDREM-EOF-OFF
               IF  ORDREM-AHEAD-EOF-OFF
                   IF  ORDREM-AHEAD-READ-OFF
                       SET ORDREM-AHEAD-READ TO TRUE
                       READ ORDREM
                       AT END
                           SET ORDREM-AHEAD-EOF TO TRUE
                           INITIALIZE ORDREM-IO-AREA-2
                       END-READ
                   END-IF
                   MOVE ORDREM-IO-AREA-2   TO ORDREM-IO-AREA
                   IF  ORDREM-AHEAD-EOF-OFF
                       READ ORDREM
                       AT END
                           SET ORDREM-AHEAD-EOF TO TRUE
                           INITIALIZE ORDREM-IO-AREA-2
                       END-READ
                   ELSE
                       SET ORDREM-EOF      TO TRUE
                       SUBTRACT 1        FROM LR-CHECK
                   END-IF
                   PERFORM ORDREM-AHEAD-FLDSET
               ELSE
                   SET ORDREM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-IF
           END-IF.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:6)  TO KUNDNR (1:6)
               MOVE ORDREM-IO-AREA (27:30) TO NAVN1 (1:30)
               MOVE ORDREM-IO-AREA (57:30) TO NAVN2 (1:30)
               MOVE ORDREM-IO-AREA (110:1) TO PTYPE (1:1)
               MOVE ORDREM-IO-AREA (111:6) TO PDATO (1:6)
               MOVE ORDREM-IO-AREA (117:1) TO PTAKST (1:1)
               MOVE ORDREM-IO-AREA (118:4) TO PVEKT (1:4)
               MOVE ORDREM-IO-AREA (122:6) TO PPORTO-IO
               INSPECT PPORTO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (128:1) TO MERKE (1:1)
               MOVE ORDREM-IO-AREA (164:1) TO STATUS-X (1:1)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (101:30) TO ADR (1:30)
               MOVE ORDREM-IO-AREA (131:4) TO PNR (1:4)
               MOVE ORDREM-IO-AREA (135:15) TO PSTED (1:15)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:30) TO VADR1 (1:30)
               MOVE ORDREM-IO-AREA (51:30) TO VADR2 (1:30)
               MOVE ORDREM-IO-AREA (81:30) TO VADR3 (1:30)
               MOVE ORDREM-IO-AREA (111:20) TO VADR4 (1:20)
           END-EVALUATE.
 
       ORDREM-IDCHK SECTION.
       ORDREM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               SET I-04                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       ORDREM-CHK-LEVEL SECTION.
       ORDREM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-01
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-01-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-01-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-01-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-01-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-02
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-02-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-02-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-02-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-02-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-04
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-04-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-04-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-04-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-04-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           END-EVALUATE.
 
       ORDREM-MATCH-SET SECTION.
       ORDREM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-M-01-M2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-M-01-M1-ORDNR
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-M-02-M2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-M-02-M1-ORDNR
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-M-04-M2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-M-04-M1-ORDNR
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           END-EVALUATE.
 
       ORDREM-AHEAD-FLDSET SECTION.
       ORDREM-AHEAD-FLDSET-P.
           MOVE ORDREM-IO-AREA-2 (19:2)    TO NXTREC (1:2).
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  OPDAT-EOF
               MOVE HIGH-VALUES            TO OPDAT-MC
                                              OPDAT-MP
           END-IF
           IF  ORDREM-EOF
               MOVE HIGH-VALUES            TO ORDREM-MC
                                              ORDREM-MP
           END-IF
           IF  OPDAT-MC < OPDAT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDREM-MC < ORDREM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  OPDAT-MC < ORDREM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET OPDAT-PROCESS       TO TRUE
                   MOVE OPDAT-MC           TO OPDAT-MP
                   IF  OPDAT-MC = ORDREM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDREM-MC < OPDAT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDREM-PROCESS      TO TRUE
                   MOVE ORDREM-MC          TO ORDREM-MP
                   IF  ORDREM-MC = OPDAT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  OPDAT-MC = ORDREM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET OPDAT-PROCESS       TO TRUE
                   MOVE OPDAT-MC           TO OPDAT-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-80)
           OR  (I-81)
               MOVE SPACES TO PAKKREC-IO-AREA
               INITIALIZE PAKKREC-IO-AREA
               MOVE FIRMA                  TO PAKKREC-IO-AREA (1:3)
               MOVE ORDNR                  TO PAKKREC-IO-AREA (4:6)
               MOVE KUNDNR                 TO PAKKREC-IO-AREA (10:6)
               MOVE NAVN1                  TO PAKKREC-IO-AREA (16:30)
               MOVE NAVN2                  TO PAKKREC-IO-AREA (46:30)
               MOVE ADR                    TO PAKKREC-IO-AREA (76:30)
               MOVE PNR                    TO PAKKREC-IO-AREA (106:4)
               MOVE PSTED                  TO PAKKREC-IO-AREA (110:15)
               MOVE PTYPE                  TO PAKKREC-IO-AREA (125:1)
               MOVE PDATO                  TO PAKKREC-IO-AREA (126:6)
               MOVE PTAKST                 TO PAKKREC-IO-AREA (132:1)
               MOVE PVEKT                  TO PAKKREC-IO-AREA (133:4)
               MOVE PPORTO-IO              TO PAKKREC-IO-AREA (137:6)
               MOVE STATUS-X               TO PAKKREC-IO-AREA (150:1)
               IF  (I-81)
                   MOVE VADR1              TO PAKKREC-IO-AREA (151:30)
                   INITIALIZE VADR1
               END-IF
               IF  (I-81)
                   MOVE VADR2              TO PAKKREC-IO-AREA (181:30)
                   INITIALIZE VADR2
               END-IF
               IF  (I-81)
                   MOVE VADR3              TO PAKKREC-IO-AREA (211:30)
                   INITIALIZE VADR3
               END-IF
               IF  (I-81)
                   MOVE VADR4              TO PAKKREC-IO-AREA (241:20)
                   INITIALIZE VADR4
               END-IF
               WRITE PAKKREC-IO-AREA
           END-IF
           IF  (I-80)
           OR  (I-81)
               MOVE SPACES TO TILUP-IO-AREA
               INITIALIZE TILUP-IO-AREA
               MOVE '1'                    TO TILUP-IO-AREA (1:1)
               MOVE FIRMA                  TO TILUP-IO-AREA (2:3)
               MOVE ORDNR                  TO TILUP-IO-AREA (5:6)
               MOVE UDATE                  TO TILUP-IO-AREA (15:6)
               WRITE TILUP-IO-AREA
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
           SET OPDAT-LEVEL-INIT            TO TRUE
           INITIALIZE OPDAT-DATA-FIELDS
           SET OPDAT-EOF-OFF               TO TRUE
           SET OPDAT-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO OPDAT-MC
                                              OPDAT-MP
           OPEN INPUT OPDAT
           SET ORDREM-LEVEL-INIT           TO TRUE
           SET ORDREM-AHEAD-EOF-OFF        TO TRUE
           SET ORDREM-AHEAD-READ-OFF       TO TRUE
           INITIALIZE ORDREM-DATA-FIELDS
           SET ORDREM-EOF-OFF              TO TRUE
           SET ORDREM-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO ORDREM-MC
                                              ORDREM-MP
           OPEN INPUT ORDREM
           OPEN OUTPUT PAKKREC
           OPEN OUTPUT TILUP.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE OPDAT
           CLOSE ORDREM
           CLOSE PAKKREC
           CLOSE TILUP.
 
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
