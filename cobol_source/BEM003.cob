       IDENTIFICATION DIVISION.
       PROGRAM-ID. BEM003R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: BEM003                                          *
      *  PROGRAMERER: ELIN                                            *
      *  PROGRAMERT.: 01.10.2007                                      *
      *  PROGRAMMET.: UTLIST AV KAMMAST, BÅDE LISTE OG FIL.           *
      *               + UTLIST AV RABBMAST                            *
      *  ENDR.DATO   TEKST.                                           *
      *  XX.XX.XXXX :                                                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BEM003.rpg
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
           SELECT KAMMAST
               ASSIGN TO KAMMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KAMMAST-STATUS
               RECORD KEY IS KAMMAST-KEY1.
           SELECT RABMAST
               ASSIGN TO RABMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RABMAST-STATUS
               RECORD KEY IS RABMAST-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT OUTHOST
               ASSIGN TO OUTHOST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTHOST-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KAMMAST
               RECORD CONTAINS 40.
       01  KAMMAST-IO-AREA.
           05  KAMMAST-IO-AREA-X.
               10  KAMMAST-KEY1.
                   15  KAMMAST-KEY1N       PICTURE S9(13).
               10  FILLER                  PICTURE X(27).
       FD RABMAST
               RECORD CONTAINS 40.
       01  RABMAST-IO-AREA.
           05  RABMAST-IO-AREA-X.
               10  RABMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(20).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD OUTHOST
               RECORD CONTAINS 100.
       01  OUTHOST-IO-AREA.
           05  OUTHOST-IO-AREA-X           PICTURE X(100).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KAMMAST-STATUS              PICTURE 99 VALUE 0.
           10  RABMAST-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTHOST-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KAMMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMAST-EOF-OFF         VALUE '0'.
               88  KAMMAST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMAST-READ-OFF        VALUE '0'.
               88  KAMMAST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMAST-PROCESS-OFF     VALUE '0'.
               88  KAMMAST-PROCESS         VALUE '1'.
           05  RABMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RABMAST-EOF-OFF         VALUE '0'.
               88  RABMAST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RABMAST-READ-OFF        VALUE '0'.
               88  RABMAST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RABMAST-PROCESS-OFF     VALUE '0'.
               88  RABMAST-PROCESS         VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LISTE-DATA-FIELDS.
               10  LISTE-AFTER-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-AFTER-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-MAX-LINES         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-LINE-COUNT        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-CLR-IO            PICTURE X VALUE 'Y'.
           05  KAMMAST-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KNR                     PICTURE X(6).
               10  KNRA                    PICTURE X(1).
               10  FRADTO-IO.
                   15  FRADTO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  TILDTO-IO.
                   15  TILDTO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RABTIL                  PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
           05  RABMAST-DATA-FIELDS.
               10  RABREC                  PICTURE X(40).
               10  RECART                  PICTURE X(1).
               10  KNR1                    PICTURE X(1).
               10  VRG                     PICTURE X(5).
               10  ALFAR                   PICTURE X(3).
      *                                      19  21 NTOREC
      *                                      28  301RAB3
               10  NTOFAK-IO.
                   15  NTOFAK              PICTURE S9(1)V9(4).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VGR                     PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  EDB1                    PICTURE X(7).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  EDB2-IO.
                   15  EDB2                PICTURE S9(7).
               10  EDBNR1-IO.
                   15  EDBNR1              PICTURE S9(7).
               10  KEY-X                   PICTURE X(10).
               10  FRA1                    PICTURE X(6).
               10  FRADTO-N-IO.
                   15  FRADTO-N            PICTURE S9(7).
               10  FRA2-IO.
                   15  FRA2                PICTURE S9(6).
               10  FDTO-IO.
                   15  FDTO                PICTURE S9(6).
               10  FDTOAA-IO.
                   15  FDTOAA              PICTURE S9(2).
               10  FDTO4-IO.
                   15  FDTO4               PICTURE S9(4).
               10  FDTODD-IO.
                   15  FDTODD              PICTURE S9(2).
               10  FDTOMM-IO.
                   15  FDTOMM              PICTURE S9(2).
               10  TIL1                    PICTURE X(6).
               10  TILDTO-N-IO.
                   15  TILDTO-N            PICTURE S9(7).
               10  TIL2-IO.
                   15  TIL2                PICTURE S9(6).
               10  TDTO-IO.
                   15  TDTO                PICTURE S9(6).
               10  TDTOAA-IO.
                   15  TDTOAA              PICTURE S9(2).
               10  TDTO4-IO.
                   15  TDTO4               PICTURE S9(4).
               10  TDTODD-IO.
                   15  TDTODD              PICTURE S9(2).
               10  TDTOMM-IO.
                   15  TDTOMM              PICTURE S9(2).
               10  PRIS1-IO.
                   15  PRIS1               PICTURE S9(7)V9(2).
               10  PRIS-N-IO.
                   15  PRIS-N              PICTURE S9(7)V9(2).
               10  PRIS2-IO.
                   15  PRIS2               PICTURE S9(7)V9(2).
               10  ANT1                    PICTURE X(4).
               10  ANT-N-IO.
                   15  ANT-N               PICTURE S9(5).
               10  ANT2-IO.
                   15  ANT2                PICTURE S9(4).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(4).
               10  ANTRAB-IO.
                   15  ANTRAB              PICTURE S9(4).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-40YY9                PICTURE Z.ZZ9.
               10  XO-21YY9                PICTURE ZZ,9.
               10  EDIT-RAB1               PICTURE Z9,9.
               10  EDIT-RAB2               PICTURE Z9,9.
               10  EDIT-PRIS2              PICTURE Z999999,99.
               10  EDIT-ANTALL             PICTURE Z999.
               10  EDIT-NULL9              PICTURE Z999999,99.
               10  EDIT-ANTRAB             PICTURE Z999.
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
               88  NOT-SET-I-OF            VALUE '0'.
               88  SET-I-OF                VALUE '1'.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KAMMAST-PROCESS
               SET KAMMAST-PROCESS-OFF     TO TRUE
               SET KAMMAST-READ            TO TRUE
           END-IF
 
           IF  KAMMAST-READ
           AND RECORD-SELECTED-OFF
               PERFORM KAMMAST-GET
               SET KAMMAST-READ-OFF        TO TRUE
               IF  NOT KAMMAST-EOF
                   SET KAMMAST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RABMAST-PROCESS
               SET RABMAST-PROCESS-OFF     TO TRUE
               SET RABMAST-READ            TO TRUE
           END-IF
 
           IF  RABMAST-READ
           AND RECORD-SELECTED-OFF
               PERFORM RABMAST-GET
               SET RABMAST-READ-OFF        TO TRUE
               IF  NOT RABMAST-EOF
                   SET RABMAST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KAMMAST-PROCESS
               PERFORM KAMMAST-IDSET
           END-IF
 
           IF  RABMAST-PROCESS
               PERFORM RABMAST-IDSET
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
 
           IF  KAMMAST-PROCESS
               PERFORM KAMMAST-FLDSET
           END-IF
 
           IF  RABMAST-PROCESS
               PERFORM RABMAST-FLDSET
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
           SET NOT-I-10                    TO TRUE
      *                    Z-ADD0         NULL9   90
           SET NOT-I-11                    TO TRUE
           IF  FIRMA = '764'
               SET I-11                    TO TRUE
           END-IF
           IF  (NOT-I-11)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-12                TO TRUE
               IF  RECART = '8'
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-91                TO TRUE
               IF  KNR1 < '0'
                   SET I-91                TO TRUE
               END-IF
      *  03N12             GOTO SLUTT
           END-IF
           IF  (I-03)
               GO TO ENDRAB-T
           END-IF
           MOVE EDBNR                      TO EDBNR-N
           MOVE EDBNR-N-IO                 TO EDB1
           MOVE EDB1                       TO EDB2-IO
           ADD EDB2 TO ZERO            GIVING EDBNR1
      *
           IF  (I-01)
               SET NOT-I-92                TO TRUE
               IF  KNRA < '0'
                   SET I-92                TO TRUE
               END-IF
               MOVE FIRMA                  TO KEY-X (1:3)
               MOVE EDB1                   TO KEY-X (4:7)
               MOVE KEY-X                  TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-30                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-30            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-30)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  TILDTO < UDATE
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO SLUTT-T
           END-IF
           MOVE FRADTO                     TO FRADTO-N
           MOVE FRADTO-N-IO (2:6)          TO FRA1
           MOVE FRA1                       TO FRA2-IO
           ADD FRA2 TO ZERO            GIVING FDTO
           MOVE FRA2 (5:2)                 TO FDTOAA-IO
           MOVE FRA2 (1:4)                 TO FDTO4
           MOVE FDTO4 (1:2)                TO FDTODD
           MOVE FDTO4 (3:2)                TO FDTOMM-IO
      *
           MOVE TILDTO                     TO TILDTO-N
           MOVE TILDTO-N-IO (2:6)          TO TIL1
           MOVE TIL1                       TO TIL2-IO
           ADD TIL2 TO ZERO            GIVING TDTO
           MOVE TIL2 (5:2)                 TO TDTOAA-IO
           MOVE TIL2 (1:4)                 TO TDTO4
           MOVE TDTO4 (1:2)                TO TDTODD
           MOVE TDTO4 (3:2)                TO TDTOMM-IO
      *
           MOVE PRIS                       TO PRIS-N
           MOVE PRIS-N-IO                  TO PRIS1-IO
           ADD PRIS1 TO ZERO           GIVING PRIS2
      *
           SET NOT-I-32                    TO TRUE
           IF  RABTIL = 'N'
               SET I-32                    TO TRUE
           END-IF
           MOVE ANT                        TO ANT-N
           MOVE ANT-N-IO (2:4)             TO ANT1
           MOVE ANT1                       TO ANT2-IO
           ADD ANT2 TO ZERO            GIVING ANTALL.
 
       ENDRAB-T.
           MOVE 1                          TO ANTRAB
           MOVE 0                          TO NULL9
           SET I-10                        TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
       KAMMAST-GET SECTION.
       KAMMAST-GET-P.
           IF  KAMMAST-EOF-OFF
               READ KAMMAST
               AT END
                   SET KAMMAST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KAMMAST-FLDSET SECTION.
       KAMMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KAMMAST-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KAMMAST-IO-AREA (4:4)  TO EDBNR-IO
               MOVE KAMMAST-IO-AREA (8:6)  TO KNR (1:6)
               MOVE KAMMAST-IO-AREA (8:1)  TO KNRA (1:1)
               MOVE KAMMAST-IO-AREA (14:4) TO FRADTO-IO
               MOVE KAMMAST-IO-AREA (18:4) TO TILDTO-IO
               MOVE KAMMAST-IO-AREA (22:5) TO PRIS-IO
               MOVE KAMMAST-IO-AREA (27:1) TO RABTIL (1:1)
               MOVE KAMMAST-IO-AREA (28:3) TO ANT-IO
               MOVE KAMMAST-IO-AREA (31:2) TO RAB1-IO
               MOVE KAMMAST-IO-AREA (33:2) TO RAB2-IO
           END-EVALUATE.
 
       KAMMAST-IDSET SECTION.
       KAMMAST-IDSET-P.
           SET I-01                        TO TRUE.
 
       RABMAST-GET SECTION.
       RABMAST-GET-P.
           IF  RABMAST-EOF-OFF
               READ RABMAST
               AT END
                   SET RABMAST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RABMAST-FLDSET SECTION.
       RABMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RABMAST-IO-AREA (1:40) TO RABREC (1:40)
               MOVE RABMAST-IO-AREA (1:1)  TO RECART (1:1)
               MOVE RABMAST-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE RABMAST-IO-AREA (5:6)  TO KNR (1:6)
               MOVE RABMAST-IO-AREA (5:1)  TO KNR1 (1:1)
               MOVE RABMAST-IO-AREA (11:5) TO VRG (1:5)
               MOVE RABMAST-IO-AREA (16:3) TO ALFAR (1:3)
               MOVE RABMAST-IO-AREA (22:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (25:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (31:5) TO NTOFAK-IO
               INSPECT NTOFAK-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RABMAST-IDSET SECTION.
       RABMAST-IDSET-P.
           SET I-03                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       LISTE-PRINT-LINE SECTION.
       LISTE-PRINT-LINE-P.
           IF  LISTE-BEFORE-SKIP > 0
               PERFORM LISTE-SKIP-BEFORE
           END-IF
           IF  LISTE-BEFORE-SPACE > 0
               PERFORM LISTE-SPACE-BEFORE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               IF  LISTE-AFTER-SPACE > 0
                   PERFORM LISTE-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               PERFORM LISTE-SPACE-AFTER
           END-IF
           IF  LISTE-LINE-COUNT NOT < LISTE-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       LISTE-SKIP-BEFORE SECTION.
       LISTE-SKIP-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-BEFORE-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-BEFORE SECTION.
       LISTE-SPACE-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER LISTE-BEFORE-SPACE LINES
           ADD LISTE-BEFORE-SPACE          TO LISTE-LINE-COUNT
           MOVE SPACES TO LISTE-IO-AREA
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-BEFORE-SPACE.
 
       LISTE-SKIP-AFTER SECTION.
       LISTE-SKIP-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-AFTER-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-AFTER SECTION.
       LISTE-SPACE-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE LISTE-AFTER-SPACE LINES
           ADD LISTE-AFTER-SPACE           TO LISTE-LINE-COUNT
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE EDB1                   TO LISTE-IO-AREA (5:7)
               MOVE KNR                    TO LISTE-IO-AREA (13:6)
               MOVE FRADTO                 TO XO-70U
               MOVE XO-70U (1:7)           TO LISTE-IO-AREA (19:7)
               MOVE UDATE                  TO LISTE-IO-AREA (27:6)
               MOVE PRIS2                  TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (32:12)
               MOVE RABTIL                 TO LISTE-IO-AREA (45:1)
               MOVE ANTALL                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (46:5)
               MOVE RAB1                   TO XO-21YY9
               MOVE XO-21YY9               TO LISTE-IO-AREA (52:4)
               MOVE RAB2                   TO XO-21YY9
               MOVE XO-21YY9               TO LISTE-IO-AREA (57:4)
               IF  (NOT-I-12)
                   MOVE 'NTO'              TO LISTE-IO-AREA (63:3)
               END-IF
               IF  (NOT-I-12)
                   MOVE NTOFAK-IO          TO LISTE-IO-AREA (67:5)
      *UTHOST H        1P
      *                                   6 "KNR/RG"
      *                                   7 ";"
      *                                  12 "VGR."
      *                                  13 ";"
      *                                  16 "ALF"
      *                                  17 " "
      *                                  37 "ARTIKKELNR."
      *                                  38 ";"
      *                                  42 "RAB1"
      *                                  43 ";"
      *                                  47 "RAB2"
      *                                  48 ";"
      *                                  58 "PRIS      "
      *                                  59 ";"
      *                                  63 "ANT."
      *                                  64 ";"
      *                                  72 "FRA DAT"
      *                                  73 ";"
      *                                  81 "TIL DAT"
      *                                  82 ";"
      *                                  85 "NTO"
      *                                  86 ";"
      *                                  91 "NTO.FAK"
      *                                  92 ";"
      *                                  95 "FNR"
      *                                  96 ";"
      *                                  97 "R"
      *                                  98 ";"
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-10)
               MOVE SPACES TO OUTHOST-IO-AREA
               INITIALIZE OUTHOST-IO-AREA
               MOVE KNR                    TO OUTHOST-IO-AREA (1:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (7:1)
               MOVE VGR                    TO OUTHOST-IO-AREA (8:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (13:1)
               IF  (NOT-I-30)
                   MOVE ALFA               TO OUTHOST-IO-AREA (14:3)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (17:1)
               IF  (NOT-I-30)
                   MOVE ARTNR              TO OUTHOST-IO-AREA (18:20)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (38:1)
               IF  (I-32)
                   MOVE '0 , '             TO OUTHOST-IO-AREA (39:4)
               END-IF
               IF  (NOT-I-32)
                   MOVE RAB1               TO EDIT-RAB1
                   MOVE EDIT-RAB1          TO OUTHOST-IO-AREA (39:4)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (43:1)
               IF  (I-32)
                   MOVE '0 , '             TO OUTHOST-IO-AREA (44:4)
               END-IF
               IF  (NOT-I-32)
                   MOVE RAB2               TO EDIT-RAB2
                   MOVE EDIT-RAB2          TO OUTHOST-IO-AREA (44:4)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (48:1)
               MOVE PRIS2                  TO EDIT-PRIS2
               MOVE EDIT-PRIS2             TO OUTHOST-IO-AREA (49:10)
               MOVE ';'                    TO OUTHOST-IO-AREA (59:1)
               MOVE ANTALL                 TO EDIT-ANTALL
               MOVE EDIT-ANTALL            TO OUTHOST-IO-AREA (60:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (64:1)
               MOVE FDTODD-IO              TO OUTHOST-IO-AREA (65:2)
               MOVE '.'                    TO OUTHOST-IO-AREA (67:1)
               MOVE FDTOMM-IO              TO OUTHOST-IO-AREA (68:2)
               MOVE '.'                    TO OUTHOST-IO-AREA (70:1)
               MOVE FDTOAA-IO              TO OUTHOST-IO-AREA (71:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (73:1)
               MOVE TDTODD-IO              TO OUTHOST-IO-AREA (74:2)
               MOVE '.'                    TO OUTHOST-IO-AREA (76:1)
               MOVE TDTOMM-IO              TO OUTHOST-IO-AREA (77:2)
               MOVE '.'                    TO OUTHOST-IO-AREA (79:1)
               MOVE TDTOAA-IO              TO OUTHOST-IO-AREA (80:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (82:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (86:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (92:1)
               MOVE FIRMA                  TO OUTHOST-IO-AREA (93:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (96:1)
               MOVE '0'                    TO OUTHOST-IO-AREA (97:1)
               IF  (I-92)
                   MOVE '1'                TO OUTHOST-IO-AREA (97:1)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (98:1)
               WRITE OUTHOST-IO-AREA
           END-IF
           IF  (I-03 AND I-10)
               MOVE SPACES TO OUTHOST-IO-AREA
               INITIALIZE OUTHOST-IO-AREA
               MOVE KNR                    TO OUTHOST-IO-AREA (1:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (7:1)
               MOVE VRG                    TO OUTHOST-IO-AREA (8:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (13:1)
               MOVE ALFAR                  TO OUTHOST-IO-AREA (14:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (17:1)
      *                     N30ARTNR     37
               MOVE ';'                    TO OUTHOST-IO-AREA (38:1)
               MOVE RAB1                   TO EDIT-RAB1
               MOVE EDIT-RAB1              TO OUTHOST-IO-AREA (39:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (43:1)
               MOVE RAB2                   TO EDIT-RAB2
               MOVE EDIT-RAB2              TO OUTHOST-IO-AREA (44:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (48:1)
               MOVE NULL9                  TO EDIT-NULL9
               MOVE EDIT-NULL9             TO OUTHOST-IO-AREA (49:10)
               MOVE ';'                    TO OUTHOST-IO-AREA (59:1)
               MOVE ANTRAB                 TO EDIT-ANTRAB
               MOVE EDIT-ANTRAB            TO OUTHOST-IO-AREA (60:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (64:1)
      *                        FDTODDX   66
      *                                  67 "."
      *                        FDTOMMX   69
      *                                  70 "."
      *                        FDTOAAX   72
               MOVE ';'                    TO OUTHOST-IO-AREA (73:1)
      *                        TDTODDX   75
      *                                  76 "."
      *                        TDTOMMX   78
      *                                  79 "."
      *                        TDTOAAX   81
               MOVE ';'                    TO OUTHOST-IO-AREA (82:1)
               IF  (NOT-I-12)
                   MOVE 'NTO'              TO OUTHOST-IO-AREA (83:3)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (86:1)
               IF  (NOT-I-12)
                   MOVE NTOFAK-IO          TO OUTHOST-IO-AREA (87:5)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (92:1)
               MOVE FIRMA                  TO OUTHOST-IO-AREA (93:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (96:1)
               MOVE '2'                    TO OUTHOST-IO-AREA (97:1)
               IF  (I-91)
                   MOVE '3'                TO OUTHOST-IO-AREA (97:1)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (98:1)
               WRITE OUTHOST-IO-AREA
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
           INITIALIZE KAMMAST-DATA-FIELDS
           SET KAMMAST-EOF-OFF             TO TRUE
           SET KAMMAST-PROCESS             TO TRUE
           OPEN INPUT KAMMAST
           INITIALIZE RABMAST-DATA-FIELDS
           SET RABMAST-EOF-OFF             TO TRUE
           SET RABMAST-PROCESS             TO TRUE
           OPEN INPUT RABMAST
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTHOST
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KAMMAST
           CLOSE RABMAST
           CLOSE VAREMAS
           CLOSE OUTHOST
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
