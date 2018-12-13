       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR090R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM VAR090                                              *
      *   PROGRAMMET DANNER INPUT FILE FOR SEQ.UPDATE OG NULLSTILLING *
      *   AV VARE.MASTER. INPUT TIL PROGRAM VAR100                    *
      *   INPUT ER RESTORDREDATA OG BESTILLINGSDATA.                  *
      *   12.10.94    ESPEN LARSEN                                    *
      *   14.04.97    STEIN SANDVOLD  LAGT INN AVD 5 SCANGROSS VALLØ  *
      *   11.07.00    STEIN SANDVOLD  LAGT INN AVD 2 AMFIBIUM         *
      *   11.07.00    STEIN SANDVOLD  LAGT INN AVD 4 MJØSBIL          *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR090.rpg
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
           SELECT BMASTER
               ASSIGN TO UT-S-BMASTER
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BMASTER-STATUS.
           SELECT RESTFIL
               ASSIGN TO UT-S-RESTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESTFIL-STATUS.
           SELECT UPDFILE
               ASSIGN TO UT-S-UPDFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UPDFILE-STATUS.
           SELECT UPDFIL2
               ASSIGN TO UT-S-UPDFIL2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UPDFIL2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BMASTER
               BLOCK CONTAINS 240
               RECORD CONTAINS 120.
       01  BMASTER-IO-AREA.
           05  BMASTER-IO-AREA-X           PICTURE X(120).
       FD RESTFIL
               BLOCK CONTAINS 150
               RECORD CONTAINS 15.
       01  RESTFIL-IO-AREA.
           05  RESTFIL-IO-AREA-X           PICTURE X(15).
       FD UPDFILE
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  UPDFILE-IO-AREA.
           05  UPDFILE-IO-AREA-X           PICTURE X(40).
       FD UPDFIL2
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  UPDFIL2-IO-AREA.
           05  UPDFIL2-IO-AREA-X           PICTURE X(40).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BMASTER-STATUS              PICTURE 99 VALUE 0.
           10  RESTFIL-STATUS              PICTURE 99 VALUE 0.
           10  UPDFILE-STATUS              PICTURE 99 VALUE 0.
           10  UPDFIL2-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BMASTER-EOF-OFF         VALUE '0'.
               88  BMASTER-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BMASTER-READ-OFF        VALUE '0'.
               88  BMASTER-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BMASTER-PROCESS-OFF     VALUE '0'.
               88  BMASTER-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BMASTER-LEVEL-INIT-OFF  VALUE '0'.
               88  BMASTER-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTFIL-EOF-OFF         VALUE '0'.
               88  RESTFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTFIL-READ-OFF        VALUE '0'.
               88  RESTFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTFIL-PROCESS-OFF     VALUE '0'.
               88  RESTFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESTFIL-LEVEL-INIT-OFF  VALUE '0'.
               88  RESTFIL-LEVEL-INIT      VALUE '1'.
           05  BMASTER-LEVEL-01.
               10  BMASTER-01-L2.
                   15  BMASTER-01-L2-FIRMA PICTURE X(3).
               10  BMASTER-01-L1.
                   15  BMASTER-01-L1-EDBNR PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  BMASTER-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  FABR                    PICTURE X(1).
               10  AVD-IO.
                   15  AVD                 PICTURE S9(1).
               10  LEVA-ELGR               PICTURE X(2).
               10  LEVUKE                  PICTURE X(2).
               10  BANT-IO.
                   15  BANT                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  BMASTER-MP                  PICTURE X(7).
           05  BMASTER-MC                  PICTURE X(7).
           05  BMASTER-M-01            REDEFINES BMASTER-MC.
               10  BMASTER-M-01-M2.
                   15  BMASTER-M-01-M2-FIRMA-G.
                       20  BMASTER-M-01-M2-FIRMA PICTURE X(3).
               10  BMASTER-M-01-M1.
                   15  BMASTER-M-01-M1-EDBNR-G.
                       20  BMASTER-M-01-M1-EDBNR PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  RESTFIL-LEVEL-02.
               10  RESTFIL-02-L2.
                   15  RESTFIL-02-L2-FIRMA PICTURE X(3).
               10  RESTFIL-02-L1.
                   15  RESTFIL-02-L1-EDBNR PICTURE S9(7).
           05  RESTFIL-DATA-FIELDS.
               10  RIREST-IO.
                   15  RIREST              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAVD                    PICTURE X(1).
           05  RESTFIL-MP                  PICTURE X(7).
           05  RESTFIL-MC                  PICTURE X(7).
           05  RESTFIL-M-02            REDEFINES RESTFIL-MC.
               10  RESTFIL-M-02-M2.
                   15  RESTFIL-M-02-M2-FIRMA-G.
                       20  RESTFIL-M-02-M2-FIRMA PICTURE X(3).
               10  RESTFIL-M-02-M1.
                   15  RESTFIL-M-02-M1-EDBNR-G.
                       20  RESTFIL-M-02-M1-EDBNR PICTURE S9(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  IREST-IO.
                   15  IREST               PICTURE S9(5).
               10  IREST2-IO.
                   15  IREST2              PICTURE S9(5).
               10  IREST3-IO.
                   15  IREST3              PICTURE S9(5).
               10  IREST4-IO.
                   15  IREST4              PICTURE S9(5).
               10  IREST5-IO.
                   15  IREST5              PICTURE S9(5).
               10  NULL5-IO.
                   15  NULL5               PICTURE S9(5).
               10  IBEST-IO.
                   15  IBEST               PICTURE S9(7).
               10  IBEST2-IO.
                   15  IBEST2              PICTURE S9(5).
               10  IBEST3-IO.
                   15  IBEST3              PICTURE S9(5).
               10  IBEST4-IO.
                   15  IBEST4              PICTURE S9(5).
               10  IBEST5-IO.
                   15  IBEST5              PICTURE S9(5).
               10  TILA-ELGR-IO.
                   15  TILA-ELGR           PICTURE S9(2).
               10  TILUKE-IO.
                   15  TILUKE              PICTURE S9(2).
               10  FAB                     PICTURE X(1).
           05  EDITTING-FIELDS.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BMASTER-PROCESS
               SET BMASTER-PROCESS-OFF     TO TRUE
               SET BMASTER-READ            TO TRUE
           END-IF
 
           IF  BMASTER-READ
               PERFORM BMASTER-GET
               SET BMASTER-READ-OFF        TO TRUE
               IF  NOT BMASTER-EOF
                   PERFORM BMASTER-MATCH-SET
               END-IF
           END-IF
 
           IF  RESTFIL-PROCESS
               SET RESTFIL-PROCESS-OFF     TO TRUE
               SET RESTFIL-READ            TO TRUE
           END-IF
 
           IF  RESTFIL-READ
               PERFORM RESTFIL-GET
               SET RESTFIL-READ-OFF        TO TRUE
               IF  NOT RESTFIL-EOF
                   PERFORM RESTFIL-MATCH-SET
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
 
           IF  BMASTER-PROCESS
               PERFORM BMASTER-IDSET
           END-IF
 
           IF  RESTFIL-PROCESS
               PERFORM RESTFIL-IDSET
           END-IF
 
           IF  BMASTER-PROCESS
               PERFORM BMASTER-CHK-LEVEL
           END-IF
 
           IF  RESTFIL-PROCESS
               PERFORM RESTFIL-CHK-LEVEL
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
 
           IF  BMASTER-PROCESS
               PERFORM BMASTER-FLDSET
           END-IF
 
           IF  RESTFIL-PROCESS
               PERFORM RESTFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BMASTER-PROCESS
           OR  RESTFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SET NOT-I-30                TO TRUE
               IF  FIRMA = '626'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  FIRMA = '938'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  FIRMA = '627'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  FIRMA = '732'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-97                TO TRUE
               IF  FIRMA = '975'
                   SET I-97                TO TRUE
               END-IF
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '658'
                   SET I-34                TO TRUE
               END-IF
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '956'
                   SET I-35                TO TRUE
               END-IF
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '913'
                   SET I-40                TO TRUE
               END-IF
      *****************************************************************
      *  NULLSTILLNG PR. EDB-NR.                                      *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE 0                      TO IREST
               MOVE 0                      TO IREST2
               MOVE 0                      TO IREST3
               MOVE 0                      TO IREST4
               MOVE 0                      TO IREST5
               MOVE 0                      TO NULL5
               MOVE 0                      TO IBEST
               MOVE 0                      TO IBEST2
               MOVE 0                      TO IBEST3
               MOVE 0                      TO IBEST4
               MOVE 0                      TO IBEST5
               MOVE 0                      TO TILA-ELGR
               MOVE 0                      TO TILUKE
               MOVE ' '                    TO FAB
      *****************************************************************
      *  RUTINE FOR RESTORDRE.                                        *
      *****************************************************************
           END-IF
           IF  (I-02)
               SET NOT-I-39                TO TRUE
           END-IF
           IF  (I-02 AND I-30)
               SET NOT-I-31                TO TRUE
               IF  RAVD = '2'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-35)
               SET NOT-I-31                TO TRUE
               IF  RAVD = '2'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-36                TO TRUE
               IF  RAVD = '3'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-34)
               SET NOT-I-33                TO TRUE
               IF  RAVD = '4'
                   SET I-33                TO TRUE
               END-IF
               SET NOT-I-37                TO TRUE
               IF  RAVD = '7'
                   SET I-37                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-97)
               SET NOT-I-98                TO TRUE
               IF  RAVD = '9'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31)
               OR  (I-02 AND I-33)
               OR  (I-02 AND I-96)
               SET I-39                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-39)
               ADD RIREST                  TO IREST ROUNDED
           END-IF
           IF  (I-02 AND I-96 AND I-39)
               ADD RIREST                  TO IREST5 ROUNDED
           END-IF
           IF  (I-02 AND I-31 AND I-39)
               ADD RIREST                  TO IREST2 ROUNDED
           END-IF
           IF  (I-02 AND I-35 AND I-31)
               ADD RIREST                  TO IREST2 ROUNDED
           END-IF
           IF  (I-02 AND I-97 AND I-97)
               ADD RIREST                  TO IREST2 ROUNDED
           END-IF
           IF  (I-02 AND I-35 AND I-36)
               ADD RIREST                  TO IREST3 ROUNDED
           END-IF
           IF  (I-02 AND I-33 AND I-39)
               ADD RIREST                  TO IREST4 ROUNDED
           END-IF
           IF  (I-02 AND I-37 AND I-39)
               ADD RIREST                  TO IREST2 ROUNDED
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR FORVENTET LEVERT ÅR OG UKE.                       *
      *         FØRSTE LEVERANSE SKAL LEGGE UT.                       *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-37                TO TRUE
               SET NOT-I-39                TO TRUE
               SET NOT-I-96                TO TRUE
               SET NOT-I-31                TO TRUE
               SET NOT-I-33                TO TRUE
               SET NOT-I-36                TO TRUE
           END-IF
           IF  (I-01 AND I-30)
               SET NOT-I-31                TO TRUE
               IF  AVD = 2
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-35)
               SET NOT-I-31                TO TRUE
               IF  AVD = 2
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-36                TO TRUE
               IF  AVD = 3
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-34)
               SET NOT-I-33                TO TRUE
               IF  AVD = 4
                   SET I-33                TO TRUE
               END-IF
               SET NOT-I-37                TO TRUE
               IF  AVD = 7
                   SET I-37                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-31)
               OR  (I-01 AND I-33)
               OR  (I-01 AND I-36)
               OR  (I-01 AND I-37)
               OR  (I-01 AND I-96)
               SET I-39                    TO TRUE
           END-IF
           IF  (I-01 AND I-39)
               GO TO AVD5R-T
           END-IF
           IF  (I-L1 AND I-01)
               SET NOT-I-67                TO TRUE
               IF  LEVA-ELGR < '00'
                   SET I-67                TO TRUE
               END-IF
               SET NOT-I-68                TO TRUE
               IF  LEVUKE < '00'
                   SET I-68                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-01 AND NOT-I-67)
               MOVE LEVA-ELGR              TO TILA-ELGR-IO
           END-IF
           IF  (I-L1 AND I-01 AND NOT-I-68)
               MOVE LEVUKE                 TO TILUKE-IO
      *****************************************************************
      *  RUTINE FOR FABRIKAT                                          *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-20                TO TRUE
               IF  FABR = 'F'
                   SET I-20                TO TRUE
               END-IF
               SET NOT-I-21                TO TRUE
               IF  FABR = 'G'
                   SET I-21                TO TRUE
               END-IF
               MOVE FABR                   TO FAB
           END-IF
           IF  (I-01 AND NOT-I-20 AND NOT-I-21)
               MOVE ' '                    TO FAB
      *****************************************************************
      *  RUTINE FOR BESTILLT ANTALL                                   *
      *****************************************************************
           END-IF
           IF  (I-01 AND I-20)
               ADD BANT TO ZERO        GIVING IBEST
           END-IF
           IF  (I-01 AND NOT-I-20 AND NOT-I-21)
               ADD BANT                    TO IBEST
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF.
 
       AVD5R-T.
           IF  (I-01 AND I-31)
               ADD BANT                    TO IBEST2
           END-IF
           IF  (I-01 AND I-36)
               ADD BANT                    TO IBEST3
           END-IF
           IF  (I-01 AND I-33)
               ADD BANT                    TO IBEST4
           END-IF
           IF  (I-01 AND I-96)
               ADD BANT                    TO IBEST5
           END-IF
           IF  (I-01 AND I-98)
               ADD BANT                    TO IBEST2
           END-IF
           IF  (I-01 AND I-37)
               ADD BANT                    TO IBEST2
           END-IF
           IF  (I-L1 AND I-01)
               SET NOT-I-67                TO TRUE
               IF  LEVA-ELGR < '00'
                   SET I-67                TO TRUE
               END-IF
               SET NOT-I-68                TO TRUE
               IF  LEVUKE < '00'
                   SET I-68                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-01 AND NOT-I-67)
               MOVE LEVA-ELGR              TO TILA-ELGR-IO
           END-IF
           IF  (I-L1 AND I-01 AND NOT-I-68)
               MOVE LEVUKE                 TO TILUKE-IO
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       BMASTER-GET SECTION.
       BMASTER-GET-P.
           IF  BMASTER-EOF-OFF
               READ BMASTER
               AT END
                   SET BMASTER-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BMASTER-FLDSET SECTION.
       BMASTER-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BMASTER-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE BMASTER-IO-AREA (87:4) TO EDBNR-IO
               MOVE BMASTER-IO-AREA (5:1)  TO FABR (1:1)
               MOVE BMASTER-IO-AREA (6:1)  TO AVD-IO
               INSPECT AVD-IO REPLACING ALL ' ' BY '0'
               MOVE BMASTER-IO-AREA (23:2) TO LEVA-ELGR (1:2)
               MOVE BMASTER-IO-AREA (25:2) TO LEVUKE (1:2)
               MOVE BMASTER-IO-AREA (101:4) TO BANT-IO
           END-EVALUATE.
 
       BMASTER-IDSET SECTION.
       BMASTER-IDSET-P.
           SET I-01                        TO TRUE.
 
       BMASTER-CHK-LEVEL SECTION.
       BMASTER-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BMASTER-LEVEL-01
               MOVE BMASTER-IO-AREA (2:3)  TO BMASTER-01-L2-FIRMA
               MOVE BMASTER-IO-AREA (87:4) TO BMASTER-01-L1-EDBNR
               IF  BMASTER-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BMASTER-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  BMASTER-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BMASTER-01-L2         TO THE-PRIOR-L2
               MOVE  BMASTER-01-L1         TO THE-PRIOR-L1
               SET BMASTER-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       BMASTER-MATCH-SET SECTION.
       BMASTER-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE BMASTER-IO-AREA (2:3)  TO BMASTER-M-01-M2-FIRMA
               MOVE BMASTER-IO-AREA (87:4) TO BMASTER-M-01-M1-EDBNR-G
           END-EVALUATE.
 
       RESTFIL-GET SECTION.
       RESTFIL-GET-P.
           IF  RESTFIL-EOF-OFF
               READ RESTFIL
               AT END
                   SET RESTFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESTFIL-FLDSET SECTION.
       RESTFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESTFIL-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE RESTFIL-IO-AREA (4:7)  TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE RESTFIL-IO-AREA (11:4) TO RIREST-IO
               MOVE RESTFIL-IO-AREA (15:1) TO RAVD (1:1)
           END-EVALUATE.
 
       RESTFIL-IDSET SECTION.
       RESTFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       RESTFIL-CHK-LEVEL SECTION.
       RESTFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESTFIL-LEVEL-02
               MOVE RESTFIL-IO-AREA (1:3)  TO RESTFIL-02-L2-FIRMA
               MOVE RESTFIL-IO-AREA (4:7)  TO RESTFIL-02-L1-EDBNR
               IF  RESTFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESTFIL-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESTFIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESTFIL-02-L2         TO THE-PRIOR-L2
               MOVE  RESTFIL-02-L1         TO THE-PRIOR-L1
               SET RESTFIL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESTFIL-MATCH-SET SECTION.
       RESTFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESTFIL-IO-AREA (1:3)  TO RESTFIL-M-02-M2-FIRMA
               MOVE RESTFIL-IO-AREA (4:7)  TO RESTFIL-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  BMASTER-EOF
               MOVE HIGH-VALUES            TO BMASTER-MC
                                              BMASTER-MP
           END-IF
           IF  RESTFIL-EOF
               MOVE HIGH-VALUES            TO RESTFIL-MC
                                              RESTFIL-MP
           END-IF
           IF  BMASTER-MC < BMASTER-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESTFIL-MC < RESTFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  BMASTER-MC < RESTFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BMASTER-PROCESS     TO TRUE
                   MOVE BMASTER-MC         TO BMASTER-MP
                   IF  BMASTER-MC = RESTFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESTFIL-MC < BMASTER-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESTFIL-PROCESS     TO TRUE
                   MOVE RESTFIL-MC         TO RESTFIL-MP
                   IF  RESTFIL-MC = BMASTER-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  BMASTER-MC = RESTFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BMASTER-PROCESS     TO TRUE
                   MOVE BMASTER-MC         TO BMASTER-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO UPDFILE-IO-AREA
               INITIALIZE UPDFILE-IO-AREA
               MOVE FIRMA                  TO UPDFILE-IO-AREA (1:3)
               MOVE EDBNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO UPDFILE-IO-AREA (4:7)
               MOVE IREST                  TO XO-50P
               MOVE XO-50P-EF              TO UPDFILE-IO-AREA (11:3)
               MOVE IBEST                  TO XO-70P
               MOVE XO-70P-EF              TO UPDFILE-IO-AREA (14:4)
               MOVE TILA-ELGR-IO           TO UPDFILE-IO-AREA (18:2)
               MOVE TILUKE-IO              TO UPDFILE-IO-AREA (20:2)
               MOVE FAB                    TO UPDFILE-IO-AREA (22:1)
               WRITE UPDFILE-IO-AREA
           END-IF
           IF  (I-L1 AND I-40)
               MOVE SPACES TO UPDFILE-IO-AREA
               INITIALIZE UPDFILE-IO-AREA
               MOVE '922'                  TO UPDFILE-IO-AREA (1:3)
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO UPDFILE-IO-AREA (4:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO UPDFILE-IO-AREA (4:7)
               END-IF
               MOVE IREST                  TO XO-50P
               MOVE XO-50P-EF              TO UPDFILE-IO-AREA (11:3)
               MOVE IBEST                  TO XO-70P
               MOVE XO-70P-EF              TO UPDFILE-IO-AREA (14:4)
               MOVE TILA-ELGR-IO           TO UPDFILE-IO-AREA (18:2)
               MOVE TILUKE-IO              TO UPDFILE-IO-AREA (20:2)
               MOVE FAB                    TO UPDFILE-IO-AREA (22:1)
               WRITE UPDFILE-IO-AREA
           END-IF
           IF  (I-L1 AND I-30)
           OR  (I-L1 AND I-34)
           OR  (I-L1 AND I-35)
               MOVE SPACES TO UPDFIL2-IO-AREA
               INITIALIZE UPDFIL2-IO-AREA
               MOVE '81'                   TO UPDFIL2-IO-AREA (1:2)
               MOVE FIRMA                  TO UPDFIL2-IO-AREA (3:3)
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO UPDFIL2-IO-AREA (6:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO UPDFIL2-IO-AREA (6:7)
               END-IF
               MOVE IREST2                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (13:3)
               MOVE IBEST2                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (16:3)
               MOVE IREST3                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (19:3)
               MOVE IBEST3                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (22:3)
               MOVE IREST4                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (25:3)
               MOVE IBEST4                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (28:3)
               MOVE IREST5                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (31:3)
               MOVE IBEST5                 TO XO-50P
               MOVE XO-50P-EF              TO UPDFIL2-IO-AREA (34:3)
               WRITE UPDFIL2-IO-AREA
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
           SET BMASTER-LEVEL-INIT          TO TRUE
           INITIALIZE BMASTER-DATA-FIELDS
           SET BMASTER-EOF-OFF             TO TRUE
           SET BMASTER-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO BMASTER-MC
                                              BMASTER-MP
           OPEN INPUT BMASTER
           SET RESTFIL-LEVEL-INIT          TO TRUE
           INITIALIZE RESTFIL-DATA-FIELDS
           SET RESTFIL-EOF-OFF             TO TRUE
           SET RESTFIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESTFIL-MC
                                              RESTFIL-MP
           OPEN INPUT RESTFIL
           OPEN OUTPUT UPDFILE
           OPEN OUTPUT UPDFIL2.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BMASTER
           CLOSE RESTFIL
           CLOSE UPDFILE
           CLOSE UPDFIL2.
 
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
