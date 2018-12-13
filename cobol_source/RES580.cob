       IDENTIFICATION DIVISION.
       PROGRAM-ID. RES580R.
      **********************************************  Z-WIN-RPG2P     *
      *  PROGRAM.......: RES580                                       *
      *  BEREGNE SALDO PR KUNDE FOR IKKE FAKTURERTE ORDRE.      *
      * 10.05.17 TM: HOPPER OVER BK="T"                         *
      * 06.04.93 EL: LAGT INN TEST PÅ KONTANT OG OPPKRAV        *
      *              INKL. PRISTILEGG (PANT,AVGIFT OL.)         *
      * 20.02.02 EL: TEST PÅ KONSERN FIRMANR.                   *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RES580.rpg
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
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT SALDOF
               ASSIGN TO UT-S-SALDOF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SALDOF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD SALDOF
               BLOCK CONTAINS 40
               RECORD CONTAINS 20.
       01  SALDOF-IO-AREA.
           05  SALDOF-IO-AREA-X            PICTURE X(20).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  SALDOF-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
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
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  ORDREM-LEVEL-01.
               10  ORDREM-01-L2.
                   15  ORDREM-01-L2-FIRMA  PICTURE X(3).
               10  ORDREM-01-L1.
                   15  ORDREM-01-L1-KUNDE  PICTURE X(6).
           05  ORDREM-DATA-FIELDS.
               10  KUNDE                   PICTURE X(6).
               10  RESKGR                  PICTURE X(1).
               10  BK                      PICTURE X(1).
               10  BETBET                  PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  TYPE-X                  PICTURE X(1).
               10  LEVANT-IO.
                   15  LEVANT              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  NETSUM-IO.
                   15  NETSUM              PICTURE S9(7)V9(2).
               10  EDBNR3-IO.
                   15  EDBNR3              PICTURE S9(3).
               10  EDBNR2-IO.
                   15  EDBNR2              PICTURE S9(2).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-09                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREM-PROCESS
               SET ORDREM-PROCESS-OFF      TO TRUE
               SET ORDREM-READ             TO TRUE
           END-IF
 
           IF  ORDREM-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREM-GET
               SET ORDREM-READ-OFF         TO TRUE
               IF  NOT ORDREM-EOF
                   SET ORDREM-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-IDSET
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-CHK-LEVEL
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
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDREM-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-03)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-19                TO TRUE
               IF  KONFNR > '000'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-60                TO TRUE
               MOVE 0                      TO BEL
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-52                TO TRUE
               IF  RESKGR < '5'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-52)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-54                TO TRUE
               IF  BK = 'T'
                   SET I-54                TO TRUE
               END-IF
           END-IF
           IF  (I-54)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               SET NOT-I-53                TO TRUE
               IF  BETBET = '07'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-53)
               SET NOT-I-53                TO TRUE
               IF  BETBET = '47'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-53)
               SET NOT-I-53                TO TRUE
               IF  BETBET = '14'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-53)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-01)
               SET NOT-I-20                TO TRUE
               IF  TYPE-X = 'S'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  TYPE-X = 'O'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               IF  TYPE-X = 'K'
                   SET I-21                TO TRUE
               END-IF
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-02 AND NOT-I-20 AND NOT-I-21)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               PERFORM BEREGN-S
           END-IF
           IF  (I-02 AND I-20)
               ADD NETSUM                  TO BEL
           END-IF
           IF  (I-02 AND I-21)
               SUBTRACT NETSUM             FROM BEL
           END-IF
           IF  (I-02)
               SET I-60                    TO TRUE
      *
           END-IF
           .
 
       SLUTT-T.
      ******************************************************
      *  SUBRUTINE FOR BEREGNING AV BELØP PR VARELINJE.    *
      ******************************************************
           CONTINUE.
 
       BEREGN-S SECTION.
       BEREGN-S-P.
           SUBTRACT NETSUM                 FROM NETSUM
      *  SNU BELØP DERSOM KREDIT-GEBYR ELLER ANNEN RETUR.
           SET NOT-I-90                    TO TRUE
           IF  EDBNR = 0
               SET I-90                    TO TRUE
           END-IF
           IF  (I-90)
               GO TO END-X-T
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  LEVANT = 0
               SET I-82                    TO TRUE
           END-IF
           IF  (I-82)
               GO TO END-X-T
           END-IF
           DIVIDE EDBNR BY 10000       GIVING EDBNR3
           DIVIDE EDBNR BY 100000      GIVING EDBNR2
           SET NOT-I-98                    TO TRUE
           IF  EDBNR2 = 94
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  EDBNR3 = 995
                   SET I-98                TO TRUE
               END-IF
           END-IF
           MULTIPLY LEVANT BY PRIS     GIVING NETTO ROUNDED
           IF  (I-98)
               MULTIPLY -1 BY NETTO    GIVING NETTO
           END-IF
           MULTIPLY RAB1 BY NETTO      GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY RAB2 BY NETTO      GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY RAB3 BY NETTO      GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL FROM NETTO  GIVING NETSUM
      *  LEGG TIL PRISTILLEGG (PANT,AVGIFT OL).
           MULTIPLY LEVANT BY PRITIL   GIVING NETTO ROUNDED
           ADD NETTO                       TO NETSUM.
 
       END-X-T.
           CONTINUE.
      ******************************************************
      *
 
       ORDREM-GET SECTION.
       ORDREM-GET-P.
           IF  ORDREM-EOF-OFF
               READ ORDREM
               AT END
                   SET ORDREM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (21:6)  TO KUNDE (1:6)
               MOVE ORDREM-IO-AREA (21:1)  TO RESKGR (1:1)
               MOVE ORDREM-IO-AREA (92:1)  TO BK (1:1)
               MOVE ORDREM-IO-AREA (94:2)  TO BETBET (1:2)
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (157:1) TO TYPE-X (1:1)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREM-IO-AREA (29:4)  TO LEVANT-IO
               MOVE ORDREM-IO-AREA (87:4)  TO EDBNR-IO
               MOVE ORDREM-IO-AREA (94:5)  TO PRIS-IO
               MOVE ORDREM-IO-AREA (99:2)  TO RAB1-IO
               MOVE ORDREM-IO-AREA (101:2) TO RAB2-IO
               MOVE ORDREM-IO-AREA (103:2) TO RAB3-IO
               MOVE ORDREM-IO-AREA (126:4) TO PRITIL-IO
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET I-02                    TO TRUE
           WHEN  OTHER
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
               MOVE ORDREM-IO-AREA (21:6)  TO ORDREM-01-L1-KUNDE
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
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-09                        TO TRUE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-60)
               MOVE SPACES TO SALDOF-IO-AREA
               INITIALIZE SALDOF-IO-AREA
               MOVE '3'                    TO SALDOF-IO-AREA (1:1)
               MOVE FIRMA                  TO SALDOF-IO-AREA (2:3)
               IF  (I-19)
                   MOVE KONFNR             TO SALDOF-IO-AREA (2:3)
               END-IF
               MOVE KUNDE                  TO SALDOF-IO-AREA (5:6)
               MOVE BEL                    TO XO-72P
               MOVE XO-72P-EF              TO SALDOF-IO-AREA (11:5)
               INITIALIZE BEL
               WRITE SALDOF-IO-AREA
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
           SET ORDREM-LEVEL-INIT           TO TRUE
           INITIALIZE ORDREM-DATA-FIELDS
           SET ORDREM-EOF-OFF              TO TRUE
           SET ORDREM-PROCESS              TO TRUE
           OPEN INPUT ORDREM
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT SALDOF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDREM
           CLOSE FIRMAF
           CLOSE SALDOF.
 
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
