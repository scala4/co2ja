       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA120R.
      **********************************************  Z-WIN-RPG2   ****
      *E 12.09.02: NULLER UT VARIABLE VED L1.
      *E 11.06.03: UTVIDET TELLER TIL 3 SIFFER. PAKKET I OUTPUT.
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA120.rpg
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
           SELECT INF
               ASSIGN TO UT-S-INF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF-STATUS.
           SELECT MASTER
               ASSIGN TO UT-S-MASTER
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTER-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INF
               BLOCK CONTAINS 4100
               RECORD CONTAINS 20.
       01  INF-IO-AREA.
           05  INF-IO-AREA-X               PICTURE X(20).
       FD MASTER
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  MASTER-IO-AREA.
           05  MASTER-IO-AREA-X            PICTURE X(80).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD OUTF
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INF-STATUS                  PICTURE 99 VALUE 0.
           10  MASTER-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-EOF-OFF             VALUE '0'.
               88  INF-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-READ-OFF            VALUE '0'.
               88  INF-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-PROCESS-OFF         VALUE '0'.
               88  INF-PROCESS             VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INF-LEVEL-INIT-OFF      VALUE '0'.
               88  INF-LEVEL-INIT          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTER-EOF-OFF          VALUE '0'.
               88  MASTER-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTER-READ-OFF         VALUE '0'.
               88  MASTER-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTER-PROCESS-OFF      VALUE '0'.
               88  MASTER-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MASTER-LEVEL-INIT-OFF   VALUE '0'.
               88  MASTER-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  INF-LEVEL-01.
               10  INF-01-L2.
                   15  INF-01-L2-FIRMA     PICTURE X(3).
               10  INF-01-L1.
                   15  INF-01-L1-VGR       PICTURE X(5).
           05  INF-DATA-FIELDS.
      *                                       1  20 REC020
               10  VGR                     PICTURE X(5).
               10  FIRMA                   PICTURE X(3).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  INF-MP                      PICTURE X(8).
           05  INF-MC                      PICTURE X(8).
           05  INF-M-01                REDEFINES INF-MC.
               10  INF-M-01-M2.
                   15  INF-M-01-M2-FIRMA-G.
                       20  INF-M-01-M2-FIRMA PICTURE X(3).
               10  INF-M-01-M1.
                   15  INF-M-01-M1-VGR-G.
                       20  INF-M-01-M1-VGR PICTURE X(5).
           05  MASTER-LEVEL-02.
               10  MASTER-02-L2.
                   15  MASTER-02-L2-FIRMA  PICTURE X(3).
               10  MASTER-02-L1.
                   15  MASTER-02-L1-VGR    PICTURE X(5).
           05  MASTER-DATA-FIELDS.
      *                                       1  80 REC080
               10  M1-IO.
                   15  M1                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M2-IO.
                   15  M2                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M3-IO.
                   15  M3                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M4-IO.
                   15  M4                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M5-IO.
                   15  M5                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M6-IO.
                   15  M6                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M7-IO.
                   15  M7                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M8-IO.
                   15  M8                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M9-IO.
                   15  M9                  PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M10-IO.
                   15  M10                 PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M11-IO.
                   15  M11                 PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  M12-IO.
                   15  M12                 PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  MASTER-MP                   PICTURE X(8).
           05  MASTER-MC                   PICTURE X(8).
           05  MASTER-M-02             REDEFINES MASTER-MC.
               10  MASTER-M-02-M2.
                   15  MASTER-M-02-M2-FIRMA-G.
                       20  MASTER-M-02-M2-FIRMA PICTURE X(3).
               10  MASTER-M-02-M1.
                   15  MASTER-M-02-M1-VGR-G.
                       20  MASTER-M-02-M1-VGR PICTURE X(5).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  NM1-IO.
                   15  NM1                 PICTURE S9(9).
               10  NM2-IO.
                   15  NM2                 PICTURE S9(9).
               10  NM3-IO.
                   15  NM3                 PICTURE S9(9).
               10  NM4-IO.
                   15  NM4                 PICTURE S9(9).
               10  NM5-IO.
                   15  NM5                 PICTURE S9(9).
               10  NM6-IO.
                   15  NM6                 PICTURE S9(9).
               10  NM7-IO.
                   15  NM7                 PICTURE S9(9).
               10  NM8-IO.
                   15  NM8                 PICTURE S9(9).
               10  NM9-IO.
                   15  NM9                 PICTURE S9(9).
               10  NM10-IO.
                   15  NM10                PICTURE S9(9).
               10  NM11-IO.
                   15  NM11                PICTURE S9(9).
               10  NM12-IO.
                   15  NM12                PICTURE S9(9).
               10  TELLER-IO.
                   15  TELLER              PICTURE S9(2).
           05  EDITTING-FIELDS.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
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
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INF-PROCESS
               SET INF-PROCESS-OFF         TO TRUE
               SET INF-READ                TO TRUE
           END-IF
 
           IF  INF-READ
               PERFORM INF-GET
               SET INF-READ-OFF            TO TRUE
               IF  NOT INF-EOF
                   PERFORM INF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM INF-MATCH-SET
               END-IF
           END-IF
 
           IF  MASTER-PROCESS
               SET MASTER-PROCESS-OFF      TO TRUE
               SET MASTER-READ             TO TRUE
           END-IF
 
           IF  MASTER-READ
               PERFORM MASTER-GET
               SET MASTER-READ-OFF         TO TRUE
               IF  NOT MASTER-EOF
                   PERFORM MASTER-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM MASTER-MATCH-SET
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
 
           IF  INF-PROCESS
               PERFORM INF-IDSET
           END-IF
 
           IF  MASTER-PROCESS
               PERFORM MASTER-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-CHK-LEVEL
           END-IF
 
           IF  MASTER-PROCESS
               PERFORM MASTER-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
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
 
           IF  INF-PROCESS
               PERFORM INF-FLDSET
           END-IF
 
           IF  MASTER-PROCESS
               PERFORM MASTER-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INF-PROCESS
           OR  MASTER-PROCESS
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
               PERFORM FISLET-S
      *  L1      FIRMA     COMP "918"                    18
      *  L1 18   VGR       COMP "     "                1818
           END-IF
           IF  (I-L1)
               SUBTRACT NM1                FROM NM1
               SUBTRACT NM2                FROM NM2
               SUBTRACT NM3                FROM NM3
               SUBTRACT NM4                FROM NM4
               SUBTRACT NM5                FROM NM5
               SUBTRACT NM6                FROM NM6
               SUBTRACT NM7                FROM NM7
               SUBTRACT NM8                FROM NM8
               SUBTRACT NM9                FROM NM9
               SUBTRACT NM10               FROM NM10
               SUBTRACT NM11               FROM NM11
               SUBTRACT NM12               FROM NM12
               SUBTRACT TELLER             FROM TELLER
           END-IF
           IF  (I-01)
               ADD BEL                     TO NM12
           END-IF
           IF  (I-02)
               ADD M2                      TO NM1
               ADD M3                      TO NM2
               ADD M4                      TO NM3
               ADD M5                      TO NM4
               ADD M6                      TO NM5
               ADD M7                      TO NM6
               ADD M8                      TO NM7
               ADD M9                      TO NM8
               ADD M10                     TO NM9
               ADD M11                     TO NM10
               ADD M12                     TO NM11
           END-IF.
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-21                TO TRUE
               IF  NM1 NOT = 0
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  NM2 NOT = 0
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  NM3 NOT = 0
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  NM4 NOT = 0
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  NM5 NOT = 0
                   SET I-25                TO TRUE
               END-IF
               SET NOT-I-26                TO TRUE
               IF  NM6 NOT = 0
                   SET I-26                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  NM7 NOT = 0
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-28                TO TRUE
               IF  NM8 NOT = 0
                   SET I-28                TO TRUE
               END-IF
               SET NOT-I-29                TO TRUE
               IF  NM9 NOT = 0
                   SET I-29                TO TRUE
               END-IF
               SET NOT-I-30                TO TRUE
               IF  NM10 NOT = 0
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  NM11 NOT = 0
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  NM12 NOT = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-21)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-22)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-23)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-24)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-25)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-26)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-27)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-28)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-29)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-30)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-31)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1 AND I-32)
               ADD 1                       TO TELLER
           END-IF
           IF  (I-L1)
               SET NOT-I-35                TO TRUE
               IF  TELLER = 0
                   SET I-35                TO TRUE
               END-IF
      *1 18                MOVE "REC020  "BUGFL2  8        LEDETXT DEBUG
      *1 18      BUGFL2    DEBUGBUGFILO   REC020           VIS FELT/IND
      *1 18                MOVE "REC080  "BUGFL2  8        LEDETXT DEBUG
      *1 18      BUGFL2    DEBUGBUGFILO   REC080           VIS FELT/IND
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
           END-IF
           .
 
       INF-GET SECTION.
       INF-GET-P.
           IF  INF-EOF-OFF
               READ INF
               AT END
                   SET INF-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF-FLDSET SECTION.
       INF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '9' )
               MOVE INF-IO-AREA (5:5)      TO VGR (1:5)
               MOVE INF-IO-AREA (2:3)      TO FIRMA (1:3)
               MOVE INF-IO-AREA (10:5)     TO BEL-IO
           END-EVALUATE.
 
       INF-IDCHK SECTION.
       INF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INF-IDSET SECTION.
       INF-IDSET-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '9' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       INF-CHK-LEVEL SECTION.
       INF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '9' )
               MOVE LOW-VALUES             TO INF-LEVEL-01
               MOVE INF-IO-AREA (2:3)      TO INF-01-L2-FIRMA
               MOVE INF-IO-AREA (5:5)      TO INF-01-L1-VGR
               IF  INF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INF-01-L2             TO THE-PRIOR-L2
               MOVE  INF-01-L1             TO THE-PRIOR-L1
               SET INF-LEVEL-INIT          TO TRUE
           END-EVALUATE.
 
       INF-MATCH-SET SECTION.
       INF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '9' )
               MOVE INF-IO-AREA (2:3)      TO INF-M-01-M2-FIRMA
               MOVE INF-IO-AREA (5:5)      TO INF-M-01-M1-VGR
           END-EVALUATE.
 
       MASTER-GET SECTION.
       MASTER-GET-P.
           IF  MASTER-EOF-OFF
               READ MASTER
               AT END
                   SET MASTER-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MASTER-FLDSET SECTION.
       MASTER-FLDSET-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '9'
            AND   MASTER-IO-AREA (2:1) = '2' )
               MOVE MASTER-IO-AREA (6:5)   TO VGR (1:5)
               MOVE MASTER-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE MASTER-IO-AREA (12:5)  TO M1-IO
               MOVE MASTER-IO-AREA (17:5)  TO M2-IO
               MOVE MASTER-IO-AREA (22:5)  TO M3-IO
               MOVE MASTER-IO-AREA (27:5)  TO M4-IO
               MOVE MASTER-IO-AREA (32:5)  TO M5-IO
               MOVE MASTER-IO-AREA (37:5)  TO M6-IO
               MOVE MASTER-IO-AREA (42:5)  TO M7-IO
               MOVE MASTER-IO-AREA (47:5)  TO M8-IO
               MOVE MASTER-IO-AREA (52:5)  TO M9-IO
               MOVE MASTER-IO-AREA (57:5)  TO M10-IO
               MOVE MASTER-IO-AREA (62:5)  TO M11-IO
               MOVE MASTER-IO-AREA (67:5)  TO M12-IO
           END-EVALUATE.
 
       MASTER-IDCHK SECTION.
       MASTER-IDCHK-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '9'
            AND   MASTER-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       MASTER-IDSET SECTION.
       MASTER-IDSET-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '9'
            AND   MASTER-IO-AREA (2:1) = '2' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       MASTER-CHK-LEVEL SECTION.
       MASTER-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '9'
            AND   MASTER-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO MASTER-LEVEL-02
               MOVE MASTER-IO-AREA (3:3)   TO MASTER-02-L2-FIRMA
               MOVE MASTER-IO-AREA (6:5)   TO MASTER-02-L1-VGR
               IF  MASTER-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MASTER-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  MASTER-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MASTER-02-L2          TO THE-PRIOR-L2
               MOVE  MASTER-02-L1          TO THE-PRIOR-L1
               SET MASTER-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       MASTER-MATCH-SET SECTION.
       MASTER-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '9'
            AND   MASTER-IO-AREA (2:1) = '2' )
               MOVE MASTER-IO-AREA (3:3)   TO MASTER-M-02-M2-FIRMA
               MOVE MASTER-IO-AREA (6:5)   TO MASTER-M-02-M1-VGR
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  INF-EOF
               MOVE HIGH-VALUES            TO INF-MC
                                              INF-MP
           END-IF
           IF  MASTER-EOF
               MOVE HIGH-VALUES            TO MASTER-MC
                                              MASTER-MP
           END-IF
           IF  INF-MC < INF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  MASTER-MC < MASTER-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INF-MC < MASTER-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INF-PROCESS         TO TRUE
                   MOVE INF-MC             TO INF-MP
                   IF  INF-MC = MASTER-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  MASTER-MC < INF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MASTER-PROCESS      TO TRUE
                   MOVE MASTER-MC          TO MASTER-MP
                   IF  MASTER-MC = INF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INF-MC = MASTER-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INF-PROCESS         TO TRUE
                   MOVE INF-MC             TO INF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-98 AND NOT-I-35)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE '92'                   TO OUTF-IO-AREA (1:2)
               MOVE FIRMA                  TO OUTF-IO-AREA (3:3)
               MOVE VGR                    TO OUTF-IO-AREA (6:5)
               MOVE NM1                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (12:5)
               INITIALIZE NM1
               MOVE NM2                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (17:5)
               INITIALIZE NM2
               MOVE NM3                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (22:5)
               INITIALIZE NM3
               MOVE NM4                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (27:5)
               INITIALIZE NM4
               MOVE NM5                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (32:5)
               INITIALIZE NM5
               MOVE NM6                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (37:5)
               INITIALIZE NM6
               MOVE NM7                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (42:5)
               INITIALIZE NM7
               MOVE NM8                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (47:5)
               INITIALIZE NM8
               MOVE NM9                    TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (52:5)
               INITIALIZE NM9
               MOVE NM10                   TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (57:5)
               INITIALIZE NM10
               MOVE NM11                   TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (62:5)
               INITIALIZE NM11
               MOVE NM12                   TO XO-90P
               MOVE XO-90P-EF              TO OUTF-IO-AREA (67:5)
               INITIALIZE NM12
               MOVE TELLER-IO              TO OUTF-IO-AREA (72:2)
               INITIALIZE TELLER-IO
               MOVE UDATE                  TO OUTF-IO-AREA (74:6)
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
           SET INF-LEVEL-INIT              TO TRUE
           INITIALIZE INF-DATA-FIELDS
           SET INF-EOF-OFF                 TO TRUE
           SET INF-PROCESS                 TO TRUE
           MOVE LOW-VALUES                 TO INF-MC
                                              INF-MP
           OPEN INPUT INF
           SET MASTER-LEVEL-INIT           TO TRUE
           INITIALIZE MASTER-DATA-FIELDS
           SET MASTER-EOF-OFF              TO TRUE
           SET MASTER-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO MASTER-MC
                                              MASTER-MP
           OPEN INPUT MASTER
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT OUTF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INF
           CLOSE MASTER
           CLOSE FIRMAF
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
