       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA129R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA129.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT VGRF
               ASSIGN TO UT-S-VGRF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VGRF-STATUS.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD VGRF
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  VGRF-IO-AREA.
           05  VGRF-IO-AREA-X              PICTURE X(80).
       FD OUTFIL
               BLOCK CONTAINS 1500
               RECORD CONTAINS 150.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(150).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  VGRF-STATUS                 PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VGRF-EOF-OFF            VALUE '0'.
               88  VGRF-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VGRF-READ-OFF           VALUE '0'.
               88  VGRF-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VGRF-PROCESS-OFF        VALUE '0'.
               88  VGRF-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VGRF-LEVEL-INIT-OFF     VALUE '0'.
               88  VGRF-LEVEL-INIT         VALUE '1'.
           05  PARAM-DATA-FIELDS.
               10  PMND-IO.
                   15  PMND                PICTURE S9(2).
           05  VGRF-LEVEL-02.
               10  VGRF-02-L1.
                   15  VGRF-02-L1-FIRMA    PICTURE X(3).
           05  VGRF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  M1-IO.
                   15  M1                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M2-IO.
                   15  M2                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M3-IO.
                   15  M3                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M4-IO.
                   15  M4                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M5-IO.
                   15  M5                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M6-IO.
                   15  M6                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M7-IO.
                   15  M7                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M8-IO.
                   15  M8                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M9-IO.
                   15  M9                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M10-IO.
                   15  M10                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M11-IO.
                   15  M11                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  M12-IO.
                   15  M12                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(11).
               10  TM01-IO.
                   15  TM01                PICTURE S9(9)V9(2).
               10  TM02-IO.
                   15  TM02                PICTURE S9(9)V9(2).
               10  TM03-IO.
                   15  TM03                PICTURE S9(9)V9(2).
               10  TM04-IO.
                   15  TM04                PICTURE S9(9)V9(2).
               10  TM05-IO.
                   15  TM05                PICTURE S9(9)V9(2).
               10  TM06-IO.
                   15  TM06                PICTURE S9(9)V9(2).
               10  TM07-IO.
                   15  TM07                PICTURE S9(9)V9(2).
               10  TM08-IO.
                   15  TM08                PICTURE S9(9)V9(2).
               10  TM09-IO.
                   15  TM09                PICTURE S9(9)V9(2).
               10  TM10-IO.
                   15  TM10                PICTURE S9(9)V9(2).
               10  TM11-IO.
                   15  TM11                PICTURE S9(9)V9(2).
               10  TM12-IO.
                   15  TM12                PICTURE S9(9)V9(2).
               10  TN01-IO.
                   15  TN01                PICTURE S9(9)V9(2).
               10  TN02-IO.
                   15  TN02                PICTURE S9(9)V9(2).
               10  TN03-IO.
                   15  TN03                PICTURE S9(9)V9(2).
               10  TN04-IO.
                   15  TN04                PICTURE S9(9)V9(2).
               10  TN05-IO.
                   15  TN05                PICTURE S9(9)V9(2).
               10  TN06-IO.
                   15  TN06                PICTURE S9(9)V9(2).
               10  TN07-IO.
                   15  TN07                PICTURE S9(9)V9(2).
               10  TN08-IO.
                   15  TN08                PICTURE S9(9)V9(2).
               10  TN09-IO.
                   15  TN09                PICTURE S9(9)V9(2).
               10  TN10-IO.
                   15  TN10                PICTURE S9(9)V9(2).
               10  TN11-IO.
                   15  TN11                PICTURE S9(9)V9(2).
               10  TN12-IO.
                   15  TN12                PICTURE S9(9)V9(2).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  VGRF-PROCESS
               SET VGRF-PROCESS-OFF        TO TRUE
               SET VGRF-READ               TO TRUE
           END-IF
 
           IF  VGRF-READ
           AND RECORD-SELECTED-OFF
               PERFORM VGRF-GET
               SET VGRF-READ-OFF           TO TRUE
               IF  NOT VGRF-EOF
                   SET VGRF-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  VGRF-PROCESS
               PERFORM VGRF-IDSET
           END-IF
 
           IF  VGRF-PROCESS
               PERFORM VGRF-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  VGRF-PROCESS
               PERFORM VGRF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VGRF-PROCESS
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
               SET NOT-I-71                TO TRUE
               IF  PMND = 01
                   SET I-71                TO TRUE
               END-IF
               SET NOT-I-72                TO TRUE
               IF  PMND = 02
                   SET I-72                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  PMND = 03
                   SET I-73                TO TRUE
               END-IF
               SET NOT-I-74                TO TRUE
               IF  PMND = 04
                   SET I-74                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  PMND = 05
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  PMND = 06
                   SET I-76                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  PMND = 07
                   SET I-77                TO TRUE
               END-IF
               SET NOT-I-78                TO TRUE
               IF  PMND = 08
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-79                TO TRUE
               IF  PMND = 09
                   SET I-79                TO TRUE
               END-IF
               SET NOT-I-80                TO TRUE
               IF  PMND = 10
                   SET I-80                TO TRUE
               END-IF
               SET NOT-I-81                TO TRUE
               IF  PMND = 11
                   SET I-81                TO TRUE
               END-IF
               SET NOT-I-82                TO TRUE
               IF  PMND = 12
                   SET I-82                TO TRUE
               END-IF
               MOVE 0                      TO NULL-X
           END-IF
           IF  (I-L1)
               SUBTRACT TM01               FROM TM01
               SUBTRACT TM02               FROM TM02
               SUBTRACT TM03               FROM TM03
               SUBTRACT TM04               FROM TM04
               SUBTRACT TM05               FROM TM05
               SUBTRACT TM06               FROM TM06
               SUBTRACT TM07               FROM TM07
               SUBTRACT TM08               FROM TM08
               SUBTRACT TM09               FROM TM09
               SUBTRACT TM10               FROM TM10
               SUBTRACT TM11               FROM TM11
               SUBTRACT TM12               FROM TM12
           END-IF
           IF  (I-02)
               ADD M1                      TO TM01
               ADD M2                      TO TM02
               ADD M3                      TO TM03
               ADD M4                      TO TM04
               ADD M5                      TO TM05
               ADD M6                      TO TM06
               ADD M7                      TO TM07
               ADD M8                      TO TM08
               ADD M9                      TO TM09
               ADD M10                     TO TM10
               ADD M11                     TO TM11
               ADD M12                     TO TM12
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               MOVE 0                      TO TN01
               MOVE 0                      TO TN02
               MOVE 0                      TO TN03
               MOVE 0                      TO TN04
               MOVE 0                      TO TN05
               MOVE 0                      TO TN06
               MOVE 0                      TO TN07
               MOVE 0                      TO TN08
               MOVE 0                      TO TN09
               MOVE 0                      TO TN10
               MOVE 0                      TO TN11
               MOVE 0                      TO TN12
           END-IF
           IF  (I-L1 AND I-71)
               ADD TM12 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-72)
               ADD TM12 TO ZERO        GIVING TN02
               ADD TM11 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-73)
               ADD TM12 TO ZERO        GIVING TN03
               ADD TM11 TO ZERO        GIVING TN02
               ADD TM10 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-74)
               ADD TM12 TO ZERO        GIVING TN04
               ADD TM11 TO ZERO        GIVING TN03
               ADD TM10 TO ZERO        GIVING TN02
               ADD TM09 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-75)
               ADD TM12 TO ZERO        GIVING TN05
               ADD TM11 TO ZERO        GIVING TN04
               ADD TM10 TO ZERO        GIVING TN03
               ADD TM09 TO ZERO        GIVING TN02
               ADD TM08 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-76)
               ADD TM12 TO ZERO        GIVING TN06
               ADD TM11 TO ZERO        GIVING TN05
               ADD TM10 TO ZERO        GIVING TN04
               ADD TM09 TO ZERO        GIVING TN03
               ADD TM08 TO ZERO        GIVING TN02
               ADD TM07 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-77)
               ADD TM12 TO ZERO        GIVING TN07
               ADD TM11 TO ZERO        GIVING TN06
               ADD TM10 TO ZERO        GIVING TN05
               ADD TM09 TO ZERO        GIVING TN04
               ADD TM08 TO ZERO        GIVING TN03
               ADD TM07 TO ZERO        GIVING TN02
               ADD TM06 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-78)
               ADD TM12 TO ZERO        GIVING TN08
               ADD TM11 TO ZERO        GIVING TN07
               ADD TM10 TO ZERO        GIVING TN06
               ADD TM09 TO ZERO        GIVING TN05
               ADD TM08 TO ZERO        GIVING TN04
               ADD TM07 TO ZERO        GIVING TN03
               ADD TM06 TO ZERO        GIVING TN02
               ADD TM05 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-79)
               ADD TM12 TO ZERO        GIVING TN09
               ADD TM11 TO ZERO        GIVING TN08
               ADD TM10 TO ZERO        GIVING TN07
               ADD TM09 TO ZERO        GIVING TN06
               ADD TM08 TO ZERO        GIVING TN05
               ADD TM07 TO ZERO        GIVING TN04
               ADD TM06 TO ZERO        GIVING TN03
               ADD TM05 TO ZERO        GIVING TN02
               ADD TM04 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-80)
               ADD TM12 TO ZERO        GIVING TN10
               ADD TM11 TO ZERO        GIVING TN09
               ADD TM10 TO ZERO        GIVING TN08
               ADD TM09 TO ZERO        GIVING TN07
               ADD TM08 TO ZERO        GIVING TN06
               ADD TM07 TO ZERO        GIVING TN05
               ADD TM06 TO ZERO        GIVING TN04
               ADD TM05 TO ZERO        GIVING TN03
               ADD TM04 TO ZERO        GIVING TN02
               ADD TM03 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-81)
               ADD TM12 TO ZERO        GIVING TN11
               ADD TM11 TO ZERO        GIVING TN10
               ADD TM10 TO ZERO        GIVING TN09
               ADD TM09 TO ZERO        GIVING TN08
               ADD TM08 TO ZERO        GIVING TN07
               ADD TM07 TO ZERO        GIVING TN06
               ADD TM06 TO ZERO        GIVING TN05
               ADD TM05 TO ZERO        GIVING TN04
               ADD TM04 TO ZERO        GIVING TN03
               ADD TM03 TO ZERO        GIVING TN02
               ADD TM02 TO ZERO        GIVING TN01
           END-IF
           IF  (I-L1 AND I-82)
               ADD TM12 TO ZERO        GIVING TN12
               ADD TM11 TO ZERO        GIVING TN11
               ADD TM10 TO ZERO        GIVING TN10
               ADD TM09 TO ZERO        GIVING TN09
               ADD TM08 TO ZERO        GIVING TN08
               ADD TM07 TO ZERO        GIVING TN07
               ADD TM06 TO ZERO        GIVING TN06
               ADD TM05 TO ZERO        GIVING TN05
               ADD TM04 TO ZERO        GIVING TN04
               ADD TM03 TO ZERO        GIVING TN03
               ADD TM02 TO ZERO        GIVING TN02
               ADD TM01 TO ZERO        GIVING TN01
           END-IF.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A' )
               MOVE PARAM-IO-AREA (26:2)   TO PMND-IO
               INSPECT PMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       VGRF-GET SECTION.
       VGRF-GET-P.
           IF  VGRF-EOF-OFF
               READ VGRF
               AT END
                   SET VGRF-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VGRF-FLDSET SECTION.
       VGRF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VGRF-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE VGRF-IO-AREA (12:5)    TO M1-IO
               MOVE VGRF-IO-AREA (17:5)    TO M2-IO
               MOVE VGRF-IO-AREA (22:5)    TO M3-IO
               MOVE VGRF-IO-AREA (27:5)    TO M4-IO
               MOVE VGRF-IO-AREA (32:5)    TO M5-IO
               MOVE VGRF-IO-AREA (37:5)    TO M6-IO
               MOVE VGRF-IO-AREA (42:5)    TO M7-IO
               MOVE VGRF-IO-AREA (47:5)    TO M8-IO
               MOVE VGRF-IO-AREA (52:5)    TO M9-IO
               MOVE VGRF-IO-AREA (57:5)    TO M10-IO
               MOVE VGRF-IO-AREA (62:5)    TO M11-IO
               MOVE VGRF-IO-AREA (67:5)    TO M12-IO
           END-EVALUATE.
 
       VGRF-IDSET SECTION.
       VGRF-IDSET-P.
           SET I-02                        TO TRUE.
 
       VGRF-CHK-LEVEL SECTION.
       VGRF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VGRF-LEVEL-02
               MOVE VGRF-IO-AREA (3:3)     TO VGRF-02-L1-FIRMA
               IF  VGRF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VGRF-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VGRF-02-L1            TO THE-PRIOR-L1
               SET VGRF-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE TN01-IO                TO OUTFIL-IO-AREA (1:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (12:1)
               MOVE TN02-IO                TO OUTFIL-IO-AREA (13:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (24:1)
               MOVE TN03-IO                TO OUTFIL-IO-AREA (25:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (36:1)
               MOVE TN04-IO                TO OUTFIL-IO-AREA (37:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (48:1)
               MOVE TN05-IO                TO OUTFIL-IO-AREA (49:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (60:1)
               MOVE TN06-IO                TO OUTFIL-IO-AREA (61:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (72:1)
               MOVE TN07-IO                TO OUTFIL-IO-AREA (73:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (84:1)
               MOVE TN08-IO                TO OUTFIL-IO-AREA (85:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (96:1)
               MOVE TN09-IO                TO OUTFIL-IO-AREA (97:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (108:1)
               MOVE TN10-IO                TO OUTFIL-IO-AREA (109:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (120:1)
               MOVE TN11-IO                TO OUTFIL-IO-AREA (121:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (132:1)
               MOVE TN12-IO                TO OUTFIL-IO-AREA (133:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (144:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (145:3)
               WRITE OUTFIL-IO-AREA
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET VGRF-LEVEL-INIT             TO TRUE
           INITIALIZE VGRF-DATA-FIELDS
           SET VGRF-EOF-OFF                TO TRUE
           SET VGRF-PROCESS                TO TRUE
           OPEN INPUT VGRF
           OPEN OUTPUT OUTFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE VGRF
           CLOSE OUTFIL.
 
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
