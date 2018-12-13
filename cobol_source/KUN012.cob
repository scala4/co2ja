       IDENTIFICATION DIVISION.
       PROGRAM-ID. KUN012R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KUN012.rpg
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
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1.
                   15  KUNDEMA-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-EOF-OFF         VALUE '0'.
               88  KUNDEMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-READ-OFF        VALUE '0'.
               88  KUNDEMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-PROCESS-OFF     VALUE '0'.
               88  KUNDEMA-PROCESS         VALUE '1'.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-DATA-FIELDS.
               10  KMKEY                   PICTURE X(9).
               10  FIRMA                   PICTURE X(3).
               10  KUNDGR                  PICTURE X(1).
               10  KNR                     PICTURE X(6).
               10  KNAVN1                  PICTURE X(30).
           05  KUNDEMX-DATA-FIELDS.
               10  EMAIL                   PICTURE X(70).
               10  EMAIL1                  PICTURE X(1).
               10  EMAIL2                  PICTURE X(1).
               10  EMAIL3                  PICTURE X(1).
               10  EMAIL4                  PICTURE X(1).
               10  EMAIL5                  PICTURE X(1).
               10  EMAIL6                  PICTURE X(1).
               10  EMAIL7                  PICTURE X(1).
               10  EMAIL8                  PICTURE X(1).
               10  EMAIL9                  PICTURE X(1).
               10  EMAI10                  PICTURE X(1).
               10  EMAI11                  PICTURE X(1).
               10  EMAI12                  PICTURE X(1).
               10  EMAI13                  PICTURE X(1).
               10  EMAI14                  PICTURE X(1).
               10  EMAI15                  PICTURE X(1).
               10  EMAI16                  PICTURE X(1).
               10  EMAI17                  PICTURE X(1).
               10  EMAI18                  PICTURE X(1).
               10  EMAI19                  PICTURE X(1).
               10  EMAI20                  PICTURE X(1).
               10  EMAI21                  PICTURE X(1).
               10  EMAI22                  PICTURE X(1).
               10  EMAI23                  PICTURE X(1).
               10  EMAI24                  PICTURE X(1).
               10  EMAI25                  PICTURE X(1).
               10  EMAI26                  PICTURE X(1).
               10  EMAI27                  PICTURE X(1).
               10  EMAI28                  PICTURE X(1).
               10  EMAI29                  PICTURE X(1).
               10  EMAI30                  PICTURE X(1).
               10  EMAI31                  PICTURE X(1).
               10  EMAI32                  PICTURE X(1).
               10  EMAI33                  PICTURE X(1).
               10  EMAI34                  PICTURE X(1).
               10  EMAI35                  PICTURE X(1).
               10  EMAI36                  PICTURE X(1).
               10  EMAI37                  PICTURE X(1).
               10  EMAI38                  PICTURE X(1).
               10  EMAI39                  PICTURE X(1).
               10  EMAI40                  PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  KXKEY                   PICTURE X(10).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5).
               10  ANTK-IO.
                   15  ANTK                PICTURE S9(5).
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KUNDEMA-PROCESS
               SET KUNDEMA-PROCESS-OFF     TO TRUE
               SET KUNDEMA-READ            TO TRUE
           END-IF
 
           IF  KUNDEMA-READ
           AND RECORD-SELECTED-OFF
               PERFORM KUNDEMA-GET
               SET KUNDEMA-READ-OFF        TO TRUE
               IF  NOT KUNDEMA-EOF
                   SET KUNDEMA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-IDSET
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
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-FLDSET
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
           SET NOT-I-70                    TO TRUE
           SET NOT-I-LR                    TO TRUE
           SET NOT-I-71                    TO TRUE
           IF  FIRMA > '828'
               SET I-LR                    TO TRUE
           END-IF
           IF  FIRMA = '828'
               SET I-71                    TO TRUE
           END-IF
           IF  (NOT-I-71)
               GO TO SLUTT-T
      *
           END-IF
           MOVE KMKEY                      TO KXKEY (1:9)
           MOVE '6'                        TO KXKEY (10:1)
           MOVE KXKEY                      TO KUNDEMX-KEY1
           READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
           INVALID KEY
               SET I-73                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-73                TO TRUE
               PERFORM KUNDEMX-FLDSET
               PERFORM KUNDEMX-IDSET
           END-READ
           IF  (I-73)
               GO TO SLUTT-T
      *
           END-IF
           ADD 1                           TO ANT
      *
           SET NOT-I-15                    TO TRUE
           IF  KUNDGR = '9'
               SET I-15                    TO TRUE
           END-IF
           IF  (I-15)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  EMAIL1 = 'Ø'
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  EMAIL2 = 'Ø'
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  EMAIL3 = 'Ø'
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  EMAIL4 = 'Ø'
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  EMAIL5 = 'Ø'
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  EMAIL6 = 'Ø'
               SET I-26                    TO TRUE
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  EMAIL7 = 'Ø'
               SET I-27                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  EMAIL8 = 'Ø'
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  EMAIL9 = 'Ø'
               SET I-29                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  EMAI10 = 'Ø'
               SET I-30                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  EMAI11 = 'Ø'
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  EMAI12 = 'Ø'
               SET I-32                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  EMAI13 = 'Ø'
               SET I-33                    TO TRUE
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  EMAI14 = 'Ø'
               SET I-34                    TO TRUE
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  EMAI15 = 'Ø'
               SET I-35                    TO TRUE
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  EMAI16 = 'Ø'
               SET I-36                    TO TRUE
           END-IF
           SET NOT-I-37                    TO TRUE
           IF  EMAI17 = 'Ø'
               SET I-37                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  EMAI18 = 'Ø'
               SET I-38                    TO TRUE
           END-IF
           SET NOT-I-39                    TO TRUE
           IF  EMAI19 = 'Ø'
               SET I-39                    TO TRUE
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  EMAI20 = 'Ø'
               SET I-40                    TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  EMAI21 = 'Ø'
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  EMAI22 = 'Ø'
               SET I-42                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  EMAI23 = 'Ø'
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  EMAI24 = 'Ø'
               SET I-44                    TO TRUE
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  EMAI25 = 'Ø'
               SET I-45                    TO TRUE
           END-IF
           SET NOT-I-46                    TO TRUE
           IF  EMAI26 = 'Ø'
               SET I-46                    TO TRUE
           END-IF
           SET NOT-I-47                    TO TRUE
           IF  EMAI27 = 'Ø'
               SET I-47                    TO TRUE
           END-IF
           SET NOT-I-48                    TO TRUE
           IF  EMAI28 = 'Ø'
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-49                    TO TRUE
           IF  EMAI29 = 'Ø'
               SET I-49                    TO TRUE
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  EMAI30 = 'Ø'
               SET I-50                    TO TRUE
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  EMAI31 = 'Ø'
               SET I-51                    TO TRUE
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  EMAI32 = 'Ø'
               SET I-52                    TO TRUE
           END-IF
           SET NOT-I-53                    TO TRUE
           IF  EMAI33 = 'Ø'
               SET I-53                    TO TRUE
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  EMAI34 = 'Ø'
               SET I-54                    TO TRUE
           END-IF
           SET NOT-I-55                    TO TRUE
           IF  EMAI35 = 'Ø'
               SET I-55                    TO TRUE
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  EMAI36 = 'Ø'
               SET I-56                    TO TRUE
           END-IF
           SET NOT-I-57                    TO TRUE
           IF  EMAI37 = 'Ø'
               SET I-57                    TO TRUE
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  EMAI38 = 'Ø'
               SET I-58                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  EMAI39 = 'Ø'
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-60                    TO TRUE
           IF  EMAI40 = 'Ø'
               SET I-60                    TO TRUE
           END-IF
      *
           SET I-70                        TO TRUE
           ADD 1                           TO ANTK.
 
       SLUTT-T.
           CONTINUE.
 
       KUNDEMA-GET SECTION.
       KUNDEMA-GET-P.
           IF  KUNDEMA-EOF-OFF
               READ KUNDEMA
               AT END
                   SET KUNDEMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:9)  TO KMKEY (1:9)
               MOVE KUNDEMA-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KUNDEMA-IO-AREA (6:1)  TO KUNDGR (1:1)
               MOVE KUNDEMA-IO-AREA (6:6)  TO KNR (1:6)
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (11:70) TO EMAIL (1:70)
               MOVE KUNDEMX-IO-AREA (11:1) TO EMAIL1 (1:1)
               MOVE KUNDEMX-IO-AREA (12:1) TO EMAIL2 (1:1)
               MOVE KUNDEMX-IO-AREA (13:1) TO EMAIL3 (1:1)
               MOVE KUNDEMX-IO-AREA (14:1) TO EMAIL4 (1:1)
               MOVE KUNDEMX-IO-AREA (15:1) TO EMAIL5 (1:1)
               MOVE KUNDEMX-IO-AREA (16:1) TO EMAIL6 (1:1)
               MOVE KUNDEMX-IO-AREA (17:1) TO EMAIL7 (1:1)
               MOVE KUNDEMX-IO-AREA (18:1) TO EMAIL8 (1:1)
               MOVE KUNDEMX-IO-AREA (19:1) TO EMAIL9 (1:1)
               MOVE KUNDEMX-IO-AREA (20:1) TO EMAI10 (1:1)
               MOVE KUNDEMX-IO-AREA (21:1) TO EMAI11 (1:1)
               MOVE KUNDEMX-IO-AREA (22:1) TO EMAI12 (1:1)
               MOVE KUNDEMX-IO-AREA (23:1) TO EMAI13 (1:1)
               MOVE KUNDEMX-IO-AREA (24:1) TO EMAI14 (1:1)
               MOVE KUNDEMX-IO-AREA (25:1) TO EMAI15 (1:1)
               MOVE KUNDEMX-IO-AREA (26:1) TO EMAI16 (1:1)
               MOVE KUNDEMX-IO-AREA (27:1) TO EMAI17 (1:1)
               MOVE KUNDEMX-IO-AREA (28:1) TO EMAI18 (1:1)
               MOVE KUNDEMX-IO-AREA (29:1) TO EMAI19 (1:1)
               MOVE KUNDEMX-IO-AREA (30:1) TO EMAI20 (1:1)
               MOVE KUNDEMX-IO-AREA (31:1) TO EMAI21 (1:1)
               MOVE KUNDEMX-IO-AREA (32:1) TO EMAI22 (1:1)
               MOVE KUNDEMX-IO-AREA (33:1) TO EMAI23 (1:1)
               MOVE KUNDEMX-IO-AREA (34:1) TO EMAI24 (1:1)
               MOVE KUNDEMX-IO-AREA (35:1) TO EMAI25 (1:1)
               MOVE KUNDEMX-IO-AREA (36:1) TO EMAI26 (1:1)
               MOVE KUNDEMX-IO-AREA (37:1) TO EMAI27 (1:1)
               MOVE KUNDEMX-IO-AREA (38:1) TO EMAI28 (1:1)
               MOVE KUNDEMX-IO-AREA (39:1) TO EMAI29 (1:1)
               MOVE KUNDEMX-IO-AREA (40:1) TO EMAI30 (1:1)
               MOVE KUNDEMX-IO-AREA (41:1) TO EMAI31 (1:1)
               MOVE KUNDEMX-IO-AREA (42:1) TO EMAI32 (1:1)
               MOVE KUNDEMX-IO-AREA (43:1) TO EMAI33 (1:1)
               MOVE KUNDEMX-IO-AREA (44:1) TO EMAI34 (1:1)
               MOVE KUNDEMX-IO-AREA (45:1) TO EMAI35 (1:1)
               MOVE KUNDEMX-IO-AREA (46:1) TO EMAI36 (1:1)
               MOVE KUNDEMX-IO-AREA (47:1) TO EMAI37 (1:1)
               MOVE KUNDEMX-IO-AREA (48:1) TO EMAI38 (1:1)
               MOVE KUNDEMX-IO-AREA (49:1) TO EMAI39 (1:1)
               MOVE KUNDEMX-IO-AREA (50:1) TO EMAI40 (1:1)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-05                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-04 AND I-70 AND NOT-I-73)
               IF  (I-21)
                   MOVE '@'                TO KUNDEMX-IO-AREA (11:1)
               END-IF
               IF  (I-22)
                   MOVE '@'                TO KUNDEMX-IO-AREA (12:1)
               END-IF
               IF  (I-23)
                   MOVE '@'                TO KUNDEMX-IO-AREA (13:1)
               END-IF
               IF  (I-24)
                   MOVE '@'                TO KUNDEMX-IO-AREA (14:1)
               END-IF
               IF  (I-25)
                   MOVE '@'                TO KUNDEMX-IO-AREA (15:1)
               END-IF
               IF  (I-26)
                   MOVE '@'                TO KUNDEMX-IO-AREA (16:1)
               END-IF
               IF  (I-27)
                   MOVE '@'                TO KUNDEMX-IO-AREA (17:1)
               END-IF
               IF  (I-28)
                   MOVE '@'                TO KUNDEMX-IO-AREA (18:1)
               END-IF
               IF  (I-29)
                   MOVE '@'                TO KUNDEMX-IO-AREA (19:1)
               END-IF
               IF  (I-30)
                   MOVE '@'                TO KUNDEMX-IO-AREA (20:1)
               END-IF
               IF  (I-31)
                   MOVE '@'                TO KUNDEMX-IO-AREA (21:1)
               END-IF
               IF  (I-32)
                   MOVE '@'                TO KUNDEMX-IO-AREA (22:1)
               END-IF
               IF  (I-33)
                   MOVE '@'                TO KUNDEMX-IO-AREA (23:1)
               END-IF
               IF  (I-34)
                   MOVE '@'                TO KUNDEMX-IO-AREA (24:1)
               END-IF
               IF  (I-35)
                   MOVE '@'                TO KUNDEMX-IO-AREA (25:1)
               END-IF
               IF  (I-36)
                   MOVE '@'                TO KUNDEMX-IO-AREA (26:1)
               END-IF
               IF  (I-37)
                   MOVE '@'                TO KUNDEMX-IO-AREA (27:1)
               END-IF
               IF  (I-38)
                   MOVE '@'                TO KUNDEMX-IO-AREA (28:1)
               END-IF
               IF  (I-39)
                   MOVE '@'                TO KUNDEMX-IO-AREA (29:1)
               END-IF
               IF  (I-40)
                   MOVE '@'                TO KUNDEMX-IO-AREA (30:1)
               END-IF
               IF  (I-41)
                   MOVE '@'                TO KUNDEMX-IO-AREA (31:1)
               END-IF
               IF  (I-42)
                   MOVE '@'                TO KUNDEMX-IO-AREA (32:1)
               END-IF
               IF  (I-43)
                   MOVE '@'                TO KUNDEMX-IO-AREA (33:1)
               END-IF
               IF  (I-44)
                   MOVE '@'                TO KUNDEMX-IO-AREA (34:1)
               END-IF
               IF  (I-45)
                   MOVE '@'                TO KUNDEMX-IO-AREA (35:1)
               END-IF
               IF  (I-46)
                   MOVE '@'                TO KUNDEMX-IO-AREA (36:1)
               END-IF
               IF  (I-47)
                   MOVE '@'                TO KUNDEMX-IO-AREA (37:1)
               END-IF
               IF  (I-48)
                   MOVE '@'                TO KUNDEMX-IO-AREA (38:1)
               END-IF
               IF  (I-49)
                   MOVE '@'                TO KUNDEMX-IO-AREA (39:1)
               END-IF
               IF  (I-50)
                   MOVE '@'                TO KUNDEMX-IO-AREA (40:1)
               END-IF
               IF  (I-51)
                   MOVE '@'                TO KUNDEMX-IO-AREA (41:1)
               END-IF
               IF  (I-52)
                   MOVE '@'                TO KUNDEMX-IO-AREA (42:1)
               END-IF
               IF  (I-53)
                   MOVE '@'                TO KUNDEMX-IO-AREA (43:1)
               END-IF
               IF  (I-54)
                   MOVE '@'                TO KUNDEMX-IO-AREA (44:1)
               END-IF
               IF  (I-55)
                   MOVE '@'                TO KUNDEMX-IO-AREA (45:1)
               END-IF
               IF  (I-56)
                   MOVE '@'                TO KUNDEMX-IO-AREA (46:1)
               END-IF
               IF  (I-57)
                   MOVE '@'                TO KUNDEMX-IO-AREA (47:1)
               END-IF
               IF  (I-58)
                   MOVE '@'                TO KUNDEMX-IO-AREA (48:1)
               END-IF
               IF  (I-59)
                   MOVE '@'                TO KUNDEMX-IO-AREA (49:1)
               END-IF
               IF  (I-60)
                   MOVE '@'                TO KUNDEMX-IO-AREA (50:1)
               END-IF
               REWRITE KUNDEMX-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = KUNDEMX'
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
           INITIALIZE KUNDEMA-DATA-FIELDS
           SET KUNDEMA-EOF-OFF             TO TRUE
           SET KUNDEMA-PROCESS             TO TRUE
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN I-O KUNDEMX.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KUNDEMA
           CLOSE KUNDEMX.
 
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
