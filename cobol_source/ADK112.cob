       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADK112R.
      **********************************************  Z-WIN-RPG2   ****
      ***************************************    XX2000XXIRXXEN       *
      *  PROGRAM....: ADK112                                          *
      *  PROGRAMERER: RUNE ERSVIK                                     *
      *  PROGRAMERT.: 03.12.97                                        *
      *  RETTET.....: 08.01.98                                        *
      *                                                               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ADK112.rpg
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
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 150
               RECORD CONTAINS 75.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(75).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
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
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  INNFIL-LEVEL-INIT       VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-KNR    PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-ORGNR  PICTURE X(5).
           05  INNFIL-DATA-FIELDS.
               10  KNR                     PICTURE X(6).
               10  ORGNR                   PICTURE X(5).
               10  FNR                     PICTURE X(3).
               10  ANTJAN-IO.
                   15  ANTJAN              PICTURE S9(5).
               10  ANTFEB-IO.
                   15  ANTFEB              PICTURE S9(5).
               10  ANTMAR-IO.
                   15  ANTMAR              PICTURE S9(5).
               10  ANTAPR-IO.
                   15  ANTAPR              PICTURE S9(5).
               10  ANTMAI-IO.
                   15  ANTMAI              PICTURE S9(5).
               10  ANTJUN-IO.
                   15  ANTJUN              PICTURE S9(5).
               10  ANTJUL-IO.
                   15  ANTJUL              PICTURE S9(5).
               10  ANTAUG-IO.
                   15  ANTAUG              PICTURE S9(5).
               10  ANTSEP-IO.
                   15  ANTSEP              PICTURE S9(5).
               10  ANTOKT-IO.
                   15  ANTOKT              PICTURE S9(5).
               10  ANTNOV-IO.
                   15  ANTNOV              PICTURE S9(5).
               10  ANTDES-IO.
                   15  ANTDES              PICTURE S9(5).
               10  STATUS-X                PICTURE X(1).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
      *****************************************************************
      *                                                               *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(5).
               10  TELLER-IO.
                   15  TELLER              PICTURE S9(5).
               10  KEY-X                   PICTURE X(9).
               10  ANTSS-IO.
                   15  ANTSS               PICTURE S9(4).
               10  ANTST-IO.
                   15  ANTST               PICTURE S9(4).
               10  ANTSN-IO.
                   15  ANTSN               PICTURE S9(4).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(5).
               10  SUM3-IO.
                   15  SUM3                PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-40YY9                PICTURE Z.ZZ9.
               10  XO-50YN9                PICTURE ZZZZ9.
               10  XO-70YN9                PICTURE ZZZZZZ9.
               10  XO-40YN9                PICTURE ZZZ9.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
 
           PERFORM HEADING-OUTPUT
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
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
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   SET INNFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFIL-PROCESS
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
           SET NOT-I-50                    TO TRUE
           IF  (I-01 AND NOT-I-49)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TID (1:6)
               MOVE SYSTEM-DATE            TO TID (7:6)
           END-IF
           IF  (I-01)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-L2 AND NOT-I-51)
               SET I-50                    TO TRUE
               SET I-51                    TO TRUE
           END-IF
           IF  (I-L1)
               MOVE 0                      TO SUM1
               ADD 1                       TO TELLER
      *
           END-IF
           IF  (I-L2)
               MOVE KNR                    TO KEY-X (4:6)
               MOVE '399'                  TO KEY-X (1:3)
               MOVE KEY-X                  TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-15                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-15            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      *
      *****************************************************************
      *  RUTINE FOR Å SUMMERE ANTALL VERKSTEDER PR. STATUSKODE.        *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-41                TO TRUE
               IF  STATUS-X = 'S'
                   SET I-41                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  STATUS-X = 'T'
                   SET I-42                TO TRUE
               END-IF
               SET NOT-I-43                TO TRUE
               IF  STATUS-X = 'N'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-41)
               ADD 1                       TO ANTSS
           END-IF
           IF  (I-01 AND I-42)
               ADD 1                       TO ANTST
           END-IF
           IF  (I-01 AND I-43)
               ADD 1                       TO ANTSN
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               IF  UMONTH = 01
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  UMONTH = 02
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  UMONTH = 03
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  UMONTH = 04
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  UMONTH = 05
                   SET I-25                TO TRUE
               END-IF
               SET NOT-I-26                TO TRUE
               IF  UMONTH = 06
                   SET I-26                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  UMONTH = 07
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-28                TO TRUE
               IF  UMONTH = 08
                   SET I-28                TO TRUE
               END-IF
               SET NOT-I-29                TO TRUE
               IF  UMONTH = 09
                   SET I-29                TO TRUE
               END-IF
               SET NOT-I-30                TO TRUE
               IF  UMONTH = 10
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  UMONTH = 11
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  UMONTH = 12
                   SET I-32                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-01)
               ADD ANTJAN                  TO SUM1
           END-IF
           IF  (I-21)
               ADD ANTJAN                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTJAN                  TO SUM3
           END-IF
           IF  (I-21)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTFEB                  TO SUM1
           END-IF
           IF  (I-22)
               ADD ANTFEB                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTFEB                  TO SUM3
           END-IF
           IF  (I-22)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTMAR                  TO SUM1
           END-IF
           IF  (I-23)
               ADD ANTMAR                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTMAR                  TO SUM3
           END-IF
           IF  (I-23)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTAPR                  TO SUM1
           END-IF
           IF  (I-24)
               ADD ANTAPR                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTAPR                  TO SUM3
           END-IF
           IF  (I-24)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTMAI                  TO SUM1
           END-IF
           IF  (I-25)
               ADD ANTMAI                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTMAI                  TO SUM3
           END-IF
           IF  (I-25)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTJUN                  TO SUM1
           END-IF
           IF  (I-26)
               ADD ANTJUN                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTJUN                  TO SUM3
           END-IF
           IF  (I-26)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTJUL                  TO SUM1
           END-IF
           IF  (I-27)
               ADD ANTJUL                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTJUL                  TO SUM3
           END-IF
           IF  (I-27)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTAUG                  TO SUM1
           END-IF
           IF  (I-28)
               ADD ANTAUG                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTAUG                  TO SUM3
           END-IF
           IF  (I-28)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTSEP                  TO SUM1
           END-IF
           IF  (I-29)
               ADD ANTSEP                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTSEP                  TO SUM3
           END-IF
           IF  (I-29)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTOKT                  TO SUM1
           END-IF
           IF  (I-30)
               ADD ANTOKT                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTOKT                  TO SUM3
           END-IF
           IF  (I-30)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTNOV                  TO SUM1
           END-IF
           IF  (I-31)
               ADD ANTNOV                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTNOV                  TO SUM3
           END-IF
           IF  (I-31)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD ANTDES                  TO SUM1
           END-IF
           IF  (I-32)
               ADD ANTDES                  TO SUM2
           END-IF
           IF  (I-01)
               ADD ANTDES                  TO SUM3
      *
           END-IF
           .
 
       SLUTT-T.
           SET I-10                        TO TRUE
      *
      *
           .
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:6)   TO KNR (1:6)
               MOVE INNFIL-IO-AREA (7:5)   TO ORGNR (1:5)
               MOVE INNFIL-IO-AREA (12:3)  TO FNR (1:3)
               MOVE INNFIL-IO-AREA (15:5)  TO ANTJAN-IO
               INSPECT ANTJAN-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (20:5)  TO ANTFEB-IO
               INSPECT ANTFEB-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (25:5)  TO ANTMAR-IO
               INSPECT ANTMAR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (30:5)  TO ANTAPR-IO
               INSPECT ANTAPR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (35:5)  TO ANTMAI-IO
               INSPECT ANTMAI-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (40:5)  TO ANTJUN-IO
               INSPECT ANTJUN-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (45:5)  TO ANTJUL-IO
               INSPECT ANTJUL-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (50:5)  TO ANTAUG-IO
               INSPECT ANTAUG-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (55:5)  TO ANTSEP-IO
               INSPECT ANTSEP-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (60:5)  TO ANTOKT-IO
               INSPECT ANTOKT-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (65:5)  TO ANTNOV-IO
               INSPECT ANTNOV-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (70:5)  TO ANTDES-IO
               INSPECT ANTDES-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (75:1)  TO STATUS-X (1:1)
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (1:6)   TO INNFIL-01-L2-KNR
               MOVE INNFIL-IO-AREA (7:5)   TO INNFIL-01-L1-ORGNR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KNAVN2 (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** FAKTURAGRUNNLAG'  TO LISTE-IO-AREA (1:19)
               MOVE ' LEVERES ELIN          ' TO LISTE-IO-AREA (20:23)
               MOVE ' ***'                 TO LISTE-IO-AREA (43:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** KJØRETØY KONTROLLER' TO LISTE-IO-AREA (1:23)
               MOVE ' FAKTURAGRUNNLAG  ***' TO LISTE-IO-AREA (24:21)
               MOVE 'PR.'                  TO LISTE-IO-AREA (53:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (57:8)
               MOVE 'KL:'                  TO LISTE-IO-AREA (67:3)
               MOVE TID                    TO EDIT-DATE
               MOVE EDIT-DATE (4:11)       TO LISTE-IO-AREA (68:11)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (89:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (93:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '--'                   TO LISTE-IO-AREA (97:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'K.NR.'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (9:9)
               MOVE 'STATUS'               TO LISTE-IO-AREA (67:6)
               MOVE 'O.NR'                 TO LISTE-IO-AREA (76:4)
               MOVE 'FIRMA'                TO LISTE-IO-AREA (81:5)
               MOVE 'H.ÅR'                 TO LISTE-IO-AREA (88:4)
               MOVE 'D.MND'                TO LISTE-IO-AREA (93:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '--'                   TO LISTE-IO-AREA (97:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** KJØRETØY KONTROLLER' TO LISTE-IO-AREA (1:23)
               MOVE ' FAKTURAGRUNNLAG  ***' TO LISTE-IO-AREA (24:21)
               MOVE 'PR.'                  TO LISTE-IO-AREA (53:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (57:8)
               MOVE 'KL:'                  TO LISTE-IO-AREA (67:3)
               MOVE TID                    TO EDIT-DATE
               MOVE EDIT-DATE (4:11)       TO LISTE-IO-AREA (68:11)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (89:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (93:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '--'                   TO LISTE-IO-AREA (97:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'K.NR.'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (9:9)
               MOVE 'STATUS'               TO LISTE-IO-AREA (67:6)
               MOVE 'O.NR'                 TO LISTE-IO-AREA (76:4)
               MOVE 'FIRMA'                TO LISTE-IO-AREA (81:5)
               MOVE 'H.ÅR'                 TO LISTE-IO-AREA (88:4)
               MOVE 'D.MND'                TO LISTE-IO-AREA (93:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '--'                   TO LISTE-IO-AREA (97:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND I-10)
           OR  (I-L1 AND I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KNR                    TO LISTE-IO-AREA (1:6)
               IF  (I-15)
                   MOVE '** UKJENT KUNDENR. **' TO LISTE-IO-AREA (9:21)
               END-IF
               IF  (NOT-I-15)
                   MOVE KNAVN1             TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (NOT-I-15)
                   MOVE KNAVN2             TO LISTE-IO-AREA (38:30)
               END-IF
               MOVE ORGNR                  TO LISTE-IO-AREA (75:5)
               MOVE FNR                    TO LISTE-IO-AREA (82:3)
               IF  (I-21)
                   MOVE ANTJAN             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-22)
                   MOVE ANTFEB             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-23)
                   MOVE ANTMAR             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-24)
                   MOVE ANTAPR             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-25)
                   MOVE ANTMAI             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-26)
                   MOVE ANTJUN             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-27)
                   MOVE ANTJUL             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-28)
                   MOVE ANTAUG             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-29)
                   MOVE ANTSEP             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-30)
                   MOVE ANTOKT             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-31)
                   MOVE ANTNOV             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               IF  (I-32)
                   MOVE ANTDES             TO XO-50YN9
                   MOVE XO-50YN9           TO LISTE-IO-AREA (93:5)
               END-IF
               MOVE SUM1                   TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (87:5)
               MOVE STATUS-X               TO LISTE-IO-AREA (70:1)
               EVALUATE TRUE
               WHEN (I-L2 AND I-10)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-L1 AND I-10)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** KONTROLLER'       TO LISTE-IO-AREA (1:14)
               MOVE ' TOTALT'              TO LISTE-IO-AREA (15:7)
               MOVE SUM2                   TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (93:5)
               MOVE SUM3                   TO XO-70YN9
               MOVE XO-70YN9               TO LISTE-IO-AREA (85:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** ANTALL VERKSTEDER' TO LISTE-IO-AREA (1:21)
               MOVE TELLER                 TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (93:5)
               MOVE '***'                  TO LISTE-IO-AREA (89:3)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** HERAV ANT. SPERRET. ' TO LISTE-IO-AREA (1:24)
               MOVE ANTSS                  TO XO-40YN9
               MOVE XO-40YN9               TO LISTE-IO-AREA (94:4)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** HERAV ANT. I TEST.  ' TO LISTE-IO-AREA (1:24)
               MOVE ANTST                  TO XO-40YN9
               MOVE XO-40YN9               TO LISTE-IO-AREA (94:4)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** HERAV ANT. I NORMAL.' TO LISTE-IO-AREA (1:24)
               MOVE ANTSN                  TO XO-40YN9
               MOVE XO-40YN9               TO LISTE-IO-AREA (94:4)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
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
           SET INNFIL-LEVEL-INIT           TO TRUE
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE KUNDEMA
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
