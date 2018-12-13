       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADK116R.
      **********************************************  Z-WIN-RPG2   ****
      *****************************************  XX2000XXIRXXEN
      * PROGRAMM : ADK116                                    *
      * LAGET AV : RUNE ERSVIK                               *
      * DATO     : 17.02.98                                  *
      * ENDRET   : 07.11.14                                  *
      ********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ADK116.rpg
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
           SELECT KKSTAT2
               ASSIGN TO KKSTAT2
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KKSTAT2-STATUS
               RECORD KEY IS KKSTAT2-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT DISKUT
               ASSIGN TO DISKUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DISKUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD KKSTAT2
               RECORD CONTAINS 1000.
       01  KKSTAT2-IO-AREA.
           05  KKSTAT2-IO-AREA-X.
               10  KKSTAT2-KEY1            PICTURE X(6).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD DISKUT
               RECORD CONTAINS 150.
       01  DISKUT-IO-AREA.
           05  DISKUT-IO-AREA-X            PICTURE X(150).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  KKSTAT2-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DISKUT-STATUS               PICTURE 99 VALUE 0.
 
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
           05  KKSTAT2-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  PARAM-DATA-FIELDS.
               10  PORGNR                  PICTURE X(5).
               10  PMND-IO.
                   15  PMND                PICTURE S9(2).
               10  PLUMIN                  PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(3).
           05  KKSTAT2-DATA-FIELDS.
               10  SREC1                   PICTURE X(110).
               10  ORGNR                   PICTURE X(5).
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
      **
           05  TEMPORARY-FIELDS.
               10  KEY-X                   PICTURE X(6).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(3).
               10  NULL5-IO.
                   15  NULL5               PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-30YN9                PICTURE ZZ9.
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
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
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
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
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
           MOVE PORGNR                     TO KEY-X (2:5)
           MOVE 'B'                        TO KEY-X (1:1)
           MOVE KEY-X                      TO KKSTAT2-KEY1
           READ KKSTAT2 RECORD KEY IS KKSTAT2-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM KKSTAT2-IDCHK
               PERFORM KKSTAT2-FLDSET
               PERFORM KKSTAT2-IDSET
           END-READ
           IF  (I-11)
               GO TO SLUTT-T
      *
      *          PORGNR    COMP ORGNR                    15
      * N15                GOTO SLUTT
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  PMND = 01
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  PMND = 02
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  PMND = 03
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  PMND = 04
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = 05
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  PMND = 06
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  PMND = 07
               SET I-26                    TO TRUE
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  PMND = 08
               SET I-27                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  PMND = 09
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  PMND = 10
               SET I-29                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  PMND = 11
               SET I-30                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  PMND = 12
               SET I-31                    TO TRUE
           END-IF
      *
           SET NOT-I-40                    TO TRUE
           IF  PLUMIN = '+'
               SET I-40                    TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  PLUMIN = '-'
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  PLUMIN = ' '
               SET I-42                    TO TRUE
           END-IF
           IF  (I-42)
               GO TO SLUTT-T
      *
           END-IF
           ADD ANT TO ZERO             GIVING ANTALL
           MOVE 0                          TO NULL5
      **
           IF  (I-20 AND I-40)
               ADD ANTALL                  TO ANTJAN
           END-IF
           IF  (I-20 AND I-41)
               SUBTRACT ANTALL             FROM ANTJAN
           END-IF
           IF  (I-21 AND I-40)
               ADD ANTALL                  TO ANTFEB
           END-IF
           IF  (I-21 AND I-41)
               SUBTRACT ANTALL             FROM ANTFEB
           END-IF
           IF  (I-22 AND I-40)
               ADD ANTALL                  TO ANTMAR
           END-IF
           IF  (I-22 AND I-41)
               SUBTRACT ANTALL             FROM ANTMAR
           END-IF
           IF  (I-23 AND I-40)
               ADD ANTALL                  TO ANTAPR
           END-IF
           IF  (I-23 AND I-41)
               SUBTRACT ANTALL             FROM ANTAPR
           END-IF
           IF  (I-24 AND I-40)
               ADD ANTALL                  TO ANTMAI
           END-IF
           IF  (I-24 AND I-41)
               SUBTRACT ANTALL             FROM ANTMAI
           END-IF
           IF  (I-25 AND I-40)
               ADD ANTALL                  TO ANTJUN
           END-IF
           IF  (I-25 AND I-41)
               SUBTRACT ANTALL             FROM ANTJUN
           END-IF
           IF  (I-26 AND I-40)
               ADD ANTALL                  TO ANTJUL
           END-IF
           IF  (I-26 AND I-41)
               SUBTRACT ANTALL             FROM ANTJUL
           END-IF
           IF  (I-27 AND I-40)
               ADD ANTALL                  TO ANTAUG
           END-IF
           IF  (I-27 AND I-41)
               SUBTRACT ANTALL             FROM ANTAUG
           END-IF
           IF  (I-28 AND I-40)
               ADD ANTALL                  TO ANTSEP
           END-IF
           IF  (I-28 AND I-41)
               SUBTRACT ANTALL             FROM ANTSEP
           END-IF
           IF  (I-29 AND I-40)
               ADD ANTALL                  TO ANTOKT
           END-IF
           IF  (I-29 AND I-41)
               SUBTRACT ANTALL             FROM ANTOKT
           END-IF
           IF  (I-30 AND I-40)
               ADD ANTALL                  TO ANTNOV
           END-IF
           IF  (I-30 AND I-41)
               SUBTRACT ANTALL             FROM ANTNOV
           END-IF
           IF  (I-31 AND I-40)
               ADD ANTALL                  TO ANTDES
           END-IF
           IF  (I-31 AND I-41)
               SUBTRACT ANTALL             FROM ANTDES
      *
           END-IF
           ADD ANTALL TO ZERO          GIVING ANTOKT
           SET I-10                        TO TRUE.
 
       SLUTT-T.
      ******************************************************
           CONTINUE.
 
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
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               MOVE PARAM-IO-AREA (11:5)   TO PORGNR (1:5)
               MOVE PARAM-IO-AREA (21:2)   TO PMND-IO
               INSPECT PMND-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (36:1)   TO PLUMIN (1:1)
               MOVE PARAM-IO-AREA (45:3)   TO ANT-IO
               INSPECT ANT-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       KKSTAT2-FLDSET SECTION.
       KKSTAT2-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KKSTAT2-IO-AREA (1:1) = 'B' )
               MOVE KKSTAT2-IO-AREA (1:110) TO SREC1 (1:110)
               MOVE KKSTAT2-IO-AREA (2:5)  TO ORGNR (1:5)
               MOVE KKSTAT2-IO-AREA (42:5) TO ANTJAN-IO
               INSPECT ANTJAN-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (47:5) TO ANTFEB-IO
               INSPECT ANTFEB-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (52:5) TO ANTMAR-IO
               INSPECT ANTMAR-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (57:5) TO ANTAPR-IO
               INSPECT ANTAPR-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (62:5) TO ANTMAI-IO
               INSPECT ANTMAI-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (67:5) TO ANTJUN-IO
               INSPECT ANTJUN-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (72:5) TO ANTJUL-IO
               INSPECT ANTJUL-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (77:5) TO ANTAUG-IO
               INSPECT ANTAUG-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (82:5) TO ANTSEP-IO
               INSPECT ANTSEP-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (87:5) TO ANTOKT-IO
               INSPECT ANTOKT-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (92:5) TO ANTNOV-IO
               INSPECT ANTNOV-IO REPLACING ALL ' ' BY '0'
               MOVE KKSTAT2-IO-AREA (97:5) TO ANTDES-IO
               INSPECT ANTDES-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KKSTAT2-IDCHK SECTION.
       KKSTAT2-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KKSTAT2-IO-AREA (1:1) = 'B' )
             OR ( KKSTAT2-IO-AREA (1:1) NOT = 'B' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KKSTAT2-IDSET SECTION.
       KKSTAT2-IDSET-P.
           EVALUATE TRUE
           WHEN ( KKSTAT2-IO-AREA (1:1) = 'B' )
               SET I-02                    TO TRUE
           WHEN ( KKSTAT2-IO-AREA (1:1) NOT = 'B' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
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
           IF  (I-10)
               IF  (I-20)
                   MOVE ANTJAN-IO          TO KKSTAT2-IO-AREA (42:5)
               END-IF
               IF  (I-21)
                   MOVE ANTFEB-IO          TO KKSTAT2-IO-AREA (47:5)
               END-IF
               IF  (I-22)
                   MOVE ANTMAR-IO          TO KKSTAT2-IO-AREA (52:5)
               END-IF
               IF  (I-23)
                   MOVE ANTAPR-IO          TO KKSTAT2-IO-AREA (57:5)
               END-IF
               IF  (I-24)
                   MOVE ANTMAI-IO          TO KKSTAT2-IO-AREA (62:5)
               END-IF
               IF  (I-25)
                   MOVE ANTJUN-IO          TO KKSTAT2-IO-AREA (67:5)
               END-IF
               IF  (I-26)
                   MOVE ANTJUL-IO          TO KKSTAT2-IO-AREA (72:5)
               END-IF
               IF  (I-27)
                   MOVE ANTAUG-IO          TO KKSTAT2-IO-AREA (77:5)
               END-IF
               IF  (I-28)
                   MOVE ANTSEP-IO          TO KKSTAT2-IO-AREA (82:5)
               END-IF
               IF  (I-29)
                   MOVE ANTOKT-IO          TO KKSTAT2-IO-AREA (87:5)
               END-IF
               IF  (I-30)
                   MOVE ANTNOV-IO          TO KKSTAT2-IO-AREA (92:5)
               END-IF
               IF  (I-31)
                   MOVE ANTDES-IO          TO KKSTAT2-IO-AREA (97:5)
      *                        NULL5 X   46
      *                        NULL5 X   51
      *                        NULL5 X   56
      *                        NULL5 X   61
      *                        NULL5 X   66
      *                        NULL5 X   71
      *                        NULL5 X   76
      *                        NULL5 X   81
      *                        NULL5 X   86
      *                        NULL5 X   91
      *                        NULL5 X   96
      *                        NULL5 X  101
               END-IF
               REWRITE KKSTAT2-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = KKSTAT2'
               END-REWRITE
           END-IF
           IF  (I-10)
               MOVE SPACES TO DISKUT-IO-AREA
               INITIALIZE DISKUT-IO-AREA
               MOVE SREC1                  TO DISKUT-IO-AREA (1:110)
               MOVE ANTDES-IO              TO DISKUT-IO-AREA (112:5)
               WRITE DISKUT-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* * * KORRIGERING AV ' TO LISTE-IO-AREA (1:21)
               MOVE 'STAT. FILE * * *'     TO LISTE-IO-AREA (22:16)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* * * KORRIGERING AV ' TO LISTE-IO-AREA (1:21)
               MOVE 'STAT. FILE * * *'     TO LISTE-IO-AREA (22:16)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-40)
                   MOVE 'LAGT TIL   '      TO LISTE-IO-AREA (1:11)
               END-IF
               IF  (I-41)
                   MOVE 'TRUKKET FRA'      TO LISTE-IO-AREA (1:11)
               END-IF
               MOVE ANTALL                 TO XO-30YN9
               MOVE XO-30YN9               TO LISTE-IO-AREA (13:3)
               MOVE 'KONTROLL(ER) I'       TO LISTE-IO-AREA (18:14)
               IF  (I-20)
                   MOVE 'JAN'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-21)
                   MOVE 'FEB'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-22)
                   MOVE 'MAR'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-23)
                   MOVE 'APR'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-24)
                   MOVE 'MAI'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-25)
                   MOVE 'JUN'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-26)
                   MOVE 'JUL'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-27)
                   MOVE 'AUG'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-28)
                   MOVE 'SEP'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-29)
                   MOVE 'OKT'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-30)
                   MOVE 'NOV'              TO LISTE-IO-AREA (33:3)
               END-IF
               IF  (I-31)
                   MOVE 'DES'              TO LISTE-IO-AREA (33:3)
               END-IF
               MOVE 'FOR K.ORG.NR: '       TO LISTE-IO-AREA (37:14)
               MOVE ORGNR                  TO LISTE-IO-AREA (53:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           INITIALIZE KKSTAT2-DATA-FIELDS
           OPEN I-O KKSTAT2
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT DISKUT.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE KKSTAT2
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE DISKUT.
 
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
