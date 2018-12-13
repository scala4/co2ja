       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACC161R.
      **********************************************  Z-WIN-RPG2      *
      *       LISTER UT ACCOUNTING-FILEN.                  *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ACC161.rpg
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
           SELECT ACCOUNT
               ASSIGN TO ACCOUNT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ACCOUNT-STATUS
               RECORD KEY IS ACCOUNT-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT DISKUT
               ASSIGN TO UT-S-DISKUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DISKUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT
               RECORD CONTAINS 90.
       01  ACCOUNT-IO-AREA.
           05  ACCOUNT-IO-AREA-X.
               10  ACCOUNT-KEY1.
                   15  ACCOUNT-KEY1N       PICTURE S9(17).
               10  FILLER                  PICTURE X(73).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD DISKUT
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  DISKUT-IO-AREA.
           05  DISKUT-IO-AREA-X            PICTURE X(80).
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
           10  ACCOUNT-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  DISKUT-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ACCOUNT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ACCOUNT-EOF-OFF         VALUE '0'.
               88  ACCOUNT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ACCOUNT-READ-OFF        VALUE '0'.
               88  ACCOUNT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ACCOUNT-PROCESS-OFF     VALUE '0'.
               88  ACCOUNT-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ACCOUNT-LEVEL-INIT-OFF  VALUE '0'.
               88  ACCOUNT-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  ACCOUNT-LEVEL-02.
               10  ACCOUNT-02-L1.
                   15  ACCOUNT-02-L1-FNR   PICTURE X(3).
           05  ACCOUNT-DATA-FIELDS.
               10  NAVN                    PICTURE X(8).
               10  KODE                    PICTURE X(1).
               10  SIGN-X                  PICTURE X(2).
               10  FNR                     PICTURE X(3).
               10  DATO                    PICTURE X(8).
               10  DAG                     PICTURE X(2).
               10  MND                     PICTURE X(2).
               10  START-X-IO.
                   15  START-X             PICTURE S9(6).
               10  STOP-X-IO.
                   15  STOP-X              PICTURE S9(6).
               10  NUMMER                  PICTURE X(4).
               10  FORM                    PICTURE X(4).
               10  KOPI                    PICTURE X(2).
               10  ENH-IO.
                   15  ENH                 PICTURE S9(6)V9(2).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(6)V9(3).
               10  FAKT                    PICTURE X(1).
               10  SLETT                   PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
      **********************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FFNR                    PICTURE X(3).
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(7)V9(3).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(5).
               10  LEVBEL-IO.
                   15  LEVBEL              PICTURE S9(6)V9(2).
               10  OPPGNR                  PICTURE X(6).
               10  FIRMNR                  PICTURE X(3).
               10  FIRMNA                  PICTURE X(30).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-62YYZR               PICTURE ZZZ.ZZZ,ZZ-.
               10  XO-63YYZR               PICTURE ZZZ.ZZZ,ZZZ-.
               10  XO-73YYZR               PICTURE Z.ZZZ.ZZZ,ZZZ-.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ACCOUNT-PROCESS
               SET ACCOUNT-PROCESS-OFF     TO TRUE
               SET ACCOUNT-READ            TO TRUE
           END-IF
 
           IF  ACCOUNT-READ
           AND RECORD-SELECTED-OFF
               PERFORM ACCOUNT-GET
               SET ACCOUNT-READ-OFF        TO TRUE
               IF  NOT ACCOUNT-EOF
                   SET ACCOUNT-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ACCOUNT-PROCESS
               PERFORM ACCOUNT-IDSET
           END-IF
 
           IF  ACCOUNT-PROCESS
               PERFORM ACCOUNT-CHK-LEVEL
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
 
           IF  ACCOUNT-PROCESS
               PERFORM ACCOUNT-FLDOFF
               PERFORM ACCOUNT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ACCOUNT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-99)
               MOVE '399'                  TO FNR
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               PERFORM SIDE1-S
           END-IF
           SET I-99                        TO TRUE
      **********************************************************
      **********************************************************
           IF  (I-L1)
               SET NOT-I-41                TO TRUE
           END-IF
           IF  (I-02)
               SET NOT-I-40                TO TRUE
               SET NOT-I-21                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  FNR NOT < '001'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-20)
               GO TO END-X-T
           END-IF
           IF  (I-02)
               SET NOT-I-28                TO TRUE
               IF  FNR = '399'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  FNR = '015'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  FNR = '998'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  FNR = '999'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-28)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-20)
               GO TO END-X-T
           END-IF
           IF  (I-77)
               GO TO END-X-T
           END-IF
           IF  (I-02 AND I-U1)
               SET NOT-I-50                TO TRUE
               IF  FAKT = 'F'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-U1 AND I-50)
               GO TO END-X-T
           END-IF
           IF  (I-02)
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-33                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-33            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-02 AND I-33)
               GO TO END-X-T
           END-IF
           IF  (I-02)
               SET NOT-I-35                TO TRUE
               IF  SLETT = 'S'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-35)
               GO TO END-X-T
           END-IF
           IF  (I-02)
               SET I-40                    TO TRUE
               SET I-41                    TO TRUE
               SET NOT-I-21                TO TRUE
               IF  FNR NOT = FFNR
                   SET I-21                TO TRUE
               END-IF
               MOVE FNR                    TO FFNR
               SET NOT-I-10                TO TRUE
               IF  KODE = '+'
                   SET I-10                TO TRUE
               END-IF
               SET NOT-I-11                TO TRUE
               IF  KODE = 'F'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  KODE = 'O'
                   SET I-12                TO TRUE
               END-IF
               ADD PRIS                    TO TOTSUM
               MOVE 1                      TO ANTFAK
               ADD PRIS TO ZERO        GIVING LEVBEL
           END-IF.
 
       END-X-T.
      ******************************************************
      *    SUBRUTINE FOR PRINTING AV SIDE 1 PR. FIRMA      *
      ******************************************************
           CONTINUE.
 
       SIDE1-S SECTION.
       SIDE1-S-P.
           SET I-86                        TO TRUE
           MOVE 'ACC161'                   TO OPPGNR
           MOVE FNR                        TO FIRMNR
           MOVE FINAVN                     TO FIRMNA
           PERFORM EXCEPTION-OUTPUT
           SET NOT-I-86                    TO TRUE.
      ******************************************************
 
       ACCOUNT-GET SECTION.
       ACCOUNT-GET-P.
           IF  ACCOUNT-EOF-OFF
               READ ACCOUNT
               AT END
                   SET ACCOUNT-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ACCOUNT-FLDOFF SECTION.
       ACCOUNT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-85                TO TRUE
               SET NOT-I-77                TO TRUE
           END-EVALUATE.
 
       ACCOUNT-FLDSET SECTION.
       ACCOUNT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ACCOUNT-IO-AREA (4:8)  TO NAVN (1:8)
               MOVE ACCOUNT-IO-AREA (30:1) TO KODE (1:1)
               MOVE ACCOUNT-IO-AREA (32:2) TO SIGN-X (1:2)
               MOVE ACCOUNT-IO-AREA (18:3) TO FNR (1:3)
               MOVE ACCOUNT-IO-AREA (34:8) TO DATO (1:8)
               MOVE ACCOUNT-IO-AREA (34:2) TO DAG (1:2)
               MOVE ACCOUNT-IO-AREA (37:2) TO MND (1:2)
               MOVE ACCOUNT-IO-AREA (42:6) TO START-X-IO
               INSPECT START-X-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (48:6) TO STOP-X-IO
               INSPECT STOP-X-IO REPLACING ALL ' ' BY '0'
               IF  STOP-X = ZERO
                   SET I-85                TO TRUE
               END-IF
               MOVE ACCOUNT-IO-AREA (54:4) TO NUMMER (1:4)
               MOVE ACCOUNT-IO-AREA (58:4) TO FORM (1:4)
               MOVE ACCOUNT-IO-AREA (62:2) TO KOPI (1:2)
               MOVE ACCOUNT-IO-AREA (64:8) TO ENH-IO
               INSPECT ENH-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (72:9) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               IF  PRIS = ZERO
                   SET I-77                TO TRUE
               END-IF
               MOVE ACCOUNT-IO-AREA (81:1) TO FAKT (1:1)
               MOVE ACCOUNT-IO-AREA (90:1) TO SLETT (1:1)
           END-EVALUATE.
 
       ACCOUNT-IDSET SECTION.
       ACCOUNT-IDSET-P.
           SET I-02                        TO TRUE.
 
       ACCOUNT-CHK-LEVEL SECTION.
       ACCOUNT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ACCOUNT-LEVEL-02
               MOVE ACCOUNT-IO-AREA (18:3) TO ACCOUNT-02-L1-FNR
               IF  ACCOUNT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ACCOUNT-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ACCOUNT-02-L1         TO THE-PRIOR-L1
               SET ACCOUNT-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
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
           IF  (I-02 AND I-40 AND I-U2)
               MOVE 'F'                    TO ACCOUNT-IO-AREA (81:1)
               REWRITE ACCOUNT-IO-AREA
           END-IF
           IF  (I-02 AND I-40)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE ' '                    TO LISTE-IO-AREA (27:1)
               MOVE FNR                    TO LISTE-IO-AREA (28:3)
               MOVE DATO                   TO LISTE-IO-AREA (32:8)
               MOVE NAVN                   TO LISTE-IO-AREA (41:8)
               MOVE SIGN-X                 TO LISTE-IO-AREA (52:2)
               IF  (I-10)
                   MOVE 'BEST.OPPGAVER  +' TO LISTE-IO-AREA (58:16)
               END-IF
               IF  (I-11)
                   MOVE 'FAST PRIS AVTALT' TO LISTE-IO-AREA (58:16)
               END-IF
               IF  (I-12)
                   MOVE 'ORDINÆR PRIS    ' TO LISTE-IO-AREA (58:16)
               END-IF
               MOVE KOPI                   TO LISTE-IO-AREA (76:2)
               MOVE FORM                   TO LISTE-IO-AREA (81:4)
               IF  (NOT-I-85)
                   MOVE START-X            TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO LISTE-IO-AREA (88:8)
               END-IF
               IF  (NOT-I-85)
                   MOVE STOP-X             TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO LISTE-IO-AREA (99:8)
               END-IF
               MOVE ENH                    TO XO-62YYZR
               MOVE XO-62YYZR              TO LISTE-IO-AREA (108:11)
               MOVE PRIS                   TO XO-63YYZR
               MOVE XO-63YYZR              TO LISTE-IO-AREA (121:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-40)
               MOVE SPACES TO DISKUT-IO-AREA
               INITIALIZE DISKUT-IO-AREA
               MOVE '399'                  TO DISKUT-IO-AREA (1:3)
               MOVE '001100'               TO DISKUT-IO-AREA (4:6)
               MOVE FNR                    TO DISKUT-IO-AREA (10:3)
               MOVE 'AD BES           '    TO DISKUT-IO-AREA (13:17)
      *                                  29 "AD EDI           "
               MOVE ANTFAK-IO              TO DISKUT-IO-AREA (30:5)
               MOVE '000'                  TO DISKUT-IO-AREA (35:3)
               MOVE LEVBEL-IO              TO DISKUT-IO-AREA (38:8)
               MOVE NAVN                   TO DISKUT-IO-AREA (46:8)
               MOVE SIGN-X                 TO DISKUT-IO-AREA (57:2)
               MOVE '             '        TO DISKUT-IO-AREA (60:13)
      *******************************************
      *  PRINTRUTINE FOR FØRSTESIDE PR. FIRMA.  *
      *******************************************
               WRITE DISKUT-IO-AREA
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '******************'   TO LISTE-IO-AREA (41:18)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '                   PROG.' TO LISTE-IO-AREA (23:24)
               MOVE OPPGNR                 TO LISTE-IO-AREA (47:6)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'ACCOUNTING LISTE        ' TO LISTE-IO-AREA (23:24)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '                        ' TO LISTE-IO-AREA (23:24)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     DENNE OPPGAVE'  TO LISTE-IO-AREA (17:19)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     FREMSTILT'      TO LISTE-IO-AREA (17:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (33:8)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     TILHØRER FIRMA' TO LISTE-IO-AREA (17:20)
               MOVE FIRMNR                 TO LISTE-IO-AREA (38:3)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE FIRMNA                 TO LISTE-IO-AREA (23:30)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '******************'   TO LISTE-IO-AREA (41:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-02 AND I-21)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AUTO DATA A/S'        TO LISTE-IO-AREA (1:13)
               MOVE '*   B E S T I L L I N G ' TO LISTE-IO-AREA (34:24)
               MOVE 'S  O P P G A V E R *' TO LISTE-IO-AREA (59:20)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (100:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (110:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (114:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (1:9)
               MOVE 'DATO     JOBNAVN  BEST. ' TO LISTE-IO-AREA (32:24)
               MOVE 'FAKT.MÅTE   '         TO LISTE-IO-AREA (58:12)
               MOVE '   KOPI FORM'         TO LISTE-IO-AREA (73:12)
               MOVE 'START'                TO LISTE-IO-AREA (88:5)
               MOVE 'STOP'                 TO LISTE-IO-AREA (99:4)
               MOVE 'ANT.ENH.   PRIS PR JOB' TO LISTE-IO-AREA (111:22)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AUTO DATA A/S'        TO LISTE-IO-AREA (1:13)
               MOVE '*   B E S T I L L I N G ' TO LISTE-IO-AREA (34:24)
               MOVE 'S  O P P G A V E R *' TO LISTE-IO-AREA (59:20)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (100:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (110:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (114:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (1:9)
               MOVE 'DATO     JOBNAVN  BEST. ' TO LISTE-IO-AREA (32:24)
               MOVE 'FAKT.MÅTE   '         TO LISTE-IO-AREA (58:12)
               MOVE '   KOPI FORM'         TO LISTE-IO-AREA (73:12)
               MOVE 'START'                TO LISTE-IO-AREA (88:5)
               MOVE 'STOP'                 TO LISTE-IO-AREA (99:4)
               MOVE 'ANT.ENH.   PRIS PR JOB' TO LISTE-IO-AREA (111:22)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-41)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT PR. FIRMA:  '  TO LISTE-IO-AREA (1:19)
               MOVE TOTSUM                 TO XO-73YYZR
               MOVE XO-73YYZR              TO LISTE-IO-AREA (119:14)
               INITIALIZE TOTSUM
               MOVE 3                      TO LISTE-AFTER-SPACE
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
           SET ACCOUNT-LEVEL-INIT          TO TRUE
           INITIALIZE ACCOUNT-DATA-FIELDS
           SET ACCOUNT-EOF-OFF             TO TRUE
           SET ACCOUNT-PROCESS             TO TRUE
           OPEN I-O ACCOUNT
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT DISKUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ACCOUNT
           CLOSE FIRMAF
           CLOSE DISKUT
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
