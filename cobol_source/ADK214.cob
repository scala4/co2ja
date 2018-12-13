       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADK214R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: ADK214                                          *
      *  PROGRAMERER: RUNE ERSVIK                                     *
      *  PROGRAMERT.: 16.06.14                                        *
      *  RETTET.....: XX.XX.XX                                        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ADK214.rpg
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
           SELECT ORGNRF
               ASSIGN TO UT-S-ORGNRF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORGNRF-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT UTLIST
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTLIST-STATUS.
           SELECT UTFIL
               ASSIGN TO UTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORGNRF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  ORGNRF-IO-AREA.
           05  ORGNRF-IO-AREA-X            PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD UTLIST
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  UTLIST-IO-PRINT.
           05  UTLIST-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 UTLIST-IO-AREA.
           05  UTLIST-IO-AREA-X            PICTURE X(132).
       FD UTFIL
               RECORD CONTAINS 150.
       01  UTFIL-IO-AREA.
           05  UTFIL-IO-AREA-X             PICTURE X(150).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORGNRF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  UTLIST-STATUS               PICTURE 99 VALUE 0.
           10  UTFIL-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORGNRF-EOF-OFF          VALUE '0'.
               88  ORGNRF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORGNRF-READ-OFF         VALUE '0'.
               88  ORGNRF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORGNRF-PROCESS-OFF      VALUE '0'.
               88  ORGNRF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORGNRF-LEVEL-INIT-OFF   VALUE '0'.
               88  ORGNRF-LEVEL-INIT       VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  UTLIST-DATA-FIELDS.
               10  UTLIST-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  UTLIST-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  UTLIST-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  UTLIST-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  UTLIST-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  UTLIST-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  UTLIST-CLR-IO           PICTURE X VALUE 'Y'.
           05  ORGNRF-LEVEL-01.
               10  ORGNRF-01-L1.
                   15  ORGNRF-01-L1-RECART PICTURE X(1).
           05  ORGNRF-DATA-FIELDS.
               10  RECART                  PICTURE X(1).
               10  FNR                     PICTURE X(3).
               10  USERID                  PICTURE X(8).
               10  PIDENT                  PICTURE X(4).
               10  SKODE                   PICTURE X(1).
               10  ORGNR-IO.
                   15  ORGNR               PICTURE S9(5).
               10  UTSKR                   PICTURE X(1).
               10  PTYPE                   PICTURE X(1).
               10  KNR                     PICTURE X(6).
               10  ANTKOP-IO.
                   15  ANTKOP              PICTURE S9(2).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
           05  KUNDEMX-DATA-FIELDS.
      *  RECART 1
               10  ORGN1                   PICTURE X(9).
      *****************************************************************
      *                                                               *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  TELLER-IO.
                   15  TELLER              PICTURE S9(5).
               10  KEY-X                   PICTURE X(9).
               10  KEY10                   PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-40YY9                PICTURE Z.ZZ9.
               10  XO-50YN9                PICTURE ZZZZ9.
               10  XO-20YN9                PICTURE Z9.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORGNRF-PROCESS
               SET ORGNRF-PROCESS-OFF      TO TRUE
               SET ORGNRF-READ             TO TRUE
           END-IF
 
           IF  ORGNRF-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORGNRF-GET
               SET ORGNRF-READ-OFF         TO TRUE
               IF  NOT ORGNRF-EOF
                   PERFORM ORGNRF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORGNRF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORGNRF-PROCESS
               PERFORM ORGNRF-IDSET
           END-IF
 
           IF  ORGNRF-PROCESS
               PERFORM ORGNRF-CHK-LEVEL
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
 
           IF  ORGNRF-PROCESS
               PERFORM ORGNRF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORGNRF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01 AND NOT-I-49)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TID (1:6)
               MOVE SYSTEM-DATE            TO TID (7:6)
           END-IF
           IF  (I-01)
               SET I-49                    TO TRUE
               ADD 1                       TO TELLER
      *
           END-IF
           IF  (I-01)
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
               MOVE KEY-X                  TO KEY10 (1:9)
      *  RECART 1
           END-IF
           IF  (I-01)
               MOVE '1'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF.
 
       ORGNRF-GET SECTION.
       ORGNRF-GET-P.
           IF  ORGNRF-EOF-OFF
               READ ORGNRF
               AT END
                   SET ORGNRF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORGNRF-FLDSET SECTION.
       ORGNRF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORGNRF-IO-AREA (1:1) = 'B' )
               MOVE ORGNRF-IO-AREA (1:1)   TO RECART (1:1)
               MOVE ORGNRF-IO-AREA (2:3)   TO FNR (1:3)
               MOVE ORGNRF-IO-AREA (5:8)   TO USERID (1:8)
               MOVE ORGNRF-IO-AREA (13:4)  TO PIDENT (1:4)
               MOVE ORGNRF-IO-AREA (17:1)  TO SKODE (1:1)
               MOVE ORGNRF-IO-AREA (18:5)  TO ORGNR-IO
               INSPECT ORGNR-IO REPLACING ALL ' ' BY '0'
               MOVE ORGNRF-IO-AREA (23:1)  TO UTSKR (1:1)
               MOVE ORGNRF-IO-AREA (24:1)  TO PTYPE (1:1)
               MOVE ORGNRF-IO-AREA (25:6)  TO KNR (1:6)
               MOVE ORGNRF-IO-AREA (31:2)  TO ANTKOP-IO
               INSPECT ANTKOP-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       ORGNRF-IDCHK SECTION.
       ORGNRF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORGNRF-IO-AREA (1:1) = 'B' )
             OR ( ORGNRF-IO-AREA (1:1) NOT = 'B' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORGNRF-IDSET SECTION.
       ORGNRF-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORGNRF-IO-AREA (1:1) = 'B' )
               SET I-01                    TO TRUE
           WHEN ( ORGNRF-IO-AREA (1:1) NOT = 'B' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       ORGNRF-CHK-LEVEL SECTION.
       ORGNRF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORGNRF-IO-AREA (1:1) = 'B' )
               MOVE LOW-VALUES             TO ORGNRF-LEVEL-01
               MOVE ORGNRF-IO-AREA (1:1)   TO ORGNRF-01-L1-RECART
               IF  ORGNRF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORGNRF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORGNRF-01-L1          TO THE-PRIOR-L1
               SET ORGNRF-LEVEL-INIT       TO TRUE
           WHEN ( ORGNRF-IO-AREA (1:1) NOT = 'B' )
               CONTINUE
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
           SET I-03                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (180:9) TO ORGN1 (1:9)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-04                        TO TRUE.
 
       UTLIST-PRINT-LINE SECTION.
       UTLIST-PRINT-LINE-P.
           IF  UTLIST-BEFORE-SKIP > 0
               PERFORM UTLIST-SKIP-BEFORE
           END-IF
           IF  UTLIST-BEFORE-SPACE > 0
               PERFORM UTLIST-SPACE-BEFORE
               IF  UTLIST-AFTER-SKIP > 0
                   PERFORM UTLIST-SKIP-AFTER
               END-IF
               IF  UTLIST-AFTER-SPACE > 0
                   PERFORM UTLIST-SPACE-AFTER
               END-IF
           ELSE
               IF  UTLIST-AFTER-SKIP > 0
                   PERFORM UTLIST-SKIP-AFTER
               END-IF
               PERFORM UTLIST-SPACE-AFTER
           END-IF
           IF  UTLIST-LINE-COUNT NOT < UTLIST-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       UTLIST-SKIP-BEFORE SECTION.
       UTLIST-SKIP-BEFORE-P.
           WRITE UTLIST-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO UTLIST-LINE-COUNT
           MOVE 0                          TO UTLIST-BEFORE-SKIP
           INITIALIZE UTLIST-IO-AREA.
 
       UTLIST-SPACE-BEFORE SECTION.
       UTLIST-SPACE-BEFORE-P.
           WRITE UTLIST-IO-PRINT        AFTER UTLIST-BEFORE-SPACE LINES
           ADD UTLIST-BEFORE-SPACE         TO UTLIST-LINE-COUNT
           MOVE SPACES TO UTLIST-IO-AREA
           INITIALIZE UTLIST-IO-AREA
           MOVE 0                          TO UTLIST-BEFORE-SPACE.
 
       UTLIST-SKIP-AFTER SECTION.
       UTLIST-SKIP-AFTER-P.
           WRITE UTLIST-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO UTLIST-LINE-COUNT
           MOVE 0                          TO UTLIST-AFTER-SKIP
           INITIALIZE UTLIST-IO-AREA.
 
       UTLIST-SPACE-AFTER SECTION.
       UTLIST-SPACE-AFTER-P.
           WRITE UTLIST-IO-PRINT       BEFORE UTLIST-AFTER-SPACE LINES
           ADD UTLIST-AFTER-SPACE          TO UTLIST-LINE-COUNT
           INITIALIZE UTLIST-IO-AREA
           MOVE 0                          TO UTLIST-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE KNR                    TO UTLIST-IO-AREA (1:6)
               IF  (I-15)
                   MOVE '** UKJENT KUNDENR. **' TO UTLIST-IO-AREA
                                                                (9:21)
               END-IF
               IF  (NOT-I-15)
                   MOVE KNAVN1             TO UTLIST-IO-AREA (8:30)
               END-IF
               IF  (NOT-I-15)
                   MOVE KNAVN2             TO UTLIST-IO-AREA (38:30)
               END-IF
               MOVE ORGNR                  TO XO-50YN9
               MOVE XO-50YN9               TO UTLIST-IO-AREA (69:5)
               MOVE FNR                    TO UTLIST-IO-AREA (75:3)
               MOVE USERID                 TO UTLIST-IO-AREA (80:8)
               MOVE PIDENT                 TO UTLIST-IO-AREA (89:4)
               MOVE SKODE                  TO UTLIST-IO-AREA (95:1)
               MOVE UTSKR                  TO UTLIST-IO-AREA (100:1)
               MOVE PTYPE                  TO UTLIST-IO-AREA (104:1)
               MOVE ANTKOP                 TO XO-20YN9
               MOVE XO-20YN9               TO UTLIST-IO-AREA (108:2)
               MOVE RECART                 TO UTLIST-IO-AREA (113:1)
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-15)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE KNR                    TO UTFIL-IO-AREA (1:6)
               MOVE ';'                    TO UTFIL-IO-AREA (7:1)
               IF  (I-15)
                   MOVE '** UKJENT KUNDENR. **' TO UTFIL-IO-AREA (9:21)
               END-IF
               IF  (NOT-I-15)
                   MOVE KNAVN1             TO UTFIL-IO-AREA (8:30)
               END-IF
               MOVE ';'                    TO UTFIL-IO-AREA (38:1)
               MOVE ORGNR-IO               TO UTFIL-IO-AREA (40:5)
               MOVE ';'                    TO UTFIL-IO-AREA (45:1)
               MOVE FNR                    TO UTFIL-IO-AREA (46:3)
               MOVE ';'                    TO UTFIL-IO-AREA (49:1)
               MOVE USERID                 TO UTFIL-IO-AREA (50:8)
               MOVE ';'                    TO UTFIL-IO-AREA (58:1)
               MOVE SKODE                  TO UTFIL-IO-AREA (59:1)
               MOVE ';'                    TO UTFIL-IO-AREA (60:1)
               MOVE ORGN1                  TO UTFIL-IO-AREA (61:9)
               MOVE ';'                    TO UTFIL-IO-AREA (70:1)
               WRITE UTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE '* * *   UTLISTING AV' TO UTLIST-IO-AREA (1:20)
               MOVE ' ORG. NR. FILE   * * *' TO UTLIST-IO-AREA (21:22)
               MOVE 'FREMFØRT '            TO UTLIST-IO-AREA (49:9)
      *                                  64 "DATO: "
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO UTLIST-IO-AREA (69:8)
               MOVE 'KL:'                  TO UTLIST-IO-AREA (81:3)
               MOVE TID                    TO EDIT-DATE
               MOVE EDIT-DATE (4:11)       TO UTLIST-IO-AREA (84:11)
               MOVE 'SIDE'                 TO UTLIST-IO-AREA (104:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO UTLIST-IO-AREA (108:5)
               MOVE 01                     TO UTLIST-BEFORE-SKIP
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE '------------------------' TO UTLIST-IO-AREA (1:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (73:24)
               MOVE '-----------------'    TO UTLIST-IO-AREA (97:17)
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE 'K.NR.'                TO UTLIST-IO-AREA (2:5)
               MOVE 'KUNDENAVN'            TO UTLIST-IO-AREA (9:9)
               MOVE 'O.NR'                 TO UTLIST-IO-AREA (69:4)
               MOVE 'FIRMA'                TO UTLIST-IO-AREA (74:5)
               MOVE 'USER-ID'              TO UTLIST-IO-AREA (80:7)
               MOVE 'P.ID'                 TO UTLIST-IO-AREA (89:4)
               MOVE 'S.K'                  TO UTLIST-IO-AREA (94:3)
               MOVE 'USKR'                 TO UTLIST-IO-AREA (98:4)
               MOVE 'P.T'                  TO UTLIST-IO-AREA (103:3)
               MOVE 'ANT'                  TO UTLIST-IO-AREA (107:3)
               MOVE 'R.A'                  TO UTLIST-IO-AREA (111:3)
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE '------------------------' TO UTLIST-IO-AREA (1:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (73:24)
               MOVE '-----------------'    TO UTLIST-IO-AREA (97:17)
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L1)
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE '* * *   UTLISTING AV' TO UTLIST-IO-AREA (1:20)
               MOVE ' ORG. NR. FILE   * * *' TO UTLIST-IO-AREA (21:22)
               MOVE 'FREMFØRT '            TO UTLIST-IO-AREA (49:9)
      *                                  64 "DATO: "
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO UTLIST-IO-AREA (69:8)
               MOVE 'KL:'                  TO UTLIST-IO-AREA (81:3)
               MOVE TID                    TO EDIT-DATE
               MOVE EDIT-DATE (4:11)       TO UTLIST-IO-AREA (84:11)
               MOVE 'SIDE'                 TO UTLIST-IO-AREA (104:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO UTLIST-IO-AREA (108:5)
               MOVE 01                     TO UTLIST-BEFORE-SKIP
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE '------------------------' TO UTLIST-IO-AREA (1:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (73:24)
               MOVE '-----------------'    TO UTLIST-IO-AREA (97:17)
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE 'K.NR.'                TO UTLIST-IO-AREA (2:5)
               MOVE 'KUNDENAVN'            TO UTLIST-IO-AREA (9:9)
               MOVE 'O.NR'                 TO UTLIST-IO-AREA (69:4)
               MOVE 'FIRMA'                TO UTLIST-IO-AREA (74:5)
               MOVE 'USER-ID'              TO UTLIST-IO-AREA (80:7)
               MOVE 'P.ID'                 TO UTLIST-IO-AREA (89:4)
               MOVE 'S.K'                  TO UTLIST-IO-AREA (94:3)
               MOVE 'USKR'                 TO UTLIST-IO-AREA (98:4)
               MOVE 'P.T'                  TO UTLIST-IO-AREA (103:3)
               MOVE 'ANT'                  TO UTLIST-IO-AREA (107:3)
               MOVE 'R.A'                  TO UTLIST-IO-AREA (111:3)
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE '------------------------' TO UTLIST-IO-AREA (1:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO UTLIST-IO-AREA
                                                               (73:24)
               MOVE '-----------------'    TO UTLIST-IO-AREA (97:17)
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO UTLIST-IO-AREA
               INITIALIZE UTLIST-IO-AREA
               MOVE '** ANTALL RECORDS   ' TO UTLIST-IO-AREA (1:20)
               MOVE TELLER                 TO XO-50YN9
               MOVE XO-50YN9               TO UTLIST-IO-AREA (100:5)
               MOVE '**'                   TO UTLIST-IO-AREA (112:2)
      *TFIL   D        1P
               MOVE 1                      TO UTLIST-AFTER-SPACE
               PERFORM UTLIST-PRINT-LINE
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
           SET ORGNRF-LEVEL-INIT           TO TRUE
           INITIALIZE ORGNRF-DATA-FIELDS
           SET ORGNRF-EOF-OFF              TO TRUE
           SET ORGNRF-PROCESS              TO TRUE
           OPEN INPUT ORGNRF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           OPEN OUTPUT UTLIST
           INITIALIZE UTLIST-IO-AREA
           INITIALIZE UTLIST-DATA-FIELDS
           MOVE 57                         TO UTLIST-MAX-LINES
           OPEN OUTPUT UTFIL.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORGNRF
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           IF UTLIST-IO-AREA NOT = SPACES
             WRITE UTLIST-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO UTLIST-IO-AREA
           END-IF
           CLOSE UTLIST
           CLOSE UTFIL.
 
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
