       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYS100R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: SYS100, SANERING AV SYSPARM.                 *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VSAMUKE                                      *
      *  LAGET DATO....: 09.10.01                                     *
      *  ENDRET........: 24.03.10 VISER FIRMAOPPLYSNINGER PÅ KVITT.   *
      *  RETTET........:                                              *
      *  INPUT.........: SEKVENSIELL UTBET.KID.FILE (SYSPARM).        *
      *  BEHANDLING....: FJERNER ALLE SYSPARM-RECORDS FOR FIRMA SOM   *
      *                  ER SLETTET.                                  *
      *  OUTPUT........: NY SYSPARM (SEKV.)                           *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: SYS100.rpg
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
           SELECT SYSPARM
               ASSIGN TO UT-S-SYSPARM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SYSPARM-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT SYSPARO
               ASSIGN TO UT-S-SYSPARO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SYSPARO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SYSPARM
               BLOCK CONTAINS 1600
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X           PICTURE X(160).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD SYSPARO
               BLOCK CONTAINS 1600
               RECORD CONTAINS 160.
       01  SYSPARO-IO-AREA.
           05  SYSPARO-IO-AREA-X           PICTURE X(160).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  SYSPARO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  SYSPARM-EOF-OFF         VALUE '0'.
               88  SYSPARM-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SYSPARM-READ-OFF        VALUE '0'.
               88  SYSPARM-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SYSPARM-PROCESS-OFF     VALUE '0'.
               88  SYSPARM-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SYSPARM-LEVEL-INIT-OFF  VALUE '0'.
               88  SYSPARM-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KLISTEO-DATA-FIELDS.
               10  KLISTEO-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-CLR-IO          PICTURE X VALUE 'Y'.
           05  SYSPARM-LEVEL-01.
               10  SYSPARM-01-L1.
                   15  SYSPARM-01-L1-SYSFIR PICTURE X(3).
           05  SYSPARM-DATA-FIELDS.
               10  REC160                  PICTURE X(160).
               10  REC010                  PICTURE X(10).
               10  SYSFIR                  PICTURE X(3).
           05  FIRMAF-DATA-FIELDS.
               10  FIRNVN                  PICTURE X(30).
               10  FIRSLE                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTGML-IO.
                   15  ANTGML              PICTURE S9(5).
               10  ANTSYS-IO.
                   15  ANTSYS              PICTURE S9(5).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YN9R               PICTURE ZZZZ9-.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  SYSPARM-PROCESS
               SET SYSPARM-PROCESS-OFF     TO TRUE
               SET SYSPARM-READ            TO TRUE
           END-IF
 
           IF  SYSPARM-READ
           AND RECORD-SELECTED-OFF
               PERFORM SYSPARM-GET
               SET SYSPARM-READ-OFF        TO TRUE
               IF  NOT SYSPARM-EOF
                   SET SYSPARM-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  SYSPARM-PROCESS
               PERFORM SYSPARM-IDSET
           END-IF
 
           IF  SYSPARM-PROCESS
               PERFORM SYSPARM-CHK-LEVEL
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
 
           IF  SYSPARM-PROCESS
               PERFORM SYSPARM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SYSPARM-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               PERFORM FISLET-S
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTGML
           END-IF
           IF  (I-01 AND NOT-I-10)
               ADD 1                       TO ANTSYS
           END-IF
           IF  (I-01 AND I-10)
               ADD 1                       TO ANTSAN
      *                    MOVE "SYSFIR  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGFLISTEO   SYSFIR           VIS INDIKATOR
      ****************************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA                      *
      ****************************************************************
           END-IF
           .
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-10                    TO TRUE
           MOVE SYSFIR                     TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-98                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-98                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-98)
               SET NOT-I-10                TO TRUE
               IF  FIRSLE = 'S'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-98)
               SET I-10                    TO TRUE
           END-IF.
      *
 
       SYSPARM-GET SECTION.
       SYSPARM-GET-P.
           IF  SYSPARM-EOF-OFF
               READ SYSPARM
               AT END
                   SET SYSPARM-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (1:160) TO REC160 (1:160)
               MOVE SYSPARM-IO-AREA (1:10) TO REC010 (1:10)
               MOVE SYSPARM-IO-AREA (1:3)  TO SYSFIR (1:3)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-01                        TO TRUE.
 
       SYSPARM-CHK-LEVEL SECTION.
       SYSPARM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SYSPARM-LEVEL-01
               MOVE SYSPARM-IO-AREA (1:3)  TO SYSPARM-01-L1-SYSFIR
               IF  SYSPARM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SYSPARM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SYSPARM-01-L1         TO THE-PRIOR-L1
               SET SYSPARM-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FIRNVN (1:30)
               MOVE FIRMAF-IO-AREA (123:1) TO FIRSLE (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
       KLISTEO-PRINT-LINE SECTION.
       KLISTEO-PRINT-LINE-P.
           IF  KLISTEO-BEFORE-SKIP > 0
               PERFORM KLISTEO-SKIP-BEFORE
           END-IF
           IF  KLISTEO-BEFORE-SPACE > 0
               PERFORM KLISTEO-SPACE-BEFORE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               IF  KLISTEO-AFTER-SPACE > 0
                   PERFORM KLISTEO-SPACE-AFTER
               END-IF
           ELSE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               PERFORM KLISTEO-SPACE-AFTER
           END-IF
           IF  KLISTEO-LINE-COUNT NOT < KLISTEO-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       KLISTEO-SKIP-BEFORE SECTION.
       KLISTEO-SKIP-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-BEFORE-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-BEFORE SECTION.
       KLISTEO-SPACE-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER KLISTEO-BEFORE-SPACE
                                                                 LINES
           ADD KLISTEO-BEFORE-SPACE        TO KLISTEO-LINE-COUNT
           MOVE SPACES TO KLISTEO-IO-AREA
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-BEFORE-SPACE.
 
       KLISTEO-SKIP-AFTER SECTION.
       KLISTEO-SKIP-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-AFTER-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-AFTER SECTION.
       KLISTEO-SPACE-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE KLISTEO-AFTER-SPACE LINES
           ADD KLISTEO-AFTER-SPACE         TO KLISTEO-LINE-COUNT
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO SYSPARO-IO-AREA
               INITIALIZE SYSPARO-IO-AREA
               MOVE REC160                 TO SYSPARO-IO-AREA (1:160)
               WRITE SYSPARO-IO-AREA
           END-IF
           IF  (I-L1 AND I-10)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'FIRMA: '              TO KLISTEO-IO-AREA (1:7)
               MOVE SYSFIR                 TO KLISTEO-IO-AREA (8:3)
               MOVE FIRNVN                 TO KLISTEO-IO-AREA (16:30)
               MOVE FIRSLE                 TO KLISTEO-IO-AREA (50:1)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF
           IF  (I-01 AND I-10)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE REC010                 TO KLISTEO-IO-AREA (1:10)
               MOVE '<<< SLETTES >>>'      TO KLISTEO-IO-AREA (12:15)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV SYSPARM.             ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VSAMUKE'          TO KLISTEO-IO-AREA (85:11)
               MOVE 'PROGRAM=SYS100'       TO KLISTEO-IO-AREA (97:14)
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV SYSPARM.             ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VSAMUKE'          TO KLISTEO-IO-AREA (85:11)
               MOVE 'PROGRAM=SYS100'       TO KLISTEO-IO-AREA (97:14)
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ GAMMEL SYSPARM' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTGML                 TO XO-50YN9R
               MOVE XO-50YN9R              TO KLISTEO-IO-AREA (31:6)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL FJERNET          ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTSAN                 TO XO-50YN9R
               MOVE XO-50YN9R              TO KLISTEO-IO-AREA (31:6)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL PÅ NY SYSPARM    ' TO KLISTEO-IO-AREA
                                                                (7:24)
               MOVE ANTSYS                 TO XO-50YN9R
               MOVE XO-50YN9R              TO KLISTEO-IO-AREA (31:6)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF
           IF  (I-U8 AND I-02)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'DUMMY'                TO KLISTEO-IO-AREA (76:5)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
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
           SET SYSPARM-LEVEL-INIT          TO TRUE
           INITIALIZE SYSPARM-DATA-FIELDS
           SET SYSPARM-EOF-OFF             TO TRUE
           SET SYSPARM-PROCESS             TO TRUE
           OPEN INPUT SYSPARM
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT SYSPARO
           OPEN OUTPUT KLISTEO
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE SYSPARM
           CLOSE FIRMAF
           CLOSE SYSPARO
           IF KLISTEO-IO-AREA NOT = SPACES
             WRITE KLISTEO-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO KLISTEO-IO-AREA
           END-IF
           CLOSE KLISTEO.
 
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
