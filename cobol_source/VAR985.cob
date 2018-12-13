       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR985R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: VAR985  ENDRER MÅNED OG ÅR I VARENAVN I VARE.MASTER  *
      *  10/12-01 PROGRAMMERT AV ESPEN LARSEN                         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR985.rpg
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
           SELECT MNDPAR
               ASSIGN TO UT-S-MNDPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MNDPAR-STATUS.
           SELECT KORRFIL
               ASSIGN TO UT-S-KORRFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORRFIL-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MNDPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  MNDPAR-IO-AREA.
           05  MNDPAR-IO-AREA-X            PICTURE X(100).
       FD KORRFIL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KORRFIL-IO-AREA.
           05  KORRFIL-IO-AREA-X           PICTURE X(80).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
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
           10  MNDPAR-STATUS               PICTURE 99 VALUE 0.
           10  KORRFIL-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDPAR-EOF-OFF          VALUE '0'.
               88  MNDPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDPAR-READ-OFF         VALUE '0'.
               88  MNDPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDPAR-PROCESS-OFF      VALUE '0'.
               88  MNDPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORRFIL-EOF-OFF         VALUE '0'.
               88  KORRFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORRFIL-READ-OFF        VALUE '0'.
               88  KORRFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORRFIL-PROCESS-OFF     VALUE '0'.
               88  KORRFIL-PROCESS         VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  MNDPAR-DATA-FIELDS.
               10  AAR                     PICTURE X(4).
               10  MND3F                   PICTURE X(3).
           05  KORRFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  EDBKEY                  PICTURE X(10).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
           05  TEMPORARY-FIELDS.
               10  ANTLES-IO.
                   15  ANTLES              PICTURE S9(5).
               10  ANTER2-IO.
                   15  ANTER2              PICTURE S9(5).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-40YY9                PICTURE Z.ZZ9.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  MNDPAR-PROCESS
               SET MNDPAR-PROCESS-OFF      TO TRUE
               SET MNDPAR-READ             TO TRUE
           END-IF
 
           IF  MNDPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM MNDPAR-GET
               SET MNDPAR-READ-OFF         TO TRUE
               IF  NOT MNDPAR-EOF
                   SET MNDPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  KORRFIL-PROCESS
               SET KORRFIL-PROCESS-OFF     TO TRUE
               SET KORRFIL-READ            TO TRUE
           END-IF
 
           IF  KORRFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM KORRFIL-GET
               SET KORRFIL-READ-OFF        TO TRUE
               IF  NOT KORRFIL-EOF
                   SET KORRFIL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  MNDPAR-PROCESS
               PERFORM MNDPAR-IDSET
           END-IF
 
           IF  KORRFIL-PROCESS
               PERFORM KORRFIL-IDSET
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
           PERFORM DETAIL-OVERFLOW
 
           IF  MNDPAR-PROCESS
               PERFORM MNDPAR-FLDSET
           END-IF
 
           IF  KORRFIL-PROCESS
               PERFORM KORRFIL-FLDSET
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
           SET NOT-I-31                    TO TRUE
           SET NOT-I-10                    TO TRUE
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO ANTLES
      *****************************************************************
      * OPPSLAG MOT VARE.MASTER OPPDATERE VARENAVN.                   *
      *****************************************************************
           MOVE EDBKEY                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-15                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-15                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (I-15)
               ADD 1                       TO ANTER2
               GO TO SLUTT-T
           END-IF
           SET I-31                        TO TRUE
           IF  (I-31)
               ADD 1                       TO ANTKOR
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       MNDPAR-GET SECTION.
       MNDPAR-GET-P.
           IF  MNDPAR-EOF-OFF
               READ MNDPAR
               AT END
                   SET MNDPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MNDPAR-FLDSET SECTION.
       MNDPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MNDPAR-IO-AREA (3:4)   TO AAR (1:4)
               MOVE MNDPAR-IO-AREA (9:3)   TO MND3F (1:3)
           END-EVALUATE.
 
       MNDPAR-IDSET SECTION.
       MNDPAR-IDSET-P.
           SET I-02                        TO TRUE.
 
       KORRFIL-GET SECTION.
       KORRFIL-GET-P.
           IF  KORRFIL-EOF-OFF
               READ KORRFIL
               AT END
                   SET KORRFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORRFIL-FLDSET SECTION.
       KORRFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KORRFIL-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KORRFIL-IO-AREA (4:7)  TO EDBNR (1:7)
               MOVE KORRFIL-IO-AREA (1:10) TO EDBKEY (1:10)
           END-EVALUATE.
 
       KORRFIL-IDSET SECTION.
       KORRFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VNAVN (1:30)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-04                        TO TRUE.
 
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
           IF  (I-01 AND I-31)
               MOVE MND3F                  TO VAREMAS-IO-AREA (50:3)
               MOVE AAR                    TO VAREMAS-IO-AREA (55:4)
               REWRITE VAREMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAREMAS'
               END-REWRITE
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DOP80UM  '            TO LISTE-IO-AREA (2:9)
               MOVE 'ENDRING VARENAVN'     TO LISTE-IO-AREA (13:16)
               MOVE 'OPPDATER VAREMASTER ' TO LISTE-IO-AREA (31:20)
               MOVE 'FOR AUTOFAKTURA  '    TO LISTE-IO-AREA (52:17)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (69:8)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (81:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'EDB.NR.'              TO LISTE-IO-AREA (5:7)
               MOVE 'GML. VARENAVN'        TO LISTE-IO-AREA (13:13)
               MOVE 'NYTT VARENAVN'        TO LISTE-IO-AREA (44:13)
               MOVE 'MERKNADER.  '         TO LISTE-IO-AREA (75:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE EDBNR                  TO LISTE-IO-AREA (5:7)
               MOVE VNAVN                  TO LISTE-IO-AREA (13:30)
               MOVE VNAVN                  TO LISTE-IO-AREA (44:30)
               MOVE MND3F                  TO LISTE-IO-AREA (58:3)
               MOVE AAR                    TO LISTE-IO-AREA (63:4)
               IF  (NOT-I-31)
                   MOVE 'UKJENT VARE.'     TO LISTE-IO-AREA (75:12)
               END-IF
               IF  (I-31)
                   MOVE 'OPPDATERT OK'     TO LISTE-IO-AREA (75:12)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DOP80UM  '            TO LISTE-IO-AREA (2:9)
               MOVE 'ENDRING VARENAVN'     TO LISTE-IO-AREA (13:16)
               MOVE 'OPPDATER VAREMASTER ' TO LISTE-IO-AREA (31:20)
               MOVE 'FOR AUTOFAKTURA  '    TO LISTE-IO-AREA (52:17)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (69:8)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (81:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'EDB.NR.'              TO LISTE-IO-AREA (5:7)
               MOVE 'GML. VARENAVN'        TO LISTE-IO-AREA (13:13)
               MOVE 'NYTT VARENAVN'        TO LISTE-IO-AREA (44:13)
               MOVE 'MERKNADER.  '         TO LISTE-IO-AREA (75:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE EDBNR                  TO LISTE-IO-AREA (5:7)
               MOVE VNAVN                  TO LISTE-IO-AREA (13:30)
               MOVE VNAVN                  TO LISTE-IO-AREA (44:30)
               MOVE MND3F                  TO LISTE-IO-AREA (58:3)
               MOVE AAR                    TO LISTE-IO-AREA (63:4)
               IF  (NOT-I-31)
                   MOVE 'UKJENT VARE.'     TO LISTE-IO-AREA (75:12)
               END-IF
               IF  (I-31)
                   MOVE 'OPPDATERT OK'     TO LISTE-IO-AREA (75:12)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTLES                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (15:6)
               MOVE 'RECORDS ER LEST INN.    ' TO LISTE-IO-AREA (22:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTKOR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (15:6)
               MOVE 'RECORDS ER I OPPDATERT. ' TO LISTE-IO-AREA (22:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTER2                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (15:6)
               MOVE 'RECORDS IKKE I VARE.FILE' TO LISTE-IO-AREA (22:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE MNDPAR-DATA-FIELDS
           SET MNDPAR-EOF-OFF              TO TRUE
           SET MNDPAR-PROCESS              TO TRUE
           OPEN INPUT MNDPAR
           INITIALIZE KORRFIL-DATA-FIELDS
           SET KORRFIL-EOF-OFF             TO TRUE
           SET KORRFIL-PROCESS             TO TRUE
           OPEN INPUT KORRFIL
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN I-O VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE MNDPAR
           CLOSE KORRFIL
           CLOSE VAREMAS
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
