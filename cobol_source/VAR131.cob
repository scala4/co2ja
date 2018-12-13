       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR131R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR131.rpg
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
           SELECT ENRLIM
               ASSIGN TO ENRLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ENRLIM-STATUS.
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
           SELECT OUTPUN
               ASSIGN TO OUTPUN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ENRLIM
               RECORD CONTAINS 80.
       01  ENRLIM-IO-AREA.
           05  ENRLIM-IO-AREA-X.
               10  ENRLIM-KEY1.
                   15  ENRLIM-KEY1N        PICTURE S9(10).
               10  FILLER                  PICTURE X(70).
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
       FD OUTPUN
               RECORD CONTAINS 100.
       01  OUTPUN-IO-AREA.
           05  OUTPUN-IO-AREA-X            PICTURE X(100).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ENRLIM-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTPUN-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ENRLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-EOF-OFF          VALUE '0'.
               88  ENRLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-READ-OFF         VALUE '0'.
               88  ENRLIM-READ             VALUE '1'.
           05  ENRLIM-LOW-KEY              PICTURE X(10).
           05  ENRLIM-HIGH-KEY             PICTURE X(10).
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
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
           05  VAREMAS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
               10  BEHINN-IO.
                   15  BEHINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEHUT-IO.
                   15  BEHUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TILGDT-IO.
                   15  TILGDT              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  TEMPORARY-FIELDS.
               10  ANTSEL-IO.
                   15  ANTSEL              PICTURE S9(9).
               10  BEH-IO.
                   15  BEH                 PICTURE S9(7)V9(2).
               10  DAT1                    PICTURE X(4).
               10  DAT2                    PICTURE X(6).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  NYVTIL-IO.
                   15  NYVTIL              PICTURE S9(6).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  EDIT-BEH                PICTURE Z999999,99.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
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
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   SET VAREMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
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
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDSET
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
           IF  (I-01)
               SET NOT-I-20                TO TRUE
               ADD 1                       TO ANTSEL
      ***********************************************************
      * REGNE UT BEHOLDNING
      ***********************************************************
           END-IF
           SUBTRACT BEHUT FROM BEHINN  GIVING BEH
           SET NOT-I-65                    TO TRUE
           IF  BEH = 0,00
               SET I-65                    TO TRUE
           END-IF
           IF  (I-65)
               GO TO SLUTT-T
      ***********************************************************
      * SJEKKER OM TILGANGSDATO ER BLANK/NULL
      ***********************************************************
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  TILGDT = 0
               SET I-66                    TO TRUE
           END-IF
           IF  (NOT-I-66)
               GO TO SLUTT-T
      ***********************************************************
      * LEGGER DAGENS DATO INN I VARIABEL
      ***********************************************************
           END-IF
           MOVE UDAY                       TO DAT1 (1:2)
           MOVE UMONTH                     TO DAT1 (3:2)
           MOVE DAT1                       TO DAT2 (1:4)
           MOVE UYEAR                      TO DAT2 (5:2)
           MOVE DAT2                       TO DATO-IO
           ADD DATO TO ZERO            GIVING NYVTIL
      *                    Z-ADD050216    NYVTIL  60                      000300
           IF  (I-01)
               SET I-20                    TO TRUE
               ADD 1                       TO ANTKOR
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ENRLIM-READ-OFF
                    OR ENRLIM-EOF
                   IF  ENRLIM-READ
                       SET ENRLIM-READ-OFF TO TRUE
                       READ ENRLIM
                       AT END
                           SET ENRLIM-EOF  TO TRUE
                           SET VAREMAS-EOF TO TRUE
                           SUBTRACT 1    FROM LR-CHECK
                       NOT AT END
                           MOVE ENRLIM-IO-AREA (1:4) TO VAREMAS-KEY1
                       END-READ
                   END-IF
                   IF  ENRLIM-EOF-OFF
                   AND ENRLIM-READ-OFF
                       READ VAREMAS
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VNAVN (1:30)
               MOVE VAREMAS-IO-AREA (97:5) TO BEHINN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO BEHUT-IO
               MOVE VAREMAS-IO-AREA (156:4) TO TILGDT-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
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
               MOVE 7                      TO LISTE-AFTER-SKIP
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
           IF  (I-01 AND I-20)
               MOVE NYVTIL                 TO XO-60P
               MOVE XO-60P-EF              TO VAREMAS-IO-AREA (156:4)
               REWRITE VAREMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAREMAS'
               END-REWRITE
           END-IF
           IF  (I-01 AND I-20)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE FIRMA                  TO OUTPUN-IO-AREA (1:3)
               MOVE ';'                    TO OUTPUN-IO-AREA (4:1)
               MOVE ALFA                   TO OUTPUN-IO-AREA (5:3)
               MOVE ';'                    TO OUTPUN-IO-AREA (8:1)
               MOVE ARTNR                  TO OUTPUN-IO-AREA (9:20)
               MOVE ';'                    TO OUTPUN-IO-AREA (29:1)
               MOVE VNAVN                  TO OUTPUN-IO-AREA (30:30)
               MOVE ';'                    TO OUTPUN-IO-AREA (60:1)
               MOVE BEH                    TO EDIT-BEH
               MOVE EDIT-BEH               TO OUTPUN-IO-AREA (61:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (71:1)
               MOVE EDBNR                  TO OUTPUN-IO-AREA (72:7)
               MOVE ';'                    TO OUTPUN-IO-AREA (79:1)
               MOVE TILGDT                 TO XO-70U
               MOVE XO-70U (1:7)           TO OUTPUN-IO-AREA (80:7)
               MOVE ';'                    TO OUTPUN-IO-AREA (87:1)
               MOVE NYVTIL-IO              TO OUTPUN-IO-AREA (88:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (94:1)
               WRITE OUTPUN-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL BEHANDLET =    ' TO LISTE-IO-AREA (3:22)
               MOVE ANTSEL                 TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (30:11)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL KORRIGERT =    ' TO LISTE-IO-AREA (3:22)
               MOVE ANTKOR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (34:7)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'JOB = VAR959A   DATO =' TO LISTE-IO-AREA (3:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
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
           SET ENRLIM-EOF-OFF              TO TRUE
           SET ENRLIM-READ                 TO TRUE
           OPEN INPUT ENRLIM
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           OPEN I-O VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTPUN.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ENRLIM
           CLOSE VAREMAS
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTPUN.
 
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
