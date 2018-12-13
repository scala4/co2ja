       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASS100R.
      **********************************************  Z-WIN-RPG2   ****
      *         STND. AVSTEMMINGSPROGRAM.                            *
      *    ORD1 = DAGLIG ORDRERUTINE.                                *
      *    FAK1 = FAKTURARUTINE FRA ORDRERUTINEN.                    *
      *    REG1 = DAGLIG REGNSKAPS RUTINE.                           *
      *    RES1 = DAGLIG RESKONTRO RUTINE.                           *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ASS100.rpg
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
           SELECT AVSTEMF
               ASSIGN TO AVSTEMF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AVSTEMF-STATUS
               RECORD KEY IS AVSTEMF-KEY1.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD AVSTEMF
               RECORD CONTAINS 1000.
       01  AVSTEMF-IO-AREA.
           05  AVSTEMF-IO-AREA-X.
               10  AVSTEMF-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD TOTALER
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  AVSTEMF-STATUS              PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
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
           05  AVSTEMF-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  TOTALER-DATA-FIELDS.
               10  TOTALER-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-CLR-IO          PICTURE X VALUE 'Y'.
           05  PARAM-DATA-FIELDS.
               10  AVSKEY                  PICTURE X(3).
               10  RUTID                   PICTURE X(4).
           05  AVSTEMF-DATA-FIELDS.
               10  ORDDTO-IO.
                   15  ORDDTO              PICTURE S9(6).
               10  ORDNYE-IO.
                   15  ORDNYE              PICTURE S9(6).
               10  ORDUTG-IO.
                   15  ORDUTG              PICTURE S9(6).
               10  ORDIKK-IO.
                   15  ORDIKK              PICTURE S9(6).
               10  ORDFAK-IO.
                   15  ORDFAK              PICTURE S9(6).
               10  ORDFIL-IO.
                   15  ORDFIL              PICTURE S9(6).
               10  ORDIFM-IO.
                   15  ORDIFM              PICTURE S9(6).
               10  ORDFAI-IO.
                   15  ORDFAI              PICTURE S9(6).
               10  ORDFAE-IO.
                   15  ORDFAE              PICTURE S9(6).
               10  ORDDT2-IO.
                   15  ORDDT2              PICTURE S9(6).
               10  ORDFI2-IO.
                   15  ORDFI2              PICTURE S9(6).
               10  ORDFA2-IO.
                   15  ORDFA2              PICTURE S9(6).
           05  TEMPORARY-FIELDS.
               10  AVSSUM-IO.
                   15  AVSSUM              PICTURE S9(6).
               10  ORDDIF-IO.
                   15  ORDDIF              PICTURE S9(6).
               10  FAKTOT-IO.
                   15  FAKTOT              PICTURE S9(6).
               10  FAKDIF-IO.
                   15  FAKDIF              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-60YY9R               PICTURE ZZZ.ZZ9-.
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
           IF  (I-01)
               PERFORM AVSLES-S
           END-IF
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  RUTID = 'ORD1'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-11 AND I-97)
               PERFORM ORDRUT-S
      *****************************************************************
      *  SUBRUTINE FOR LES AV AVSTEMMINGSFILE. RUTINE = KEY.          *
      *****************************************************************
           END-IF
           .
 
       AVSLES-S SECTION.
       AVSLES-S-P.
           MOVE AVSKEY                     TO AVSTEMF-KEY1
           READ AVSTEMF RECORD KEY IS AVSTEMF-KEY1
           INVALID KEY
               SET I-95                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-95                TO TRUE
               PERFORM AVSTEMF-IDCHK
               PERFORM AVSTEMF-FLDSET
               PERFORM AVSTEMF-IDSET
           END-READ
           IF  (NOT-I-95)
               SET I-97                    TO TRUE
           END-IF.
      *******************************************************
      *   SUBRUTINE FOR AVSTEMMING AV DAGLIG ORDRERUTINE.   *
      *******************************************************
 
       ORDRUT-S SECTION.
       ORDRUT-S-P.
           ADD ORDFI2                      TO AVSSUM
           ADD ORDNYE                      TO AVSSUM
           SUBTRACT ORDUTG                 FROM AVSSUM
           SUBTRACT ORDIKK                 FROM AVSSUM
           SUBTRACT ORDFAK                 FROM AVSSUM
           SUBTRACT ORDFIL FROM AVSSUM GIVING ORDDIF
           SET NOT-I-51                    TO TRUE
           IF  ORDDIF NOT = 0
               SET I-51                    TO TRUE
           END-IF
           ADD ORDFA2 TO ORDFAE        GIVING FAKTOT
           SUBTRACT FAKTOT FROM ORDFAK GIVING FAKDIF
           SET NOT-I-52                    TO TRUE
           IF  FAKDIF NOT = 0
               SET I-52                    TO TRUE
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
           WHEN ( PARAM-IO-AREA (1:1) = 'P' )
               MOVE PARAM-IO-AREA (17:3)   TO AVSKEY (1:3)
               MOVE PARAM-IO-AREA (17:4)   TO RUTID (1:4)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       AVSTEMF-FLDSET SECTION.
       AVSTEMF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( AVSTEMF-IO-AREA (1:1) = 'O'
            AND   AVSTEMF-IO-AREA (2:1) = 'R'
            AND   AVSTEMF-IO-AREA (3:1) = 'D' )
               MOVE AVSTEMF-IO-AREA (11:6) TO ORDDTO-IO
               INSPECT ORDDTO-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (17:6) TO ORDNYE-IO
               INSPECT ORDNYE-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (23:6) TO ORDUTG-IO
               INSPECT ORDUTG-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (29:6) TO ORDIKK-IO
               INSPECT ORDIKK-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (35:6) TO ORDFAK-IO
               INSPECT ORDFAK-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (41:6) TO ORDFIL-IO
               INSPECT ORDFIL-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (47:6) TO ORDIFM-IO
               INSPECT ORDIFM-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (53:6) TO ORDFAI-IO
               INSPECT ORDFAI-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (59:6) TO ORDFAE-IO
               INSPECT ORDFAE-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (131:6) TO ORDDT2-IO
               INSPECT ORDDT2-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (161:6) TO ORDFI2-IO
               INSPECT ORDFI2-IO REPLACING ALL ' ' BY '0'
               MOVE AVSTEMF-IO-AREA (173:6) TO ORDFA2-IO
               INSPECT ORDFA2-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       AVSTEMF-IDCHK SECTION.
       AVSTEMF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( AVSTEMF-IO-AREA (1:1) = 'O'
            AND   AVSTEMF-IO-AREA (2:1) = 'R'
            AND   AVSTEMF-IO-AREA (3:1) = 'D' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       AVSTEMF-IDSET SECTION.
       AVSTEMF-IDSET-P.
           EVALUATE TRUE
           WHEN ( AVSTEMF-IO-AREA (1:1) = 'O'
            AND   AVSTEMF-IO-AREA (2:1) = 'R'
            AND   AVSTEMF-IO-AREA (3:1) = 'D' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       TOTALER-PRINT-LINE SECTION.
       TOTALER-PRINT-LINE-P.
           IF  TOTALER-BEFORE-SKIP > 0
               PERFORM TOTALER-SKIP-BEFORE
           END-IF
           IF  TOTALER-BEFORE-SPACE > 0
               PERFORM TOTALER-SPACE-BEFORE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               IF  TOTALER-AFTER-SPACE > 0
                   PERFORM TOTALER-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               PERFORM TOTALER-SPACE-AFTER
           END-IF
           IF  TOTALER-LINE-COUNT NOT < TOTALER-MAX-LINES
               MOVE 7                      TO TOTALER-AFTER-SKIP
           END-IF.
 
       TOTALER-SKIP-BEFORE SECTION.
       TOTALER-SKIP-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-BEFORE-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-BEFORE SECTION.
       TOTALER-SPACE-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER TOTALER-BEFORE-SPACE
                                                                 LINES
           ADD TOTALER-BEFORE-SPACE        TO TOTALER-LINE-COUNT
           MOVE SPACES TO TOTALER-IO-AREA
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-BEFORE-SPACE.
 
       TOTALER-SKIP-AFTER SECTION.
       TOTALER-SKIP-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-AFTER-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-AFTER SECTION.
       TOTALER-SPACE-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE TOTALER-AFTER-SPACE LINES
           ADD TOTALER-AFTER-SPACE         TO TOTALER-LINE-COUNT
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-AFTER-SPACE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'AVSTEMMING PROG. ASS100 ' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE ' **** OPRATØR ASSISTENT ' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE 'SYSTEM ****   FREMKJØRT ' TO TOTALER-IO-AREA
                                                               (49:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (73:8)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 3                      TO TOTALER-BEFORE-SPACE
               MOVE 3                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               IF  (I-11)
                   MOVE 'O R D R E R U T I N E . ' TO TOTALER-IO-AREA
                                                               (25:24)
               END-IF
               MOVE 'OPPDATERT.'           TO TOTALER-IO-AREA (63:10)
               IF  (I-11)
                   MOVE ORDDTO             TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO TOTALER-IO-AREA (73:8)
               END-IF
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (49:24)
               MOVE '--------'             TO TOTALER-IO-AREA (73:8)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-LR AND I-11 AND I-97)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '  ANTALL ORDRE PÅ ORDRE-' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'FILE FRA FØR............' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE ORDFI2                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (49:7)
               MOVE '            PR.         ' TO TOTALER-IO-AREA
                                                               (57:24)
               MOVE ORDDT2                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (73:8)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '- ANTALL ORDRE SOM BLE U' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'TGÅRMELDT NÅ............' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE ORDUTG                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (49:7)
               MOVE '                        ' TO TOTALER-IO-AREA
                                                               (57:24)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '- ANTALL ORDRE SOM BLE F' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'JERNET NÅ...............' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE ORDIKK                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (49:7)
               MOVE '  RESTORDRE O.S.V.      ' TO TOTALER-IO-AREA
                                                               (57:24)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '- ANTALL ESSO-ORDRE FJER' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'NET. EGEN FAKT.RUTINE...' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE ORDFAE                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (49:7)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '- ANTALL ORDRE FJERNET E' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'TTER FAKT...............' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE ORDDT2                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (40:8)
               MOVE ORDFAK                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (49:7)
               IF  (I-52)
                   MOVE '**** DIFF =             ' TO TOTALER-IO-AREA
                                                               (57:24)
               END-IF
               IF  (I-52)
                   MOVE FAKDIF             TO XO-60YY9R
                   MOVE XO-60YY9R          TO TOTALER-IO-AREA (73:8)
               END-IF
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '= ANTALL ORDRE PÅ ORDREF' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'ILE NÅ..................' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE ORDFIL                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (49:7)
               IF  (NOT-I-51)
                   MOVE '     AVSTEMT OG I ORDEN.' TO TOTALER-IO-AREA
                                                               (57:24)
               END-IF
               IF  (I-51)
                   MOVE '***** DIFF =            ' TO TOTALER-IO-AREA
                                                               (57:24)
               END-IF
               IF  (I-51)
                   MOVE ORDDIF             TO XO-60YY9R
                   MOVE XO-60YY9R          TO TOTALER-IO-AREA (73:8)
               END-IF
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '= ANTALL ORDRE SOM IKKE ' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE 'ER FERDIGMELDT..........' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE ORDIFM                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (49:7)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'OPPRATØR ASSISTENTEN HAR' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE ' AVSTEMT ORDRERUTINEN.  ' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE 2                      TO TOTALER-BEFORE-SPACE
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               IF  (NOT-I-51 AND NOT-I-52)
                   MOVE '   A L T   S T E M M E R' TO TOTALER-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-51)
                   MOVE '******* F E I L ******* ' TO TOTALER-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-52)
                   MOVE '******* F E I L ******* ' TO TOTALER-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-51)
                   MOVE ' DIFF. ANT. ORDRE TOTAL.' TO TOTALER-IO-AREA
                                                               (25:24)
               END-IF
               IF  (I-52)
                   MOVE ' DIFF. ANT. ORDRE FAKT. ' TO TOTALER-IO-AREA
                                                               (49:24)
               END-IF
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
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
           INITIALIZE AVSTEMF-DATA-FIELDS
           OPEN INPUT AVSTEMF
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE AVSTEMF
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
