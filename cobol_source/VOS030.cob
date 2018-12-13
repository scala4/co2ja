       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS030R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: VOS030, GRUNNLAG VERKSTEDORDRE MEKANIKERSTAT.*
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VOS02A                                       *
      *  LAGET DATO....: 10.10.96                                     *
      *  ENDRET........: 14.11.96 UTVIDET TABELL TIL 100 ELEMENTER.   *
      *  RETTET........:                                              *
      *  INPUT.........: SEKVENSIELL VERKSTED ORDRE FILE (VERK030).   *
      *  BEHANDLING....: LESER ORDRENR-FILE OG HENTER KEY TIL VERKSTED*
      *                  FILE. HENTER MEKANIKER FRA VERKSTEDORDREN.   *
      *  OUTPUT........: DAGENS VERKSTEDORDRE.                        *
      *                  KONTROLL-LISTE NÅR UPSI 1 ER PÅ.             *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS030.rpg
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
           SELECT FIRTAB
               ASSIGN TO UT-S-FIRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRTAB-STATUS.
           SELECT ORDNRSD
               ASSIGN TO UT-S-ORDNRSD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDNRSD-STATUS.
           SELECT VERK030
               ASSIGN TO VERK030
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VERK030-STATUS
               RECORD KEY IS VERK030-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT VERKSTA
               ASSIGN TO UT-S-VERKSTA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKSTA-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FIRTAB-IO-AREA.
           05  FIRTAB-IO-AREA-X            PICTURE X(80).
       FD ORDNRSD
               BLOCK CONTAINS 1000
               RECORD CONTAINS 100.
       01  ORDNRSD-IO-AREA.
           05  ORDNRSD-IO-AREA-X           PICTURE X(100).
       FD VERK030
               RECORD CONTAINS 200.
       01  VERK030-IO-AREA.
           05  VERK030-IO-AREA-X.
               10  VERK030-KEY1            PICTURE X(15).
               10  FILLER                  PICTURE X(185).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VERKSTA
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VERKSTA-IO-AREA.
           05  VERKSTA-IO-AREA-X           PICTURE X(60).
      *FLISTEO O   F  80  80            PRINTERSYSLST
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABFIR-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFIR-TABLE.
               10  TABFIR-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY TABFIR-I
                                                      TABFIR-S.
                   15  TABFIR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FIRTAB-STATUS               PICTURE 99 VALUE 0.
           10  ORDNRSD-STATUS              PICTURE 99 VALUE 0.
           10  VERK030-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  VERKSTA-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRTAB-EOF-OFF          VALUE '0'.
               88  FIRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRSD-EOF-OFF         VALUE '0'.
               88  ORDNRSD-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRSD-READ-OFF        VALUE '0'.
               88  ORDNRSD-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRSD-PROCESS-OFF     VALUE '0'.
               88  ORDNRSD-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDNRSD-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDNRSD-LEVEL-INIT      VALUE '1'.
           05  VERK030-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  ORDNRSD-LEVEL-01.
               10  ORDNRSD-01-L1.
                   15  ORDNRSD-01-L1-ONMFNR PICTURE X(3).
           05  ORDNRSD-DATA-FIELDS.
               10  ONMFNR                  PICTURE X(3).
               10  ONMONR                  PICTURE X(6).
               10  ONMSTA                  PICTURE X(1).
               10  ONMDSO-IO.
                   15  ONMDSO              PICTURE S9(6).
               10  ONMKUN                  PICTURE X(6).
               10  ONMFUD-IO.
                   15  ONMFUD              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ONMVON                  PICTURE X(6).
               10  ONMTYP                  PICTURE X(1).
               10  ONMSKD                  PICTURE X(1).
           05  VERK030-DATA-FIELDS.
               10  VOSMEK                  PICTURE X(3).
               10  VOSSUM-IO.
                   15  VOSSUM              PICTURE S9(9)V9(2).
           05  FIRMAF-DATA-FIELDS.
               10  FIRNVN                  PICTURE X(30).
      *****************************************************************
      * HOUSEKEEPING.                                                 *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(6).
               10  VKEY9                   PICTURE X(9).
               10  VKEY6                   PICTURE X(6).
               10  VKEY15                  PICTURE X(15).
               10  FDATO-IO.
                   15  FDATO               PICTURE S9(6).
               10  ONMFUD-N-IO.
                   15  ONMFUD-N            PICTURE S9(7).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(6).
           05  EDITTING-FIELDS.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDNRSD-PROCESS
               SET ORDNRSD-PROCESS-OFF     TO TRUE
               SET ORDNRSD-READ            TO TRUE
           END-IF
 
           IF  ORDNRSD-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDNRSD-GET
               SET ORDNRSD-READ-OFF        TO TRUE
               IF  NOT ORDNRSD-EOF
                   SET ORDNRSD-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDNRSD-PROCESS
               PERFORM ORDNRSD-IDSET
           END-IF
 
           IF  ORDNRSD-PROCESS
               PERFORM ORDNRSD-CHK-LEVEL
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
 
           IF  ORDNRSD-PROCESS
               PERFORM ORDNRSD-FLDOFF
               PERFORM ORDNRSD-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDNRSD-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-99)
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET I-98                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      *****************************************************************
      * RECORDART ANNET ENN H4 LESES OVER. FIRMA IKKE I TAB OG UNUM.  *
      * ORDRENR LESES OVER.                                           *
      *****************************************************************
           END-IF
           SET NOT-I-50                    TO TRUE
           ADD 1                           TO ANTINN
           IF  (I-L1)
               SET NOT-I-10                TO TRUE
               SET TABFIR-S                TO TABFIR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFIR-I FROM 1 BY 1
                         UNTIL TABFIR-I >= TABFIR-MAX
                            OR I-10
                   IF  ONMFNR = TABFIR (TABFIR-I)
                       SET I-10            TO TRUE
                       SET TABFIR-S        TO TABFIR-I
                   END-IF
               END-PERFORM
               MOVE ONMFNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-21)
               SET NOT-I-10                TO TRUE
           END-IF
           IF  (NOT-I-10)
               GO TO SLUTT-T
           END-IF
           MOVE ONMFNR                     TO VKEY9 (1:3)
           MOVE ONMVON                     TO VKEY9 (4:6)
           MOVE 'H4'                       TO VKEY6 (1:2)
           MOVE '0000'                     TO VKEY6 (3:4)
           MOVE VKEY9                      TO VKEY15 (1:9)
           MOVE VKEY6                      TO VKEY15 (10:6)
           MOVE VKEY15                     TO VERK030-KEY1
           READ VERK030 RECORD KEY IS VERK030-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM VERK030-FLDSET
               PERFORM VERK030-IDSET
           END-READ
           MOVE ONMFUD                     TO ONMFUD-N
           MOVE ONMFUD-N-IO (2:6)          TO FDATO-IO
           IF  (I-20)
               MOVE ONMDSO                 TO FDATO-IO
           END-IF
           ADD 1                           TO ANTUT
           SET I-50                        TO TRUE.
 
       SLUTT-T.
      *0 01                MOVE "VOKEY   "BUGFL1  8        DISPLAY FIELD
      *0 01      BUGFL1    DEBUGFLISTEO   VOKEY            VIS INDIKATOR
      *****************************************************************
      * DAGENS VERKSTEDORDRE                                          *
      *****************************************************************
           CONTINUE.
 
       ORDNRSD-GET SECTION.
       ORDNRSD-GET-P.
           IF  ORDNRSD-EOF-OFF
               READ ORDNRSD
               AT END
                   SET ORDNRSD-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDNRSD-FLDOFF SECTION.
       ORDNRSD-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-20                TO TRUE
           END-EVALUATE.
 
       ORDNRSD-FLDSET SECTION.
       ORDNRSD-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRSD-IO-AREA (2:3)  TO ONMFNR (1:3)
               MOVE ORDNRSD-IO-AREA (5:6)  TO ONMONR (1:6)
               MOVE ORDNRSD-IO-AREA (11:1) TO ONMSTA (1:1)
               MOVE ORDNRSD-IO-AREA (12:6) TO ONMDSO-IO
               INSPECT ONMDSO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDNRSD-IO-AREA (25:6) TO ONMKUN (1:6)
               MOVE ORDNRSD-IO-AREA (47:4) TO ONMFUD-IO
               IF  ONMFUD = ZERO
                   SET I-20                TO TRUE
               END-IF
               MOVE ORDNRSD-IO-AREA (70:6) TO ONMVON (1:6)
               MOVE ORDNRSD-IO-AREA (76:1) TO ONMTYP (1:1)
               MOVE ORDNRSD-IO-AREA (77:1) TO ONMSKD (1:1)
           END-EVALUATE.
 
       ORDNRSD-IDSET SECTION.
       ORDNRSD-IDSET-P.
           SET I-01                        TO TRUE.
 
       ORDNRSD-CHK-LEVEL SECTION.
       ORDNRSD-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDNRSD-LEVEL-01
               MOVE ORDNRSD-IO-AREA (2:3)  TO ORDNRSD-01-L1-ONMFNR
               IF  ORDNRSD-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDNRSD-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDNRSD-01-L1         TO THE-PRIOR-L1
               SET ORDNRSD-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VERK030-FLDSET SECTION.
       VERK030-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VERK030-IO-AREA (58:3) TO VOSMEK (1:3)
               MOVE VERK030-IO-AREA (137:11) TO VOSSUM-IO
               INSPECT VOSSUM-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       VERK030-IDSET SECTION.
       VERK030-IDSET-P.
           SET I-02                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FIRNVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
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
 
       FIRTAB-LOAD SECTION.
       FIRTAB-LOAD-P.
           OPEN INPUT FIRTAB
           SET TABFIR-I                    TO 1
           PERFORM UNTIL FIRTAB-EOF
               READ FIRTAB
               AT END
                   SET FIRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FIRTAB-IO-AREA (1:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FIRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO VERKSTA-IO-AREA
               INITIALIZE VERKSTA-IO-AREA
               MOVE VKEY15                 TO VERKSTA-IO-AREA (1:15)
               MOVE ONMONR                 TO VERKSTA-IO-AREA (16:6)
               IF  (I-11)
                   MOVE '   '              TO VERKSTA-IO-AREA (22:3)
               END-IF
               IF  (I-02)
                   MOVE VOSMEK             TO VERKSTA-IO-AREA (22:3)
               END-IF
               MOVE FDATO-IO               TO VERKSTA-IO-AREA (25:6)
               MOVE ONMSTA                 TO VERKSTA-IO-AREA (31:1)
               IF  (I-02)
                   MOVE VOSSUM-IO          TO VERKSTA-IO-AREA (32:11)
               END-IF
               WRITE VERKSTA-IO-AREA
           END-IF
           IF  (I-01 AND I-50)
           AND (I-U1)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE ONMONR                 TO KLISTEO-IO-AREA (11:6)
               MOVE ONMTYP                 TO KLISTEO-IO-AREA (22:1)
               MOVE ONMSTA                 TO KLISTEO-IO-AREA (32:1)
               MOVE FDATO-IO               TO KLISTEO-IO-AREA (41:6)
               MOVE VOSMEK                 TO KLISTEO-IO-AREA (63:3)
               MOVE ONMKUN                 TO KLISTEO-IO-AREA (67:6)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-U1 AND I-L1)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTEDORDRE. GRUNNLAG ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'MEKANIKER-STATISTIKK.   ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (59:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (69:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (80:8)
               MOVE 'JOB=VOS02A'           TO KLISTEO-IO-AREA (90:10)
               MOVE 'PROGRAM=VOS030'       TO KLISTEO-IO-AREA (102:14)
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE ONMFNR                 TO KLISTEO-IO-AREA (11:3)
               MOVE FIRNVN                 TO KLISTEO-IO-AREA (16:30)
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'DAGENS VERKSTED-ORDRE' TO KLISTEO-IO-AREA (57:21)
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ORD.NR'               TO KLISTEO-IO-AREA (11:6)
               MOVE 'TYPE    '             TO KLISTEO-IO-AREA (19:8)
               MOVE 'STATUS  '             TO KLISTEO-IO-AREA (29:8)
               MOVE 'FM DATO'              TO KLISTEO-IO-AREA (40:7)
               MOVE 'MEK'                  TO KLISTEO-IO-AREA (63:3)
               MOVE 'KUNDE'                TO KLISTEO-IO-AREA (68:5)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-U1 AND I-OF)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTEDORDRE. GRUNNLAG ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'MEKANIKER-STATISTIKK.   ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (59:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (69:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (80:8)
               MOVE 'JOB=VOS02A'           TO KLISTEO-IO-AREA (90:10)
               MOVE 'PROGRAM=VOS030'       TO KLISTEO-IO-AREA (102:14)
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE ONMFNR                 TO KLISTEO-IO-AREA (11:3)
               MOVE FIRNVN                 TO KLISTEO-IO-AREA (16:30)
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'DAGENS VERKSTED-ORDRE' TO KLISTEO-IO-AREA (57:21)
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ORD.NR'               TO KLISTEO-IO-AREA (11:6)
               MOVE 'TYPE    '             TO KLISTEO-IO-AREA (19:8)
               MOVE 'STATUS  '             TO KLISTEO-IO-AREA (29:8)
               MOVE 'FM DATO'              TO KLISTEO-IO-AREA (40:7)
               MOVE 'MEK'                  TO KLISTEO-IO-AREA (63:3)
               MOVE 'KUNDE'                TO KLISTEO-IO-AREA (68:5)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-U1 AND I-L1)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL LEST   :'      TO KLISTEO-IO-AREA (11:15)
               MOVE ANTINN                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTINN
               MOVE 1                      TO KLISTEO-BEFORE-SPACE
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL SKREVET:'      TO KLISTEO-IO-AREA (11:15)
               MOVE ANTUT                  TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTUT
               MOVE 1                      TO KLISTEO-BEFORE-SPACE
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
           PERFORM FIRTAB-LOAD
           SET ORDNRSD-LEVEL-INIT          TO TRUE
           INITIALIZE ORDNRSD-DATA-FIELDS
           SET ORDNRSD-EOF-OFF             TO TRUE
           SET ORDNRSD-PROCESS             TO TRUE
           OPEN INPUT ORDNRSD
           INITIALIZE VERK030-DATA-FIELDS
           OPEN INPUT VERK030
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT VERKSTA
           OPEN OUTPUT KLISTEO
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           SET TABFIR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDNRSD
           CLOSE VERK030
           CLOSE FIRMAF
           CLOSE VERKSTA
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
