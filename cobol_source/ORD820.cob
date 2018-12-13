       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD820R.
      **********************************************  Z-WIN-RPG2   ****
      *       O R D R E R U T I N E P R O G R A M   O R D 8 2 0       *
      *       -------------------------------------------------       *
      *  1. MERGE ORDRESUMRECORDS OG DANNE NY ORDRESUMFILE.           *
      *      31/1-1991 AV ESPEN LARSEN.                               *
      *  12.03.93  RETTET OPP TILDELING AV FERDIGM.DATO               *
      *            DENNE DATO BLE RETTET TILBAKE TIL ORDREDATO        *
      *            DAGEN ETTER FERDIGMELDING.                         *
      *            NÅ BLIR FM-DATO OVERFØRT FRA GAMMEL RECORD.        *
      *  18.02-97  LAGEROVERFØRING BLIR TATT MED. HAR RUTID=L.        *
      *            SKAL KUN VÆRE MED PÅ PLUKKER STAT.                 *
      *  09.10.97  HENTER DATO FRA AUTOPAR.                           *
      *  13.05.08  RETTET COMP FEIL PÅ APDAY TIL APDAG I OUTPUT - EN  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD820.rpg
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
           SELECT MASTINN
               ASSIGN TO UT-S-MASTINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTINN-STATUS.
           SELECT ORDSUMR
               ASSIGN TO UT-S-ORDSUMR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSUMR-STATUS.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT MASTUT
               ASSIGN TO UT-S-MASTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MASTINN
               BLOCK CONTAINS 4100
               RECORD CONTAINS 100.
       01  MASTINN-IO-AREA.
           05  MASTINN-IO-AREA-X           PICTURE X(100).
       FD ORDSUMR
               BLOCK CONTAINS 4100
               RECORD CONTAINS 100.
       01  ORDSUMR-IO-AREA.
           05  ORDSUMR-IO-AREA-X           PICTURE X(100).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD MASTUT
               BLOCK CONTAINS 4100
               RECORD CONTAINS 100.
       01  MASTUT-IO-AREA.
           05  MASTUT-IO-AREA-X            PICTURE X(100).
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
           10  MASTINN-STATUS              PICTURE 99 VALUE 0.
           10  ORDSUMR-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  MASTUT-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-EOF-OFF         VALUE '0'.
               88  MASTINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-READ-OFF        VALUE '0'.
               88  MASTINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-PROCESS-OFF     VALUE '0'.
               88  MASTINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MASTINN-LEVEL-INIT-OFF  VALUE '0'.
               88  MASTINN-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMR-EOF-OFF         VALUE '0'.
               88  ORDSUMR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMR-READ-OFF        VALUE '0'.
               88  ORDSUMR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMR-PROCESS-OFF     VALUE '0'.
               88  ORDSUMR-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDSUMR-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDSUMR-LEVEL-INIT      VALUE '1'.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  MASTINN-LEVEL-01.
               10  MASTINN-01-L2.
                   15  MASTINN-01-L2-FIRMA PICTURE X(3).
               10  MASTINN-01-L1.
                   15  MASTINN-01-L1-ORDNR PICTURE X(6).
           05  MASTINN-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  ORDATO                  PICTURE X(6).
               10  FMDATO                  PICTURE X(6).
               10  MASREC                  PICTURE X(100).
           05  MASTINN-MP                  PICTURE X(9).
           05  MASTINN-MC                  PICTURE X(9).
           05  MASTINN-M-01            REDEFINES MASTINN-MC.
               10  MASTINN-M-01-M2.
                   15  MASTINN-M-01-M2-FIRMA-G.
                       20  MASTINN-M-01-M2-FIRMA PICTURE X(3).
               10  MASTINN-M-01-M1.
                   15  MASTINN-M-01-M1-ORDNR-G.
                       20  MASTINN-M-01-M1-ORDNR PICTURE X(6).
           05  ORDSUMR-LEVEL-02.
               10  ORDSUMR-02-L2.
                   15  ORDSUMR-02-L2-FIRMA PICTURE X(3).
               10  ORDSUMR-02-L1.
                   15  ORDSUMR-02-L1-ORDNR PICTURE X(6).
           05  ORDSUMR-DATA-FIELDS.
               10  STATUS-X                PICTURE X(1).
               10  RUTID                   PICTURE X(1).
               10  BK                      PICTURE X(1).
               10  FMIDAG                  PICTURE X(1).
               10  ORDAT2                  PICTURE X(6).
               10  FMDAT2                  PICTURE X(6).
               10  NYEREC                  PICTURE X(100).
           05  ORDSUMR-MP                  PICTURE X(9).
           05  ORDSUMR-MC                  PICTURE X(9).
           05  ORDSUMR-M-02            REDEFINES ORDSUMR-MC.
               10  ORDSUMR-M-02-M2.
                   15  ORDSUMR-M-02-M2-FIRMA-G.
                       20  ORDSUMR-M-02-M2-FIRMA PICTURE X(3).
               10  ORDSUMR-M-02-M1.
                   15  ORDSUMR-M-02-M1-ORDNR-G.
                       20  ORDSUMR-M-02-M1-ORDNR PICTURE X(6).
           05  AUTOPAR-DATA-FIELDS.
               10  APDATO-IO.
                   15  APDATO              PICTURE S9(6).
               10  APDAG-IO.
                   15  APDAG               PICTURE S9(2).
               10  APMND-IO.
                   15  APMND               PICTURE S9(2).
               10  APAAR-IO.
                   15  APAAR               PICTURE S9(2).
      *****************************************************************
      * OPPSLAG MOT PARAMFIL.AUTODATA FOR HENTING AV AKTUELL DATO.    *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  APKEY                   PICTURE X(3).
               10  ANTOLD-IO.
                   15  ANTOLD              PICTURE S9(7).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(7).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(7).
               10  ANTUTM-IO.
                   15  ANTUTM              PICTURE S9(7).
               10  ANTNY-IO.
                   15  ANTNY               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  MASTINN-PROCESS
               SET MASTINN-PROCESS-OFF     TO TRUE
               SET MASTINN-READ            TO TRUE
           END-IF
 
           IF  MASTINN-READ
               PERFORM MASTINN-GET
               SET MASTINN-READ-OFF        TO TRUE
               IF  NOT MASTINN-EOF
                   PERFORM MASTINN-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDSUMR-PROCESS
               SET ORDSUMR-PROCESS-OFF     TO TRUE
               SET ORDSUMR-READ            TO TRUE
           END-IF
 
           IF  ORDSUMR-READ
               PERFORM ORDSUMR-GET
               SET ORDSUMR-READ-OFF        TO TRUE
               IF  NOT ORDSUMR-EOF
                   PERFORM ORDSUMR-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-IDSET
           END-IF
 
           IF  ORDSUMR-PROCESS
               PERFORM ORDSUMR-IDSET
           END-IF
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-CHK-LEVEL
           END-IF
 
           IF  ORDSUMR-PROCESS
               PERFORM ORDSUMR-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-FLDOFF
               PERFORM MASTINN-FLDSET
           END-IF
 
           IF  ORDSUMR-PROCESS
               PERFORM ORDSUMR-FLDOFF
               PERFORM ORDSUMR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  MASTINN-PROCESS
           OR  ORDSUMR-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-09)
               MOVE 'B01'                  TO APKEY
               MOVE APKEY                  TO AUTOPAR-KEY1
               READ AUTOPAR RECORD KEY IS AUTOPAR-KEY1
               INVALID KEY
                   SET I-99                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-99            TO TRUE
                   PERFORM AUTOPAR-FLDSET
                   PERFORM AUTOPAR-IDSET
               END-READ
               SET I-09                    TO TRUE
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTOLD
           END-IF
           IF  (I-02)
               SET NOT-I-25                TO TRUE
               IF  STATUS-X = 'U'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  STATUS-X = 'V'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  BK = 'R'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  BK = 'T'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  BK = 'F'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  BK = 'O'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  BK = 'V'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  BK = 'W'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  BK = 'B'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               SET NOT-I-30                TO TRUE
               IF  FMIDAG = '*'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-MR AND NOT-I-25)
               ADD 1                       TO ANTNYE
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTKOR
           END-IF
           IF  (I-02 AND I-MR AND I-25)
               ADD 1                       TO ANTUTM
           END-IF
           IF  (I-01 AND NOT-I-MR)
               OR  (I-02 AND NOT-I-25)
               ADD 1                       TO ANTNY
           END-IF.
 
       MASTINN-GET SECTION.
       MASTINN-GET-P.
           IF  MASTINN-EOF-OFF
               READ MASTINN
               AT END
                   SET MASTINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MASTINN-FLDOFF SECTION.
       MASTINN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-11                TO TRUE
           END-EVALUATE.
 
       MASTINN-FLDSET SECTION.
       MASTINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MASTINN-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE MASTINN-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE MASTINN-IO-AREA (11:6) TO ORDATO (1:6)
               MOVE MASTINN-IO-AREA (95:6) TO FMDATO (1:6)
               IF  FMDATO = SPACES
                   SET I-11                TO TRUE
               END-IF
               MOVE MASTINN-IO-AREA (1:100) TO MASREC (1:100)
           END-EVALUATE.
 
       MASTINN-IDSET SECTION.
       MASTINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       MASTINN-CHK-LEVEL SECTION.
       MASTINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO MASTINN-LEVEL-01
               MOVE MASTINN-IO-AREA (2:3)  TO MASTINN-01-L2-FIRMA
               MOVE MASTINN-IO-AREA (5:6)  TO MASTINN-01-L1-ORDNR
               IF  MASTINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MASTINN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  MASTINN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MASTINN-01-L2         TO THE-PRIOR-L2
               MOVE  MASTINN-01-L1         TO THE-PRIOR-L1
               SET MASTINN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       MASTINN-MATCH-SET SECTION.
       MASTINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE MASTINN-IO-AREA (2:3)  TO MASTINN-M-01-M2-FIRMA
               MOVE MASTINN-IO-AREA (5:6)  TO MASTINN-M-01-M1-ORDNR
           END-EVALUATE.
 
       ORDSUMR-GET SECTION.
       ORDSUMR-GET-P.
           IF  ORDSUMR-EOF-OFF
               READ ORDSUMR
               AT END
                   SET ORDSUMR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDSUMR-FLDOFF SECTION.
       ORDSUMR-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-12                TO TRUE
           END-EVALUATE.
 
       ORDSUMR-FLDSET SECTION.
       ORDSUMR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDSUMR-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDSUMR-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDSUMR-IO-AREA (25:1) TO STATUS-X (1:1)
               MOVE ORDSUMR-IO-AREA (26:1) TO RUTID (1:1)
               MOVE ORDSUMR-IO-AREA (31:1) TO BK (1:1)
               MOVE ORDSUMR-IO-AREA (47:1) TO FMIDAG (1:1)
               MOVE ORDSUMR-IO-AREA (11:6) TO ORDAT2 (1:6)
               MOVE ORDSUMR-IO-AREA (95:6) TO FMDAT2 (1:6)
               IF  FMDAT2 = SPACES
                   SET I-12                TO TRUE
               END-IF
               MOVE ORDSUMR-IO-AREA (1:100) TO NYEREC (1:100)
           END-EVALUATE.
 
       ORDSUMR-IDSET SECTION.
       ORDSUMR-IDSET-P.
           SET I-02                        TO TRUE.
 
       ORDSUMR-CHK-LEVEL SECTION.
       ORDSUMR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDSUMR-LEVEL-02
               MOVE ORDSUMR-IO-AREA (2:3)  TO ORDSUMR-02-L2-FIRMA
               MOVE ORDSUMR-IO-AREA (5:6)  TO ORDSUMR-02-L1-ORDNR
               IF  ORDSUMR-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSUMR-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDSUMR-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSUMR-02-L2         TO THE-PRIOR-L2
               MOVE  ORDSUMR-02-L1         TO THE-PRIOR-L1
               SET ORDSUMR-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       ORDSUMR-MATCH-SET SECTION.
       ORDSUMR-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDSUMR-IO-AREA (2:3)  TO ORDSUMR-M-02-M2-FIRMA
               MOVE ORDSUMR-IO-AREA (5:6)  TO ORDSUMR-M-02-M1-ORDNR
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (4:6)  TO APDATO-IO
               INSPECT APDATO-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (4:2)  TO APDAG-IO
               INSPECT APDAG-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (6:2)  TO APMND-IO
               INSPECT APMND-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (8:2)  TO APAAR-IO
               INSPECT APAAR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  MASTINN-EOF
               MOVE HIGH-VALUES            TO MASTINN-MC
                                              MASTINN-MP
           END-IF
           IF  ORDSUMR-EOF
               MOVE HIGH-VALUES            TO ORDSUMR-MC
                                              ORDSUMR-MP
           END-IF
           IF  MASTINN-MC < MASTINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDSUMR-MC < ORDSUMR-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  MASTINN-MC < ORDSUMR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MASTINN-PROCESS     TO TRUE
                   MOVE MASTINN-MC         TO MASTINN-MP
                   IF  MASTINN-MC = ORDSUMR-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDSUMR-MC < MASTINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDSUMR-PROCESS     TO TRUE
                   MOVE ORDSUMR-MC         TO ORDSUMR-MP
                   IF  ORDSUMR-MC = MASTINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  MASTINN-MC = ORDSUMR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MASTINN-PROCESS     TO TRUE
                   MOVE MASTINN-MC         TO MASTINN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-MR)
               MOVE SPACES TO MASTUT-IO-AREA
               INITIALIZE MASTUT-IO-AREA
               MOVE MASREC                 TO MASTUT-IO-AREA (1:100)
               MOVE ' '                    TO MASTUT-IO-AREA (41:1)
               MOVE ' '                    TO MASTUT-IO-AREA (47:1)
               IF  (I-11)
                   MOVE ORDATO             TO MASTUT-IO-AREA (95:6)
               END-IF
               WRITE MASTUT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-25 AND NOT-I-MR)
               MOVE SPACES TO MASTUT-IO-AREA
               INITIALIZE MASTUT-IO-AREA
               MOVE NYEREC                 TO MASTUT-IO-AREA (1:100)
               IF  (I-12)
                   MOVE ORDAT2             TO MASTUT-IO-AREA (95:6)
               END-IF
               IF  (I-30)
                   MOVE APDAG-IO           TO MASTUT-IO-AREA (95:2)
               END-IF
               IF  (I-30)
                   MOVE APMND-IO           TO MASTUT-IO-AREA (97:2)
               END-IF
               IF  (I-30)
                   MOVE APAAR-IO           TO MASTUT-IO-AREA (99:2)
               END-IF
               WRITE MASTUT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-25 AND I-MR)
               MOVE SPACES TO MASTUT-IO-AREA
               INITIALIZE MASTUT-IO-AREA
               MOVE NYEREC                 TO MASTUT-IO-AREA (1:100)
               IF  (NOT-I-30)
                   MOVE FMDATO             TO MASTUT-IO-AREA (95:6)
               END-IF
               IF  (I-30)
                   MOVE APDAG-IO           TO MASTUT-IO-AREA (95:2)
               END-IF
               IF  (I-30)
                   MOVE APMND-IO           TO MASTUT-IO-AREA (97:2)
               END-IF
               IF  (I-30)
                   MOVE APAAR-IO           TO MASTUT-IO-AREA (99:2)
               END-IF
               WRITE MASTUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SJEFSLISTE PROG. ORD820 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALER FOR'          TO LISTE-IO-AREA (11:11)
               MOVE 'OPPDATERING '         TO LISTE-IO-AREA (23:12)
               MOVE 'ORDRE.SUM.FILE'       TO LISTE-IO-AREA (36:14)
               MOVE APDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (51:8)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTOLD                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (12:9)
               MOVE 'RECORDS PÅ GML. MASTER.' TO LISTE-IO-AREA (23:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTUTM                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (12:9)
               MOVE 'RECORDS UTGÅR MELDT.   ' TO LISTE-IO-AREA (23:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTKOR                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (12:9)
               MOVE 'RECORDS BYTTET UT.     ' TO LISTE-IO-AREA (23:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTNYE                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (12:9)
               MOVE 'RECORDS VAR NYE IDAG.  ' TO LISTE-IO-AREA (23:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTNY                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (12:9)
               MOVE 'RECORDS PÅ NY MASTER.  ' TO LISTE-IO-AREA (23:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           SET MASTINN-LEVEL-INIT          TO TRUE
           INITIALIZE MASTINN-DATA-FIELDS
           SET MASTINN-EOF-OFF             TO TRUE
           SET MASTINN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO MASTINN-MC
                                              MASTINN-MP
           OPEN INPUT MASTINN
           SET ORDSUMR-LEVEL-INIT          TO TRUE
           INITIALIZE ORDSUMR-DATA-FIELDS
           SET ORDSUMR-EOF-OFF             TO TRUE
           SET ORDSUMR-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO ORDSUMR-MC
                                              ORDSUMR-MP
           OPEN INPUT ORDSUMR
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           OPEN OUTPUT MASTUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE MASTINN
           CLOSE ORDSUMR
           CLOSE AUTOPAR
           CLOSE MASTUT
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
