       IDENTIFICATION DIVISION.
       PROGRAM-ID. ONR403R.
      **********************************************  Z-WIN-RPG2   ****
      * DELETE RECORDS PÅ ORDRENR.FILE:                    *
      * 1 MERKET SLETTES, FEIL FIRMANR, FEIL RECORDART.    *
      * FIRMA SKAL SLETTES, FERDIG OG ELDERE ENN 1 ÅR.     *
      * 27/8-98 4 SIFFER ÅRSTALL VIA IBM SUBRUT ILNY224    *
      *  7/9-99 OVERFØRER FIRMANR. I ALT.KEY. UPSI 1       *
      *  1/1-00 FAKT.ORDNR. SLETTES ETTER 12 MND.          *
      * 12/5-00 GRØNLAND AUTO 676 SKAL IKKE SLETTE PÅBEG.  *
      *         ORDRE FØR ETTER 5 ÅR.                      *
      * 06/6/02 SLETTE TILBUDORDRE ETTER 12. MND.          *
      * 12/9/18 FJERNET CALL ILNY224 OG SATT "20" I ÅRHUNDR*
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ONR403.rpg
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
           SELECT ORDNRM
               ASSIGN TO ORDNRM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDNRM-STATUS
               RECORD KEY IS ORDNRM-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KOPIUT
               ASSIGN TO UT-S-KOPIUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPIUT-STATUS.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDNRM
               RECORD CONTAINS 100.
       01  ORDNRM-IO-AREA.
           05  ORDNRM-IO-AREA-X.
               10  ORDNRM-KEY1.
                   15  ORDNRM-KEY1N        PICTURE S9(9).
               10  FILLER                  PICTURE X(91).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KOPIUT
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  KOPIUT-IO-AREA.
           05  KOPIUT-IO-AREA-X            PICTURE X(100).
       FD TOTALER
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDNRM-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KOPIUT-STATUS               PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
           10  LKFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ORDNRM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRM-EOF-OFF          VALUE '0'.
               88  ORDNRM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRM-READ-OFF         VALUE '0'.
               88  ORDNRM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRM-PROCESS-OFF      VALUE '0'.
               88  ORDNRM-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDNRM-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDNRM-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
      *PSDS: DATA STRUCTURE FIELDS
           05  PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(28).
               10  R                       PICTURE X(8).
               10  FILLER                  PICTURE X(44).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  P-IO.
                   15  P                   PICTURE S9(3).
               10  FILLER                  PICTURE X(41).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(10).
               10  S-IO.
                   15  S                   PICTURE S9(5).
               10  FILLER                  PICTURE X(65).
      *DSDS: DATA STRUCTURE FIELDS
           05  LKFELT-XX-DATA-FIELDS.
               10  AFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(2).
           05  FILLER REDEFINES LKFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(3).
               10  PFIRMA                  PICTURE X(2).
           05  ORDNRM-LEVEL-01.
               10  ORDNRM-01-L1.
                   15  ORDNRM-01-L1-FIRMA  PICTURE X(3).
           05  ORDNRM-DATA-FIELDS.
               10  ORDREC                  PICTURE X(100).
               10  RECART                  PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  FIRNUM-IO.
                   15  FIRNUM              PICTURE S9(3).
               10  TYPE-X                  PICTURE X(1).
               10  TYPMND                  PICTURE X(2).
               10  TYPAAR                  PICTURE X(2).
               10  SLETT                   PICTURE X(1).
               10  OTYPE                   PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTGML-IO.
                   15  ANTGML              PICTURE S9(7).
               10  ANTR-IO.
                   15  ANTR                PICTURE S9(7).
               10  ANTS-IO.
                   15  ANTS                PICTURE S9(7).
               10  YEAR2                   PICTURE X(2).
               10  LIMIT1                  PICTURE X(6).
               10  LIMIT-X-IO.
                   15  LIMIT-X             PICTURE S9(6).
               10  LIMITA                  PICTURE X(6).
               10  LIMIT5-IO.
                   15  LIMIT5              PICTURE S9(6).
               10  LIMITB                  PICTURE X(6).
               10  TYPMAR                  PICTURE X(6).
               10  YEAR4                   PICTURE X(4).
           05  EDITTING-FIELDS.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
               10  EDIT-LIMIT-X            PICTURE ZZZZ.ZZ.
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
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDNRM-PROCESS
               SET ORDNRM-PROCESS-OFF      TO TRUE
               SET ORDNRM-READ             TO TRUE
           END-IF
 
           IF  ORDNRM-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDNRM-GET
               SET ORDNRM-READ-OFF         TO TRUE
               IF  NOT ORDNRM-EOF
                   SET ORDNRM-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDNRM-PROCESS
               PERFORM ORDNRM-IDSET
           END-IF
 
           IF  ORDNRM-PROCESS
               PERFORM ORDNRM-CHK-LEVEL
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
 
           IF  ORDNRM-PROCESS
               PERFORM ORDNRM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDNRM-PROCESS
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
           SET NOT-I-10                    TO TRUE
           IF  RECART NOT = '1'
               SET I-10                    TO TRUE
           END-IF
           IF  (I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1 AND I-U1)
               PERFORM FIRMUZ-S
           END-IF
           IF  (I-L1)
               PERFORM FISLET-S
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  SLETT = 'S'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-01 AND I-99)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-01 AND I-98)
               SET I-10                    TO TRUE
           END-IF
           SET NOT-I-97                    TO TRUE
           IF  FIRMA < '001'
               SET I-97                    TO TRUE
           END-IF
           IF  (I-01 AND I-97)
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-50)
               PERFORM LIMRUT-S
           END-IF
           SET I-50                        TO TRUE
           PERFORM GMLRST-S
           IF  (I-75)
               SET I-10                    TO TRUE
               ADD 1                       TO ANTGML
           END-IF.
 
       SLUTT-T.
           IF  (NOT-I-10)
               ADD 1                       TO ANTR
           END-IF
           IF  (I-10)
               ADD 1                       TO ANTS
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
           END-IF
           .
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBPROGRAM FOR DANNE PAKKED FIRMANR U/ZONE         *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
       FIRMUZ-S SECTION.
       FIRMUZ-S-P.
           MOVE FIRMA                      TO AFIRMA
           CALL 'ONR407' USING LKFELT-XX-DATA-FIELDS.
      ******************************************************
      *    SUBRUTINE FOR Å TREKKE 12 MND FRA ÅR/MND        *
      ******************************************************
 
       LIMRUT-S SECTION.
       LIMRUT-S-P.
           MOVE UYEAR                      TO YEAR2
           PERFORM AARRUT-S
           MOVE YEAR4                      TO LIMIT1 (1:4)
           MOVE UMONTH                     TO LIMIT1 (5:2)
           MOVE LIMIT1                     TO LIMIT-X-IO
           SUBTRACT 100                    FROM LIMIT-X
           MOVE LIMIT-X                    TO LIMITA
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE LIMITA (6:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO LIMITA (6:1)
      *    SUBRUTINE FOR Å TREKKE 24 MND FRA ÅR/MND FOR FAKTURERT
      *    BRUKES IKKE. FAKTURERTE ORDRE SLETTES ETTER  12 MND.
      *          LIMIT     SUB  100       LIMIT5  60        - 24 MND.
      *                    MOVE LIMIT5    LIMITF  6         ALFA.
      *                    MLLZO"1"       LIMITF            ALFA.
      *    SUBRUTINE FOR Å TREKKE 60 MND FRA ÅR/MND FOR INNMELDING.
           SUBTRACT 400 FROM LIMIT-X   GIVING LIMIT5
           MOVE LIMIT5                     TO LIMITB
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE LIMITB (6:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO LIMITB (6:1).
      ***********************************************************
      *    SUBRUTINE FOR SLETTING AV DIVERSE GAMLE ORDRE-NR.       *
      **************************************************************
 
       GMLRST-S SECTION.
       GMLRST-S-P.
           SET NOT-I-75                    TO TRUE
           MOVE TYPAAR                     TO YEAR2
           PERFORM AARRUT-S
           MOVE YEAR4                      TO TYPMAR (1:4)
           MOVE TYPMND                     TO TYPMAR (5:2)
           SET NOT-I-76                    TO TRUE
           IF  TYPMND NOT > '00'
               SET I-76                    TO TRUE
           END-IF
           SET NOT-I-69                    TO TRUE
           IF  OTYPE = 'T'
               SET I-69                    TO TRUE
           END-IF
           IF  (NOT-I-69)
               SET NOT-I-69                TO TRUE
               IF  OTYPE = 'U'
                   SET I-69                TO TRUE
               END-IF
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  TYPE-X = 'I'
               SET I-70                    TO TRUE
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  TYPE-X = 'K'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  TYPE-X = 'P'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  TYPE-X = 'F'
               SET I-71                    TO TRUE
           END-IF
           IF  (NOT-I-70 AND NOT-I-71 AND NOT-I-76)
               SET NOT-I-75                TO TRUE
               IF  TYPMAR < LIMITA
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-71 AND NOT-I-76)
               SET NOT-I-75                TO TRUE
               IF  TYPMAR < LIMITA
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-70 AND NOT-I-76)
               SET NOT-I-75                TO TRUE
               IF  TYPMAR < LIMITB
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-69 AND NOT-I-76)
               SET NOT-I-75                TO TRUE
               IF  TYPMAR < LIMITA
                   SET I-75                TO TRUE
               END-IF
           END-IF
           SET NOT-I-77                    TO TRUE
           IF  FIRMA = '676'
               SET I-77                    TO TRUE
           END-IF
           SET NOT-I-72                    TO TRUE
           IF  TYPE-X = 'J'
               SET I-72                    TO TRUE
           END-IF
           IF  (I-77 AND I-72 AND NOT-I-76)
               SET NOT-I-75                TO TRUE
               IF  TYPMAR < LIMITB
                   SET I-75                TO TRUE
               END-IF
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR 4 SIFFER ÅRSTALL IBM SUBRUT.      *
      ******************************************************
 
       AARRUT-S SECTION.
       AARRUT-S-P.
           MOVE '20'                       TO YEAR4 (1:2)
           MOVE YEAR2                      TO YEAR4 (3:2).
      ******************************************************
 
       ORDNRM-GET SECTION.
       ORDNRM-GET-P.
           IF  ORDNRM-EOF-OFF
               READ ORDNRM
               AT END
                   SET ORDNRM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDNRM-FLDSET SECTION.
       ORDNRM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRM-IO-AREA (1:100) TO ORDREC (1:100)
               MOVE ORDNRM-IO-AREA (1:1)   TO RECART (1:1)
               MOVE ORDNRM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDNRM-IO-AREA (2:3)   TO FIRNUM-IO
               INSPECT FIRNUM-IO REPLACING ALL ' ' BY '0'
               MOVE ORDNRM-IO-AREA (11:1)  TO TYPE-X (1:1)
               MOVE ORDNRM-IO-AREA (14:2)  TO TYPMND (1:2)
               MOVE ORDNRM-IO-AREA (16:2)  TO TYPAAR (1:2)
               MOVE ORDNRM-IO-AREA (24:1)  TO SLETT (1:1)
               MOVE ORDNRM-IO-AREA (76:1)  TO OTYPE (1:1)
           END-EVALUATE.
 
       ORDNRM-IDSET SECTION.
       ORDNRM-IDSET-P.
           SET I-01                        TO TRUE.
 
       ORDNRM-CHK-LEVEL SECTION.
       ORDNRM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDNRM-LEVEL-01
               MOVE ORDNRM-IO-AREA (2:3)   TO ORDNRM-01-L1-FIRMA
               IF  ORDNRM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDNRM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDNRM-01-L1          TO THE-PRIOR-L1
               SET ORDNRM-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO KOPIUT-IO-AREA
               INITIALIZE KOPIUT-IO-AREA
               MOVE ORDREC                 TO KOPIUT-IO-AREA (1:100)
               IF  (I-U1)
                   MOVE PFIRMA             TO KOPIUT-IO-AREA (55:2)
               END-IF
               WRITE KOPIUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTR                   TO XO-70YY9
               MOVE XO-70YY9               TO TOTALER-IO-AREA (2:9)
               MOVE 'RECORDS PÅ ORDENR.FILE' TO TOTALER-IO-AREA (13:22)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTS                   TO XO-70YY9
               MOVE XO-70YY9               TO TOTALER-IO-AREA (2:9)
               MOVE 'ER FJERNET NÅ'        TO TOTALER-IO-AREA (13:13)
               MOVE 'RESTORDRELIMIT ER'    TO TOTALER-IO-AREA (41:17)
               MOVE LIMIT-X                TO EDIT-LIMIT-X
               MOVE EDIT-LIMIT-X           TO TOTALER-IO-AREA (59:7)
               MOVE 'ANT. GML.'            TO TOTALER-IO-AREA (72:9)
               MOVE ANTGML                 TO XO-70YY9
               MOVE XO-70YY9               TO TOTALER-IO-AREA (79:9)
               MOVE 3                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'KJØREDATO ER'         TO TOTALER-IO-AREA (12:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (25:8)
               MOVE 2                      TO TOTALER-AFTER-SPACE
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
           SET ORDNRM-LEVEL-INIT           TO TRUE
           INITIALIZE ORDNRM-DATA-FIELDS
           SET ORDNRM-EOF-OFF              TO TRUE
           SET ORDNRM-PROCESS              TO TRUE
           OPEN INPUT ORDNRM
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT KOPIUT
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDNRM
           CLOSE FIRMAF
           CLOSE KOPIUT
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
