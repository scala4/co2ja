       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK022R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: RSK022, LEGGER INN REGNSKAPSPERIODE I REG.TRANS.*
      *  LAGET AV   : MORTEN TUVRØNNINGEN                             *
      *  ENDRET.....: 03.03.98 LEGGER INN GJELDENDE PERIODE HVIS DET  *
      *                        ER FEIL I DATO.                        *
      *               29.05.01 ENDRET TEKST PÅ KVITTERINGSLISTEN.     *
      *               18.09.09 LEGGER INN PERIODEN BILAGSDATOEN HØRER *
      *                        TIL I STEDET FOR NESTE PERIODE VED     *
      *                        FREMDATERINGER.                        *
      *               15.04.11 UVIDET REGFILE FRA 120 TIL 240 BYTE  MT*
      *  FÅR........: PARAMETERFILE MED REGNSKAPSPERIODE (AUTOPAR),   *
      *               SAMTLIGE DAGENS REGNSKAPSTRANSER (DAGREG).      *
      *  GJØR.......: HENTER REGSKAPSPARAMETER MED INNEVÆRENDE PERIODE*
      *               OG BEREGNER NESTE PERIODE. SETTER INNEVÆRENDE   *
      *               PERIODE PÅ TRANSER MED BILAGSDATO LIK ELLER     *
      *               MINDRE ENN INNEVÆRENDE PERIODE, OG NESTE PERIODE*
      *               PÅ ALLE TRANSER MED BILAGSMÅNED FRAM I TID.     *
      *               LEGGER PÅ BILAGSÅRHUNDRE PÅ ALLE TRANSER: HVIS  *
      *               BILAGSÅR > 80 SETTES 19 ELLERS SETTES 20 (HENTES*
      *               VIA SUBPROGRAM "DATO8SIF".                      *
      *               UPSI-1 ER PÅ: BRUKER FORRIGE PERIODE FRA AUTOPAR*
      *  GIR........: DAGENS REGNSKAPSTRANSER.                        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK022.rpg
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
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT DAGREG
               ASSIGN TO UT-S-DAGREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGREG-STATUS.
           SELECT NYREG
               ASSIGN TO UT-S-NYREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYREG-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD DAGREG
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  DAGREG-IO-AREA.
           05  DAGREG-IO-AREA-X            PICTURE X(240).
       FD NYREG
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  NYREG-IO-AREA.
           05  NYREG-IO-AREA-X             PICTURE X(240).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  DAGREG-STATUS               PICTURE 99 VALUE 0.
           10  NYREG-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-EOF-OFF          VALUE '0'.
               88  DAGREG-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-READ-OFF         VALUE '0'.
               88  DAGREG-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-PROCESS-OFF      VALUE '0'.
               88  DAGREG-PROCESS          VALUE '1'.
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
           05  CONSOLE-IO-AREA.
               10  CONSOLE-IO-AREA-X       PICTURE X(800).
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
           05  DTOPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTOARH                  PICTURE X(2).
               10  FILLER                  PICTURE X(63).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTO8SI                  PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTOPER                  PICTURE X(6).
               10  FILLER                  PICTURE X(59).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
           05  AUTOPAR-DATA-FIELDS.
               10  PARPER                  PICTURE X(6).
               10  FORPER                  PICTURE X(6).
               10  PARARH                  PICTURE X(2).
               10  PARMND                  PICTURE X(2).
           05  DAGREG-DATA-FIELDS.
               10  REC240                  PICTURE X(240).
               10  BILDTO                  PICTURE X(6).
               10  BELOP-IO.
                   15  BELOP               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TEGN                    PICTURE X(1).
               10  BEL132-IO.
                   15  BEL132              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  TEMPORARY-FIELDS.
               10  ANTTOT-IO.
                   15  ANTTOT              PICTURE S9(6).
               10  BELTOT-IO.
                   15  BELTOT              PICTURE S9(9)V9(2).
               10  BET132-IO.
                   15  BET132              PICTURE S9(11)V9(2).
               10  ANTPER-IO.
                   15  ANTPER              PICTURE S9(6).
               10  BELPER-IO.
                   15  BELPER              PICTURE S9(9)V9(2).
               10  BELPEU-IO.
                   15  BELPEU              PICTURE S9(11)V9(2).
               10  ANTNES-IO.
                   15  ANTNES              PICTURE S9(6).
               10  BELNES-IO.
                   15  BELNES              PICTURE S9(9)V9(2).
               10  BELNEU-IO.
                   15  BELNEU              PICTURE S9(11)V9(2).
               10  ANTFEI-IO.
                   15  ANTFEI              PICTURE S9(6).
               10  BELFEI-IO.
                   15  BELFEI              PICTURE S9(9)V9(2).
               10  BELFEU-IO.
                   15  BELFEU              PICTURE S9(11)V9(2).
               10  AUTKEY                  PICTURE X(3).
               10  NUMPER-IO.
                   15  NUMPER              PICTURE S9(6).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  NESPER                  PICTURE X(6).
           05  EDITTING-FIELDS.
               10  XO-60YN9R               PICTURE ZZZZZ9-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
               10  EDIT-BELTOT             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  DAGREG-PROCESS
               SET DAGREG-PROCESS-OFF      TO TRUE
               SET DAGREG-READ             TO TRUE
           END-IF
 
           IF  DAGREG-READ
           AND RECORD-SELECTED-OFF
               PERFORM DAGREG-GET
               SET DAGREG-READ-OFF         TO TRUE
               IF  NOT DAGREG-EOF
                   SET DAGREG-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  DAGREG-PROCESS
               PERFORM DAGREG-IDSET
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
 
           IF  DAGREG-PROCESS
               PERFORM DAGREG-FLDOFF
               PERFORM DAGREG-FLDSET
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
           IF  (NOT-I-99)
               PERFORM PARRUT-S
           END-IF
           PERFORM DTORUT-S
           IF  (NOT-I-98 AND I-20)
               SET I-98                    TO TRUE
           END-IF
           ADD 1                           TO ANTTOT
           IF  (I-11)
               ADD BELOP                   TO BELTOT
           END-IF
           IF  (NOT-I-11)
               SUBTRACT BELOP              FROM BELTOT
           END-IF
           IF  (I-11)
               ADD BEL132                  TO BET132
           END-IF
           IF  (NOT-I-11)
               SUBTRACT BEL132             FROM BET132
           END-IF
           IF  (NOT-I-98 AND I-31)
               ADD 1                       TO ANTPER
           END-IF
           IF  (I-11 AND NOT-I-98 AND I-31)
               ADD BELOP                   TO BELPER
           END-IF
           IF  (NOT-I-11 AND NOT-I-98 AND I-31)
               SUBTRACT BELOP              FROM BELPER
           END-IF
           IF  (I-11 AND NOT-I-98 AND I-31)
               ADD BELOP                   TO BELPEU
           END-IF
           IF  (NOT-I-11 AND NOT-I-98 AND I-31)
               SUBTRACT BELOP              FROM BELPEU
           END-IF
           IF  (NOT-I-98 AND I-30)
               ADD 1                       TO ANTNES
           END-IF
           IF  (I-11 AND NOT-I-98 AND I-30)
               ADD BELOP                   TO BELNES
           END-IF
           IF  (NOT-I-11 AND NOT-I-98 AND I-30)
               SUBTRACT BELOP              FROM BELNES
           END-IF
           IF  (I-11 AND NOT-I-98 AND I-30)
               ADD BELOP                   TO BELNEU
           END-IF
           IF  (NOT-I-11 AND NOT-I-98 AND I-30)
               SUBTRACT BELOP              FROM BELNEU
           END-IF
           IF  (I-98)
               ADD 1                       TO ANTFEI
               SET NOT-I-13                TO TRUE
               IF  ANTFEI > 0
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-11 AND I-98)
               ADD BELOP                   TO BELFEI
           END-IF
           IF  (NOT-I-11 AND I-98)
               SUBTRACT BELOP              FROM BELFEI
           END-IF
           IF  (I-11 AND I-98)
               ADD BELOP                   TO BELFEU
           END-IF
           IF  (NOT-I-11 AND I-98)
               SUBTRACT BELOP              FROM BELFEU
      *                    MOVE "PARPER  "BUGFL1  8        LEDETXT DEBUG
      *          BUGFL1    DEBUGBUGFILO   PARPER           VIS FELT/IND
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE PARAMETER PERIODER.                    * *
      *****************************************************************
           END-IF
           .
 
       PARRUT-S SECTION.
       PARRUT-S-P.
           MOVE 'A01'                      TO AUTKEY
           MOVE AUTKEY                     TO AUTOPAR-KEY1
           READ AUTOPAR RECORD KEY IS AUTOPAR-KEY1
           INVALID KEY
               SET I-20                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-20                TO TRUE
               PERFORM AUTOPAR-FLDOFF
               PERFORM AUTOPAR-FLDSET
               PERFORM AUTOPAR-IDSET
           END-READ
      *R    U8             MOVE "      "  PARPER           TEST FEIL
      *R    U8             SETON                     15    TEST FEIL
      *R    U7             MOVE "199712"  PARPER           TEST ÅRSKIFTE
      *R    U7             MOVE "12"      PARMND           ------""------
           IF  (NOT-I-20 AND I-15)
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               GO TO ENDPAR-T
           END-IF
           IF  (I-U1)
               MOVE FORPER                 TO PARPER
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  PARMND NOT < '12'
               SET I-21                    TO TRUE
           END-IF
           MOVE PARPER                     TO NUMPER-IO
           IF  (I-21)
               ADD 100                     TO NUMPER
               MOVE PARMND                 TO MND-IO
               SUBTRACT MND                FROM NUMPER
           END-IF
           ADD 1                           TO NUMPER
           MOVE NUMPER                     TO NESPER
           SET I-99                        TO TRUE.
 
       ENDPAR-T.
           CONTINUE.
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-30                    TO TRUE
           SET NOT-I-31                    TO TRUE
           SET NOT-I-98                    TO TRUE
           MOVE 'B'                        TO DTOKOD
           MOVE BILDTO                     TO DTODTO
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-98                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-98                    TO TRUE
           END-IF
      *R    U6             MOVE "199802"  DTOPER            TEST SKIFTE
           IF  (NOT-I-98)
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
               IF  DTOPER NOT > PARPER
                   SET I-31                TO TRUE
               END-IF
               IF  DTOPER > PARPER
                   SET I-30                TO TRUE
               END-IF
           END-IF.
      *
 
       AUTOPAR-FLDOFF SECTION.
       AUTOPAR-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (12:6) TO PARPER (1:6)
               IF  PARPER = SPACES
                   SET I-15                TO TRUE
               END-IF
               MOVE AUTOPAR-IO-AREA (18:6) TO FORPER (1:6)
               MOVE AUTOPAR-IO-AREA (12:2) TO PARARH (1:2)
               MOVE AUTOPAR-IO-AREA (16:2) TO PARMND (1:2)
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       DAGREG-GET SECTION.
       DAGREG-GET-P.
           IF  DAGREG-EOF-OFF
               READ DAGREG
               AT END
                   SET DAGREG-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGREG-FLDOFF SECTION.
       DAGREG-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-11                TO TRUE
           END-EVALUATE.
 
       DAGREG-FLDSET SECTION.
       DAGREG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGREG-IO-AREA (1:240) TO REC240 (1:240)
               MOVE DAGREG-IO-AREA (13:6)  TO BILDTO (1:6)
               MOVE DAGREG-IO-AREA (27:5)  TO BELOP-IO
               MOVE DAGREG-IO-AREA (32:1)  TO TEGN (1:1)
               IF  TEGN = SPACES
                   SET I-11                TO TRUE
               END-IF
               MOVE DAGREG-IO-AREA (121:7) TO BEL132-IO
           END-EVALUATE.
 
       DAGREG-IDSET SECTION.
       DAGREG-IDSET-P.
           SET I-02                        TO TRUE.
 
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
           IF  (I-02)
               MOVE SPACES TO NYREG-IO-AREA
               INITIALIZE NYREG-IO-AREA
               MOVE REC240                 TO NYREG-IO-AREA (1:240)
               IF  (I-98)
                   MOVE PARPER             TO NYREG-IO-AREA (41:6)
               END-IF
               IF  (NOT-I-98 AND I-31)
                   MOVE PARPER             TO NYREG-IO-AREA (41:6)
               END-IF
               IF  (NOT-I-98 AND I-30)
                   MOVE NESPER             TO NYREG-IO-AREA (41:6)
               END-IF
               IF  (NOT-I-98 AND I-30)
                   MOVE DTOPER             TO NYREG-IO-AREA (41:6)
               END-IF
               IF  (I-98)
                   MOVE PARARH             TO NYREG-IO-AREA (60:2)
               END-IF
               IF  (NOT-I-98)
                   MOVE DTOARH             TO NYREG-IO-AREA (60:2)
               END-IF
               WRITE NYREG-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '>>>> '                TO CONSOLE-IO-AREA (1:5)
               MOVE 'REGNSKAPSPERIODE FRA AUT' TO CONSOLE-IO-AREA
                                                                (6:24)
               MOVE 'OPAR:'                TO CONSOLE-IO-AREA (30:5)
               MOVE PARPER                 TO CONSOLE-IO-AREA (36:6)
               MOVE 'NESTE PERIODE:'       TO CONSOLE-IO-AREA (43:14)
               MOVE NESPER                 TO CONSOLE-IO-AREA (58:6)
               MOVE ' <<<<'                TO CONSOLE-IO-AREA (65:5)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-01 AND I-20)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '>>>>>>>>> '           TO CONSOLE-IO-AREA (1:10)
               MOVE 'FEIL/BLANK REG.PER. I' TO CONSOLE-IO-AREA (11:21)
               MOVE ' PARAMETER <<<<<<<<<' TO CONSOLE-IO-AREA (32:20)
               DISPLAY CONSOLE-IO-AREA
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-U8 AND I-LR AND I-OF)
           AND (I-97)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DTODTO                 TO LISTE-IO-AREA (115:6)
               MOVE DTO8SI                 TO LISTE-IO-AREA (113:8)
               MOVE DTOMEL                 TO LISTE-IO-AREA (64:57)
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RSK022 ' TO LISTE-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '************************' TO LISTE-IO-AREA (41:24)
               MOVE '****************'     TO LISTE-IO-AREA (65:16)
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'INNLEGGING AV REGNSKAPSP' TO LISTE-IO-AREA (23:24)
               MOVE 'ERIODE I DAGENS REGNSKAP' TO LISTE-IO-AREA (47:24)
               MOVE 'STRANSER'             TO LISTE-IO-AREA (71:8)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'KONTROLLER AT DENNE OG N' TO LISTE-IO-AREA (23:24)
               MOVE 'ESTE PERIODE ER RIKTIG. ' TO LISTE-IO-AREA (47:24)
               MOVE '        '             TO LISTE-IO-AREA (71:8)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'VED FEIL KONTAKTES SYSTE' TO LISTE-IO-AREA (23:24)
               MOVE 'MANSVARLIG.             ' TO LISTE-IO-AREA (47:24)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'ELLERS KAN LISTEN KASTES' TO LISTE-IO-AREA (23:24)
               MOVE '.                       ' TO LISTE-IO-AREA (47:24)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*                   ' TO LISTE-IO-AREA (17:20)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     FREMSTILT'      TO LISTE-IO-AREA (17:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (33:8)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (80:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '************************' TO LISTE-IO-AREA (41:24)
               MOVE '****************'     TO LISTE-IO-AREA (65:16)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RSK022 ' TO LISTE-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'REGNSKAPSPERIODE FRA AUT' TO LISTE-IO-AREA (1:24)
               MOVE 'OPAR:'                TO LISTE-IO-AREA (25:5)
               MOVE PARPER                 TO LISTE-IO-AREA (31:6)
               MOVE 'NESTE PERIODE:'       TO LISTE-IO-AREA (38:14)
               MOVE NESPER                 TO LISTE-IO-AREA (53:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-20)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '>>>>>>>>> '           TO LISTE-IO-AREA (1:10)
               MOVE 'FEIL/BLANK REG.PER. I' TO LISTE-IO-AREA (11:21)
               MOVE ' PARAMETER <<<<<<<<<' TO LISTE-IO-AREA (32:20)
               MOVE '<=======================' TO LISTE-IO-AREA (72:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '>>>>>>>>> '           TO LISTE-IO-AREA (1:10)
               MOVE 'REGFILE OG AKKHBOK MÅ' TO LISTE-IO-AREA (11:21)
               MOVE ' RETTES    <<<<<<<<<' TO LISTE-IO-AREA (32:20)
               MOVE '<=======================' TO LISTE-IO-AREA (72:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER MED PERIO' TO LISTE-IO-AREA (1:24)
               MOVE 'DE '                  TO LISTE-IO-AREA (25:3)
               MOVE PARPER                 TO LISTE-IO-AREA (28:6)
               MOVE ':'                    TO LISTE-IO-AREA (35:1)
               MOVE ANTPER                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELPER                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELPEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER FREMDATER' TO LISTE-IO-AREA (1:24)
               MOVE 'T'                    TO LISTE-IO-AREA (25:1)
               MOVE ':'                    TO LISTE-IO-AREA (35:1)
               MOVE ANTNES                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELNES                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELNEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER MED FEIL ' TO LISTE-IO-AREA (1:24)
               MOVE 'PERIODE   :'          TO LISTE-IO-AREA (25:11)
               MOVE ANTFEI                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELFEI                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELFEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
               IF  (I-13)
                   MOVE '<=======================' TO LISTE-IO-AREA
                                                               (72:24)
               END-IF
               IF  (I-13)
                   MOVE 'PERIODE RETTET TIL: ' TO LISTE-IO-AREA (98:20)
               END-IF
               IF  (I-13)
                   MOVE PARPER             TO LISTE-IO-AREA (118:6)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER TOTALT   ' TO LISTE-IO-AREA (1:24)
               MOVE ':'                    TO LISTE-IO-AREA (35:1)
               MOVE ANTTOT                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELTOT                 TO EDIT-BELTOT
               MOVE EDIT-BELTOT            TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BET132                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           INITIALIZE DAGREG-DATA-FIELDS
           SET DAGREG-EOF-OFF              TO TRUE
           SET DAGREG-PROCESS              TO TRUE
           OPEN INPUT DAGREG
           OPEN OUTPUT NYREG
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE AUTOPAR
           CLOSE DAGREG
           CLOSE NYREG
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
