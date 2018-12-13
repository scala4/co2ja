       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO083R.
      *    KONV. IFRA RSK083 UTVIDET RECORD.     ***TXT***ok ss***    *
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM.......: RKO083, UTPLUKK TIL SALDOLISTE.               *
      * ENDR..........: 23.07.98 TAKLER ÅR 2000.                      *
      *                 17.08.98 ENDRET TEKST I FEILMELDING/TEST-UPSI.*
      *                 18.08.98 SETTER PÅ H0 V/FEIL I DATO.          *
      *                 06.10.99 TATT UT ONLINE.                      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO083.rpg
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
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT RESKOMA
               ASSIGN TO RESKOMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKOMA-STATUS
               RECORD KEY IS RESKOMA-KEY1.
           SELECT SALDO
               ASSIGN TO UT-S-SALDO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SALDO-STATUS.
           SELECT INKASSO
               ASSIGN TO UT-S-INKASSO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INKASSO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
       FD RESKOMA
               RECORD CONTAINS 200.
       01  RESKOMA-IO-AREA.
           05  RESKOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  RESKOMA-KEY1.
                   15  RESKOMA-KEY1N       PICTURE S9(14).
               10  FILLER                  PICTURE X(185).
       FD SALDO
               BLOCK CONTAINS 2500
               RECORD CONTAINS 50.
       01  SALDO-IO-AREA.
           05  SALDO-IO-AREA-X             PICTURE X(50).
       FD INKASSO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  INKASSO-IO-AREA.
           05  INKASSO-IO-AREA-X           PICTURE X(200).
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  RESKOMA-STATUS              PICTURE 99 VALUE 0.
           10  SALDO-STATUS                PICTURE 99 VALUE 0.
           10  INKASSO-STATUS              PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-EOF-OFF          VALUE '0'.
               88  RESPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-READ-OFF         VALUE '0'.
               88  RESPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-PROCESS-OFF      VALUE '0'.
               88  RESPAR-PROCESS          VALUE '1'.
           05  RESKOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-EOF-OFF         VALUE '0'.
               88  RESKOMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-READ-OFF        VALUE '0'.
               88  RESKOMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-PROCESS-OFF     VALUE '0'.
               88  RESKOMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKOMA-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKOMA-LEVEL-INIT      VALUE '1'.
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
               10  DTO8SI                  PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
           05  RESPAR-DATA-FIELDS.
               10  PERDAT                  PICTURE X(6).
               10  INKDAT                  PICTURE X(6).
               10  PR2DAT                  PICTURE X(6).
      ******************************************************************
           05  RESKOMA-LEVEL-02.
               10  RESKOMA-02-L1.
                   15  RESKOMA-02-L1-RESKEY PICTURE X(9).
           05  RESKOMA-DATA-FIELDS.
               10  RREC                    PICTURE X(200).
               10  REC60                   PICTURE X(60).
               10  RBDATO                  PICTURE X(6).
               10  RFDATO                  PICTURE X(6).
               10  RBEL-IO.
                   15  RBEL                PICTURE S9(7)V9(2).
               10  RPA-ELGINK              PICTURE X(1).
               10  BETM                    PICTURE X(2).
      *                                      78  78 ONLINE
               10  RESKEY                  PICTURE X(9).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  PERDA8                  PICTURE X(8).
               10  INKDA8                  PICTURE X(8).
               10  PR2DA8                  PICTURE X(8).
               10  TOTSAL-IO.
                   15  TOTSAL              PICTURE S9(8)V9(2).
               10  PURSAL-IO.
                   15  PURSAL              PICTURE S9(7)V9(2).
               10  INKSAL-IO.
                   15  INKSAL              PICTURE S9(7)V9(2).
               10  BPSAL-IO.
                   15  BPSAL               PICTURE S9(7)V9(2).
               10  SALB06-IO.
                   15  SALB06              PICTURE S9(7)V9(2).
               10  RBDAT8                  PICTURE X(8).
               10  RFDAT8                  PICTURE X(8).
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESPAR-PROCESS
               SET RESPAR-PROCESS-OFF      TO TRUE
               SET RESPAR-READ             TO TRUE
           END-IF
 
           IF  RESPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESPAR-GET
               SET RESPAR-READ-OFF         TO TRUE
               IF  NOT RESPAR-EOF
                   PERFORM RESPAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RESPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RESKOMA-PROCESS
               SET RESKOMA-PROCESS-OFF     TO TRUE
               SET RESKOMA-READ            TO TRUE
           END-IF
 
           IF  RESKOMA-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKOMA-GET
               SET RESKOMA-READ-OFF        TO TRUE
               IF  NOT RESKOMA-EOF
                   PERFORM RESKOMA-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RESKOMA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-IDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-FLDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKOMA-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-97)
               SET I-H0                    TO TRUE
           END-IF
           IF  (I-01)
               MOVE PERDAT                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-98)
               SET I-97                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO PERDA8
      *
           END-IF
           IF  (I-01)
               MOVE INKDAT                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-98)
               SET I-97                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO INKDA8
      *
           END-IF
           IF  (I-01)
               MOVE PR2DAT                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-98)
               SET I-97                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO PR2DA8
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               SUBTRACT TOTSAL             FROM TOTSAL
               SUBTRACT PURSAL             FROM PURSAL
               SUBTRACT INKSAL             FROM INKSAL
               SUBTRACT BPSAL              FROM BPSAL
               SUBTRACT SALB06             FROM SALB06
               SET NOT-I-91                TO TRUE
      *  02                SETOF                     121314
           END-IF
           IF  (I-02)
               SET NOT-I-12                TO TRUE
               SET NOT-I-14                TO TRUE
               SET NOT-I-15                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-90                TO TRUE
               SET NOT-I-20                TO TRUE
               SET NOT-I-21                TO TRUE
      ******************************************************
           END-IF
           IF  (I-02)
               ADD RBEL                    TO TOTSAL
      *  02      ONLINE    COMP "1"                      11 REG. I DAG
      *  11                GOTO SLUTT
           END-IF
           IF  (I-02)
               MOVE RBDATO                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-02 AND NOT-I-98)
               MOVE DTO8SI                 TO RBDAT8
           END-IF
           IF  (I-02 AND I-98)
               SET NOT-I-12                TO TRUE
               IF  RBDATO > PERDAT
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-98)
               SET NOT-I-12                TO TRUE
               IF  RBDAT8 > PERDA8
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-12)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET NOT-I-14                TO TRUE
               IF  RPA-ELGINK = '1'
                   SET I-14                TO TRUE
               END-IF
               MOVE RFDATO                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-02 AND NOT-I-98)
               MOVE DTO8SI                 TO RFDAT8
           END-IF
           IF  (I-02 AND I-98)
               SET NOT-I-15                TO TRUE
               IF  RFDATO NOT > PR2DAT
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-98)
               SET NOT-I-15                TO TRUE
               IF  RFDAT8 NOT > PR2DA8
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-14 AND I-15)
               ADD RBEL                    TO PURSAL
      *          A         TAG
           END-IF
           IF  (I-02)
               SET I-90                    TO TRUE
               SET I-91                    TO TRUE
           END-IF
           IF  (I-02 AND I-14)
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND I-98)
               SET NOT-I-16                TO TRUE
               IF  RFDATO NOT > INKDAT
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-98)
               SET NOT-I-16                TO TRUE
               IF  RFDAT8 NOT > INKDA8
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16)
               SET NOT-I-20                TO TRUE
               IF  BETM = '02'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  BETM = '03'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  BETM = '05'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  BETM = '08'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  BETM = '15'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  BETM = '16'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  BETM = '04'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND I-20)
               ADD RBEL                    TO BPSAL
           END-IF
           IF  (I-02 AND I-16)
               SET NOT-I-26                TO TRUE
               IF  BETM = '06'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND I-26)
               ADD RBEL                    TO SALB06
           END-IF
           IF  (I-02 AND I-16)
               SET NOT-I-21                TO TRUE
               IF  BETM = '02'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '03'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '04'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '05'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '06'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '09'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '10'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '13'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16 AND I-21)
               ADD RBEL                    TO INKSAL
           END-IF.
 
       SLUTT-T.
      ******************************************************
           CONTINUE.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-98                    TO TRUE
           MOVE 'B'                        TO DTOKOD
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-98                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-98                    TO TRUE
           END-IF.
      *
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-17                TO TRUE
               IF  TOTSAL NOT > 0,00
                   SET I-17                TO TRUE
               END-IF
               SET NOT-I-18                TO TRUE
               IF  PURSAL NOT > 0,00
                   SET I-18                TO TRUE
               END-IF
               SET NOT-I-19                TO TRUE
               IF  BPSAL NOT > 0,00
                   SET I-19                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  SALB06 NOT > 0,00
                   SET I-27                TO TRUE
               END-IF
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
           END-IF
           .
 
       RESPAR-GET SECTION.
       RESPAR-GET-P.
           IF  RESPAR-EOF-OFF
               READ RESPAR
               AT END
                   SET RESPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESPAR-FLDSET SECTION.
       RESPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               MOVE RESPAR-IO-AREA (13:6)  TO PERDAT (1:6)
               MOVE RESPAR-IO-AREA (94:6)  TO INKDAT (1:6)
               MOVE RESPAR-IO-AREA (107:6) TO PR2DAT (1:6)
           END-EVALUATE.
 
       RESPAR-IDCHK SECTION.
       RESPAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESPAR-IDSET SECTION.
       RESPAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       RESKOMA-GET SECTION.
       RESKOMA-GET-P.
           IF  RESKOMA-EOF-OFF
               READ RESKOMA
               AT END
                   SET RESKOMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKOMA-FLDSET SECTION.
       RESKOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESKOMA-IO-AREA (1:1) = '0'
            AND   RESKOMA-IO-AREA (2:1) = '2' )
               MOVE RESKOMA-IO-AREA (1:200) TO RREC (1:200)
               MOVE RESKOMA-IO-AREA (1:60) TO REC60 (1:60)
               MOVE RESKOMA-IO-AREA (24:6) TO RBDATO (1:6)
               MOVE RESKOMA-IO-AREA (42:6) TO RFDATO (1:6)
               MOVE RESKOMA-IO-AREA (48:9) TO RBEL-IO
               INSPECT RBEL-IO REPLACING ALL ' ' BY '0'
               MOVE RESKOMA-IO-AREA (60:1) TO RPA-ELGINK (1:1)
               MOVE RESKOMA-IO-AREA (61:2) TO BETM (1:2)
               MOVE RESKOMA-IO-AREA (3:9)  TO RESKEY (1:9)
           END-EVALUATE.
 
       RESKOMA-IDCHK SECTION.
       RESKOMA-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESKOMA-IO-AREA (1:1) = '0'
            AND   RESKOMA-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESKOMA-IDSET SECTION.
       RESKOMA-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESKOMA-IO-AREA (1:1) = '0'
            AND   RESKOMA-IO-AREA (2:1) = '2' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       RESKOMA-CHK-LEVEL SECTION.
       RESKOMA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RESKOMA-IO-AREA (1:1) = '0'
            AND   RESKOMA-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO RESKOMA-LEVEL-02
               MOVE RESKOMA-IO-AREA (3:9)  TO RESKOMA-02-L1-RESKEY
               IF  RESKOMA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKOMA-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKOMA-02-L1         TO THE-PRIOR-L1
               SET RESKOMA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-90)
               MOVE SPACES TO INKASSO-IO-AREA
               INITIALIZE INKASSO-IO-AREA
               MOVE RREC                   TO INKASSO-IO-AREA (1:200)
               MOVE BETM                   TO INKASSO-IO-AREA (61:2)
               WRITE INKASSO-IO-AREA
           END-IF
           IF  (I-01 AND I-98)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> CANCEL RSK083  ==>  ' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE 'FEIL I DATO (SUBRUT DATO' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE '8SIF)       '         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'PERDA8: '             TO CONSOLE-IO-AREA (5:8)
               MOVE PERDA8                 TO CONSOLE-IO-AREA (13:8)
               MOVE 'INKDA8: '             TO CONSOLE-IO-AREA (21:8)
               MOVE INKDA8                 TO CONSOLE-IO-AREA (31:8)
               MOVE 'PR2DA8: '             TO CONSOLE-IO-AREA (21:8)
               MOVE PR2DA8                 TO CONSOLE-IO-AREA (31:8)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (4:57)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-02 AND I-98)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> PROGRAM: RSK083 ' TO CONSOLE-IO-AREA (1:20)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (14:57)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> REC:'             TO CONSOLE-IO-AREA (1:8)
               MOVE REC60                  TO CONSOLE-IO-AREA (11:60)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               DISPLAY CONSOLE-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-91)
               MOVE SPACES TO SALDO-IO-AREA
               INITIALIZE SALDO-IO-AREA
               MOVE '9'                    TO SALDO-IO-AREA (1:1)
               MOVE RESKEY                 TO SALDO-IO-AREA (2:9)
               MOVE TOTSAL-IO              TO SALDO-IO-AREA (11:10)
               IF  (I-17)
                   MOVE '1'                TO SALDO-IO-AREA (21:1)
               END-IF
               MOVE PURSAL-IO              TO SALDO-IO-AREA (22:9)
               IF  (I-18)
                   MOVE '1'                TO SALDO-IO-AREA (31:1)
               END-IF
               MOVE INKSAL-IO              TO SALDO-IO-AREA (32:9)
               IF  (I-19)
                   MOVE '1'                TO SALDO-IO-AREA (41:1)
               END-IF
               IF  (I-27)
                   MOVE '1'                TO SALDO-IO-AREA (42:1)
      *****************************************************************
      * FEILMELDINGER PÅ KONSOLLET:                                   *
      *****************************************************************
               END-IF
               WRITE SALDO-IO-AREA
           END-IF
           IF  (I-U1 AND I-U2 AND I-U3)
           AND (I-LR AND I-U8 AND I-U7)
           AND (I-99)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE PSDS                   TO CONSOLE-IO-AREA (1:80)
               MOVE R                      TO CONSOLE-IO-AREA (73:8)
               MOVE P-IO                   TO CONSOLE-IO-AREA (78:3)
               MOVE S-IO                   TO CONSOLE-IO-AREA (76:5)
               MOVE DTODTO                 TO CONSOLE-IO-AREA (75:6)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (24:57)
               MOVE '***'                  TO CONSOLE-IO-AREA (2:3)
               DISPLAY CONSOLE-IO-AREA
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
           INITIALIZE RESPAR-DATA-FIELDS
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN INPUT RESPAR
           SET RESKOMA-LEVEL-INIT          TO TRUE
           INITIALIZE RESKOMA-DATA-FIELDS
           SET RESKOMA-EOF-OFF             TO TRUE
           SET RESKOMA-PROCESS             TO TRUE
           OPEN INPUT RESKOMA
           OPEN OUTPUT SALDO
           OPEN OUTPUT INKASSO.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESPAR
           CLOSE RESKOMA
           CLOSE SALDO
           CLOSE INKASSO.
 
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
