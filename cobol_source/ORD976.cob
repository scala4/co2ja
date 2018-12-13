       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD976R.
      **********************************************  Z-WIN-RPG2   ****
      *       O R D R E R U T I N E P R O G R A M   O R D 9 7 6       *
      *       -------------------------------------------------       *
      *  2. DANNE RECORD KUNDE.VGR.TOTAL.FILE                         *
      *      24/4-2002 AV ESPEN LARSEN.                               *
      * 23/4-2003 DANNER RECORD MED 0 SALG DENNE MND I ÅR OG I FJOR.  *
      *12/12-2003 HENTER INNEVÆRENDE MND. FRA NYTT FELT I AUTOPAR.    *
      *19/09-2008 LAGT INN SELVKOST                                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD976.rpg
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
           SELECT INNF
               ASSIGN TO UT-S-INNF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNF-STATUS.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT KUNDTOT
               ASSIGN TO KUNDTOT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDTOT-STATUS
               RECORD KEY IS KUNDTOT-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD INNF
               BLOCK CONTAINS 82
               RECORD CONTAINS 41.
       01  INNF-IO-AREA.
           05  INNF-IO-AREA-X              PICTURE X(41).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD KUNDTOT
               RECORD CONTAINS 57.
       01  KUNDTOT-IO-AREA.
           05  KUNDTOT-IO-AREA-X.
               10  KUNDTOT-KEY1.
                   15  KUNDTOT-KEY1N       PICTURE S9(17).
               10  FILLER                  PICTURE X(40).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNF-STATUS                 PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  KUNDTOT-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-EOF-OFF            VALUE '0'.
               88  INNF-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-READ-OFF           VALUE '0'.
               88  INNF-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-PROCESS-OFF        VALUE '0'.
               88  INNF-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNF-LEVEL-INIT-OFF     VALUE '0'.
               88  INNF-LEVEL-INIT         VALUE '1'.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDTOT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INNF-LEVEL-01.
               10  INNF-01-L4.
                   15  INNF-01-L4-FIRMA    PICTURE X(3).
               10  INNF-01-L3.
                   15  INNF-01-L3-TOTTYP   PICTURE X(1).
               10  INNF-01-L2.
                   15  INNF-01-L2-KNRVGR   PICTURE X(11).
               10  INNF-01-L1.
                   15  INNF-01-L1-MND      PICTURE X(2).
           05  INNF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  TOTTYP                  PICTURE X(1).
               10  KNRVGR                  PICTURE X(11).
               10  MND                     PICTURE X(2).
               10  MNDNR-IO.
                   15  MNDNR               PICTURE S9(2).
               10  SALDPA-ELG-IO.
                   15  SALDPA-ELG          PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SALDPF-IO.
                   15  SALDPF              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SELKPA-ELG-IO.
                   15  SELKPA-ELG          PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SELKPF-IO.
                   15  SELKPF              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  AUTOPAR-DATA-FIELDS.
               10  APMND-IO.
                   15  APMND               PICTURE S9(2).
      *****************************************************************
      * OPPSLAG MOT PARAMFIL.AUTODATA FOR HENTING AV AKTUELL DATO.    *
      * I januar må det tilpasse år og mnd.                           *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(11).
               10  THE-PRIOR-L1            PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  APKEY                   PICTURE X(3).
               10  PMND-IO.
                   15  PMND                PICTURE S9(2).
               10  SUMDPA-ELG-IO.
                   15  SUMDPA-ELG          PICTURE S9(9).
               10  SUMDPF-IO.
                   15  SUMDPF              PICTURE S9(9).
               10  SELDPA-ELG-IO.
                   15  SELDPA-ELG          PICTURE S9(9).
               10  SELDPF-IO.
                   15  SELDPF              PICTURE S9(9).
               10  MMND                    PICTURE X(2).
               10  SMND-IO.
                   15  SMND                PICTURE S9(2).
               10  BELDPA-ELG-IO.
                   15  BELDPA-ELG          PICTURE S9(9)V9(2).
               10  BELDPF-IO.
                   15  BELDPF              PICTURE S9(9)V9(2).
               10  SLKDPA-ELG-IO.
                   15  SLKDPA-ELG          PICTURE S9(9)V9(2).
               10  SLKDPF-IO.
                   15  SLKDPF              PICTURE S9(9)V9(2).
               10  BELAKA-ELG-IO.
                   15  BELAKA-ELG          PICTURE S9(9)V9(2).
               10  BELAKF-IO.
                   15  BELAKF              PICTURE S9(9)V9(2).
               10  SELAKA-ELG-IO.
                   15  SELAKA-ELG          PICTURE S9(9)V9(2).
               10  SELAKF-IO.
                   15  SELAKF              PICTURE S9(9)V9(2).
               10  SUMAKA-ELG-IO.
                   15  SUMAKA-ELG          PICTURE S9(9).
               10  SUMAKF-IO.
                   15  SUMAKF              PICTURE S9(9).
               10  SLKAKA-ELG-IO.
                   15  SLKAKA-ELG          PICTURE S9(9).
               10  SLKAKF-IO.
                   15  SLKAKF              PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNF-PROCESS
               SET INNF-PROCESS-OFF        TO TRUE
               SET INNF-READ               TO TRUE
           END-IF
 
           IF  INNF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNF-GET
               SET INNF-READ-OFF           TO TRUE
               IF  NOT INNF-EOF
                   SET INNF-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-IDSET
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNF-PROCESS
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
               ADD APMND TO ZERO       GIVING PMND
               SET I-09                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å LEGGE UT MÅNEDER MED 0 SALG I ÅR OG I FJOR.      *
      *****************************************************************
           END-IF
           IF  (I-L2)
               GO TO HRUT-T
           END-IF.
 
       NXTMND-T.
           IF  (I-01)
               ADD 1                       TO SMND
               SET NOT-I-23                TO TRUE
               IF  MNDNR > SMND
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-23)
               GO TO HRUT-T
           END-IF
           IF  (I-01)
               MOVE 0                      TO SUMDPA-ELG
               MOVE 0                      TO SUMDPF
               MOVE 0                      TO SELDPA-ELG
               MOVE 0                      TO SELDPF
               MOVE SMND                   TO MMND
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE MMND (2:1)             TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO MMND (2:1)
               SET I-25                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-25                TO TRUE
               GO TO NXTMND-T
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           END-IF
           .
 
       HRUT-T.
           IF  (I-01)
               ADD MNDNR TO ZERO       GIVING SMND
           END-IF
           IF  (I-L1)
               SUBTRACT BELDPA-ELG         FROM BELDPA-ELG
               SUBTRACT BELDPF             FROM BELDPF
               SUBTRACT SLKDPA-ELG         FROM SLKDPA-ELG
               SUBTRACT SLKDPF             FROM SLKDPF
           END-IF
           IF  (I-01)
               ADD SALDPA-ELG              TO BELDPA-ELG
               ADD SALDPF                  TO BELDPF
               ADD SELKPA-ELG              TO SLKDPA-ELG
               ADD SELKPF                  TO SLKDPF
      *
           END-IF
           IF  (I-L2)
               SUBTRACT BELAKA-ELG         FROM BELAKA-ELG
               SUBTRACT BELAKF             FROM BELAKF
               SUBTRACT SELAKA-ELG         FROM SELAKA-ELG
               SUBTRACT SELAKF             FROM SELAKF
           END-IF
           IF  (I-01)
               ADD SALDPA-ELG              TO BELAKA-ELG
               ADD SALDPF                  TO BELAKF
               ADD SELKPA-ELG              TO SELAKA-ELG
               ADD SELKPF                  TO SELAKF
      *****************************************************************
      *                 T O T A L R U T I N E   P R .   M N D.        *
      *****************************************************************
           END-IF
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD BELDPA-ELG TO ZERO  GIVING SUMDPA-ELG ROUNDED
               ADD BELDPF TO ZERO      GIVING SUMDPF ROUNDED
               ADD SLKDPA-ELG TO ZERO  GIVING SELDPA-ELG ROUNDED
               ADD SLKDPF TO ZERO      GIVING SELDPF ROUNDED
      *
           END-IF
           IF  (I-L1)
               ADD BELAKA-ELG TO ZERO  GIVING SUMAKA-ELG ROUNDED
               ADD BELAKF TO ZERO      GIVING SUMAKF ROUNDED
               ADD SELAKA-ELG TO ZERO  GIVING SLKAKA-ELG ROUNDED
               ADD SELAKF TO ZERO      GIVING SLKAKF ROUNDED
               SET I-24                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-24                TO TRUE
      *****************************************************************
      * TOTALRUTINE FOR Å LEGGE UT MÅNEDER UTEN SALG TIL SLUTT.       *
      *****************************************************************
           END-IF
           .
 
       NL2MND-T.
           IF  (I-L2)
               ADD 1                       TO SMND
               SET NOT-I-23                TO TRUE
               IF  SMND NOT > PMND
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-23)
               GO TO ENDL2-T
           END-IF
           IF  (I-L2)
               MOVE 0                      TO SUMDPA-ELG
               MOVE 0                      TO SUMDPF
               MOVE 0                      TO SELDPA-ELG
               MOVE 0                      TO SELDPF
               MOVE SMND                   TO MMND
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE MMND (2:1)             TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO MMND (2:1)
               SET I-25                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-25                TO TRUE
               GO TO NL2MND-T
           END-IF.
 
       ENDL2-T.
           CONTINUE.
 
       INNF-GET SECTION.
       INNF-GET-P.
           IF  INNF-EOF-OFF
               READ INNF
               AT END
                   SET INNF-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNF-FLDSET SECTION.
       INNF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (1:3)     TO FIRMA (1:3)
               MOVE INNF-IO-AREA (4:1)     TO TOTTYP (1:1)
               MOVE INNF-IO-AREA (5:11)    TO KNRVGR (1:11)
               MOVE INNF-IO-AREA (16:2)    TO MND (1:2)
               MOVE INNF-IO-AREA (16:2)    TO MNDNR-IO
               INSPECT MNDNR-IO REPLACING ALL ' ' BY '0'
               MOVE INNF-IO-AREA (18:6)    TO SALDPA-ELG-IO
               MOVE INNF-IO-AREA (24:6)    TO SALDPF-IO
               MOVE INNF-IO-AREA (30:6)    TO SELKPA-ELG-IO
               MOVE INNF-IO-AREA (36:6)    TO SELKPF-IO
           END-EVALUATE.
 
       INNF-IDSET SECTION.
       INNF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNF-CHK-LEVEL SECTION.
       INNF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNF-LEVEL-01
               MOVE INNF-IO-AREA (1:3)     TO INNF-01-L4-FIRMA
               MOVE INNF-IO-AREA (4:1)     TO INNF-01-L3-TOTTYP
               MOVE INNF-IO-AREA (5:11)    TO INNF-01-L2-KNRVGR
               MOVE INNF-IO-AREA (16:2)    TO INNF-01-L1-MND
               IF  INNF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNF-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INNF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNF-01-L4            TO THE-PRIOR-L4
               MOVE  INNF-01-L3            TO THE-PRIOR-L3
               MOVE  INNF-01-L2            TO THE-PRIOR-L2
               MOVE  INNF-01-L1            TO THE-PRIOR-L1
               SET INNF-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (76:2) TO APMND-IO
               INSPECT APMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-25)
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRMA                  TO KUNDTOT-IO-AREA (1:3)
               MOVE TOTTYP                 TO KUNDTOT-IO-AREA (4:1)
               MOVE KNRVGR                 TO KUNDTOT-IO-AREA (5:11)
               MOVE MMND                   TO KUNDTOT-IO-AREA (16:2)
               MOVE SUMDPA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (18:5)
               MOVE SUMDPF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (23:5)
               MOVE SUMAKA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (28:5)
               MOVE SUMAKF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (33:5)
               MOVE SELDPA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (38:5)
               MOVE SELDPF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (43:5)
               MOVE SLKAKA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (48:5)
               MOVE SLKAKF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (53:5)
               WRITE KUNDTOT-IO-AREA
           END-IF
           IF  (I-24)
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRMA                  TO KUNDTOT-IO-AREA (1:3)
               MOVE TOTTYP                 TO KUNDTOT-IO-AREA (4:1)
               MOVE KNRVGR                 TO KUNDTOT-IO-AREA (5:11)
               MOVE MND                    TO KUNDTOT-IO-AREA (16:2)
               MOVE SUMDPA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (18:5)
               MOVE SUMDPF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (23:5)
               MOVE SUMAKA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (28:5)
               MOVE SUMAKF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (33:5)
               MOVE SELDPA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (38:5)
               MOVE SELDPF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (43:5)
               MOVE SLKAKA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (48:5)
               MOVE SLKAKF                 TO XO-90P
               MOVE XO-90P-EF              TO KUNDTOT-IO-AREA (53:5)
               WRITE KUNDTOT-IO-AREA
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
           SET INNF-LEVEL-INIT             TO TRUE
           INITIALIZE INNF-DATA-FIELDS
           SET INNF-EOF-OFF                TO TRUE
           SET INNF-PROCESS                TO TRUE
           OPEN INPUT INNF
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           OPEN OUTPUT KUNDTOT.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNF
           CLOSE AUTOPAR
           CLOSE KUNDTOT.
 
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
