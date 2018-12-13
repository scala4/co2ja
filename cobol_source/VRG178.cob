       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG178R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET DANNER LAGERVERDIRECORD PR. VAREGRUPPE TIL OPPDATERING      *
      *   I FILEN LAGERBEHOLNING SISTE 12 MND. SOM BENYTTES FOR Å DANNE        *
      *   SVS OG LAGERBEHOLDNINGSFILE I SALG OG INNKJØPSSTATISTIKKEN.          *
      * DETTE PROGRAM HENTER UT LAGERVERDIEN FRA BEHOLNINGSFILE OG ERSTATTER   *
      *   TIDLIGERE PROGRAM STA110.                                            *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG178.rpg
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
           SELECT REGPAR
               ASSIGN TO UT-S-REGPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGPAR-STATUS.
           SELECT BEHFILE
               ASSIGN TO UT-S-BEHFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BEHFILE-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT FVERDI
               ASSIGN TO UT-S-FVERDI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FVERDI-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REGPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  REGPAR-IO-AREA.
           05  REGPAR-IO-AREA-X            PICTURE X(100).
       FD BEHFILE
               BLOCK CONTAINS 4100
               RECORD CONTAINS 164.
       01  BEHFILE-IO-AREA.
           05  BEHFILE-IO-AREA-X           PICTURE X(164).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD FVERDI
               BLOCK CONTAINS 9440
               RECORD CONTAINS 20.
       01  FVERDI-IO-AREA.
           05  FVERDI-IO-AREA-X            PICTURE X(20).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  ARB-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE S9(9).
           05  ARB-TABLE.
               10  ARB-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARB-I
                                                      ARB-S.
                   15  ARB                 PICTURE S9(9).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REGPAR-STATUS               PICTURE 99 VALUE 0.
           10  BEHFILE-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  FVERDI-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-EOF-OFF          VALUE '0'.
               88  REGPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-READ-OFF         VALUE '0'.
               88  REGPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-PROCESS-OFF      VALUE '0'.
               88  REGPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BEHFILE-EOF-OFF         VALUE '0'.
               88  BEHFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BEHFILE-READ-OFF        VALUE '0'.
               88  BEHFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BEHFILE-PROCESS-OFF     VALUE '0'.
               88  BEHFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BEHFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  BEHFILE-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  REGPAR-DATA-FIELDS.
               10  REGPER-IO.
                   15  REGPER              PICTURE S9(4).
               10  PARMND-IO.
                   15  PARMND              PICTURE S9(2).
           05  BEHFILE-LEVEL-03.
               10  BEHFILE-03-L1.
                   15  BEHFILE-03-L1-FIRMA PICTURE X(3).
           05  BEHFILE-DATA-FIELDS.
               10  RA                      PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  XI-ARA-GRP.
                   15  XI-ARA              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  XI-ARB-GRP.
                   15  XI-ARB              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
               10  FMNDNR-IO.
                   15  FMNDNR              PICTURE S9(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  LAGUTA-ELG-IO.
                   15  LAGUTA-ELG          PICTURE S9(9).
               10  TEST-X-IO.
                   15  TEST-X              PICTURE S9(2).
               10  REGMA-ELGN-IO.
                   15  REGMA-ELGN          PICTURE S9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  REGPAR-PROCESS
               SET REGPAR-PROCESS-OFF      TO TRUE
               SET REGPAR-READ             TO TRUE
           END-IF
 
           IF  REGPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM REGPAR-GET
               SET REGPAR-READ-OFF         TO TRUE
               IF  NOT REGPAR-EOF
                   SET REGPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  BEHFILE-PROCESS
               SET BEHFILE-PROCESS-OFF     TO TRUE
               SET BEHFILE-READ            TO TRUE
           END-IF
 
           IF  BEHFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM BEHFILE-GET
               SET BEHFILE-READ-OFF        TO TRUE
               IF  NOT BEHFILE-EOF
                   SET BEHFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-IDSET
           END-IF
 
           IF  BEHFILE-PROCESS
               PERFORM BEHFILE-IDSET
           END-IF
 
           IF  BEHFILE-PROCESS
               PERFORM BEHFILE-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-FLDSET
           END-IF
 
           IF  BEHFILE-PROCESS
               PERFORM BEHFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BEHFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-90                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-90            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
      *****************************************************************
      ******   FØRSTE REC.PÅ NYTT FIRMA BEREGNE REGNSKAPSMÅNED   ******
      *****************************************************************
      *
           END-IF
           IF  (I-L1)
               PERFORM REGMND-S
      *
      ****************************************************************
      ****  RUTINE FOR TESTING AV RECART OG BEREGNE LAGERVERDI  ******
      ****************************************************************
           END-IF
           IF  (I-03)
               SET NOT-I-36                TO TRUE
               IF  RA = 'LV'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-36)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               ADD ARB (X) TO ZERO     GIVING LAGUTA-ELG
      *
           END-IF
           .
 
       SLUTT-T.
      ****************************************************************
      ******  SUBRUTINE FOR OG BEREGNE REGNSKAPSMÅNED           ******
      ****************************************************************
           CONTINUE.
 
       REGMND-S SECTION.
       REGMND-S-P.
           SUBTRACT FMNDNR FROM PARMND GIVING TEST-X
           ADD 1                           TO TEST-X
           SET NOT-I-41                    TO TRUE
           IF  TEST-X NOT > 0
               SET I-41                    TO TRUE
           END-IF
           IF  (NOT-I-41)
               ADD TEST-X TO ZERO      GIVING REGMA-ELGN
           END-IF
           IF  (I-41)
               ADD 12 TO TEST-X        GIVING REGMA-ELGN
           END-IF
           ADD REGMA-ELGN TO ZERO      GIVING X.
      ****************************************************************
 
       REGPAR-GET SECTION.
       REGPAR-GET-P.
           IF  REGPAR-EOF-OFF
               READ REGPAR
               AT END
                   SET REGPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REGPAR-FLDSET SECTION.
       REGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE REGPAR-IO-AREA (3:4)   TO REGPER-IO
               INSPECT REGPER-IO REPLACING ALL ' ' BY '0'
               MOVE REGPAR-IO-AREA (9:2)   TO PARMND-IO
               INSPECT PARMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       REGPAR-IDSET SECTION.
       REGPAR-IDSET-P.
           SET I-02                        TO TRUE.
 
       BEHFILE-GET SECTION.
       BEHFILE-GET-P.
           IF  BEHFILE-EOF-OFF
               READ BEHFILE
               AT END
                   SET BEHFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BEHFILE-FLDSET SECTION.
       BEHFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BEHFILE-IO-AREA (1:2)  TO RA (1:2)
               MOVE BEHFILE-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE BEHFILE-IO-AREA (6:5)  TO VGR (1:5)
               MOVE 71                     TO BW-A
               PERFORM VARYING ARA-I FROM ARA-MAX BY -1
                         UNTIL ARA-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE BEHFILE-IO-AREA (BW-A:5) TO XI-ARA-GRP
                   MOVE XI-ARA             TO ARA (ARA-I)
               END-PERFORM
               MOVE 131                    TO BW-A
               PERFORM VARYING ARB-I FROM ARB-MAX BY -1
                         UNTIL ARB-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE BEHFILE-IO-AREA (BW-A:5) TO XI-ARB-GRP
                   MOVE XI-ARB             TO ARB (ARB-I)
               END-PERFORM
           END-EVALUATE.
 
       BEHFILE-IDSET SECTION.
       BEHFILE-IDSET-P.
           SET I-03                        TO TRUE.
 
       BEHFILE-CHK-LEVEL SECTION.
       BEHFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BEHFILE-LEVEL-03
               MOVE BEHFILE-IO-AREA (3:3)  TO BEHFILE-03-L1-FIRMA
               IF  BEHFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BEHFILE-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BEHFILE-03-L1         TO THE-PRIOR-L1
               SET BEHFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (134:2) TO FMNDNR-IO
               INSPECT FMNDNR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-36)
               MOVE SPACES TO FVERDI-IO-AREA
               INITIALIZE FVERDI-IO-AREA
               MOVE '9'                    TO FVERDI-IO-AREA (1:1)
               MOVE FIRMA                  TO FVERDI-IO-AREA (2:3)
               MOVE VGR                    TO FVERDI-IO-AREA (5:5)
               MOVE LAGUTA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO FVERDI-IO-AREA (10:5)
               WRITE FVERDI-IO-AREA
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
           INITIALIZE REGPAR-DATA-FIELDS
           SET REGPAR-EOF-OFF              TO TRUE
           SET REGPAR-PROCESS              TO TRUE
           OPEN INPUT REGPAR
           SET BEHFILE-LEVEL-INIT          TO TRUE
           INITIALIZE BEHFILE-DATA-FIELDS
           SET BEHFILE-EOF-OFF             TO TRUE
           SET BEHFILE-PROCESS             TO TRUE
           OPEN INPUT BEHFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT FVERDI.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           PERFORM VARYING ARB-I FROM 1 BY 1
                     UNTIL ARB-I > ARB-MAX
               INITIALIZE ARB (ARB-I)
           END-PERFORM
           SET ARB-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGPAR
           CLOSE BEHFILE
           CLOSE FIRMAF
           CLOSE FVERDI.
 
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
