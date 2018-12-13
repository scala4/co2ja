       IDENTIFICATION DIVISION.
       PROGRAM-ID. STY006R.
      ******************************************* :   Z-WIN-RPG2   ****
      *   PROGRAM STY006                                                       *
      *   PROGRAMMET FJERNER RECORDS FRA SALG INNEVÆRENDE MÅNED, VED           *
      *   OVERGANG TIL NY FAKTURA-MÅNED.  (SISTE MÅNED FERDIG FAKTURERT)       *
      *   DENNE FILE BRUKES FOR FELT 9 I LAGERSTYRINGEN                        *
      *                                                                        *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STY006.rpg
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
           SELECT LAGPAR
               ASSIGN TO UT-S-LAGPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LAGPAR-STATUS.
           SELECT SFILE
               ASSIGN TO UT-S-SFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SFILE-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT NYFILE
               ASSIGN TO UT-S-NYFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD LAGPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  LAGPAR-IO-AREA.
           05  LAGPAR-IO-AREA-X            PICTURE X(200).
       FD SFILE
               BLOCK CONTAINS 1960
               RECORD CONTAINS 20.
       01  SFILE-IO-AREA.
           05  SFILE-IO-AREA-X             PICTURE X(20).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD NYFILE
               BLOCK CONTAINS 1960
               RECORD CONTAINS 20.
       01  NYFILE-IO-AREA.
           05  NYFILE-IO-AREA-X            PICTURE X(20).
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
           10  LAGPAR-STATUS               PICTURE 99 VALUE 0.
           10  SFILE-STATUS                PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  NYFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-EOF-OFF          VALUE '0'.
               88  LAGPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-READ-OFF         VALUE '0'.
               88  LAGPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-PROCESS-OFF      VALUE '0'.
               88  LAGPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SFILE-EOF-OFF           VALUE '0'.
               88  SFILE-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SFILE-READ-OFF          VALUE '0'.
               88  SFILE-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SFILE-PROCESS-OFF       VALUE '0'.
               88  SFILE-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SFILE-LEVEL-INIT-OFF    VALUE '0'.
               88  SFILE-LEVEL-INIT        VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
      *DSDS: DATA STRUCTURE FIELDS
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  LAGPAR-DATA-FIELDS.
               10  PMND                    PICTURE X(2).
               10  PA-ELGR                 PICTURE X(2).
               10  GRUPPE                  PICTURE X(1).
           05  SFILE-LEVEL-02.
               10  SFILE-02-L1.
                   15  SFILE-02-L1-FIRMA   PICTURE X(3).
           05  SFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SDATO                   PICTURE X(4).
               10  REC                     PICTURE X(20).
           05  FIRMAF-DATA-FIELDS.
               10  FGRUPP                  PICTURE X(1).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  RDATO                   PICTURE X(4).
               10  SDATO8                  PICTURE X(8).
               10  RDATO8                  PICTURE X(8).
               10  TELL1-IO.
                   15  TELL1               PICTURE S9(5).
               10  TELL2-IO.
                   15  TELL2               PICTURE S9(5).
               10  TELL3-IO.
                   15  TELL3               PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YNZ                PICTURE ZZZZZ.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  LAGPAR-PROCESS
               SET LAGPAR-PROCESS-OFF      TO TRUE
               SET LAGPAR-READ             TO TRUE
           END-IF
 
           IF  LAGPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM LAGPAR-GET
               SET LAGPAR-READ-OFF         TO TRUE
               IF  NOT LAGPAR-EOF
                   SET LAGPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  SFILE-PROCESS
               SET SFILE-PROCESS-OFF       TO TRUE
               SET SFILE-READ              TO TRUE
           END-IF
 
           IF  SFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM SFILE-GET
               SET SFILE-READ-OFF          TO TRUE
               IF  NOT SFILE-EOF
                   PERFORM SFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET SFILE-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  LAGPAR-PROCESS
               PERFORM LAGPAR-IDSET
           END-IF
 
           IF  SFILE-PROCESS
               PERFORM SFILE-IDSET
           END-IF
 
           IF  SFILE-PROCESS
               PERFORM SFILE-CHK-LEVEL
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
 
           IF  LAGPAR-PROCESS
               PERFORM LAGPAR-FLDSET
           END-IF
 
           IF  SFILE-PROCESS
               PERFORM SFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               MOVE PA-ELGR                TO RDATO (1:2)
               MOVE PMND                   TO RDATO (3:2)
               SET NOT-I-15                TO TRUE
               IF  GRUPPE = '*'
                   SET I-15                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-35                TO TRUE
           END-IF
           IF  (I-L1 AND I-15)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-30                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-30            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-15 AND NOT-I-30)
               SET NOT-I-35                TO TRUE
               IF  FGRUPP = '1'
                   SET I-35                TO TRUE
               END-IF
      *
           END-IF
           SET NOT-I-20                    TO TRUE
      ***** RUTINE FOR Å SNU DATO TIL ÅR MND DAG OG 4 SIFFERET ÅR
      *
           MOVE 'B'                        TO DATOK
           MOVE SDATO                      TO DATO6 (3:4)
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO SDATO8
      *
           MOVE 'B'                        TO DATOK
           MOVE RDATO                      TO DATO6 (3:4)
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO RDATO8
           IF  (I-02 AND NOT-I-15)
               OR  (I-02 AND I-15 AND I-35)
               SET NOT-I-20                TO TRUE
               IF  SDATO8 NOT > RDATO8
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               ADD 1                       TO TELL1
           END-IF
           IF  (I-02 AND I-20)
               ADD 1                       TO TELL2
           END-IF
           IF  (I-02 AND NOT-I-20)
               ADD 1                       TO TELL3
           END-IF.
 
       LAGPAR-GET SECTION.
       LAGPAR-GET-P.
           IF  LAGPAR-EOF-OFF
               READ LAGPAR
               AT END
                   SET LAGPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LAGPAR-FLDSET SECTION.
       LAGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LAGPAR-IO-AREA (5:2)   TO PMND (1:2)
               MOVE LAGPAR-IO-AREA (7:2)   TO PA-ELGR (1:2)
               MOVE LAGPAR-IO-AREA (9:1)   TO GRUPPE (1:1)
           END-EVALUATE.
 
       LAGPAR-IDSET SECTION.
       LAGPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       SFILE-GET SECTION.
       SFILE-GET-P.
           IF  SFILE-EOF-OFF
               READ SFILE
               AT END
                   SET SFILE-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SFILE-FLDSET SECTION.
       SFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = 'D' )
               MOVE SFILE-IO-AREA (18:3)   TO FIRMA (1:3)
               MOVE SFILE-IO-AREA (2:7)    TO EDBNR (1:7)
               MOVE SFILE-IO-AREA (9:5)    TO ANT-IO
               MOVE SFILE-IO-AREA (14:4)   TO SDATO (1:4)
               MOVE SFILE-IO-AREA (1:20)   TO REC (1:20)
           END-EVALUATE.
 
       SFILE-IDCHK SECTION.
       SFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = 'D' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       SFILE-IDSET SECTION.
       SFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = 'D' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       SFILE-CHK-LEVEL SECTION.
       SFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = 'D' )
               MOVE LOW-VALUES             TO SFILE-LEVEL-02
               MOVE SFILE-IO-AREA (18:3)   TO SFILE-02-L1-FIRMA
               IF  SFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SFILE-02-L1           TO THE-PRIOR-L1
               SET SFILE-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (503:1) TO FGRUPP (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-20)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               MOVE REC                    TO NYFILE-IO-AREA (1:20)
               WRITE NYFILE-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL ANTALL REC.'    TO LISTE-IO-AREA (10:17)
               MOVE TELL1                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (28:5)
               MOVE 'ANTALL RECORD FJERNET' TO LISTE-IO-AREA (35:21)
               MOVE TELL2                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (57:5)
               MOVE 'ANTALL RECORD TIL NYFILE' TO LISTE-IO-AREA (64:24)
               MOVE TELL3                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (89:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 01                     TO LISTE-AFTER-SKIP
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
           INITIALIZE LAGPAR-DATA-FIELDS
           SET LAGPAR-EOF-OFF              TO TRUE
           SET LAGPAR-PROCESS              TO TRUE
           OPEN INPUT LAGPAR
           SET SFILE-LEVEL-INIT            TO TRUE
           INITIALIZE SFILE-DATA-FIELDS
           SET SFILE-EOF-OFF               TO TRUE
           SET SFILE-PROCESS               TO TRUE
           OPEN INPUT SFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT NYFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE LAGPAR
           CLOSE SFILE
           CLOSE FIRMAF
           CLOSE NYFILE
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
