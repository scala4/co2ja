       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK230R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM FAK230.                                               *
      * 31.08.2000 ØKT RECORDLENGDE FRA 80 TIL 100.                   *
      * 06/02/03 FIRMA 581 (KOLBERG HICO) SKAL KUN HA FAKT.           *
      * 18/03/03 BEREGNING AV SUM EKS. MVA SOM ER FJERNET.            *
      * 08/10/03 TREKKER OGSÅ I FRA ØREAVRUNDING.                     *
      * 21/03/04 FIRMA 961 (LØNSETHAGEN ÅLESUND) SKAL KUN HA FAKT.    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK230.rpg
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
           SELECT PERREG
               ASSIGN TO UT-S-PERREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PERREG-STATUS.
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
       FD PERREG
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  PERREG-IO-AREA.
           05  PERREG-IO-AREA-X            PICTURE X(100).
       FD NYREG
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  NYREG-IO-AREA.
           05  NYREG-IO-AREA-X             PICTURE X(100).
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
           10  PERREG-STATUS               PICTURE 99 VALUE 0.
           10  NYREG-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PERREG-EOF-OFF          VALUE '0'.
               88  PERREG-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PERREG-READ-OFF         VALUE '0'.
               88  PERREG-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PERREG-PROCESS-OFF      VALUE '0'.
               88  PERREG-PROCESS          VALUE '1'.
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
           05  PERREG-DATA-FIELDS.
               10  REC2                    PICTURE X(100).
               10  KTO                     PICTURE X(5).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2).
               10  FIRMA                   PICTURE X(3).
               10  S2                      PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  BELO-ELGP2-IO.
                   15  BELO-ELGP2          PICTURE S9(7)V9(2).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(9)V9(2).
               10  SLEBEL-IO.
                   15  SLEBEL              PICTURE S9(9)V9(2).
               10  SLEBEX-IO.
                   15  SLEBEX              PICTURE S9(9)V9(2).
               10  REGBEL-IO.
                   15  REGBEL              PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PERREG-PROCESS
               SET PERREG-PROCESS-OFF      TO TRUE
               SET PERREG-READ             TO TRUE
           END-IF
 
           IF  PERREG-READ
           AND RECORD-SELECTED-OFF
               PERFORM PERREG-GET
               SET PERREG-READ-OFF         TO TRUE
               IF  NOT PERREG-EOF
                   SET PERREG-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PERREG-PROCESS
               PERFORM PERREG-IDSET
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
 
           IF  PERREG-PROCESS
               PERFORM PERREG-FLDSET
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
           SET NOT-I-50                    TO TRUE
      *****************************************************************
      * FIRMA SOM IKKE KJØRER REGNSKAPSSYSTEMET.                      *
      * FIRMA 581 OG 961 ER IKKE LENGERE FULLBRUKER KUNDER.           *
      *****************************************************************
           SET NOT-I-48                    TO TRUE
           IF  FIRMA = '581'
               SET I-48                    TO TRUE
           END-IF
           IF  (I-48)
               SET I-50                    TO TRUE
           END-IF
           SET NOT-I-49                    TO TRUE
           IF  FIRMA = '961'
               SET I-49                    TO TRUE
           END-IF
           IF  (I-49)
               SET I-50                    TO TRUE
      *****************************************************************
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  KTO = '2201 '
               SET I-22                    TO TRUE
           END-IF
           IF  (I-48 AND NOT-I-22)
               SET NOT-I-22                TO TRUE
               IF  KTO = '6040 '
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-49 AND NOT-I-22)
               SET NOT-I-22                TO TRUE
               IF  KTO = '6020 '
                   SET I-22                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  S2 = '-'
               SET I-15                    TO TRUE
           END-IF
           IF  (I-15)
               MULTIPLY -1 BY BEL2     GIVING BELO-ELGP2
           END-IF
           IF  (NOT-I-15)
               MULTIPLY +1 BY BEL2     GIVING BELO-ELGP2
           END-IF
           ADD BELO-ELGP2                  TO FAKBEL
           IF  (I-50)
               ADD BELO-ELGP2              TO SLEBEL
           END-IF
           IF  (I-48 AND NOT-I-22)
               ADD BELO-ELGP2              TO SLEBEX
           END-IF
           IF  (I-49 AND NOT-I-22)
               ADD BELO-ELGP2              TO SLEBEX
           END-IF
           IF  (NOT-I-50)
               ADD BELO-ELGP2              TO REGBEL
           END-IF.
 
       PERREG-GET SECTION.
       PERREG-GET-P.
           IF  PERREG-EOF-OFF
               READ PERREG
               AT END
                   SET PERREG-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PERREG-FLDSET SECTION.
       PERREG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PERREG-IO-AREA (1:100) TO REC2 (1:100)
               MOVE PERREG-IO-AREA (32:5)  TO KTO (1:5)
               MOVE PERREG-IO-AREA (37:9)  TO BEL2-IO
               INSPECT BEL2-IO REPLACING ALL ' ' BY '0'
               MOVE PERREG-IO-AREA (22:3)  TO FIRMA (1:3)
               MOVE PERREG-IO-AREA (46:1)  TO S2 (1:1)
           END-EVALUATE.
 
       PERREG-IDSET SECTION.
       PERREG-IDSET-P.
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
           IF  (I-02 AND NOT-I-50)
               MOVE SPACES TO NYREG-IO-AREA
               INITIALIZE NYREG-IO-AREA
               MOVE REC2                   TO NYREG-IO-AREA (1:100)
               WRITE NYREG-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE ' FRA FAK230   '       TO LISTE-IO-AREA (25:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (38:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FAKBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (6:15)
               MOVE ' REGNSKAPSBELØP FRA FAK0' TO LISTE-IO-AREA (22:24)
               MOVE '85                      ' TO LISTE-IO-AREA (46:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE SLEBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (6:15)
               MOVE ' REGNSKAPSBELØP SOM ER F' TO LISTE-IO-AREA (22:24)
               MOVE 'JERNET.                 ' TO LISTE-IO-AREA (46:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE SLEBEX                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (6:15)
               MOVE ' REG.BELØP EKSL. MVA OG ' TO LISTE-IO-AREA (22:24)
               MOVE 'ØREAVR. SOM ER FJERNET. ' TO LISTE-IO-AREA (46:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REGBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (6:15)
               MOVE ' REGNSKAPSBELØP TIL REGN' TO LISTE-IO-AREA (22:24)
               MOVE 'SKAPSRUTINEN.           ' TO LISTE-IO-AREA (46:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' AVSTEMMES MED RESKONTRO' TO LISTE-IO-AREA (22:24)
               MOVE ' OG FAKTURATOTAL FAK085 ' TO LISTE-IO-AREA (46:24)
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
           INITIALIZE PERREG-DATA-FIELDS
           SET PERREG-EOF-OFF              TO TRUE
           SET PERREG-PROCESS              TO TRUE
           OPEN INPUT PERREG
           OPEN OUTPUT NYREG
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PERREG
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
