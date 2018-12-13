       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK225R.
      **********************************************  Z-WIN-RPG2   ****
      * KOPIERING OG SUMMERING AV RESKONTRORECORDS.          *
      * 11/12-91 FIRMA 648 (ARENDAL YRKESSKOLE) SKAL KUN     *
      *          HA FAKTURERING. RECORDS FJERNES.            *
      *  5/ 3/99 NY RUTINE FOR OMDØPING AV FIRMANUMMER.      *
      * 28/ 6/99 NY RUTINE FOR OMDØPING AV KUNDENR.          *
      * 20/10/99 BLANKER UT MOMSBEL. POS 51-59.              *
      * 06/02/03 FIRMA 581 (KOLBERG HICO) SKAL KUN HA FAKT.  *
      * 19/ 5/03 FJERNET RUTINE FOR OMDØPING AV KUNDENR.     *
      * 21/03/04 FIRMA 961 (LØNSETHAGEN ) SKAL KUN HA FAKT.  *
      ********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK225.rpg
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
           SELECT PERRES
               ASSIGN TO UT-S-PERRES
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PERRES-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT NYRES
               ASSIGN TO UT-S-NYRES
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYRES-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PERRES
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  PERRES-IO-AREA.
           05  PERRES-IO-AREA-X            PICTURE X(70).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD NYRES
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  NYRES-IO-AREA.
           05  NYRES-IO-AREA-X             PICTURE X(70).
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
           10  PERRES-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  NYRES-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PERRES-EOF-OFF          VALUE '0'.
               88  PERRES-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PERRES-READ-OFF         VALUE '0'.
               88  PERRES-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PERRES-PROCESS-OFF      VALUE '0'.
               88  PERRES-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PERRES-LEVEL-INIT-OFF   VALUE '0'.
               88  PERRES-LEVEL-INIT       VALUE '1'.
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
           05  PERRES-LEVEL-02.
               10  PERRES-02-L1.
                   15  PERRES-02-L1-FIRMA  PICTURE X(3).
           05  PERRES-DATA-FIELDS.
               10  RESREC                  PICTURE X(70).
               10  RESNR                   PICTURE X(6).
               10  RESNR1                  PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  SUMIN-IO.
                   15  SUMIN               PICTURE S9(9)V9(2).
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(5).
               10  REGFNR                  PICTURE X(3).
               10  SUMSL-IO.
                   15  SUMSL               PICTURE S9(9)V9(2).
               10  ANTSL-IO.
                   15  ANTSL               PICTURE S9(5).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(5).
               10  SUMUT-IO.
                   15  SUMUT               PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PERRES-PROCESS
               SET PERRES-PROCESS-OFF      TO TRUE
               SET PERRES-READ             TO TRUE
           END-IF
 
           IF  PERRES-READ
           AND RECORD-SELECTED-OFF
               PERFORM PERRES-GET
               SET PERRES-READ-OFF         TO TRUE
               IF  NOT PERRES-EOF
                   SET PERRES-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PERRES-PROCESS
               PERFORM PERRES-IDSET
           END-IF
 
           IF  PERRES-PROCESS
               PERFORM PERRES-CHK-LEVEL
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
 
           IF  PERRES-PROCESS
               PERFORM PERRES-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  PERRES-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-90                TO TRUE
               IF  KONFNR > '000'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           ADD BEL2                        TO SUMIN
           ADD 1                           TO ANTIN
      *****************************************************************
      * RUTINE FOR FIRMA HVOR RESKONRODATA SKAL FJERNES.              *
      *****************************************************************
           SET NOT-I-48                    TO TRUE
           IF  FIRMA = '581'
               SET I-48                    TO TRUE
           END-IF
           IF  (NOT-I-48)
               SET NOT-I-48                TO TRUE
               IF  FIRMA = '961'
                   SET I-48                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR ENDRING AV FIRMANR. I RESKONTRORECORD.             *
      * KONSERN FIRMANR. HENTES          15/2/2000                    *
      *****************************************************************
           END-IF
           MOVE FIRMA                      TO REGFNR
           IF  (I-90)
               MOVE KONFNR                 TO REGFNR
      *****************************************************************
      * RUTINE FOR ENDRING AV KUNDENR. I RESKONTRORECORD.             *
      * LAGT INN FOR: BILVAREHUSET NOR.  28/6/1999, FJERNET=19.05.03  *
      *****************************************************************
      *                    MOVE RESNR     KUNDNR  6         KUNDENR.
      *          FIRMA     COMP "694"                    31 BV ULLEVÅL
      * N31      FIRMA     COMP "695"                    31 BV RYEN
      * N31      FIRMA     COMP "696"                    31 BV VESTKANT.
      * N31      FIRMA     COMP "697"                    31 BV LAGUNEN
      * N31      FIRMA     COMP "698"                    31 BV ÅSANE
      * N31      FIRMA     COMP "699"                    31 BV TROMSØ
      *  31      RESNR1    COMP "1"                      32 KUNDENR.
      *  31      RESNR1    COMP "9"                      33 LEVNR.
      *  31 32             MOVEL"11"      KUNDNR            NYTT RESKNR.
      *  31 33             MOVEL"91"      KUNDNR            NYTT RESKNR.
      *****************************************************************
      * SUMMERINGS RUTINE                                             *
      *****************************************************************
           END-IF
           IF  (I-48)
               ADD BEL2                    TO SUMSL
               ADD 1                       TO ANTSL
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO ANTUT
           ADD BEL2                        TO SUMUT.
 
       SLUTT-T.
           CONTINUE.
 
       PERRES-GET SECTION.
       PERRES-GET-P.
           IF  PERRES-EOF-OFF
               READ PERRES
               AT END
                   SET PERRES-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PERRES-FLDSET SECTION.
       PERRES-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PERRES-IO-AREA (1:70)  TO RESREC (1:70)
               MOVE PERRES-IO-AREA (5:6)   TO RESNR (1:6)
               MOVE PERRES-IO-AREA (5:1)   TO RESNR1 (1:1)
               MOVE PERRES-IO-AREA (44:3)  TO FIRMA (1:3)
               MOVE PERRES-IO-AREA (35:9)  TO BEL2-IO
               INSPECT BEL2-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PERRES-IDSET SECTION.
       PERRES-IDSET-P.
           SET I-02                        TO TRUE.
 
       PERRES-CHK-LEVEL SECTION.
       PERRES-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PERRES-LEVEL-02
               MOVE PERRES-IO-AREA (44:3)  TO PERRES-02-L1-FIRMA
               IF  PERRES-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PERRES-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PERRES-02-L1          TO THE-PRIOR-L1
               SET PERRES-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
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
           IF  (I-02 AND NOT-I-48)
               MOVE SPACES TO NYRES-IO-AREA
               INITIALIZE NYRES-IO-AREA
               MOVE RESREC                 TO NYRES-IO-AREA (1:70)
      *                        KUNDNR    10
               MOVE REGFNR                 TO NYRES-IO-AREA (44:3)
               MOVE '         '            TO NYRES-IO-AREA (51:9)
               WRITE NYRES-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE ' FRA FAK225   '       TO LISTE-IO-AREA (25:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (38:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTIN                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE SUMIN                  TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (13:15)
               MOVE 'RECORDS OG RESKONTROBEL.' TO LISTE-IO-AREA (32:24)
               MOVE 'FRA FAKTURARUTINEN.'  TO LISTE-IO-AREA (57:19)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTSL                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE SUMSL                  TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (13:15)
               MOVE 'RECORDS OG RESKONTROBEL.' TO LISTE-IO-AREA (32:24)
               MOVE 'SOM SKAL FJERNES.  '  TO LISTE-IO-AREA (57:19)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTUT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE SUMUT                  TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (13:15)
               MOVE 'RECORDS OG RESKONTROBEL.' TO LISTE-IO-AREA (32:24)
               MOVE ' TIL RESKONTRORUTINEN.' TO LISTE-IO-AREA (55:22)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMES MED REGNSKAP  ' TO LISTE-IO-AREA (27:24)
               MOVE 'OG FAKTURATOTAL FAK085' TO LISTE-IO-AREA (55:22)
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
           SET PERRES-LEVEL-INIT           TO TRUE
           INITIALIZE PERRES-DATA-FIELDS
           SET PERRES-EOF-OFF              TO TRUE
           SET PERRES-PROCESS              TO TRUE
           OPEN INPUT PERRES
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT NYRES
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PERRES
           CLOSE FIRMAF
           CLOSE NYRES
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
