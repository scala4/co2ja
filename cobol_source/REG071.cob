       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG071R.
      **********************************************  Z-WIN-RPG2   ****
      *PROGRAM REG071 AV ESPEN LARSEN  22.01.2003                     *
      *MERGE INN NYE MARKEDS DATA I MARKEDS.UTGIFTER.MASTER.          *
      *                                                               *
      * UPSI 1 : SKRIVER TOTALER PÅ PRINTER.                          *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG071.rpg
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
           SELECT GMLREC
               ASSIGN TO UT-S-GMLREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLREC-STATUS.
           SELECT NYEREC
               ASSIGN TO UT-S-NYEREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYEREC-STATUS.
           SELECT MUFILE
               ASSIGN TO UT-S-MUFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MUFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GMLREC
               BLOCK CONTAINS 100
               RECORD CONTAINS 50.
       01  GMLREC-IO-AREA.
           05  GMLREC-IO-AREA-X            PICTURE X(50).
       FD NYEREC
               BLOCK CONTAINS 100
               RECORD CONTAINS 50.
       01  NYEREC-IO-AREA.
           05  NYEREC-IO-AREA-X            PICTURE X(50).
       FD MUFILE
               BLOCK CONTAINS 100
               RECORD CONTAINS 50.
       01  MUFILE-IO-AREA.
           05  MUFILE-IO-AREA-X            PICTURE X(50).
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
           10  GMLREC-STATUS               PICTURE 99 VALUE 0.
           10  NYEREC-STATUS               PICTURE 99 VALUE 0.
           10  MUFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREC-EOF-OFF          VALUE '0'.
               88  GMLREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREC-READ-OFF         VALUE '0'.
               88  GMLREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREC-PROCESS-OFF      VALUE '0'.
               88  GMLREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEREC-EOF-OFF          VALUE '0'.
               88  NYEREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEREC-READ-OFF         VALUE '0'.
               88  NYEREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEREC-PROCESS-OFF      VALUE '0'.
               88  NYEREC-PROCESS          VALUE '1'.
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
           05  GMLREC-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  PERAAR                  PICTURE X(2).
               10  BILNR                   PICTURE X(6).
               10  RECGML                  PICTURE X(50).
           05  GMLREC-MP                   PICTURE X(11).
           05  GMLREC-MC                   PICTURE X(11).
           05  GMLREC-M-01             REDEFINES GMLREC-MC.
               10  GMLREC-M-01-M3.
                   15  GMLREC-M-01-M3-FIRMA-G.
                       20  GMLREC-M-01-M3-FIRMA PICTURE X(3).
               10  GMLREC-M-01-M2.
                   15  GMLREC-M-01-M2-PERAAR-G.
                       20  GMLREC-M-01-M2-PERAAR PICTURE X(2).
               10  GMLREC-M-01-M1.
                   15  GMLREC-M-01-M1-BILNR-G.
                       20  GMLREC-M-01-M1-BILNR PICTURE X(6).
           05  NYEREC-DATA-FIELDS.
               10  RECNYE                  PICTURE X(50).
           05  NYEREC-MP                   PICTURE X(11).
           05  NYEREC-MC                   PICTURE X(11).
           05  NYEREC-M-02             REDEFINES NYEREC-MC.
               10  NYEREC-M-02-M3.
                   15  NYEREC-M-02-M3-FIRMA-G.
                       20  NYEREC-M-02-M3-FIRMA PICTURE X(3).
               10  NYEREC-M-02-M2.
                   15  NYEREC-M-02-M2-PERAAR-G.
                       20  NYEREC-M-02-M2-PERAAR PICTURE X(2).
               10  NYEREC-M-02-M1.
                   15  NYEREC-M-02-M1-BILNR-G.
                       20  NYEREC-M-02-M1-BILNR PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTGML-IO.
                   15  ANTGML              PICTURE S9(5).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(5).
               10  ANTSLE-IO.
                   15  ANTSLE              PICTURE S9(5).
               10  ANTFIL-IO.
                   15  ANTFIL              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  GMLREC-PROCESS
               SET GMLREC-PROCESS-OFF      TO TRUE
               SET GMLREC-READ             TO TRUE
           END-IF
 
           IF  GMLREC-READ
               PERFORM GMLREC-GET
               SET GMLREC-READ-OFF         TO TRUE
               IF  NOT GMLREC-EOF
                   PERFORM GMLREC-MATCH-SET
               END-IF
           END-IF
 
           IF  NYEREC-PROCESS
               SET NYEREC-PROCESS-OFF      TO TRUE
               SET NYEREC-READ             TO TRUE
           END-IF
 
           IF  NYEREC-READ
               PERFORM NYEREC-GET
               SET NYEREC-READ-OFF         TO TRUE
               IF  NOT NYEREC-EOF
                   PERFORM NYEREC-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  GMLREC-PROCESS
               PERFORM GMLREC-IDSET
           END-IF
 
           IF  NYEREC-PROCESS
               PERFORM NYEREC-IDSET
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
 
           IF  GMLREC-PROCESS
               PERFORM GMLREC-FLDSET
           END-IF
 
           IF  NYEREC-PROCESS
               PERFORM NYEREC-FLDSET
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
           IF  (I-01)
               ADD 1                       TO ANTGML
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTNYE
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTSLE
           END-IF
           IF  (I-01 AND NOT-I-MR)
               ADD 1                       TO ANTFIL
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTFIL
           END-IF.
 
       GMLREC-GET SECTION.
       GMLREC-GET-P.
           IF  GMLREC-EOF-OFF
               READ GMLREC
               AT END
                   SET GMLREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLREC-FLDSET SECTION.
       GMLREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLREC-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE GMLREC-IO-AREA (4:2)   TO PERAAR (1:2)
               MOVE GMLREC-IO-AREA (16:6)  TO BILNR (1:6)
               MOVE GMLREC-IO-AREA (1:50)  TO RECGML (1:50)
           END-EVALUATE.
 
       GMLREC-IDSET SECTION.
       GMLREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLREC-MATCH-SET SECTION.
       GMLREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLREC-IO-AREA (1:3)   TO GMLREC-M-01-M3-FIRMA
               MOVE GMLREC-IO-AREA (4:2)   TO GMLREC-M-01-M2-PERAAR
               MOVE GMLREC-IO-AREA (16:6)  TO GMLREC-M-01-M1-BILNR
           END-EVALUATE.
 
       NYEREC-GET SECTION.
       NYEREC-GET-P.
           IF  NYEREC-EOF-OFF
               READ NYEREC
               AT END
                   SET NYEREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYEREC-FLDSET SECTION.
       NYEREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEREC-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE NYEREC-IO-AREA (4:2)   TO PERAAR (1:2)
               MOVE NYEREC-IO-AREA (16:6)  TO BILNR (1:6)
               MOVE NYEREC-IO-AREA (1:50)  TO RECNYE (1:50)
           END-EVALUATE.
 
       NYEREC-IDSET SECTION.
       NYEREC-IDSET-P.
           SET I-02                        TO TRUE.
 
       NYEREC-MATCH-SET SECTION.
       NYEREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEREC-IO-AREA (1:3)   TO NYEREC-M-02-M3-FIRMA
               MOVE NYEREC-IO-AREA (4:2)   TO NYEREC-M-02-M2-PERAAR
               MOVE NYEREC-IO-AREA (16:6)  TO NYEREC-M-02-M1-BILNR
           END-EVALUATE.
 
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
           IF  GMLREC-EOF
               MOVE HIGH-VALUES            TO GMLREC-MC
                                              GMLREC-MP
           END-IF
           IF  NYEREC-EOF
               MOVE HIGH-VALUES            TO NYEREC-MC
                                              NYEREC-MP
           END-IF
           IF  GMLREC-MC < GMLREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  NYEREC-MC < NYEREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLREC-MC < NYEREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLREC-PROCESS      TO TRUE
                   MOVE GMLREC-MC          TO GMLREC-MP
                   IF  GMLREC-MC = NYEREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  NYEREC-MC < GMLREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET NYEREC-PROCESS      TO TRUE
                   MOVE NYEREC-MC          TO NYEREC-MP
                   IF  NYEREC-MC = GMLREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLREC-MC = NYEREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLREC-PROCESS      TO TRUE
                   MOVE GMLREC-MC          TO GMLREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-MR)
               MOVE SPACES TO MUFILE-IO-AREA
               INITIALIZE MUFILE-IO-AREA
               MOVE RECGML                 TO MUFILE-IO-AREA (1:50)
               WRITE MUFILE-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO MUFILE-IO-AREA
               INITIALIZE MUFILE-IO-AREA
               MOVE RECNYE                 TO MUFILE-IO-AREA (1:50)
               WRITE MUFILE-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. REG071 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC FRA FØR   '   TO LISTE-IO-AREA (3:18)
               MOVE ANTGML                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (30:6)
               MOVE 'DATO'                 TO LISTE-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT NYE REC NÅ    '   TO LISTE-IO-AREA (3:18)
               MOVE ANTNYE                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (30:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC FJERNET   '   TO LISTE-IO-AREA (3:18)
               MOVE ANTSLE                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (30:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC PÅ NY FILE'   TO LISTE-IO-AREA (3:18)
               MOVE ANTFIL                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (30:6)
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE GMLREC-DATA-FIELDS
           SET GMLREC-EOF-OFF              TO TRUE
           SET GMLREC-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO GMLREC-MC
                                              GMLREC-MP
           OPEN INPUT GMLREC
           INITIALIZE NYEREC-DATA-FIELDS
           SET NYEREC-EOF-OFF              TO TRUE
           SET NYEREC-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO NYEREC-MC
                                              NYEREC-MP
           OPEN INPUT NYEREC
           OPEN OUTPUT MUFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLREC
           CLOSE NYEREC
           CLOSE MUFILE
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
