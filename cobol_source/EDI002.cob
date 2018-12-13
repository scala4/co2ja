       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDI002R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: EDI002.rpg
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
           SELECT EDIREC
               ASSIGN TO UT-S-EDIREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDIREC-STATUS.
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD EDIREC
               BLOCK CONTAINS 8856
               RECORD CONTAINS 164.
       01  EDIREC-IO-AREA.
           05  EDIREC-IO-AREA-X            PICTURE X(164).
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
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
           10  EDIREC-STATUS               PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  EDIREC-EOF-OFF          VALUE '0'.
               88  EDIREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  EDIREC-READ-OFF         VALUE '0'.
               88  EDIREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  EDIREC-PROCESS-OFF      VALUE '0'.
               88  EDIREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  EDIREC-LEVEL-INIT-OFF   VALUE '0'.
               88  EDIREC-LEVEL-INIT       VALUE '1'.
           05  ORDREM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  EDIREC-LEVEL-01.
               10  EDIREC-01-L1.
                   15  EDIREC-01-L1-FIRMNR PICTURE X(3).
           05  EDIREC-DATA-FIELDS.
               10  KEY-X                   PICTURE X(20).
               10  KEY1                    PICTURE X(10).
               10  FIRMNR                  PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  ORDPRI-IO.
                   15  ORDPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REGPRI-IO.
                   15  REGPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KOSPRI-IO.
                   15  KOSPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REC                     PICTURE X(164).
           05  ORDREM-DATA-FIELDS.
               10  KUNNR                   PICTURE X(6).
               10  STATUS-X                PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  KUNDE                   PICTURE X(6).
               10  KEY19                   PICTURE X(19).
               10  KEY9                    PICTURE X(9).
               10  HEDKEY                  PICTURE X(20).
               10  ANTFRA-IO.
                   15  ANTFRA              PICTURE S9(5).
               10  SUMFRA-IO.
                   15  SUMFRA              PICTURE S9(7)V9(2).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(5).
               10  SUMFAK-IO.
                   15  SUMFAK              PICTURE S9(7)V9(2).
               10  ANTFRT-IO.
                   15  ANTFRT              PICTURE S9(5).
               10  SUMFRT-IO.
                   15  SUMFRT              PICTURE S9(7)V9(2).
               10  ANTFAT-IO.
                   15  ANTFAT              PICTURE S9(5).
               10  SUMFAT-IO.
                   15  SUMFAT              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  EDIREC-PROCESS
               SET EDIREC-PROCESS-OFF      TO TRUE
               SET EDIREC-READ             TO TRUE
           END-IF
 
           IF  EDIREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM EDIREC-GET
               SET EDIREC-READ-OFF         TO TRUE
               IF  NOT EDIREC-EOF
                   SET EDIREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  EDIREC-PROCESS
               PERFORM EDIREC-IDSET
           END-IF
 
           IF  EDIREC-PROCESS
               PERFORM EDIREC-CHK-LEVEL
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
 
           IF  EDIREC-PROCESS
               PERFORM EDIREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  EDIREC-PROCESS
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
               SET NOT-I-15                TO TRUE
               MOVE '      '               TO KUNDE
               MOVE KEY1                   TO KEY19 (1:10)
               MOVE '      '               TO KEY9 (1:6)
               MOVE '   '                  TO KEY9 (7:3)
               MOVE KEY9                   TO KEY19 (11:9)
               MOVE KEY19                  TO HEDKEY (1:19)
               MOVE '1'                    TO HEDKEY (20:1)
               MOVE HEDKEY                 TO ORDREM-KEY1
               READ ORDREM RECORD KEY IS ORDREM-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM ORDREM-FLDSET
                   PERFORM ORDREM-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-11)
               SET I-15                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-11)
               SET NOT-I-15                TO TRUE
               IF  STATUS-X = 'C'
                   SET I-15                TO TRUE
               END-IF
               MOVE KUNNR                  TO KUNDE
           END-IF
           IF  (I-01)
               MOVE KEY-X                  TO ORDREM-KEY1
               READ ORDREM RECORD KEY IS ORDREM-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM ORDREM-FLDSET
                   PERFORM ORDREM-IDSET
               END-READ
      *****************************************************************
      * SUMMERING AV FIRMATOTALER.                                    *
      *****************************************************************
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTFRA
               ADD ORDPRI                  TO SUMFRA
           END-IF
           IF  (I-01 AND I-15)
               ADD 1                       TO ANTFAK
               ADD ORDPRI                  TO SUMFAK
      *****************************************************************
      * SUMMERING AV GRANDTOTALER.                                    *
      *****************************************************************
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTFRT
               ADD ORDPRI                  TO SUMFRT
           END-IF
           IF  (I-01 AND I-15)
               ADD 1                       TO ANTFAT
               ADD ORDPRI                  TO SUMFAT
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-12                TO TRUE
               IF  ORDPRI = 0
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-12)
               SET NOT-I-12                TO TRUE
               IF  KOSPRI = 0
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-12)
               SET I-11                    TO TRUE
           END-IF.
 
       EDIREC-GET SECTION.
       EDIREC-GET-P.
           IF  EDIREC-EOF-OFF
               READ EDIREC
               AT END
                   SET EDIREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       EDIREC-FLDSET SECTION.
       EDIREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE EDIREC-IO-AREA (1:20)  TO KEY-X (1:20)
               MOVE EDIREC-IO-AREA (1:10)  TO KEY1 (1:10)
               MOVE EDIREC-IO-AREA (2:3)   TO FIRMNR (1:3)
               MOVE EDIREC-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE EDIREC-IO-AREA (94:5)  TO ORDPRI-IO
               MOVE EDIREC-IO-AREA (105:5) TO REGPRI-IO
               MOVE EDIREC-IO-AREA (121:5) TO KOSPRI-IO
               MOVE EDIREC-IO-AREA (1:164) TO REC (1:164)
           END-EVALUATE.
 
       EDIREC-IDSET SECTION.
       EDIREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       EDIREC-CHK-LEVEL SECTION.
       EDIREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO EDIREC-LEVEL-01
               MOVE EDIREC-IO-AREA (2:3)   TO EDIREC-01-L1-FIRMNR
               IF  EDIREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  EDIREC-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  EDIREC-01-L1          TO THE-PRIOR-L1
               SET EDIREC-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDREM-IO-AREA (21:6)  TO KUNNR (1:6)
               MOVE ORDREM-IO-AREA (164:1) TO STATUS-X (1:1)
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
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
           IF  (I-01 AND I-10 AND NOT-I-11)
               MOVE REC                    TO ORDREM-IO-AREA (1:164)
               WRITE ORDREM-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad WRITE - file = ORDREM'
               END-WRITE
           END-IF
           IF  (I-01 AND NOT-I-10 AND NOT-I-11)
               MOVE ORDPRI                 TO XO-72P
               MOVE XO-72P-EF              TO ORDREM-IO-AREA (94:5)
               MOVE REGPRI                 TO XO-72P
               MOVE XO-72P-EF              TO ORDREM-IO-AREA (105:5)
               REWRITE ORDREM-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = ORDREM'
               END-REWRITE
           END-IF
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV FRAKT FOR' TO LISTE-IO-AREA (1:24)
               MOVE 'FIRMA:'               TO LISTE-IO-AREA (26:6)
               MOVE FIRMNR                 TO LISTE-IO-AREA (33:3)
               MOVE 'KJØREDATO ER'         TO LISTE-IO-AREA (41:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (54:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-15)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDRENR.'             TO LISTE-IO-AREA (1:8)
               MOVE ORDNR                  TO LISTE-IO-AREA (9:6)
               MOVE 'MED FRAKTBEL.'        TO LISTE-IO-AREA (16:13)
               MOVE ORDPRI                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (28:13)
               MOVE 'ER ALLEREDE FAKTURERT.' TO LISTE-IO-AREA (42:22)
               IF  (NOT-I-11)
                   MOVE 'KUNDENR.'         TO LISTE-IO-AREA (65:8)
               END-IF
               IF  (NOT-I-11)
                   MOVE KUNDE              TO LISTE-IO-AREA (73:6)
               END-IF
               IF  (NOT-I-10)
                   MOVE 'REG. FRAKT FRA FØR' TO LISTE-IO-AREA (80:18)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV FRAKT FOR' TO LISTE-IO-AREA (1:24)
               MOVE 'FIRMA:'               TO LISTE-IO-AREA (26:6)
               MOVE FIRMNR                 TO LISTE-IO-AREA (33:3)
               MOVE 'KJØREDATO ER'         TO LISTE-IO-AREA (41:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (54:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFRA                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               INITIALIZE ANTFRA
               MOVE 'FRAKTER MED SUMBELØP' TO LISTE-IO-AREA (8:20)
               MOVE SUMFRA                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (28:13)
               INITIALIZE SUMFRA
               MOVE 'ER BEHANDLET         ' TO LISTE-IO-AREA (42:21)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFAK                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               INITIALIZE ANTFAK
               MOVE 'FRAKTER MED SUMBELØP' TO LISTE-IO-AREA (8:20)
               MOVE SUMFAK                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (28:13)
               INITIALIZE SUMFAK
               MOVE 'ER ALLEREDE FAKTURERT' TO LISTE-IO-AREA (42:21)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV FRAKTER  ' TO LISTE-IO-AREA (1:24)
               MOVE 'GRANDTOTALER.'        TO LISTE-IO-AREA (26:13)
               MOVE 'KJØREDATO ER'         TO LISTE-IO-AREA (41:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (54:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFRT                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               MOVE 'FRAKTER MED SUMBELØP' TO LISTE-IO-AREA (8:20)
               MOVE SUMFRT                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (28:13)
               MOVE 'ER BEHANDLET         ' TO LISTE-IO-AREA (42:21)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFAT                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (1:6)
               MOVE 'FRAKTER MED SUMBELØP' TO LISTE-IO-AREA (8:20)
               MOVE SUMFAT                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (28:13)
               MOVE 'ER ALLEREDE FAKTURERT' TO LISTE-IO-AREA (42:21)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           SET EDIREC-LEVEL-INIT           TO TRUE
           INITIALIZE EDIREC-DATA-FIELDS
           SET EDIREC-EOF-OFF              TO TRUE
           SET EDIREC-PROCESS              TO TRUE
           OPEN INPUT EDIREC
           INITIALIZE ORDREM-DATA-FIELDS
           OPEN I-O ORDREM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE EDIREC
           CLOSE ORDREM
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
