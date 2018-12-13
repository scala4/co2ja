       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA913R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA913.rpg
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
           SELECT SALGF
               ASSIGN TO UT-S-SALGF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SALGF-STATUS.
           SELECT STATTAB
               ASSIGN TO STATTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS STATTAB-STATUS
               RECORD KEY IS STATTAB-KEY1.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SALGF
               BLOCK CONTAINS 4070
               RECORD CONTAINS 110.
       01  SALGF-IO-AREA.
           05  SALGF-IO-AREA-X             PICTURE X(110).
       FD STATTAB
               RECORD CONTAINS 40.
       01  STATTAB-IO-AREA.
           05  STATTAB-IO-AREA-X.
               10  STATTAB-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(32).
       FD OUTFIL
               BLOCK CONTAINS 4030
               RECORD CONTAINS 130.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(130).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  SALGF-STATUS                PICTURE 99 VALUE 0.
           10  STATTAB-STATUS              PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  SALGF-EOF-OFF           VALUE '0'.
               88  SALGF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SALGF-READ-OFF          VALUE '0'.
               88  SALGF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SALGF-PROCESS-OFF       VALUE '0'.
               88  SALGF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SALGF-LEVEL-INIT-OFF    VALUE '0'.
               88  SALGF-LEVEL-INIT        VALUE '1'.
           05  STATTAB-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SALGF-LEVEL-01.
               10  SALGF-01-L1.
                   15  SALGF-01-L1-HND     PICTURE X(3).
           05  SALGF-DATA-FIELDS.
               10  REC                     PICTURE X(110).
               10  HND                     PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  KNR2                    PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
           05  STATTAB-DATA-FIELDS.
               10  SELGER                  PICTURE X(20).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FNRTYP                  PICTURE X(5).
               10  SKEY                    PICTURE X(8).
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
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  SALGF-PROCESS
               SET SALGF-PROCESS-OFF       TO TRUE
               SET SALGF-READ              TO TRUE
           END-IF
 
           IF  SALGF-READ
           AND RECORD-SELECTED-OFF
               PERFORM SALGF-GET
               SET SALGF-READ-OFF          TO TRUE
               IF  NOT SALGF-EOF
                   SET SALGF-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  SALGF-PROCESS
               PERFORM SALGF-IDSET
           END-IF
 
           IF  SALGF-PROCESS
               PERFORM SALGF-CHK-LEVEL
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
 
           IF  SALGF-PROCESS
               PERFORM SALGF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SALGF-PROCESS
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
               MOVE FIRMA                  TO FNRTYP (1:3)
               SET NOT-I-20                TO TRUE
               IF  FIRMA = '970'
                   SET I-20                TO TRUE
               END-IF
               MOVE '03'                   TO FNRTYP (4:2)
           END-IF
           IF  (I-L1 AND I-20)
               MOVE '01'                   TO FNRTYP (4:2)
           END-IF
           IF  (I-L1)
               MOVE FNRTYP                 TO SKEY (1:5)
               MOVE HND                    TO SKEY (6:3)
               MOVE SKEY                   TO STATTAB-KEY1
               READ STATTAB RECORD KEY IS STATTAB-KEY1
               INVALID KEY
                   SET I-99                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-99            TO TRUE
                   PERFORM STATTAB-FLDSET
                   PERFORM STATTAB-IDSET
               END-READ
           END-IF.
 
       SALGF-GET SECTION.
       SALGF-GET-P.
           IF  SALGF-EOF-OFF
               READ SALGF
               AT END
                   SET SALGF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SALGF-FLDSET SECTION.
       SALGF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SALGF-IO-AREA (1:110)  TO REC (1:110)
               MOVE SALGF-IO-AREA (54:3)   TO HND (1:3)
               MOVE SALGF-IO-AREA (45:6)   TO KNR (1:6)
               MOVE SALGF-IO-AREA (45:2)   TO KNR2 (1:2)
               MOVE SALGF-IO-AREA (51:3)   TO FIRMA (1:3)
           END-EVALUATE.
 
       SALGF-IDSET SECTION.
       SALGF-IDSET-P.
           SET I-01                        TO TRUE.
 
       SALGF-CHK-LEVEL SECTION.
       SALGF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SALGF-LEVEL-01
               MOVE SALGF-IO-AREA (54:3)   TO SALGF-01-L1-HND
               IF  SALGF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SALGF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SALGF-01-L1           TO THE-PRIOR-L1
               SET SALGF-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       STATTAB-FLDSET SECTION.
       STATTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATTAB-IO-AREA (11:20) TO SELGER (1:20)
           END-EVALUATE.
 
       STATTAB-IDSET SECTION.
       STATTAB-IDSET-P.
           SET I-04                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE REC                    TO OUTFIL-IO-AREA (1:110)
               MOVE HND                    TO OUTFIL-IO-AREA (108:3)
               IF  (I-20 AND I-99)
                   MOVE '   '              TO OUTFIL-IO-AREA (108:3)
               END-IF
               IF  (NOT-I-99)
                   MOVE SELGER             TO OUTFIL-IO-AREA (111:20)
               END-IF
               IF  (I-99)
                   MOVE 'UUUUUUUUUUUUUUUUUUUU' TO OUTFIL-IO-AREA
                                                              (111:20)
               END-IF
               WRITE OUTFIL-IO-AREA
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
           SET SALGF-LEVEL-INIT            TO TRUE
           INITIALIZE SALGF-DATA-FIELDS
           SET SALGF-EOF-OFF               TO TRUE
           SET SALGF-PROCESS               TO TRUE
           OPEN INPUT SALGF
           INITIALIZE STATTAB-DATA-FIELDS
           OPEN INPUT STATTAB
           OPEN OUTPUT OUTFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE SALGF
           CLOSE STATTAB
           CLOSE OUTFIL.
 
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
