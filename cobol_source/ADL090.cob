       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADL090R.
      *** XADL17B ******************************* :   Z-WIN-RPG2   ****
      *                                          XX2000XXIRXXMT       *
      ******************************************************
      *  PROGRAM: ADL090                                   *
      *  UTPLUKK FRA LØNNSMASTER.                          *
      *  UPSI 0 = UTPLUKK FRA FIRMANR.TABELL               *
      *  UPSI 1 = UTPLUKK FRA LØNNSPARAMETER (LØNNSLISTE)  *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ADL090.rpg
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
           SELECT INTAB
               ASSIGN TO UT-S-INTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INTAB-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT LONNMAS
               ASSIGN TO LONNMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS LONNMAS-STATUS
               RECORD KEY IS LONNMAS-KEY1.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  INTAB-IO-AREA.
           05  INTAB-IO-AREA-X             PICTURE X(80).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1.
                   15  FIRMAF-KEY1N        PICTURE S9(3).
               10  FILLER                  PICTURE X(994).
       FD LONNMAS
               RECORD CONTAINS 1025.
       01  LONNMAS-IO-AREA.
           05  LONNMAS-IO-AREA-X.
               10  LONNMAS-KEY1.
                   15  LONNMAS-KEY1N       PICTURE S9(6).
               10  FILLER                  PICTURE X(1019).
       FD UTFILE
               BLOCK CONTAINS 4100
               RECORD CONTAINS 1025.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(1025).
       WORKING-STORAGE SECTION.
       77  TABFNR-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFNR-TABLE.
               10  TABFNR-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY TABFNR-I
                                                      TABFNR-S.
                   15  TABFNR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INTAB-STATUS                PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LONNMAS-STATUS              PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INTAB-EOF-OFF           VALUE '0'.
               88  INTAB-EOF               VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-EOF-OFF          VALUE '0'.
               88  FIRMAF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-READ-OFF         VALUE '0'.
               88  FIRMAF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-PROCESS-OFF      VALUE '0'.
               88  FIRMAF-PROCESS          VALUE '1'.
           05  LONNMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  LONNMAS-EOF-OFF         VALUE '0'.
               88  LONNMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LONNMAS-READ-OFF        VALUE '0'.
               88  LONNMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LONNMAS-PROCESS-OFF     VALUE '0'.
               88  LONNMAS-PROCESS         VALUE '1'.
           05  FIRMAF-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  LISTM                   PICTURE X(1).
               10  LISTF                   PICTURE X(1).
               10  LMDATO-IO.
                   15  LMDATO              PICTURE S9(6).
               10  LFDATO-IO.
                   15  LFDATO              PICTURE S9(6).
           05  FIRMAF-MP                   PICTURE X(3).
           05  FIRMAF-MC                   PICTURE X(3).
           05  FIRMAF-M-01             REDEFINES FIRMAF-MC.
               10  FIRMAF-M-01-M1.
                   15  FIRMAF-M-01-M1-FNR-G.
                       20  FIRMAF-M-01-M1-FNR PICTURE X(3).
           05  LONNMAS-DATA-FIELDS.
               10  LREC1                   PICTURE X(250).
               10  LREC2                   PICTURE X(250).
               10  LREC3                   PICTURE X(250).
               10  LREC4                   PICTURE X(250).
               10  LREC5                   PICTURE X(25).
           05  LONNMAS-MP                  PICTURE X(3).
           05  LONNMAS-MC                  PICTURE X(3).
           05  LONNMAS-M-02            REDEFINES LONNMAS-MC.
               10  LONNMAS-M-02-M1.
                   15  LONNMAS-M-02-M1-FNR-G.
                       20  LONNMAS-M-02-M1-FNR PICTURE X(3).
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
           IF  FIRMAF-PROCESS
               SET FIRMAF-PROCESS-OFF      TO TRUE
               SET FIRMAF-READ             TO TRUE
           END-IF
 
           IF  FIRMAF-READ
               PERFORM FIRMAF-GET
               SET FIRMAF-READ-OFF         TO TRUE
               IF  NOT FIRMAF-EOF
                   PERFORM FIRMAF-MATCH-SET
               END-IF
           END-IF
 
           IF  LONNMAS-PROCESS
               SET LONNMAS-PROCESS-OFF     TO TRUE
               SET LONNMAS-READ            TO TRUE
           END-IF
 
           IF  LONNMAS-READ
               PERFORM LONNMAS-GET
               SET LONNMAS-READ-OFF        TO TRUE
               IF  NOT LONNMAS-EOF
                   PERFORM LONNMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-IDSET
           END-IF
 
           IF  LONNMAS-PROCESS
               PERFORM LONNMAS-IDSET
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-FLDSET
           END-IF
 
           IF  LONNMAS-PROCESS
               PERFORM LONNMAS-FLDSET
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
           SET NOT-I-25                    TO TRUE
           IF  (I-01)
               SET NOT-I-30                TO TRUE
               SET NOT-I-15                TO TRUE
               IF  LMDATO = UDATE
                   SET I-15                TO TRUE
               END-IF
               SET NOT-I-16                TO TRUE
               IF  LISTM = 'J'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-15 AND I-16)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-01)
               SET NOT-I-15                TO TRUE
               IF  LFDATO = UDATE
                   SET I-15                TO TRUE
               END-IF
               SET NOT-I-16                TO TRUE
               IF  LISTF = 'J'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-15 AND I-16)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-U1)
               GO TO PARRUT-T
      *******  RUTINE FOR UTPLUKK FRA FIRMANR.TABELL *******
           END-IF
           SET NOT-I-25                    TO TRUE
           SET TABFNR-S                    TO TABFNR-I
           PERFORM WITH TEST AFTER
                   VARYING TABFNR-I FROM 1 BY 1
                     UNTIL TABFNR-I >= TABFNR-MAX
                        OR I-25
               IF  FNR = TABFNR (TABFNR-I)
                   SET I-25                TO TRUE
                   SET TABFNR-S            TO TABFNR-I
               END-IF
           END-PERFORM
           GO TO SLUTT-T
      *******  RUTINE FOR UTPLUKK HVIS LØNNSARKIVSLISTE ØNSKES ******
           .
 
       PARRUT-T.
           IF  (I-MR AND I-30)
               SET I-25                    TO TRUE
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       FIRMAF-GET SECTION.
       FIRMAF-GET-P.
           IF  FIRMAF-EOF-OFF
               READ FIRMAF
               AT END
                   SET FIRMAF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (5:3)   TO FNR (1:3)
               MOVE FIRMAF-IO-AREA (639:1) TO LISTM (1:1)
               MOVE FIRMAF-IO-AREA (679:1) TO LISTF (1:1)
               MOVE FIRMAF-IO-AREA (629:6) TO LMDATO-IO
               INSPECT LMDATO-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (669:6) TO LFDATO-IO
               INSPECT LFDATO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRMAF-MATCH-SET SECTION.
       FIRMAF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (5:3)   TO FIRMAF-M-01-M1-FNR
           END-EVALUATE.
 
       LONNMAS-GET SECTION.
       LONNMAS-GET-P.
           IF  LONNMAS-EOF-OFF
               READ LONNMAS
               AT END
                   SET LONNMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LONNMAS-FLDSET SECTION.
       LONNMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LONNMAS-IO-AREA (2:3)  TO FNR (1:3)
               MOVE LONNMAS-IO-AREA (1:250) TO LREC1 (1:250)
               MOVE LONNMAS-IO-AREA (251:250) TO LREC2 (1:250)
               MOVE LONNMAS-IO-AREA (501:250) TO LREC3 (1:250)
               MOVE LONNMAS-IO-AREA (751:250) TO LREC4 (1:250)
               MOVE LONNMAS-IO-AREA (1001:25) TO LREC5 (1:25)
           END-EVALUATE.
 
       LONNMAS-IDSET SECTION.
       LONNMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       LONNMAS-MATCH-SET SECTION.
       LONNMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE LONNMAS-IO-AREA (2:3)  TO LONNMAS-M-02-M1-FNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FIRMAF-EOF
               MOVE HIGH-VALUES            TO FIRMAF-MC
                                              FIRMAF-MP
           END-IF
           IF  LONNMAS-EOF
               MOVE HIGH-VALUES            TO LONNMAS-MC
                                              LONNMAS-MP
           END-IF
           IF  FIRMAF-MC < FIRMAF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  LONNMAS-MC < LONNMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FIRMAF-MC < LONNMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRMAF-PROCESS      TO TRUE
                   MOVE FIRMAF-MC          TO FIRMAF-MP
                   IF  FIRMAF-MC = LONNMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  LONNMAS-MC < FIRMAF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET LONNMAS-PROCESS     TO TRUE
                   MOVE LONNMAS-MC         TO LONNMAS-MP
                   IF  LONNMAS-MC = FIRMAF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FIRMAF-MC = LONNMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRMAF-PROCESS      TO TRUE
                   MOVE FIRMAF-MC          TO FIRMAF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       INTAB-LOAD SECTION.
       INTAB-LOAD-P.
           OPEN INPUT INTAB
           SET TABFNR-I                    TO 1
           PERFORM UNTIL INTAB-EOF
               READ INTAB
               AT END
                   SET INTAB-EOF           TO TRUE
               NOT AT END
                   MOVE INTAB-IO-AREA (1:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (4:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (7:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (10:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (13:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (16:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (19:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (22:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (25:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (28:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (31:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (34:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (37:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (40:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (43:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (46:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (49:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (52:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (55:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE INTAB-IO-AREA (58:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE INTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-25)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE LREC1                  TO UTFILE-IO-AREA (1:250)
               MOVE LREC2                  TO UTFILE-IO-AREA (251:250)
               MOVE LREC3                  TO UTFILE-IO-AREA (501:250)
               MOVE LREC4                  TO UTFILE-IO-AREA (751:250)
               MOVE LREC5                  TO UTFILE-IO-AREA (1001:25)
               WRITE UTFILE-IO-AREA
           END-IF
           IF  (I-01 AND I-30 AND I-U1)
               MOVE ' '                    TO FIRMAF-IO-AREA (639:1)
               MOVE ' '                    TO FIRMAF-IO-AREA (679:1)
               REWRITE FIRMAF-IO-AREA
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
           PERFORM INTAB-LOAD
           INITIALIZE FIRMAF-DATA-FIELDS
           SET FIRMAF-EOF-OFF              TO TRUE
           SET FIRMAF-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FIRMAF-MC
                                              FIRMAF-MP
           OPEN I-O FIRMAF
           INITIALIZE LONNMAS-DATA-FIELDS
           SET LONNMAS-EOF-OFF             TO TRUE
           SET LONNMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO LONNMAS-MC
                                              LONNMAS-MP
           OPEN INPUT LONNMAS
           OPEN OUTPUT UTFILE.
           SET TABFNR-I                    TO 1.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FIRMAF
           CLOSE LONNMAS
           CLOSE UTFILE.
 
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
