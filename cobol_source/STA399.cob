       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA399R.
      **********************************************  Z-WIN-RPG2   ****
      *  NULLSTILLE PR MND ANTALLSSTATISTIKK ORDRE AUTODATA.  ***
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA399.rpg
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
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD REGPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  REGPAR-IO-AREA.
           05  REGPAR-IO-AREA-X            PICTURE X(100).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1.
                   15  FIRMAF-KEY1N        PICTURE S9(3).
               10  FILLER                  PICTURE X(994).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REGPAR-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
 
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
           05  REGPAR-DATA-FIELDS.
               10  MNDNR                   PICTURE X(2).
           05  FIRMAF-DATA-FIELDS.
               10  VANT-IO.
                   15  VANT                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  OANT-IO.
                   15  OANT                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VAKK-IO.
                   15  VAKK                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  OAKK-IO.
                   15  OAKK                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  DOANT-IO.
                   15  DOANT               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  DOAKK-IO.
                   15  DOAKK               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  TEMPORARY-FIELDS.
               10  NULL70-IO.
                   15  NULL70              PICTURE S9(7).
               10  NULL50-IO.
                   15  NULL50              PICTURE S9(5).
               10  NULL90-IO.
                   15  NULL90              PICTURE S9(9).
               10  NYVAKK-IO.
                   15  NYVAKK              PICTURE S9(9).
               10  NYOAKK-IO.
                   15  NYOAKK              PICTURE S9(7).
               10  NYDOAK-IO.
                   15  NYDOAK              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
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
 
           IF  FIRMAF-PROCESS
               SET FIRMAF-PROCESS-OFF      TO TRUE
               SET FIRMAF-READ             TO TRUE
           END-IF
 
           IF  FIRMAF-READ
           AND RECORD-SELECTED-OFF
               PERFORM FIRMAF-GET
               SET FIRMAF-READ-OFF         TO TRUE
               IF  NOT FIRMAF-EOF
                   SET FIRMAF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-IDSET
           END-IF
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-IDSET
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
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-FLDSET
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
           IF  (I-02)
               SET NOT-I-12                TO TRUE
               IF  MNDNR = '12'
                   SET I-12                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           MOVE 0                          TO NULL70
           MOVE 0                          TO NULL50
           MOVE 0                          TO NULL90
           ADD VANT TO VAKK            GIVING NYVAKK
           ADD OANT TO OAKK            GIVING NYOAKK
           ADD DOANT TO DOAKK          GIVING NYDOAK.
 
       SLUTT-T.
           CONTINUE.
 
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
               MOVE REGPAR-IO-AREA (9:2)   TO MNDNR (1:2)
           END-EVALUATE.
 
       REGPAR-IDSET SECTION.
       REGPAR-IDSET-P.
           SET I-02                        TO TRUE.
 
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
               MOVE FIRMAF-IO-AREA (787:4) TO VANT-IO
               MOVE FIRMAF-IO-AREA (791:3) TO OANT-IO
               MOVE FIRMAF-IO-AREA (794:5) TO VAKK-IO
               MOVE FIRMAF-IO-AREA (799:4) TO OAKK-IO
               MOVE FIRMAF-IO-AREA (901:3) TO DOANT-IO
               MOVE FIRMAF-IO-AREA (904:4) TO DOAKK-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE NULL70                 TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (787:4)
               MOVE NULL50                 TO XO-50P
               MOVE XO-50P-EF              TO FIRMAF-IO-AREA (791:3)
               IF  (NOT-I-12)
                   MOVE NYVAKK             TO XO-90P
                   MOVE XO-90P-EF          TO FIRMAF-IO-AREA (794:5)
                   INITIALIZE NYVAKK
               END-IF
               IF  (I-12)
                   MOVE NULL90             TO XO-90P
                   MOVE XO-90P-EF          TO FIRMAF-IO-AREA (794:5)
               END-IF
               IF  (NOT-I-12)
                   MOVE NYOAKK             TO XO-70P
                   MOVE XO-70P-EF          TO FIRMAF-IO-AREA (799:4)
                   INITIALIZE NYOAKK
               END-IF
               IF  (I-12)
                   MOVE NULL70             TO XO-70P
                   MOVE XO-70P-EF          TO FIRMAF-IO-AREA (799:4)
               END-IF
               MOVE NULL50                 TO XO-50P
               MOVE XO-50P-EF              TO FIRMAF-IO-AREA (901:3)
               IF  (NOT-I-12)
                   MOVE NYDOAK             TO XO-70P
                   MOVE XO-70P-EF          TO FIRMAF-IO-AREA (904:4)
                   INITIALIZE NYDOAK
               END-IF
               IF  (I-12)
                   MOVE NULL70             TO XO-70P
                   MOVE XO-70P-EF          TO FIRMAF-IO-AREA (904:4)
               END-IF
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
           INITIALIZE REGPAR-DATA-FIELDS
           SET REGPAR-EOF-OFF              TO TRUE
           SET REGPAR-PROCESS              TO TRUE
           OPEN INPUT REGPAR
           INITIALIZE FIRMAF-DATA-FIELDS
           SET FIRMAF-EOF-OFF              TO TRUE
           SET FIRMAF-PROCESS              TO TRUE
           OPEN I-O FIRMAF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGPAR
           CLOSE FIRMAF.
 
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
