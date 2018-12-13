       IDENTIFICATION DIVISION.
       PROGRAM-ID. SER112R.
      **********************************************  Z-WIN-RPG2   ****
      * HENTE ALFA OG ARTIKKELNUMMER FRA VAREARKIVET.      *
      * FOR Å PRINTE DETTE I PROGRAM SER115                *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: SER112.rpg
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
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT DAGORDR
               ASSIGN TO UT-S-DAGORDR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGORDR-STATUS.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREREC
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(200).
       FD DAGORDR
               BLOCK CONTAINS 3000
               RECORD CONTAINS 60.
       01  DAGORDR-IO-AREA.
           05  DAGORDR-IO-AREA-X           PICTURE X(60).
       FD UTFILE
               BLOCK CONTAINS 9000
               RECORD CONTAINS 90.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(90).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  DAGORDR-STATUS              PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-EOF-OFF         VALUE '0'.
               88  VAREREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-READ-OFF        VALUE '0'.
               88  VAREREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-PROCESS-OFF     VALUE '0'.
               88  VAREREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGORDR-EOF-OFF         VALUE '0'.
               88  DAGORDR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGORDR-READ-OFF        VALUE '0'.
               88  DAGORDR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGORDR-PROCESS-OFF     VALUE '0'.
               88  DAGORDR-PROCESS         VALUE '1'.
           05  VAREREC-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  VAREREC-MP                  PICTURE X(10).
           05  VAREREC-MC                  PICTURE X(10).
           05  VAREREC-M-01            REDEFINES VAREREC-MC.
               10  VAREREC-M-01-M2.
                   15  VAREREC-M-01-M2-FIRMA-G.
                       20  VAREREC-M-01-M2-FIRMA PICTURE X(3).
               10  VAREREC-M-01-M1.
                   15  VAREREC-M-01-M1-EDBNR-G.
                       20  VAREREC-M-01-M1-EDBNR PICTURE X(7).
           05  DAGORDR-DATA-FIELDS.
               10  REC1                    PICTURE X(60).
           05  DAGORDR-MP                  PICTURE X(10).
           05  DAGORDR-MC                  PICTURE X(10).
           05  DAGORDR-M-02            REDEFINES DAGORDR-MC.
               10  DAGORDR-M-02-M2.
                   15  DAGORDR-M-02-M2-FIRMA-G.
                       20  DAGORDR-M-02-M2-FIRMA PICTURE X(3).
               10  DAGORDR-M-02-M1.
                   15  DAGORDR-M-02-M1-EDBNR-G.
                       20  DAGORDR-M-02-M1-EDBNR PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  BEHOLD-IO.
                   15  BEHOLD              PICTURE S9(7)V9(2).
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
           IF  VAREREC-PROCESS
               SET VAREREC-PROCESS-OFF     TO TRUE
               SET VAREREC-READ            TO TRUE
           END-IF
 
           IF  VAREREC-READ
               PERFORM VAREREC-GET
               SET VAREREC-READ-OFF        TO TRUE
               IF  NOT VAREREC-EOF
                   PERFORM VAREREC-MATCH-SET
               END-IF
           END-IF
 
           IF  DAGORDR-PROCESS
               SET DAGORDR-PROCESS-OFF     TO TRUE
               SET DAGORDR-READ            TO TRUE
           END-IF
 
           IF  DAGORDR-READ
               PERFORM DAGORDR-GET
               SET DAGORDR-READ-OFF        TO TRUE
               IF  NOT DAGORDR-EOF
                   PERFORM DAGORDR-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-IDSET
           END-IF
 
           IF  DAGORDR-PROCESS
               PERFORM DAGORDR-IDSET
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
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-FLDSET
           END-IF
 
           IF  DAGORDR-PROCESS
               PERFORM DAGORDR-FLDSET
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
           IF  (I-MR)
               SUBTRACT ANTUT FROM ANTINN GIVING BEHOLD
           END-IF.
 
       VAREREC-GET SECTION.
       VAREREC-GET-P.
           IF  VAREREC-EOF-OFF
               READ VAREREC
               AT END
                   SET VAREREC-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREREC-FLDSET SECTION.
       VAREREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREREC-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREREC-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREREC-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREREC-IO-AREA (97:5) TO ANTINN-IO
               MOVE VAREREC-IO-AREA (102:5) TO ANTUT-IO
           END-EVALUATE.
 
       VAREREC-IDSET SECTION.
       VAREREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREREC-MATCH-SET SECTION.
       VAREREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (3:3)  TO VAREREC-M-01-M2-FIRMA
               MOVE VAREREC-IO-AREA (6:7)  TO VAREREC-M-01-M1-EDBNR
           END-EVALUATE.
 
       DAGORDR-GET SECTION.
       DAGORDR-GET-P.
           IF  DAGORDR-EOF-OFF
               READ DAGORDR
               AT END
                   SET DAGORDR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGORDR-FLDSET SECTION.
       DAGORDR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGORDR-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE DAGORDR-IO-AREA (53:7) TO EDBNR (1:7)
               MOVE DAGORDR-IO-AREA (1:60) TO REC1 (1:60)
           END-EVALUATE.
 
       DAGORDR-IDSET SECTION.
       DAGORDR-IDSET-P.
           SET I-02                        TO TRUE.
 
       DAGORDR-MATCH-SET SECTION.
       DAGORDR-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGORDR-IO-AREA (3:3)  TO DAGORDR-M-02-M2-FIRMA
               MOVE DAGORDR-IO-AREA (53:7) TO DAGORDR-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VAREREC-EOF
               MOVE HIGH-VALUES            TO VAREREC-MC
                                              VAREREC-MP
           END-IF
           IF  DAGORDR-EOF
               MOVE HIGH-VALUES            TO DAGORDR-MC
                                              DAGORDR-MP
           END-IF
           IF  VAREREC-MC < VAREREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  DAGORDR-MC < DAGORDR-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREREC-MC < DAGORDR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREREC-PROCESS     TO TRUE
                   MOVE VAREREC-MC         TO VAREREC-MP
                   IF  VAREREC-MC = DAGORDR-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGORDR-MC < VAREREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGORDR-PROCESS     TO TRUE
                   MOVE DAGORDR-MC         TO DAGORDR-MP
                   IF  DAGORDR-MC = VAREREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREREC-MC = DAGORDR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREREC-PROCESS     TO TRUE
                   MOVE VAREREC-MC         TO VAREREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE REC1                   TO UTFILE-IO-AREA (1:60)
               IF  (NOT-I-MR)
                   MOVE '   '              TO UTFILE-IO-AREA (6:3)
               END-IF
               IF  (I-MR)
                   MOVE ALFA               TO UTFILE-IO-AREA (6:3)
               END-IF
               IF  (NOT-I-MR)
                   MOVE EDBNR              TO UTFILE-IO-AREA (71:7)
               END-IF
               IF  (NOT-I-MR)
                   MOVE 'UKJENT EDBNR'     TO UTFILE-IO-AREA (69:12)
               END-IF
               IF  (I-MR)
                   MOVE ARTNR              TO UTFILE-IO-AREA (61:20)
               END-IF
               IF  (I-MR)
                   MOVE BEHOLD-IO          TO UTFILE-IO-AREA (82:9)
               END-IF
               WRITE UTFILE-IO-AREA
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
           INITIALIZE VAREREC-DATA-FIELDS
           SET VAREREC-EOF-OFF             TO TRUE
           SET VAREREC-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREREC-MC
                                              VAREREC-MP
           OPEN INPUT VAREREC
           INITIALIZE DAGORDR-DATA-FIELDS
           SET DAGORDR-EOF-OFF             TO TRUE
           SET DAGORDR-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO DAGORDR-MC
                                              DAGORDR-MP
           OPEN INPUT DAGORDR
           OPEN OUTPUT UTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREREC
           CLOSE DAGORDR
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
