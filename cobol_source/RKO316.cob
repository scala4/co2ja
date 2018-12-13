       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO316R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RKO316, PLUKKER UT FIRMA SOM SKAL VÆRE MED I *
      *                  JOBBEN.                                      *
      *  PROGRAMMERER..: M. TUVRØNNINGEN                              *
      *  KJØRES I JOBB.: DOP40ME                                      *
      *  LAGET DATO....: 09.11.10                                     *
      *  RETTET........: 29.12.16 BAH UTVIDET LINJENR I RESFILE1      *
      *                               TATT FRA FILLER                 *
      *  INPUT.......... PARAMETERFIL, RESKONTROMASTER                *
      *  BEHANDLING....: LESER TRANSER FRA FIRNR SOM HAR BESTILT 13.  *
      *  OUTPUT........: RESKONTROMASTER FOR VALGTE FIRMA FRA PARAM   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO316.rpg
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
           SELECT PARFILI
               ASSIGN TO UT-S-PARFILI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARFILI-STATUS.
           SELECT RESFILI
               ASSIGN TO UT-S-RESFILI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILI-STATUS.
           SELECT RESFILO
               ASSIGN TO UT-S-RESFILO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARFILI
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARFILI-IO-AREA.
           05  PARFILI-IO-AREA-X           PICTURE X(80).
       FD RESFILI
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  RESFILI-IO-AREA.
           05  RESFILI-IO-AREA-X           PICTURE X(120).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD RESFILO
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  RESFILO-IO-AREA.
           05  RESFILO-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARFILI-STATUS              PICTURE 99 VALUE 0.
           10  RESFILI-STATUS              PICTURE 99 VALUE 0.
           10  RESFILO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILI-EOF-OFF         VALUE '0'.
               88  PARFILI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILI-READ-OFF        VALUE '0'.
               88  PARFILI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILI-PROCESS-OFF     VALUE '0'.
               88  PARFILI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILI-EOF-OFF         VALUE '0'.
               88  RESFILI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILI-READ-OFF        VALUE '0'.
               88  RESFILI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILI-PROCESS-OFF     VALUE '0'.
               88  RESFILI-PROCESS         VALUE '1'.
           05  PARFILI-DATA-FIELDS.
               10  FIRNR                   PICTURE X(3).
           05  PARFILI-MP                  PICTURE X(3).
           05  PARFILI-MC                  PICTURE X(3).
           05  PARFILI-M-01            REDEFINES PARFILI-MC.
               10  PARFILI-M-01-M1.
                   15  PARFILI-M-01-M1-FIRNR-G.
                       20  PARFILI-M-01-M1-FIRNR PICTURE X(3).
           05  RESFILI-DATA-FIELDS.
               10  RESKNR                  PICTURE X(6).
               10  TK                      PICTURE X(2).
               10  BILDT-IO.
                   15  BILDT               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BILNR-IO.
                   15  BILNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  REFNR-IO.
                   15  REFNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BELOP-IO.
                   15  BELOP               PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BETBET                  PICTURE X(2).
               10  BILART                  PICTURE X(1).
               10  PERIOD                  PICTURE X(6).
               10  TEKST                   PICTURE X(24).
           05  RESFILI-MP                  PICTURE X(3).
           05  RESFILI-MC                  PICTURE X(3).
           05  RESFILI-M-02            REDEFINES RESFILI-MC.
               10  RESFILI-M-02-M1.
                   15  RESFILI-M-02-M1-FIRNR-G.
                       20  RESFILI-M-02-M1-FIRNR PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  REFNRO-IO.
                   15  REFNRO              PICTURE S9(6).
               10  BILNRO-IO.
                   15  BILNRO              PICTURE S9(6).
               10  BILDTO-IO.
                   15  BILDTO              PICTURE S9(6).
               10  BELOPO-IO.
                   15  BELOPO              PICTURE S9(7)V9(2).
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
           IF  PARFILI-PROCESS
               SET PARFILI-PROCESS-OFF     TO TRUE
               SET PARFILI-READ            TO TRUE
           END-IF
 
           IF  PARFILI-READ
               PERFORM PARFILI-GET
               SET PARFILI-READ-OFF        TO TRUE
               IF  NOT PARFILI-EOF
                   PERFORM PARFILI-MATCH-SET
               END-IF
           END-IF
 
           IF  RESFILI-PROCESS
               SET RESFILI-PROCESS-OFF     TO TRUE
               SET RESFILI-READ            TO TRUE
           END-IF
 
           IF  RESFILI-READ
               PERFORM RESFILI-GET
               SET RESFILI-READ-OFF        TO TRUE
               IF  NOT RESFILI-EOF
                   PERFORM RESFILI-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  PARFILI-PROCESS
               PERFORM PARFILI-IDSET
           END-IF
 
           IF  RESFILI-PROCESS
               PERFORM RESFILI-IDSET
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
 
           IF  PARFILI-PROCESS
               PERFORM PARFILI-FLDSET
           END-IF
 
           IF  RESFILI-PROCESS
               PERFORM RESFILI-FLDSET
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
           IF  (I-02 AND I-MR)
               ADD REFNR TO ZERO       GIVING REFNRO
      *  02 MR 10          MOVE "REFNR   "BUGFL2  8        LEDETXT DEBUG
      *  02 MR 10BUGFL2    DEBUGBUGFILO   REFNR            VIS FELT/IND
           END-IF
           IF  (I-02 AND I-MR)
               ADD BILNR TO ZERO       GIVING BILNRO
               ADD BILDT TO ZERO       GIVING BILDTO
               ADD BELOP TO ZERO       GIVING BELOPO
           END-IF.
 
       PARFILI-GET SECTION.
       PARFILI-GET-P.
           IF  PARFILI-EOF-OFF
               READ PARFILI
               AT END
                   SET PARFILI-EOF         TO TRUE
               END-READ
           END-IF.
 
       PARFILI-FLDSET SECTION.
       PARFILI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARFILI-IO-AREA (1:3)  TO FIRNR (1:3)
           END-EVALUATE.
 
       PARFILI-IDSET SECTION.
       PARFILI-IDSET-P.
           SET I-01                        TO TRUE.
 
       PARFILI-MATCH-SET SECTION.
       PARFILI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE PARFILI-IO-AREA (1:3)  TO PARFILI-M-01-M1-FIRNR
           END-EVALUATE.
 
       RESFILI-GET SECTION.
       RESFILI-GET-P.
           IF  RESFILI-EOF-OFF
               READ RESFILI
               AT END
                   SET RESFILI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESFILI-FLDSET SECTION.
       RESFILI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESFILI-IO-AREA (1:3)  TO FIRNR (1:3)
               MOVE RESFILI-IO-AREA (4:6)  TO RESKNR (1:6)
               MOVE RESFILI-IO-AREA (17:2) TO TK (1:2)
               MOVE RESFILI-IO-AREA (19:4) TO BILDT-IO
               MOVE RESFILI-IO-AREA (23:4) TO BILNR-IO
               MOVE RESFILI-IO-AREA (27:4) TO REFNR-IO
               MOVE RESFILI-IO-AREA (35:6) TO BELOP-IO
               MOVE RESFILI-IO-AREA (41:2) TO BETBET (1:2)
               MOVE RESFILI-IO-AREA (49:1) TO BILART (1:1)
               MOVE RESFILI-IO-AREA (59:6) TO PERIOD (1:6)
               MOVE RESFILI-IO-AREA (65:24) TO TEKST (1:24)
           END-EVALUATE.
 
       RESFILI-IDSET SECTION.
       RESFILI-IDSET-P.
           SET I-02                        TO TRUE.
 
       RESFILI-MATCH-SET SECTION.
       RESFILI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESFILI-IO-AREA (1:3)  TO RESFILI-M-02-M1-FIRNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  PARFILI-EOF
               MOVE HIGH-VALUES            TO PARFILI-MC
                                              PARFILI-MP
           END-IF
           IF  RESFILI-EOF
               MOVE HIGH-VALUES            TO RESFILI-MC
                                              RESFILI-MP
           END-IF
           IF  PARFILI-MC < PARFILI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESFILI-MC < RESFILI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  PARFILI-MC < RESFILI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PARFILI-PROCESS     TO TRUE
                   MOVE PARFILI-MC         TO PARFILI-MP
                   IF  PARFILI-MC = RESFILI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESFILI-MC < PARFILI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESFILI-PROCESS     TO TRUE
                   MOVE RESFILI-MC         TO RESFILI-MP
                   IF  RESFILI-MC = PARFILI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  PARFILI-MC = RESFILI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PARFILI-PROCESS     TO TRUE
                   MOVE PARFILI-MC         TO PARFILI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-MR AND I-02)
               MOVE SPACES TO RESFILO-IO-AREA
               INITIALIZE RESFILO-IO-AREA
               MOVE FIRNR                  TO RESFILO-IO-AREA (1:3)
               MOVE RESKNR                 TO RESFILO-IO-AREA (4:6)
               MOVE REFNRO-IO              TO RESFILO-IO-AREA (10:6)
               MOVE TK                     TO RESFILO-IO-AREA (16:2)
               MOVE BETBET                 TO RESFILO-IO-AREA (18:2)
               MOVE TEKST                  TO RESFILO-IO-AREA (20:24)
               MOVE BILNRO-IO              TO RESFILO-IO-AREA (44:6)
               MOVE BILART                 TO RESFILO-IO-AREA (50:1)
               MOVE BILDTO-IO              TO RESFILO-IO-AREA (51:6)
               MOVE BELOPO-IO              TO RESFILO-IO-AREA (57:9)
               MOVE PERIOD                 TO RESFILO-IO-AREA (66:6)
               WRITE RESFILO-IO-AREA
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
           INITIALIZE PARFILI-DATA-FIELDS
           SET PARFILI-EOF-OFF             TO TRUE
           SET PARFILI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO PARFILI-MC
                                              PARFILI-MP
           OPEN INPUT PARFILI
           INITIALIZE RESFILI-DATA-FIELDS
           SET RESFILI-EOF-OFF             TO TRUE
           SET RESFILI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESFILI-MC
                                              RESFILI-MP
           OPEN INPUT RESFILI
           OPEN OUTPUT RESFILO.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARFILI
           CLOSE RESFILI
           CLOSE RESFILO.
 
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
