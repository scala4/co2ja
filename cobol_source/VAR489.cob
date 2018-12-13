       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR489R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET LESER VSAM "VARETIL" OG DANNER EN NY REC. FOR    *
      *  HVER OVERSTOCK LOCATION DERSOM DENNE IKKE ER SPACES.        *
      *  ENDRINGER :                                                 *
      *  DATO      AV   BESKRIVELSE                                  *
      *  08.11.06  TOM  LAGT INN LAGERLOC FOR LAGER 13 FOR FIRMA 938 *
      *  30.11.06  TOM  LAGT INN LAGERLOC FOR LAGER 13 FOR FIRMA 732 *
      ****************************************************************
      * RETIL IP  F     200            KSDS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR489.rpg
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
           SELECT VARETIL
               ASSIGN TO UT-S-VARETIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARETIL-STATUS.
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARETIL
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X           PICTURE X(200).
       FD OUTFILE
               BLOCK CONTAINS 40
               RECORD CONTAINS 20.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(20).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-EOF-OFF         VALUE '0'.
               88  VARETIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-READ-OFF        VALUE '0'.
               88  VARETIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-PROCESS-OFF     VALUE '0'.
               88  VARETIL-PROCESS         VALUE '1'.
           05  VARETIL-DATA-FIELDS.
               10  ART2                    PICTURE X(1).
               10  VFIRMA                  PICTURE X(3).
               10  VEDBNR                  PICTURE X(7).
               10  LOC1                    PICTURE X(6).
               10  LOC2                    PICTURE X(6).
               10  LOC3                    PICTURE X(6).
               10  LOC4                    PICTURE X(6).
               10  LOC5                    PICTURE X(6).
               10  LOC6                    PICTURE X(6).
               10  LOC7                    PICTURE X(6).
               10  LOC8                    PICTURE X(6).
               10  LOC9                    PICTURE X(6).
               10  LOC10                   PICTURE X(6).
               10  LOC11                   PICTURE X(6).
               10  ART13                   PICTURE X(1).
               10  VFIR13                  PICTURE X(3).
               10  VEDB13                  PICTURE X(7).
               10  LOC13                   PICTURE X(6).
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
           SET NOT-I-09                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VARETIL-PROCESS
               SET VARETIL-PROCESS-OFF     TO TRUE
               SET VARETIL-READ            TO TRUE
           END-IF
 
           IF  VARETIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM VARETIL-GET
               SET VARETIL-READ-OFF        TO TRUE
               IF  NOT VARETIL-EOF
                   PERFORM VARETIL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET VARETIL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-IDSET
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
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-FLDOFF
               PERFORM VARETIL-FLDSET
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
           SET NOT-I-10                    TO TRUE
      *  09                SETON                     LR    FERDIG LEST  I PARAM
      * N02                GOTO SLUTT                      KUN "VARETIL-RECORDS"
           SET I-10                        TO TRUE
           IF  (I-03)
               SET NOT-I-32                TO TRUE
               IF  VFIR13 = '938'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  VFIR13 = '627'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  VFIR13 = '732'
                   SET I-32                TO TRUE
               END-IF
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       VARETIL-GET SECTION.
       VARETIL-GET-P.
           IF  VARETIL-EOF-OFF
               READ VARETIL
               AT END
                   SET VARETIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARETIL-FLDOFF SECTION.
       VARETIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
               SET NOT-I-20                TO TRUE
               SET NOT-I-21                TO TRUE
               SET NOT-I-22                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-27                TO TRUE
               SET NOT-I-28                TO TRUE
               SET NOT-I-29                TO TRUE
               SET NOT-I-30                TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '1' )
               SET NOT-I-31                TO TRUE
           END-EVALUATE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
               MOVE VARETIL-IO-AREA (2:1)  TO ART2 (1:1)
               MOVE VARETIL-IO-AREA (3:3)  TO VFIRMA (1:3)
               MOVE VARETIL-IO-AREA (6:7)  TO VEDBNR (1:7)
               MOVE VARETIL-IO-AREA (13:6) TO LOC1 (1:6)
               IF  LOC1 = SPACES
                   SET I-20                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (25:6) TO LOC2 (1:6)
               IF  LOC2 = SPACES
                   SET I-21                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (37:6) TO LOC3 (1:6)
               IF  LOC3 = SPACES
                   SET I-22                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (49:6) TO LOC4 (1:6)
               IF  LOC4 = SPACES
                   SET I-23                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (61:6) TO LOC5 (1:6)
               IF  LOC5 = SPACES
                   SET I-24                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (73:6) TO LOC6 (1:6)
               IF  LOC6 = SPACES
                   SET I-25                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (85:6) TO LOC7 (1:6)
               IF  LOC7 = SPACES
                   SET I-26                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (97:6) TO LOC8 (1:6)
               IF  LOC8 = SPACES
                   SET I-27                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (109:6) TO LOC9 (1:6)
               IF  LOC9 = SPACES
                   SET I-28                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (121:6) TO LOC10 (1:6)
               IF  LOC10 = SPACES
                   SET I-29                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (133:6) TO LOC11 (1:6)
               IF  LOC11 = SPACES
                   SET I-30                TO TRUE
               END-IF
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '1' )
               MOVE VARETIL-IO-AREA (2:1)  TO ART13 (1:1)
               MOVE VARETIL-IO-AREA (3:3)  TO VFIR13 (1:3)
               MOVE VARETIL-IO-AREA (6:7)  TO VEDB13 (1:7)
               MOVE VARETIL-IO-AREA (13:6) TO LOC13 (1:6)
               IF  LOC13 = SPACES
                   SET I-31                TO TRUE
               END-IF
           END-EVALUATE.
 
       VARETIL-IDCHK SECTION.
       VARETIL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
             OR ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '1' )
             OR ( VARETIL-IO-AREA (1:1) = '8' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
               SET I-02                    TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '1' )
               SET I-03                    TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8' )
               SET I-09                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-10 AND NOT-I-20)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC1                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-21)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC2                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-22)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC3                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-23)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC4                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-24)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC5                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-25)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC6                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-26)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC7                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-27)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC8                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-28)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC9                   TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-29)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC10                  TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-30)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC11                  TO OUTFILE-IO-AREA (4:6)
               MOVE VEDBNR                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART2                   TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-03 AND I-32 AND NOT-I-31)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE VFIR13                 TO OUTFILE-IO-AREA (1:3)
               MOVE LOC13                  TO OUTFILE-IO-AREA (4:6)
               MOVE VEDB13                 TO OUTFILE-IO-AREA (10:7)
               MOVE ART13                  TO OUTFILE-IO-AREA (17:1)
               WRITE OUTFILE-IO-AREA
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
           INITIALIZE VARETIL-DATA-FIELDS
           SET VARETIL-EOF-OFF             TO TRUE
           SET VARETIL-PROCESS             TO TRUE
           OPEN INPUT VARETIL
           OPEN OUTPUT OUTFILE.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARETIL
           CLOSE OUTFILE.
 
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
