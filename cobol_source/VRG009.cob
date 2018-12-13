       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG009R.
      **********************************************  Z-WIN-RPG2      *
      *********************************************************************
      *  MERGE  KOPI FRA BUP48UB - MOT VAREARKIVKOPI.
      *  FJERNE DE ART.SOM ER MERKET FOR IKKE OPPDATERING (X OG L)
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG009.rpg
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
           SELECT VAREFIL
               ASSIGN TO UT-S-VAREFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREFIL-STATUS.
           SELECT KOPI
               ASSIGN TO UT-S-KOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPI-STATUS.
           SELECT UTFIL
               ASSIGN TO UT-S-UTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREFIL
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  VAREFIL-IO-AREA.
           05  VAREFIL-IO-AREA-X           PICTURE X(200).
       FD KOPI
               BLOCK CONTAINS 280
               RECORD CONTAINS 140.
       01  KOPI-IO-AREA.
           05  KOPI-IO-AREA-X              PICTURE X(140).
       FD UTFIL
               BLOCK CONTAINS 280
               RECORD CONTAINS 140.
       01  UTFIL-IO-AREA.
           05  UTFIL-IO-AREA-X             PICTURE X(140).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREFIL-STATUS              PICTURE 99 VALUE 0.
           10  KOPI-STATUS                 PICTURE 99 VALUE 0.
           10  UTFIL-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREFIL-EOF-OFF         VALUE '0'.
               88  VAREFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREFIL-READ-OFF        VALUE '0'.
               88  VAREFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREFIL-PROCESS-OFF     VALUE '0'.
               88  VAREFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREFIL-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREFIL-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-EOF-OFF            VALUE '0'.
               88  KOPI-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-READ-OFF           VALUE '0'.
               88  KOPI-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-PROCESS-OFF        VALUE '0'.
               88  KOPI-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOPI-LEVEL-INIT-OFF     VALUE '0'.
               88  KOPI-LEVEL-INIT         VALUE '1'.
           05  VAREFIL-LEVEL-01.
               10  VAREFIL-01-L1.
                   15  VAREFIL-01-L1-FIRMA PICTURE X(3).
           05  VAREFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  AUTOP                   PICTURE X(1).
           05  VAREFIL-MP                  PICTURE X(10).
           05  VAREFIL-MC                  PICTURE X(10).
           05  VAREFIL-M-01            REDEFINES VAREFIL-MC.
               10  VAREFIL-M-01-M2.
                   15  VAREFIL-M-01-M2-FIRMA-G.
                       20  VAREFIL-M-01-M2-FIRMA PICTURE X(3).
               10  VAREFIL-M-01-M1.
                   15  VAREFIL-M-01-M1-EDBNR-G.
                       20  VAREFIL-M-01-M1-EDBNR PICTURE X(7).
           05  KOPI-LEVEL-02.
               10  KOPI-02-L1.
                   15  KOPI-02-L1-FIRMA    PICTURE X(3).
           05  KOPI-DATA-FIELDS.
               10  REC                     PICTURE X(140).
           05  KOPI-MP                     PICTURE X(10).
           05  KOPI-MC                     PICTURE X(10).
           05  KOPI-M-02               REDEFINES KOPI-MC.
               10  KOPI-M-02-M2.
                   15  KOPI-M-02-M2-FIRMA-G.
                       20  KOPI-M-02-M2-FIRMA PICTURE X(3).
               10  KOPI-M-02-M1.
                   15  KOPI-M-02-M1-EDBNR-G.
                       20  KOPI-M-02-M1-EDBNR PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREFIL-PROCESS
               SET VAREFIL-PROCESS-OFF     TO TRUE
               SET VAREFIL-READ            TO TRUE
           END-IF
 
           IF  VAREFIL-READ
               PERFORM VAREFIL-GET
               SET VAREFIL-READ-OFF        TO TRUE
               IF  NOT VAREFIL-EOF
                   PERFORM VAREFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  KOPI-PROCESS
               SET KOPI-PROCESS-OFF        TO TRUE
               SET KOPI-READ               TO TRUE
           END-IF
 
           IF  KOPI-READ
               PERFORM KOPI-GET
               SET KOPI-READ-OFF           TO TRUE
               IF  NOT KOPI-EOF
                   PERFORM KOPI-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  VAREFIL-PROCESS
               PERFORM VAREFIL-IDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-IDSET
           END-IF
 
           IF  VAREFIL-PROCESS
               PERFORM VAREFIL-CHK-LEVEL
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-CHK-LEVEL
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
 
           IF  VAREFIL-PROCESS
               PERFORM VAREFIL-FLDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREFIL-PROCESS
           OR  KOPI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02 AND I-MR)
               SET NOT-I-40                TO TRUE
               IF  AUTOP = 'X'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  AUTOP = 'L'
                   SET I-40                TO TRUE
               END-IF
           END-IF.
 
       VAREFIL-GET SECTION.
       VAREFIL-GET-P.
           IF  VAREFIL-EOF-OFF
               READ VAREFIL
               AT END
                   SET VAREFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREFIL-FLDSET SECTION.
       VAREFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREFIL-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREFIL-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREFIL-IO-AREA (96:1) TO AUTOP (1:1)
           END-EVALUATE.
 
       VAREFIL-IDSET SECTION.
       VAREFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREFIL-CHK-LEVEL SECTION.
       VAREFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREFIL-LEVEL-01
               MOVE VAREFIL-IO-AREA (3:3)  TO VAREFIL-01-L1-FIRMA
               IF  VAREFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREFIL-01-L1         TO THE-PRIOR-L1
               SET VAREFIL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREFIL-MATCH-SET SECTION.
       VAREFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREFIL-IO-AREA (3:3)  TO VAREFIL-M-01-M2-FIRMA
               MOVE VAREFIL-IO-AREA (6:7)  TO VAREFIL-M-01-M1-EDBNR
           END-EVALUATE.
 
       KOPI-GET SECTION.
       KOPI-GET-P.
           IF  KOPI-EOF-OFF
               READ KOPI
               AT END
                   SET KOPI-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KOPI-FLDSET SECTION.
       KOPI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPI-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE KOPI-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE KOPI-IO-AREA (1:140)   TO REC (1:140)
           END-EVALUATE.
 
       KOPI-IDSET SECTION.
       KOPI-IDSET-P.
           SET I-02                        TO TRUE.
 
       KOPI-CHK-LEVEL SECTION.
       KOPI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KOPI-LEVEL-02
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-02-L1-FIRMA
               IF  KOPI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOPI-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOPI-02-L1            TO THE-PRIOR-L1
               SET KOPI-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       KOPI-MATCH-SET SECTION.
       KOPI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-M-02-M2-FIRMA
               MOVE KOPI-IO-AREA (6:7)     TO KOPI-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VAREFIL-EOF
               MOVE HIGH-VALUES            TO VAREFIL-MC
                                              VAREFIL-MP
           END-IF
           IF  KOPI-EOF
               MOVE HIGH-VALUES            TO KOPI-MC
                                              KOPI-MP
           END-IF
           IF  VAREFIL-MC < VAREFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KOPI-MC < KOPI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREFIL-MC < KOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREFIL-PROCESS     TO TRUE
                   MOVE VAREFIL-MC         TO VAREFIL-MP
                   IF  VAREFIL-MC = KOPI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KOPI-MC < VAREFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KOPI-PROCESS        TO TRUE
                   MOVE KOPI-MC            TO KOPI-MP
                   IF  KOPI-MC = VAREFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREFIL-MC = KOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREFIL-PROCESS     TO TRUE
                   MOVE VAREFIL-MC         TO VAREFIL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR AND NOT-I-40)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE REC                    TO UTFIL-IO-AREA (1:140)
               WRITE UTFIL-IO-AREA
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
           SET VAREFIL-LEVEL-INIT          TO TRUE
           INITIALIZE VAREFIL-DATA-FIELDS
           SET VAREFIL-EOF-OFF             TO TRUE
           SET VAREFIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREFIL-MC
                                              VAREFIL-MP
           OPEN INPUT VAREFIL
           SET KOPI-LEVEL-INIT             TO TRUE
           INITIALIZE KOPI-DATA-FIELDS
           SET KOPI-EOF-OFF                TO TRUE
           SET KOPI-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO KOPI-MC
                                              KOPI-MP
           OPEN INPUT KOPI
           OPEN OUTPUT UTFIL.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREFIL
           CLOSE KOPI
           CLOSE UTFIL.
 
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
