       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA141R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGR:  STA141 (KOPIERT FRA STA040)                             *
      * LAGET AV: RUNE ERSVIK        DATO: 31.03.98  RETTET: 31.03.98   *
      * PLUKKER UT DATA TIL FAKT.VARERECMASTER.                         *
      *******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA141.rpg
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
           SELECT STATREC
               ASSIGN TO UT-S-STATREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STATREC-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD STATREC
               BLOCK CONTAINS 9430
               RECORD CONTAINS 82.
       01  STATREC-IO-AREA.
           05  STATREC-IO-AREA-X           PICTURE X(82).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD OUTF
               BLOCK CONTAINS 8855
               RECORD CONTAINS 115.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(115).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  STATREC-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  STATREC-EOF-OFF         VALUE '0'.
               88  STATREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STATREC-READ-OFF        VALUE '0'.
               88  STATREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STATREC-PROCESS-OFF     VALUE '0'.
               88  STATREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  STATREC-LEVEL-INIT-OFF  VALUE '0'.
               88  STATREC-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  STATREC-LEVEL-01.
               10  STATREC-01-L2.
                   15  STATREC-01-L2-FNR   PICTURE X(3).
               10  STATREC-01-L1.
                   15  STATREC-01-L1-EDBNR PICTURE X(7).
           05  STATREC-DATA-FIELDS.
               10  REC                     PICTURE X(82).
               10  FNR                     PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
           05  VARETIL-DATA-FIELDS.
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  RESKEY                  PICTURE X(10).
               10  KEY12                   PICTURE X(12).
           05  EDITTING-FIELDS.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  STATREC-PROCESS
               SET STATREC-PROCESS-OFF     TO TRUE
               SET STATREC-READ            TO TRUE
           END-IF
 
           IF  STATREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM STATREC-GET
               SET STATREC-READ-OFF        TO TRUE
               IF  NOT STATREC-EOF
                   SET STATREC-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  STATREC-PROCESS
               PERFORM STATREC-IDSET
           END-IF
 
           IF  STATREC-PROCESS
               PERFORM STATREC-CHK-LEVEL
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
 
           IF  STATREC-PROCESS
               PERFORM STATREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  STATREC-PROCESS
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
               MOVE FNR                    TO RESKEY (1:3)
               MOVE EDBNR                  TO RESKEY (4:7)
               MOVE RESKEY                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-52                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-52            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
               MOVE '80'                   TO KEY12 (1:2)
               MOVE RESKEY                 TO KEY12 (3:10)
               MOVE KEY12                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-53                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-53            TO TRUE
                   PERFORM VARETIL-FLDSET
                   PERFORM VARETIL-IDSET
               END-READ
           END-IF.
 
       STATREC-GET SECTION.
       STATREC-GET-P.
           IF  STATREC-EOF-OFF
               READ STATREC
               AT END
                   SET STATREC-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       STATREC-FLDSET SECTION.
       STATREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATREC-IO-AREA (1:82) TO REC (1:82)
               MOVE STATREC-IO-AREA (51:3) TO FNR (1:3)
               MOVE STATREC-IO-AREA (16:7) TO EDBNR (1:7)
           END-EVALUATE.
 
       STATREC-IDSET SECTION.
       STATREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       STATREC-CHK-LEVEL SECTION.
       STATREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO STATREC-LEVEL-01
               MOVE STATREC-IO-AREA (51:3) TO STATREC-01-L2-FNR
               MOVE STATREC-IO-AREA (16:7) TO STATREC-01-L1-EDBNR
               IF  STATREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  STATREC-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  STATREC-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  STATREC-01-L2         TO THE-PRIOR-L2
               MOVE  STATREC-01-L1         TO THE-PRIOR-L1
               SET STATREC-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (68:4) TO LEVNR-IO
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC                    TO OUTF-IO-AREA (1:82)
               MOVE ALFA                   TO OUTF-IO-AREA (83:3)
               MOVE LEVNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO OUTF-IO-AREA (105:7)
               IF  (I-53)
                   MOVE '999999'           TO OUTF-IO-AREA (106:6)
               END-IF
               MOVE ARTNR                  TO OUTF-IO-AREA (86:20)
               IF  (I-52)
                   MOVE 'IKKE I VAREARKIV    ' TO OUTF-IO-AREA (86:20)
               END-IF
               WRITE OUTF-IO-AREA
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
           SET STATREC-LEVEL-INIT          TO TRUE
           INITIALIZE STATREC-DATA-FIELDS
           SET STATREC-EOF-OFF             TO TRUE
           SET STATREC-PROCESS             TO TRUE
           OPEN INPUT STATREC
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           OPEN OUTPUT OUTF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE STATREC
           CLOSE VAREMAS
           CLOSE VARETIL
           CLOSE OUTF.
 
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
