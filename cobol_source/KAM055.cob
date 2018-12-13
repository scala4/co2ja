       IDENTIFICATION DIVISION.
       PROGRAM-ID. KAM055R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM BRUKES I FAST JOBB XKAM764A     XX2000XXIRXXRE
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KAM055.rpg
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
           SELECT EDBNR
               ASSIGN TO UT-S-EDBNR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDBNR-STATUS.
           SELECT KAMMAST
               ASSIGN TO KAMMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KAMMAST-STATUS
               RECORD KEY IS KAMMAST-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD EDBNR
               BLOCK CONTAINS 80
               RECORD CONTAINS 8.
       01  EDBNR-IO-AREA.
           05  EDBNR-IO-AREA-X             PICTURE X(8).
       FD KAMMAST
               RECORD CONTAINS 40.
       01  KAMMAST-IO-AREA.
           05  KAMMAST-IO-AREA-X.
               10  KAMMAST-KEY1.
                   15  KAMMAST-KEY1N       PICTURE S9(13).
               10  FILLER                  PICTURE X(27).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD OUTPUT-X
               BLOCK CONTAINS 8000
               RECORD CONTAINS 40.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(40).
       WORKING-STORAGE SECTION.
       77  TABEDB-MAX   VALUE 9999         PICTURE 9(4) USAGE BINARY.
       77  TABNYE-MAX   VALUE 9999         PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABEDB-TABLE.
               10  TABEDB-ENTRY
                                           OCCURS 9999 TIMES
                                           INDEXED BY TABEDB-I
                                                      TABEDB-S
                                                      TABNYE-I
                                                      TABNYE-S.
                   15  TABEDB              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
                   15  TABNYE              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  EDBNR-STATUS                PICTURE 99 VALUE 0.
           10  KAMMAST-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBNR-EOF-OFF           VALUE '0'.
               88  EDBNR-EOF               VALUE '1'.
           05  KAMMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMAST-EOF-OFF         VALUE '0'.
               88  KAMMAST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMAST-READ-OFF        VALUE '0'.
               88  KAMMAST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMAST-PROCESS-OFF     VALUE '0'.
               88  KAMMAST-PROCESS         VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KAMMAST-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDB-IO.
                   15  EDB                 PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KNR                     PICTURE X(6).
               10  REC                     PICTURE X(40).
           05  VAREMAS-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  TEMPORARY-FIELDS.
               10  KEY-X                   PICTURE X(10).
               10  TABNYE-N-IO.
                   15  TABNYE-N            PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KAMMAST-PROCESS
               SET KAMMAST-PROCESS-OFF     TO TRUE
               SET KAMMAST-READ            TO TRUE
           END-IF
 
           IF  KAMMAST-READ
           AND RECORD-SELECTED-OFF
               PERFORM KAMMAST-GET
               SET KAMMAST-READ-OFF        TO TRUE
               IF  NOT KAMMAST-EOF
                   SET KAMMAST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KAMMAST-PROCESS
               PERFORM KAMMAST-IDSET
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
 
           IF  KAMMAST-PROCESS
               PERFORM KAMMAST-FLDSET
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
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '764'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-10)
               GO TO END-X-T
           END-IF
           IF  (I-01)
               SET NOT-I-20                TO TRUE
               SET TABEDB-S                TO TABEDB-I
               PERFORM WITH TEST AFTER
                       VARYING TABEDB-I FROM 1 BY 1
                         UNTIL TABEDB-I >= TABEDB-MAX
                            OR I-20
                   IF  EDB = TABEDB (TABEDB-I)
                       SET I-20            TO TRUE
                       SET TABEDB-S        TO TABEDB-I
                   END-IF
               END-PERFORM
               SET TABEDB-I                TO TABEDB-S
               IF  I-20
               AND TABEDB-I NOT > TABNYE-MAX
                   SET TABNYE-I            TO TABEDB-I
               END-IF
               MOVE '787'                  TO KEY-X (1:3)
               MOVE TABNYE(TABNYE-I)       TO TABNYE(TABNYE-I)-N
               MOVE TABNYE(TABNYE-I)-N-IO  TO KEY-X (4:7)
               MOVE KEY-X                  TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-21)
               SET I-25                    TO TRUE
           END-IF.
 
       END-X-T.
      *AREMAS D        10 25
      *                                 117 "B"
           CONTINUE.
 
       KAMMAST-GET SECTION.
       KAMMAST-GET-P.
           IF  KAMMAST-EOF-OFF
               READ KAMMAST
               AT END
                   SET KAMMAST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KAMMAST-FLDSET SECTION.
       KAMMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KAMMAST-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KAMMAST-IO-AREA (4:4)  TO EDB-IO
               MOVE KAMMAST-IO-AREA (8:6)  TO KNR (1:6)
               MOVE KAMMAST-IO-AREA (1:40) TO REC (1:40)
           END-EVALUATE.
 
       KAMMAST-IDSET SECTION.
       KAMMAST-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       EDBNR-LOAD SECTION.
       EDBNR-LOAD-P.
           OPEN INPUT EDBNR
           SET TABEDB-I                    TO 1
           PERFORM UNTIL EDBNR-EOF
               READ EDBNR
               AT END
                   SET EDBNR-EOF           TO TRUE
               NOT AT END
                   MOVE EDBNR-IO-AREA (1:14) TO TABEDB-ENTRY (TABEDB-I)
                   SET TABEDB-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE EDBNR.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-10 AND I-25 AND I-20)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE REC                    TO OUTPUT-X-IO-AREA (1:40)
               MOVE '787'                  TO OUTPUT-X-IO-AREA (1:3)
               MOVE TABNYE (TABNYE-I)      TO XO-70P
               MOVE XO-70P-EF              TO OUTPUT-X-IO-AREA (4:4)
               WRITE OUTPUT-X-IO-AREA
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
           PERFORM EDBNR-LOAD
           INITIALIZE KAMMAST-DATA-FIELDS
           SET KAMMAST-EOF-OFF             TO TRUE
           SET KAMMAST-PROCESS             TO TRUE
           OPEN INPUT KAMMAST
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTPUT-X.
           SET TABEDB-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KAMMAST
           CLOSE VAREMAS
           CLOSE OUTPUT-X.
 
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
