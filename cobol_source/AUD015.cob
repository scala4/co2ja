       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUD015R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: AUD015   COPY AV ORD072 MEN SEQ. FILE I INPUT.  *
      *  PROGRAMERER: Stein Sandvold                                  *
      *  PROGRAMERT.: 03.01.17                                        *
      *                                                               *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: AUD015.rpg
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
           SELECT INNDATA
               ASSIGN TO UT-S-INNDATA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNDATA-STATUS.
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS OPPSMAS-STATUS
               RECORD KEY IS OPPSMAS-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT UTDATA
               ASSIGN TO UT-S-UTDATA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTDATA-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNDATA
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  INNDATA-IO-AREA.
           05  INNDATA-IO-AREA-X           PICTURE X(80).
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X.
               10  OPPSMAS-KEY1            PICTURE X(21).
               10  FILLER                  PICTURE X(9).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD UTDATA
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  UTDATA-IO-AREA.
           05  UTDATA-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNDATA-STATUS              PICTURE 99 VALUE 0.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  UTDATA-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNDATA-EOF-OFF         VALUE '0'.
               88  INNDATA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNDATA-READ-OFF        VALUE '0'.
               88  INNDATA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNDATA-PROCESS-OFF     VALUE '0'.
               88  INNDATA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNDATA-LEVEL-INIT-OFF  VALUE '0'.
               88  INNDATA-LEVEL-INIT      VALUE '1'.
           05  OPPSMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INNDATA-LEVEL-01.
               10  INNDATA-01-L2.
                   15  INNDATA-01-L2-FIRMA PICTURE X(3).
               10  INNDATA-01-L1.
                   15  INNDATA-01-L1-AVSKNR PICTURE X(6).
           05  INNDATA-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  SNR                     PICTURE X(3).
               10  AVSKNR                  PICTURE X(6).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(14).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(5).
           05  OPPSMAS-DATA-FIELDS.
               10  EDBNR                   PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  UPRIS-IO.
                   15  UPRIS               PICTURE S9(7)V9(2).
               10  VARTNR                  PICTURE X(20).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  VARKEY                  PICTURE X(21).
               10  FELT4                   PICTURE X(4).
               10  FELT18                  PICTURE X(18).
               10  ANTBST-IO.
                   15  ANTBST              PICTURE S9(5)V9(2).
               10  NYEDBN                  PICTURE X(7).
               10  VARK                    PICTURE X(10).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(5)V9(2).
           05  EDITTING-FIELDS.
               10  EDIT-ANTALL             PICTURE Z99999.
               10  EDIT-UPRIS              PICTURE Z999999,99.
               10  EDIT-BEL                PICTURE Z999999,99.
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNDATA-PROCESS
               SET INNDATA-PROCESS-OFF     TO TRUE
               SET INNDATA-READ            TO TRUE
           END-IF
 
           IF  INNDATA-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNDATA-GET
               SET INNDATA-READ-OFF        TO TRUE
               IF  NOT INNDATA-EOF
                   SET INNDATA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNDATA-PROCESS
               PERFORM INNDATA-IDSET
           END-IF
 
           IF  INNDATA-PROCESS
               PERFORM INNDATA-CHK-LEVEL
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
 
           IF  INNDATA-PROCESS
               PERFORM INNDATA-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNDATA-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '399'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-11)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET I-50                    TO TRUE
               SET NOT-I-93                TO TRUE
               IF  AVSKNR = '100997'
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-93)
               SET NOT-I-93                TO TRUE
               IF  AVSKNR = '100998'
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-93)
               SET NOT-I-93                TO TRUE
               IF  AVSKNR = '100999'
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-93)
               SET NOT-I-50                TO TRUE
               GO TO SLUTT-T
      *  RUTINE FOR VARELINJERECORDS.                                 *
      *  OM DETTE EAN-NR IKKE ER INNMELDT SETTES EDB-NR TIL 9999981   *
      *     PASS PÅ OG MELDE DETTE INN I VARE.MASTER PÅ NYE FIRMA.    *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE FIRMA                  TO VARKEY (1:3)
               MOVE ALFA                   TO FELT4 (1:3)
               MOVE ' '                    TO FELT4 (4:1)
               MOVE ARTNR                  TO FELT18 (5:14)
               MOVE FELT4                  TO FELT18 (1:4)
               MOVE FELT18                 TO VARKEY (4:18)
               MOVE VARKEY                 TO OPPSMAS-KEY1
               READ OPPSMAS RECORD KEY IS OPPSMAS-KEY1
               INVALID KEY
                   SET I-13                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-13            TO TRUE
                   PERFORM OPPSMAS-FLDSET
                   PERFORM OPPSMAS-IDSET
               END-READ
               ADD ANTALL TO ZERO      GIVING ANTBST
           END-IF
           IF  (I-01 AND NOT-I-13)
               MOVE EDBNR                  TO NYEDBN
           END-IF
           IF  (I-01 AND I-13)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE FIRMA                  TO VARK (1:3)
               MOVE NYEDBN                 TO VARK (4:7)
               MOVE VARK                   TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-16)
               MULTIPLY UPRIS BY ANTALL GIVING BEL
      *****************************************************************
      * SLUTT RUTINE.                                                 *
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       INNDATA-GET SECTION.
       INNDATA-GET-P.
           IF  INNDATA-EOF-OFF
               READ INNDATA
               AT END
                   SET INNDATA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNDATA-FLDSET SECTION.
       INNDATA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNDATA-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE INNDATA-IO-AREA (4:3)  TO SNR (1:3)
               MOVE INNDATA-IO-AREA (7:6)  TO AVSKNR (1:6)
               MOVE INNDATA-IO-AREA (13:3) TO ALFA (1:3)
               MOVE INNDATA-IO-AREA (16:14) TO ARTNR (1:14)
               MOVE INNDATA-IO-AREA (30:5) TO ANTALL-IO
               INSPECT ANTALL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNDATA-IDSET SECTION.
       INNDATA-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNDATA-CHK-LEVEL SECTION.
       INNDATA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNDATA-LEVEL-01
               MOVE INNDATA-IO-AREA (1:3)  TO INNDATA-01-L2-FIRMA
               MOVE INNDATA-IO-AREA (7:6)  TO INNDATA-01-L1-AVSKNR
               IF  INNDATA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNDATA-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNDATA-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNDATA-01-L2         TO THE-PRIOR-L2
               MOVE  INNDATA-01-L1         TO THE-PRIOR-L1
               SET INNDATA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       OPPSMAS-FLDSET SECTION.
       OPPSMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (23:7) TO EDBNR (1:7)
           END-EVALUATE.
 
       OPPSMAS-IDSET SECTION.
       OPPSMAS-IDSET-P.
           SET I-05                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (75:9) TO UPRIS-IO
               INSPECT UPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (16:20) TO VARTNR (1:20)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-06                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50 AND NOT-I-16)
               MOVE SPACES TO UTDATA-IO-AREA
               INITIALIZE UTDATA-IO-AREA
               MOVE '2;4;'                 TO UTDATA-IO-AREA (1:4)
               MOVE AVSKNR                 TO UTDATA-IO-AREA (5:6)
               MOVE ';'                    TO UTDATA-IO-AREA (11:1)
               MOVE VARTNR                 TO UTDATA-IO-AREA (12:20)
               MOVE ';'                    TO UTDATA-IO-AREA (32:1)
               MOVE ANTALL                 TO EDIT-ANTALL
               MOVE EDIT-ANTALL            TO UTDATA-IO-AREA (33:6)
               MOVE ';'                    TO UTDATA-IO-AREA (41:1)
               MOVE UPRIS                  TO EDIT-UPRIS
               MOVE EDIT-UPRIS             TO UTDATA-IO-AREA (42:10)
               MOVE ';'                    TO UTDATA-IO-AREA (52:1)
               MOVE BEL                    TO EDIT-BEL
               MOVE EDIT-BEL               TO UTDATA-IO-AREA (54:10)
      *                                  64 ";"
      *                        EDBNR     73
               WRITE UTDATA-IO-AREA
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
           SET INNDATA-LEVEL-INIT          TO TRUE
           INITIALIZE INNDATA-DATA-FIELDS
           SET INNDATA-EOF-OFF             TO TRUE
           SET INNDATA-PROCESS             TO TRUE
           OPEN INPUT INNDATA
           INITIALIZE OPPSMAS-DATA-FIELDS
           OPEN INPUT OPPSMAS
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT UTDATA.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNDATA
           CLOSE OPPSMAS
           CLOSE VAREMAS
           CLOSE UTDATA.
 
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
