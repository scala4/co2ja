       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOM001R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET DANNER OVERSIKT OVER RECORDS SOM ER ENDRET SIDEN *
      * FORRIGE KJØRING                                             *
      ***************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FOM001.rpg
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
           SELECT TYPETAB
               ASSIGN TO UT-S-TYPETAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TYPETAB-STATUS.
           SELECT VAREMAS
               ASSIGN TO UT-S-VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT BEHUT
               ASSIGN TO UT-S-BEHUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BEHUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TYPETAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TYPETAB-IO-AREA.
           05  TYPETAB-IO-AREA-X           PICTURE X(80).
       FD VAREMAS
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD BEHUT
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  BEHUT-IO-AREA.
           05  BEHUT-IO-AREA-X             PICTURE X(200).
       WORKING-STORAGE SECTION.
       77  TABVRG-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       77  TABUKE-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABVRG-TABLE.
               10  TABVRG-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY TABVRG-I
                                                      TABVRG-S
                                                      TABUKE-I
                                                      TABUKE-S.
                   15  TABVRG              PICTURE X(8).
                   15  TABUKE              PICTURE X(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TYPETAB-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  BEHUT-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TYPETAB-EOF-OFF         VALUE '0'.
               88  TYPETAB-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-DATA-FIELDS.
               10  KEY-X                   PICTURE X(10).
               10  FIRMA                   PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
               10  GMLS-IO.
                   15  GMLS                PICTURE S9(7)V9(2).
               10  UPRIS-IO.
                   15  UPRIS               PICTURE S9(7)V9(2).
               10  BEHINN-IO.
                   15  BEHINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEHUT-XX-IO.
                   15  BEHUT-XX            PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VRG                     PICTURE X(5).
           05  VARETIL-DATA-FIELDS.
               10  EAN                     PICTURE X(13).
               10  LARTNR                  PICTURE X(20).
               10  GMLLN1-IO.
                   15  GMLLN1              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  TEMPORARY-FIELDS.
               10  KEY12                   PICTURE X(12).
               10  VRGALF                  PICTURE X(8).
               10  LEVUKE                  PICTURE X(10).
               10  BEHOLD-IO.
                   15  BEHOLD              PICTURE S9(7)V9(2).
               10  TOTSVS-IO.
                   15  TOTSVS              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  EDIT-GMLLN1             PICTURE Z999999.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   SET VAREMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
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
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDSET
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
           MOVE '80'                       TO KEY12 (1:2)
           MOVE KEY-X                      TO KEY12 (3:10)
           MOVE KEY12                      TO VARETIL-KEY1
           READ VARETIL RECORD KEY IS VARETIL-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM VARETIL-FLDSET
               PERFORM VARETIL-IDSET
           END-READ
      *
           MOVE VRG                        TO VRGALF (1:5)
           MOVE ALFA                       TO VRGALF (6:3)
           SET NOT-I-15                    TO TRUE
           SET TABVRG-S                    TO TABVRG-I
           PERFORM WITH TEST AFTER
                   VARYING TABVRG-I FROM 1 BY 1
                     UNTIL TABVRG-I >= TABVRG-MAX
                        OR I-15
               IF  VRGALF = TABVRG (TABVRG-I)
                   SET I-15                TO TRUE
                   SET TABVRG-S            TO TABVRG-I
               END-IF
           END-PERFORM
           SET TABVRG-I                    TO TABVRG-S
           IF  I-15
           AND TABVRG-I NOT > TABUKE-MAX
               SET TABUKE-I                TO TABVRG-I
           END-IF
           IF  (I-15)
               MOVE TABUKE(TABUKE-I)       TO LEVUKE (9:2)
      *
           END-IF
           SUBTRACT BEHUT-XX FROM BEHINN GIVING BEHOLD
           SET NOT-I-80                    TO TRUE
           IF  BEHOLD < 0
               SET I-80                    TO TRUE
           END-IF
           MULTIPLY GMLS BY BEHOLD     GIVING TOTSVS.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:10) TO KEY-X (1:10)
               MOVE VAREMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VNAVN (1:30)
               MOVE VAREMAS-IO-AREA (66:9) TO GMLS-IO
               INSPECT GMLS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (75:9) TO UPRIS-IO
               INSPECT UPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (97:5) TO BEHINN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO BEHUT-XX-IO
               MOVE VAREMAS-IO-AREA (118:5) TO VRG (1:5)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (148:13) TO EAN (1:13)
               MOVE VARETIL-IO-AREA (13:20) TO LARTNR (1:20)
               MOVE VARETIL-IO-AREA (68:4) TO GMLLN1-IO
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-03                        TO TRUE.
 
       TYPETAB-LOAD SECTION.
       TYPETAB-LOAD-P.
           OPEN INPUT TYPETAB
           SET TABVRG-I                    TO 1
           PERFORM UNTIL TYPETAB-EOF
               READ TYPETAB
               AT END
                   SET TYPETAB-EOF         TO TRUE
               NOT AT END
                   MOVE TYPETAB-IO-AREA (1:10) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TYPETAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO BEHUT-IO-AREA
               INITIALIZE BEHUT-IO-AREA
               MOVE ALFA                   TO BEHUT-IO-AREA (1:3)
               MOVE ARTNR                  TO BEHUT-IO-AREA (5:20)
               MOVE ';'                    TO BEHUT-IO-AREA (25:1)
               MOVE VNAVN                  TO BEHUT-IO-AREA (26:30)
               MOVE ';'                    TO BEHUT-IO-AREA (56:1)
               MOVE 'NR  '                 TO BEHUT-IO-AREA (57:4)
               MOVE ';'                    TO BEHUT-IO-AREA (61:1)
               IF  (I-80)
                   MOVE '-'                TO BEHUT-IO-AREA (62:1)
               END-IF
               MOVE BEHOLD                 TO XO-72YN9
               MOVE XO-72YN9               TO BEHUT-IO-AREA (68:10)
               MOVE ';'                    TO BEHUT-IO-AREA (78:1)
               IF  (I-80)
                   MOVE '-'                TO BEHUT-IO-AREA (79:1)
               END-IF
               MOVE BEHOLD                 TO XO-72YN9
               MOVE XO-72YN9               TO BEHUT-IO-AREA (85:10)
               MOVE ';'                    TO BEHUT-IO-AREA (95:1)
               MOVE GMLS                   TO XO-72YN9
               MOVE XO-72YN9               TO BEHUT-IO-AREA (100:10)
               MOVE ';'                    TO BEHUT-IO-AREA (110:1)
               IF  (I-80)
                   MOVE '-'                TO BEHUT-IO-AREA (111:1)
               END-IF
               MOVE TOTSVS                 TO XO-72YN9
               MOVE XO-72YN9               TO BEHUT-IO-AREA (116:10)
               MOVE ';'                    TO BEHUT-IO-AREA (127:1)
               MOVE '                '     TO BEHUT-IO-AREA (128:16)
               MOVE ';'                    TO BEHUT-IO-AREA (144:1)
               MOVE '          '           TO BEHUT-IO-AREA (145:10)
               IF  (I-15)
                   MOVE LEVUKE             TO BEHUT-IO-AREA (145:10)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (155:1)
               MOVE '       '              TO BEHUT-IO-AREA (156:7)
               IF  (NOT-I-11)
                   MOVE GMLLN1             TO EDIT-GMLLN1
                   MOVE EDIT-GMLLN1        TO BEHUT-IO-AREA (156:7)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (163:1)
               WRITE BEHUT-IO-AREA
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
           PERFORM TYPETAB-LOAD
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           OPEN INPUT VAREMAS
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           OPEN OUTPUT BEHUT.
           SET TABVRG-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREMAS
           CLOSE VARETIL
           CLOSE BEHUT.
 
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
