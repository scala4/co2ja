       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA188R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMENDRING LOGG FOR PROGRAM STA188.                       *
      * 11.11.02 HAFNOR VIL ATT NOEN HND.DIST SKAL KOMME SAMLET PR.   *
      *          SELGER FØRST PÅ LISTA, DISSE TILDELES SELGERNR.      *
      *          ØVRIGE HND.DIST FÅR SELGERNR. 999. UPSI 1 MÅ VÆRE PÅ.*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA188.rpg
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
           SELECT PRTFILE
               ASSIGN TO UT-S-PRTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFILE-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
           SELECT STATTAB
               ASSIGN TO STATTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS STATTAB-STATUS
               RECORD KEY IS STATTAB-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD PRTFILE
               BLOCK CONTAINS 4200
               RECORD CONTAINS 210.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(210).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
       FD STATTAB
               RECORD CONTAINS 40.
       01  STATTAB-IO-AREA.
           05  STATTAB-IO-AREA-X.
               10  STATTAB-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(32).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  STATTAB-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PRTFILE-EOF-OFF         VALUE '0'.
               88  PRTFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRTFILE-READ-OFF        VALUE '0'.
               88  PRTFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRTFILE-PROCESS-OFF     VALUE '0'.
               88  PRTFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PRTFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  PRTFILE-LEVEL-INIT      VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  STATTAB-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRTFILE-LEVEL-01.
               10  PRTFILE-01-L2.
                   15  PRTFILE-01-L2-FIRMA PICTURE X(3).
               10  PRTFILE-01-L1.
                   15  PRTFILE-01-L1-VGR   PICTURE X(5).
           05  PRTFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  HND                     PICTURE X(3).
           05  VAGRMAS-DATA-FIELDS.
               10  VTEKST                  PICTURE X(34).
               10  STATGR                  PICTURE X(3).
               10  KONTO                   PICTURE X(4).
           05  STATTAB-DATA-FIELDS.
               10  STEKST                  PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  SELGNR                  PICTURE X(3).
               10  SKEY5                   PICTURE X(5).
               10  SKEY                    PICTURE X(8).
               10  VGRKEY                  PICTURE X(8).
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
           IF  PRTFILE-PROCESS
               SET PRTFILE-PROCESS-OFF     TO TRUE
               SET PRTFILE-READ            TO TRUE
           END-IF
 
           IF  PRTFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM PRTFILE-GET
               SET PRTFILE-READ-OFF        TO TRUE
               IF  NOT PRTFILE-EOF
                   SET PRTFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PRTFILE-PROCESS
               PERFORM PRTFILE-IDSET
           END-IF
 
           IF  PRTFILE-PROCESS
               PERFORM PRTFILE-CHK-LEVEL
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
 
           IF  PRTFILE-PROCESS
               PERFORM PRTFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  PRTFILE-PROCESS
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
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '918'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '950'
                   SET I-18                TO TRUE
               END-IF
      *  L2N18   FIRMA     COMP "910"                    18 PER HAGEN
      *****************************************************
      *  EGEN TEST PÅ HAFNOR FOR Å FJERNE SELGERNR.
      *****************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-19                TO TRUE
               IF  FIRMA = '950'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-20                TO TRUE
      *****************************************************
      *  EGEN TEST PÅ HAFNOR FOR Å SAMLE NOEN HAND.DIST PR. SELGER.
      *  KUN HVIS UPSI 1 ER PÅ.
      *****************************************************
           END-IF
           IF  (I-U1 AND I-19)
               MOVE '999'                  TO SELGNR
           END-IF
           IF  (I-U1)
               SET NOT-I-51                TO TRUE
               IF  HND = '101'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '102'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '103'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '112'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '113'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '116'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '119'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '120'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '161'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '162'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND I-19 AND I-51)
               MOVE '102'                  TO SELGNR
           END-IF
           IF  (I-U1)
               SET NOT-I-51                TO TRUE
               IF  HND = '104'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '107'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '112'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '117'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '121'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '122'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '123'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '130'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  HND = '131'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND I-19 AND I-51)
               MOVE '107'                  TO SELGNR
      *****************************************************
      *  RUTINE FOR OPPSLAG PÅ VAREGRUPPEMASTER.          *
      *****************************************************
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO SKEY5 (1:3)
               MOVE '04'                   TO SKEY5 (4:2)
               MOVE SKEY5                  TO SKEY (1:5)
               MOVE FIRMA                  TO VGRKEY (1:3)
           END-IF
           IF  (I-L1)
               MOVE VGR                    TO VGRKEY (4:5)
               MOVE VGRKEY                 TO VAGRMAS-KEY1
               READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
               INVALID KEY
                   SET I-86                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-86            TO TRUE
                   PERFORM VAGRMAS-FLDOFF
                   PERFORM VAGRMAS-FLDSET
                   PERFORM VAGRMAS-IDSET
               END-READ
      *****************************************************
      *  RUTINE FOR OPPSLAG PÅ STATISTIKKTABELL.          *
      *****************************************************
           END-IF
           IF  (NOT-I-L1)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-18)
               GO TO SLUTT-T
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-09)
               GO TO SLUTT-T
           END-IF
           MOVE STATGR                     TO SKEY (6:3)
           MOVE SKEY                       TO STATTAB-KEY1
           READ STATTAB RECORD KEY IS STATTAB-KEY1
           INVALID KEY
               SET I-87                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-87                TO TRUE
               PERFORM STATTAB-FLDSET
               PERFORM STATTAB-IDSET
           END-READ
           IF  (NOT-I-87)
               SET I-20                    TO TRUE
      *****************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       PRTFILE-GET SECTION.
       PRTFILE-GET-P.
           IF  PRTFILE-EOF-OFF
               READ PRTFILE
               AT END
                   SET PRTFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PRTFILE-FLDSET SECTION.
       PRTFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRTFILE-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE PRTFILE-IO-AREA (22:5) TO VGR (1:5)
               MOVE PRTFILE-IO-AREA (9:3)  TO HND (1:3)
           END-EVALUATE.
 
       PRTFILE-IDSET SECTION.
       PRTFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       PRTFILE-CHK-LEVEL SECTION.
       PRTFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PRTFILE-LEVEL-01
               MOVE PRTFILE-IO-AREA (3:3)  TO PRTFILE-01-L2-FIRMA
               MOVE PRTFILE-IO-AREA (22:5) TO PRTFILE-01-L1-VGR
               IF  PRTFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PRTFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PRTFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PRTFILE-01-L2         TO THE-PRIOR-L2
               MOVE  PRTFILE-01-L1         TO THE-PRIOR-L1
               SET PRTFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDOFF SECTION.
       VAGRMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (11:34) TO VTEKST (1:34)
               MOVE VAGRMAS-IO-AREA (48:3) TO STATGR (1:3)
               IF  STATGR = SPACES
                   SET I-09                TO TRUE
               END-IF
               MOVE VAGRMAS-IO-AREA (69:4) TO KONTO (1:4)
               IF  KONTO = SPACES
                   SET I-08                TO TRUE
               END-IF
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       STATTAB-FLDSET SECTION.
       STATTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATTAB-IO-AREA (11:30) TO STEKST (1:30)
           END-EVALUATE.
 
       STATTAB-IDSET SECTION.
       STATTAB-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-18)
               IF  (NOT-I-86)
                   MOVE VTEKST             TO PRTFILE-IO-AREA (87:34)
               END-IF
               IF  (I-86)
                   MOVE 'VAREGRUPPE UKJENT' TO PRTFILE-IO-AREA (87:17)
               END-IF
               IF  (I-86)
                   MOVE '                 ' TO PRTFILE-IO-AREA (104:17)
               END-IF
               IF  (NOT-I-08)
                   MOVE 'KOSTN.KTO        ' TO PRTFILE-IO-AREA (87:17)
               END-IF
               IF  (NOT-I-08)
                   MOVE '                 ' TO PRTFILE-IO-AREA (104:17)
               END-IF
               REWRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-01 AND I-18)
               IF  (I-20)
                   MOVE STATGR             TO PRTFILE-IO-AREA (22:3)
               END-IF
               IF  (I-20)
                   MOVE '  '               TO PRTFILE-IO-AREA (25:2)
               END-IF
               IF  (I-20)
                   MOVE STEKST             TO PRTFILE-IO-AREA (87:30)
               END-IF
               IF  (NOT-I-20 AND NOT-I-86)
                   MOVE VTEKST             TO PRTFILE-IO-AREA (87:34)
               END-IF
               IF  (I-19 AND NOT-I-20)
                   MOVE 'SSSSS'            TO PRTFILE-IO-AREA (22:5)
               END-IF
               IF  (NOT-I-U1 AND I-19 AND NOT-I-U3)
                   MOVE '   '              TO PRTFILE-IO-AREA (6:3)
               END-IF
               IF  (I-U1 AND I-19)
                   MOVE SELGNR             TO PRTFILE-IO-AREA (6:3)
               END-IF
               IF  (NOT-I-20 AND I-86)
                   MOVE 'VAREGRUPPE UKJENT' TO PRTFILE-IO-AREA (87:17)
               END-IF
               IF  (NOT-I-20 AND I-86)
                   MOVE '                 ' TO PRTFILE-IO-AREA (104:17)
               END-IF
               IF  (NOT-I-08)
                   MOVE 'KOSTN.KTO        ' TO PRTFILE-IO-AREA (87:17)
               END-IF
               IF  (NOT-I-08)
                   MOVE '                 ' TO PRTFILE-IO-AREA (104:17)
               END-IF
               REWRITE PRTFILE-IO-AREA
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
           SET PRTFILE-LEVEL-INIT          TO TRUE
           INITIALIZE PRTFILE-DATA-FIELDS
           SET PRTFILE-EOF-OFF             TO TRUE
           SET PRTFILE-PROCESS             TO TRUE
           OPEN I-O PRTFILE
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           INITIALIZE STATTAB-DATA-FIELDS
           OPEN INPUT STATTAB.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PRTFILE
           CLOSE VAGRMAS
           CLOSE STATTAB.
 
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
