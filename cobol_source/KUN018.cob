       IDENTIFICATION DIVISION.
       PROGRAM-ID. KUN018R.
      **********************************************  Z-WIN-RPG2    ***
      *  PROGRAM....: KUN018                   JOBB:XKUNIE            *
      *  PROGRAMERER: RUNE                                            *
      *  PROGRAMERT.: 14.03.2018                                      *
      *                                                               *
      *  PROGRAMMET.: LISTER UT FELTER FRA KUNDEMA OG KUNDEMX TIL FIL *
      *               INFOEASY FORMAT                                 *
      *  ENDR.DATO   TEKST.                                           *
      *  XX.XX.XXXX -                                                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KUN018.rpg
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
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(200).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD UTFILE
               BLOCK CONTAINS 2800
               RECORD CONTAINS 1400.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(1400).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INFILE-LEVEL-01.
               10  INFILE-01-L2.
                   15  INFILE-01-L2-FIRMA  PICTURE X(3).
               10  INFILE-01-L1.
                   15  INFILE-01-L1-KNR    PICTURE X(6).
           05  INFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  ALFA                    PICTURE X(4).
               10  NAVN1                   PICTURE X(30).
               10  NAVN15                  PICTURE X(15).
               10  PBOKS                   PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  PSTED                   PICTURE X(15).
               10  PNR                     PICTURE X(4).
               10  KREDGR-IO.
                   15  KREDGR              PICTURE S9(4).
               10  BGIRO-IO.
                   15  BGIRO               PICTURE S9(11).
           05  KUNDEMX-DATA-FIELDS.
      *  RECART 1
               10  TL1                     PICTURE X(16).
               10  GDPR1                   PICTURE X(1).
               10  KPERS1                  PICTURE X(30).
               10  FKNR                    PICTURE X(6).
               10  ORGNR1                  PICTURE X(9).
      *  RECART 2
               10  FAX                     PICTURE X(16).
               10  FAX13                   PICTURE X(13).
      *  RECART 6
               10  EMAIL6                  PICTURE X(60).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KEY9                    PICTURE X(9).
               10  KEY10                   PICTURE X(10).
               10  TLF                     PICTURE X(16).
               10  TLF13                   PICTURE X(13).
               10  ORGNR                   PICTURE X(9).
               10  KPERS                   PICTURE X(30).
               10  KFKNR                   PICTURE X(6).
               10  GDPR                    PICTURE X(1).
               10  EMAIL                   PICTURE X(60).
               10  EMAIL5                  PICTURE X(50).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   SET INFILE-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
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
               SET NOT-I-22                TO TRUE
      ****
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO KEY9 (1:3)
           END-IF
           IF  (I-L1)
               MOVE KNR                    TO KEY9 (4:6)
               MOVE KEY9                   TO KEY10 (1:9)
      *  RECART 1
           END-IF
           IF  (I-L1)
               MOVE '1'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-11)
               MOVE TL1                    TO TLF
               MOVE TL1 (1:13)             TO TLF13
               MOVE ORGNR1                 TO ORGNR
               MOVE KPERS1                 TO KPERS
               MOVE FKNR                   TO KFKNR
               MOVE GDPR1                  TO GDPR
               SET NOT-I-22                TO TRUE
               IF  GDPR = 'N'
                   SET I-22                TO TRUE
               END-IF
      *  RECART 6
           END-IF
           IF  (I-L1)
               MOVE '6'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-16)
               MOVE EMAIL6                 TO EMAIL
               MOVE EMAIL6 (1:50)          TO EMAIL5
      *  RECART 2
           END-IF
           IF  (I-L1)
               MOVE '2'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF.
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (6:6)   TO KNR (1:6)
               MOVE INFILE-IO-AREA (12:4)  TO ALFA (1:4)
               MOVE INFILE-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE INFILE-IO-AREA (16:15) TO NAVN15 (1:15)
               MOVE INFILE-IO-AREA (46:30) TO PBOKS (1:30)
               MOVE INFILE-IO-AREA (76:30) TO ADR (1:30)
               MOVE INFILE-IO-AREA (106:15) TO PSTED (1:15)
               MOVE INFILE-IO-AREA (121:4) TO PNR (1:4)
               MOVE INFILE-IO-AREA (129:4) TO KREDGR-IO
               INSPECT KREDGR-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (133:11) TO BGIRO-IO
               INSPECT BGIRO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (3:3)   TO INFILE-01-L2-FIRMA
               MOVE INFILE-IO-AREA (6:6)   TO INFILE-01-L1-KNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L2          TO THE-PRIOR-L2
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (11:16) TO TL1 (1:16)
               MOVE KUNDEMX-IO-AREA (44:1) TO GDPR1 (1:1)
               MOVE KUNDEMX-IO-AREA (45:30) TO KPERS1 (1:30)
               MOVE KUNDEMX-IO-AREA (161:6) TO FKNR (1:6)
               MOVE KUNDEMX-IO-AREA (180:9) TO ORGNR1 (1:9)
               MOVE KUNDEMX-IO-AREA (152:16) TO FAX (1:16)
               MOVE KUNDEMX-IO-AREA (152:13) TO FAX13 (1:13)
               MOVE KUNDEMX-IO-AREA (11:60) TO EMAIL6 (1:60)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE '8'                    TO UTFILE-IO-AREA (1:1)
               MOVE 'J'                    TO UTFILE-IO-AREA (2:1)
               MOVE KNR                    TO UTFILE-IO-AREA (3:6)
               MOVE NAVN15                 TO UTFILE-IO-AREA (9:15)
               MOVE NAVN1                  TO UTFILE-IO-AREA (24:30)
      *                        REF       83
               MOVE PBOKS                  TO UTFILE-IO-AREA (84:30)
               MOVE ADR                    TO UTFILE-IO-AREA (114:30)
               MOVE PNR                    TO UTFILE-IO-AREA (144:4)
               MOVE '  '                   TO UTFILE-IO-AREA (148:2)
               MOVE PSTED                  TO UTFILE-IO-AREA (150:15)
               MOVE '        '             TO UTFILE-IO-AREA (165:8)
               MOVE '                       ' TO UTFILE-IO-AREA
                                                              (173:23)
      *                        LAND     195
               IF  (NOT-I-11)
                   MOVE TLF13              TO UTFILE-IO-AREA (196:13)
               END-IF
               MOVE '             '        TO UTFILE-IO-AREA (209:13)
      *                        TLF2     221
               IF  (NOT-I-10)
                   MOVE FAX13              TO UTFILE-IO-AREA (222:13)
               END-IF
               MOVE '             '        TO UTFILE-IO-AREA (235:13)
      *                        MOBIL    247
               IF  (NOT-I-16)
                   MOVE EMAIL5             TO UTFILE-IO-AREA (248:50)
               END-IF
               MOVE '    '                 TO UTFILE-IO-AREA (298:4)
               MOVE '                        ' TO UTFILE-IO-AREA
                                                              (302:24)
               MOVE '                    ' TO UTFILE-IO-AREA (327:20)
               IF  (NOT-I-11)
                   MOVE ORGNR              TO UTFILE-IO-AREA (347:9)
               END-IF
               MOVE '         '            TO UTFILE-IO-AREA (356:9)
               MOVE '      '               TO UTFILE-IO-AREA (365:6)
      *                        EIERNR   370
               MOVE '1'                    TO UTFILE-IO-AREA (371:1)
               MOVE BGIRO-IO               TO UTFILE-IO-AREA (372:11)
               MOVE '  '                   TO UTFILE-IO-AREA (383:2)
               MOVE KPERS                  TO UTFILE-IO-AREA (385:30)
               MOVE '     '                TO UTFILE-IO-AREA (415:5)
               MOVE '0'                    TO UTFILE-IO-AREA (420:1)
      *                        RSKGR    420
               MOVE '  '                   TO UTFILE-IO-AREA (421:2)
      *                        SORTKD   422
               MOVE ' '                    TO UTFILE-IO-AREA (423:1)
      *                        INKAS    423
               MOVE '        '             TO UTFILE-IO-AREA (424:8)
      *                        OPDATO   431
               MOVE '        '             TO UTFILE-IO-AREA (428:8)
      *                        SIDENR   435
               MOVE '  '                   TO UTFILE-IO-AREA (436:2)
      *                        FORFAL   437
               MOVE '  '                   TO UTFILE-IO-AREA (441:2)
      *                        VALUTA   442
               MOVE ' '                    TO UTFILE-IO-AREA (443:1)
      *                        RSKTYP   443
               MOVE 'J'                    TO UTFILE-IO-AREA (444:1)
      *                        AKTIVR   444
               MOVE '     '                TO UTFILE-IO-AREA (445:5)
      *                        RENTE    449
               MOVE ' '                    TO UTFILE-IO-AREA (450:1)
      *                        SPRAK    450
               MOVE ' '                    TO UTFILE-IO-AREA (451:1)
      *                        RDIMA    451
               MOVE '  '                   TO UTFILE-IO-AREA (452:2)
      *                        DAGER    453
               MOVE '     '                TO UTFILE-IO-AREA (454:5)
      *                        SATS     458
               MOVE '      '               TO UTFILE-IO-AREA (459:6)
      *                        FACTNR   464
               MOVE '                    ' TO UTFILE-IO-AREA (465:20)
               MOVE '     '                TO UTFILE-IO-AREA (485:5)
      *                        REFNR    489
               MOVE '         '            TO UTFILE-IO-AREA (490:9)
      *                        RSKPNR   498
               MOVE '        000.00'       TO UTFILE-IO-AREA (499:14)
               MOVE KREDGR-IO              TO UTFILE-IO-AREA (503:4)
               MOVE '         '            TO UTFILE-IO-AREA (513:9)
      *                        KTONR2   521
               MOVE KNR                    TO UTFILE-IO-AREA (516:6)
               MOVE '         '            TO UTFILE-IO-AREA (522:9)
      *                        EIER2    530
               MOVE '  '                   TO UTFILE-IO-AREA (531:2)
      *                        LK       532
               MOVE '             '        TO UTFILE-IO-AREA (533:13)
      *                        GLN      545
               MOVE '                    ' TO UTFILE-IO-AREA (546:20)
               MOVE '                    ' TO UTFILE-IO-AREA (566:20)
               MOVE '          '           TO UTFILE-IO-AREA (586:10)
      *                        UNAVN    595
               MOVE '                    ' TO UTFILE-IO-AREA (596:20)
               MOVE '                    ' TO UTFILE-IO-AREA (616:20)
               MOVE '          '           TO UTFILE-IO-AREA (636:10)
      *                        UREF     645
               MOVE '                    ' TO UTFILE-IO-AREA (646:20)
               MOVE '                    ' TO UTFILE-IO-AREA (666:20)
               MOVE '          '           TO UTFILE-IO-AREA (686:10)
      *                        UADR1    695
               MOVE '                    ' TO UTFILE-IO-AREA (696:20)
               MOVE '                    ' TO UTFILE-IO-AREA (716:20)
               MOVE '          '           TO UTFILE-IO-AREA (736:10)
      *                        UADR2    745
               MOVE '               '      TO UTFILE-IO-AREA (746:15)
      *                        UPNR     760
               MOVE '                    ' TO UTFILE-IO-AREA (761:20)
               MOVE '                    ' TO UTFILE-IO-AREA (781:20)
               MOVE '                    ' TO UTFILE-IO-AREA (801:20)
               MOVE '                    ' TO UTFILE-IO-AREA (821:20)
               MOVE '                    ' TO UTFILE-IO-AREA (841:20)
      *                        UPSTED   860
               MOVE '                    ' TO UTFILE-IO-AREA (861:20)
               MOVE '                    ' TO UTFILE-IO-AREA (881:20)
               MOVE '          '           TO UTFILE-IO-AREA (901:10)
      *                        ULAND    910
               MOVE '              '       TO UTFILE-IO-AREA (911:14)
      *                        UGIRO    924
               MOVE '                    ' TO UTFILE-IO-AREA (925:20)
               MOVE '              '       TO UTFILE-IO-AREA (945:14)
      *                        IBAN     958
               MOVE '           '          TO UTFILE-IO-AREA (959:11)
      *                        BIC      969
               MOVE '         '            TO UTFILE-IO-AREA (970:9)
      *                        TILHA    978
               MOVE ' '                    TO UTFILE-IO-AREA (979:1)
      *                        PRIVAT   979
               MOVE '                    ' TO UTFILE-IO-AREA (980:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1000:20)
               MOVE '          '           TO UTFILE-IO-AREA (1020:10)
      *                        FIK     1029
               MOVE '                    ' TO UTFILE-IO-AREA (1030:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1050:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1070:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1090:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1110:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1130:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1150:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1170:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1190:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1210:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1230:20)
               MOVE '                    ' TO UTFILE-IO-AREA (1250:20)
               MOVE '          '           TO UTFILE-IO-AREA (1270:10)
               IF  (NOT-I-16)
                   MOVE EMAIL              TO UTFILE-IO-AREA (1030:60)
               END-IF
               MOVE '0'                    TO UTFILE-IO-AREA (1280:1)
               IF  (I-22)
                   MOVE '1'                TO UTFILE-IO-AREA (1280:1)
      *                        VILSL   1280
               END-IF
               MOVE '         '            TO UTFILE-IO-AREA (1281:9)
      *                        FORMAL  1289
               MOVE '         '            TO UTFILE-IO-AREA (1290:9)
      *                        SAMTYK  1298
               MOVE '        '             TO UTFILE-IO-AREA (1299:8)
      *                        DTSAMT  1306
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
           MOVE 1                          TO LR-CHECK
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           OPEN OUTPUT UTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
           CLOSE KUNDEMX
           CLOSE UTFILE.
 
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
