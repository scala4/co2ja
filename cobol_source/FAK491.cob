       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK491R.
      *****************************************************************
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring excel på Report Web *****************
      *  NYTT 22.10.09 TIL FOMA :KOPI AV FAK490 MED EGEN OPPGAVENR.   *
      *                          OPPGAVENUMMER = FAK27                *
      *  FAKTURAUTSKRIFT KONTANT.FAKTURA RUTINEN.                     *
      *  ENDR 21.06.95: FORETAKSNR BLE LAGT UT I FAKTURANR.           *
      *  ENDR 03.11.03: NY VERSJON AV KONTANTFAKTURA.                 *
      *                 BRUKER RBS.                                   *
      *                 SPILTTET LISTER I 48 LINJER OG A4 (72) PR SIDE*
      *  ENDR 04.11.03: SKRIVER A4 OG MAIL HVIS IKKE LOKALPRINTER     *
      *                 ER LAGT INN I RBS1.                           *
      *  ENDR 22.03.17: LAGT TIL FLERE LINJER (LINTYP) I UTSKRIFT. BH *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK491.rpg
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
           SELECT ENRLIM
               ASSIGN TO ENRLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ENRLIM-STATUS.
           SELECT INFILE
               ASSIGN TO INFILE
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS INFILE-STATUS
               RECORD KEY IS INFILE-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT DIVFILE
               ASSIGN TO DIVFILE
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS DIVFILE-STATUS
               RECORD KEY IS DIVFILE-KEY1.
           SELECT FAKTUR1
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUR1-STATUS.
           SELECT FAKTUR2
               ASSIGN TO UT-S-FAKTUR2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUR2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ENRLIM
               RECORD CONTAINS 80.
       01  ENRLIM-IO-AREA.
           05  ENRLIM-IO-AREA-X.
               10  ENRLIM-KEY1.
                   15  ENRLIM-KEY1N        PICTURE S9(11).
               10  FILLER                  PICTURE X(69).
       FD INFILE
               RECORD CONTAINS 146.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X.
               10  INFILE-KEY1             PICTURE X(11).
               10  FILLER                  PICTURE X(135).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD DIVFILE
               RECORD CONTAINS 400.
       01  DIVFILE-IO-AREA.
           05  DIVFILE-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  DIVFILE-KEY1            PICTURE X(11).
               10  FILLER                  PICTURE X(388).
       FD FAKTUR1
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  FAKTUR1-IO-PRINT.
           05  FAKTUR1-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 FAKTUR1-IO-AREA.
           05  FAKTUR1-IO-AREA-X           PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD FAKTUR2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  FAKTUR2-IO-PRINT.
           05  FAKTUR2-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 FAKTUR2-IO-AREA.
           05  FAKTUR2-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ENRLIM-STATUS               PICTURE 99 VALUE 0.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  DIVFILE-STATUS              PICTURE 99 VALUE 0.
           10  FAKTUR1-STATUS              PICTURE 99 VALUE 0.
           10  FAKTUR2-STATUS              PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ENRLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-EOF-OFF          VALUE '0'.
               88  ENRLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-READ-OFF         VALUE '0'.
               88  ENRLIM-READ             VALUE '1'.
           05  ENRLIM-LOW-KEY              PICTURE X(11).
           05  ENRLIM-HIGH-KEY             PICTURE X(11).
           05  INFILE-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  DIVFILE-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FAKTUR1-DATA-FIELDS.
               10  FAKTUR1-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR1-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR1-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR1-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR1-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR1-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR1-CLR-IO          PICTURE X VALUE 'Y'.
           05  FAKTUR2-DATA-FIELDS.
               10  FAKTUR2-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR2-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR2-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR2-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR2-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR2-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FAKTUR2-CLR-IO          PICTURE X VALUE 'Y'.
      *DSDS: DATA STRUCTURE FIELDS
           05  LDATA-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FIRNVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBB                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BFILL1                  PICTURE X(50).
               10  FILLER                  PICTURE X(107).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(150).
               10  BFILL2                  PICTURE X(50).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(200).
               10  BFILL3                  PICTURE X(57).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
           05  INFILE-LEVEL-01.
               10  INFILE-01-L2.
                   15  INFILE-01-L2-FIRMNR PICTURE X(3).
               10  INFILE-01-L1.
                   15  INFILE-01-L1-BRNR   PICTURE X(6).
           05  INFILE-DATA-FIELDS.
               10  LINJE                   PICTURE X(80).
               10  FIRMNR                  PICTURE X(3).
               10  BRNR                    PICTURE X(6).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(4).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(3).
               10  BEL3-IO.
                   15  BEL3                PICTURE S9(2).
               10  SIGN-X                  PICTURE X(1).
               10  LINTYP                  PICTURE X(2).
           05  FIRMAF-DATA-FIELDS.
               10  HEAD1                   PICTURE X(50).
               10  HEAD2                   PICTURE X(50).
               10  HEAD3                   PICTURE X(50).
               10  HEAD4                   PICTURE X(50).
               10  FTAKNR-IO.
                   15  FTAKNR              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  DIVFILE-DATA-FIELDS.
               10  TILPRI                  PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  DIVKY1                  PICTURE X(8).
               10  DIVKEY                  PICTURE X(11).
               10  HLIN1                   PICTURE X(80).
               10  HLIN2                   PICTURE X(80).
               10  HLIN3                   PICTURE X(80).
               10  HLIN4                   PICTURE X(80).
               10  HLIN5                   PICTURE X(80).
               10  TRSPTA-IO.
                   15  TRSPTA              PICTURE S9(9).
               10  AREAY1-IO.
                   15  AREAY1              PICTURE S9(7).
               10  AREAX1-IO.
                   15  AREAX1              PICTURE S9(9).
               10  TRSPT-IO.
                   15  TRSPT               PICTURE S9(9).
               10  TELLER-IO.
                   15  TELLER              PICTURE S9(4).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-90D                  PICTURE S9(9).
               10  XO-90U                  PICTURE 9(9).
               10  EDIT-TRSPTA             PICTURE ZZZZ.ZZZ,ZZ-.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
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
           SET NOT-I-39                    TO TRUE
           IF  (I-38)
               SET I-39                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           SET NOT-I-71                    TO TRUE
           SET NOT-I-72                    TO TRUE
           SET NOT-I-73                    TO TRUE
           SET NOT-I-74                    TO TRUE
           SET NOT-I-75                    TO TRUE
           SET NOT-I-78                    TO TRUE
           SET NOT-I-79                    TO TRUE
           SET NOT-I-80                    TO TRUE
           SET NOT-I-81                    TO TRUE
           SET NOT-I-82                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-85                    TO TRUE
           SET NOT-I-87                    TO TRUE
           IF  (I-L2)
               SET NOT-I-95                TO TRUE
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-41                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-41            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               MOVE 'FAK27'                TO DIVKY1 (1:5)
               MOVE FIRMNR                 TO DIVKY1 (6:3)
               MOVE DIVKY1                 TO DIVKEY (1:8)
               MOVE '000'                  TO DIVKEY (9:3)
               MOVE DIVKEY                 TO DIVFILE-KEY1
               READ DIVFILE RECORD KEY IS DIVFILE-KEY1
               INVALID KEY
                   SET I-42                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-42            TO TRUE
                   PERFORM DIVFILE-FLDSET
                   PERFORM DIVFILE-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-42)
               SET NOT-I-95                TO TRUE
               IF  TILPRI = 'PDF'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-42 AND NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  TILPRI NOT > '   '
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-42)
               SET I-95                    TO TRUE
           END-IF
           IF  (I-L2 AND I-95)
               PERFORM RBSPDF-S
           END-IF
           IF  (I-L2 AND NOT-I-95)
               PERFORM RBSPRI-S
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  LINTYP = '01'
               SET I-71                    TO TRUE
           END-IF
           SET NOT-I-72                    TO TRUE
           IF  LINTYP = '02'
               SET I-72                    TO TRUE
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  LINTYP = '03'
               SET I-73                    TO TRUE
           END-IF
           SET NOT-I-74                    TO TRUE
           IF  LINTYP = '04'
               SET I-74                    TO TRUE
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  LINTYP = '05'
               SET I-75                    TO TRUE
           END-IF
           SET NOT-I-78                    TO TRUE
           IF  LINTYP = '08'
               SET I-78                    TO TRUE
           END-IF
           SET NOT-I-79                    TO TRUE
           IF  LINTYP = '09'
               SET I-79                    TO TRUE
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  LINTYP = '10'
               SET I-80                    TO TRUE
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  LINTYP = '11'
               SET I-81                    TO TRUE
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  LINTYP = '12'
               SET I-82                    TO TRUE
           END-IF
           SET NOT-I-83                    TO TRUE
           IF  LINTYP = '13'
               SET I-83                    TO TRUE
           END-IF
           SET NOT-I-84                    TO TRUE
           IF  LINTYP = '14'
               SET I-84                    TO TRUE
           END-IF
           SET NOT-I-85                    TO TRUE
           IF  LINTYP = '15'
               SET I-85                    TO TRUE
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  LINTYP = '16'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE LINJE                  TO HLIN1
           END-IF
           IF  (I-72)
               MOVE LINJE                  TO HLIN2
           END-IF
           IF  (I-73)
               MOVE LINJE                  TO HLIN3
           END-IF
           IF  (I-74)
               MOVE LINJE                  TO HLIN4
           END-IF
           IF  (I-75)
               MOVE LINJE                  TO HLIN5
           END-IF
           IF  (NOT-I-78)
               GO TO SLUTT-T
           END-IF
           PERFORM AKKRUT-S
           IF  (I-95)
               SET NOT-I-38                TO TRUE
               IF  TELLER NOT < 45
                   SET I-38                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-38                TO TRUE
               IF  TELLER NOT < 22
                   SET I-38                TO TRUE
               END-IF
           END-IF
           IF  (I-38)
               MOVE 0                      TO TELLER
               MOVE TRSPT                  TO TRSPTA-IO
      *                    EXSR AKKRUT
           END-IF
           .
 
       SLUTT-T.
      *0 55                MOVE "RESTAA  "BUGFL1  8        DISPLAY FIELD
      *0 55      BUGFL1    DEBUGBUGFILO   RESTAA           VIS INDIKATOR
           CONTINUE.
 
       RBSPDF-S SECTION.
       RBSPDF-S-P.
           MOVE ' '                        TO BBEST
           MOVE 'FAK27'                    TO LONR
           MOVE FIRMNR                     TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK491  '                 TO LPROG
           MOVE 'FAK50A  '                 TO BJOBB
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS021 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSPRI-S SECTION.
       RBSPRI-S-P.
           MOVE ' '                        TO BBEST
           MOVE 'FAK27'                    TO LONR
           MOVE FIRMNR                     TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK491  '                 TO LPROG
           MOVE 'FAK50A  '                 TO BJOBB
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS021' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      *******************************************************
      ******************************************************
      *    SUBRUTINE FOR AKKUMULERING AV TRANSPORT.        *
      ******************************************************
 
       AKKRUT-S SECTION.
       AKKRUT-S-P.
           MOVE BEL2                       TO AREAY1-IO (5:3)
           MOVE BEL1                       TO AREAY1 (1:4)
           MOVE BEL3                       TO AREAX1-IO (8:2)
           MOVE AREAY1                     TO AREAX1 (1:7)
           SET NOT-I-60                    TO TRUE
           IF  SIGN-X = '-'
               SET I-60                    TO TRUE
           END-IF
           IF  (I-60)
               MULTIPLY -1 BY AREAX1   GIVING AREAX1
           END-IF
           IF  (NOT-I-60)
               MULTIPLY 1 BY AREAX1    GIVING AREAX1
           END-IF
           ADD AREAX1                      TO TRSPT
           ADD 1                           TO TELLER.
      *******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               MOVE 0                      TO TRSPT
               MOVE 0                      TO PAGE0
               MOVE 0                      TO TELLER
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ENRLIM-READ-OFF
                    OR ENRLIM-EOF
                   IF  ENRLIM-READ
                       SET ENRLIM-READ-OFF TO TRUE
                       READ ENRLIM
                       AT END
                           SET ENRLIM-EOF  TO TRUE
                           SET INFILE-EOF  TO TRUE
                       NOT AT END
                           MOVE ENRLIM-IO-AREA (1:4) TO INFILE-KEY1
                       END-READ
                   END-IF
                   IF  ENRLIM-EOF-OFF
                   AND ENRLIM-READ-OFF
                       READ INFILE
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (12:80) TO LINJE (1:80)
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (73:4)  TO BEL1-IO
               INSPECT BEL1-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (78:3)  TO BEL2-IO
               INSPECT BEL2-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (82:2)  TO BEL3-IO
               INSPECT BEL3-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (84:1)  TO SIGN-X (1:1)
               MOVE INFILE-IO-AREA (10:2)  TO LINTYP (1:2)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (144:3) TO INFILE-01-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-01-L1-BRNR
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
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (204:50) TO HEAD1 (1:50)
               MOVE FIRMAF-IO-AREA (254:50) TO HEAD2 (1:50)
               MOVE FIRMAF-IO-AREA (304:50) TO HEAD3 (1:50)
               MOVE FIRMAF-IO-AREA (354:50) TO HEAD4 (1:50)
               MOVE FIRMAF-IO-AREA (896:5) TO FTAKNR-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
       DIVFILE-FLDSET SECTION.
       DIVFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DIVFILE-IO-AREA (68:3) TO TILPRI (1:3)
           END-EVALUATE.
 
       DIVFILE-IDSET SECTION.
       DIVFILE-IDSET-P.
           SET I-03                        TO TRUE.
 
       FAKTUR1-PRINT-LINE SECTION.
       FAKTUR1-PRINT-LINE-P.
           IF  FAKTUR1-BEFORE-SKIP > 0
               PERFORM FAKTUR1-SKIP-BEFORE
           END-IF
           IF  FAKTUR1-BEFORE-SPACE > 0
               PERFORM FAKTUR1-SPACE-BEFORE
               IF  FAKTUR1-AFTER-SKIP > 0
                   PERFORM FAKTUR1-SKIP-AFTER
               END-IF
               IF  FAKTUR1-AFTER-SPACE > 0
                   PERFORM FAKTUR1-SPACE-AFTER
               END-IF
           ELSE
               IF  FAKTUR1-AFTER-SKIP > 0
                   PERFORM FAKTUR1-SKIP-AFTER
               END-IF
               PERFORM FAKTUR1-SPACE-AFTER
           END-IF
           IF  FAKTUR1-LINE-COUNT NOT < FAKTUR1-MAX-LINES
               MOVE 7                      TO FAKTUR1-AFTER-SKIP
           END-IF.
 
       FAKTUR1-SKIP-BEFORE SECTION.
       FAKTUR1-SKIP-BEFORE-P.
           WRITE FAKTUR1-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO FAKTUR1-LINE-COUNT
           MOVE 0                          TO FAKTUR1-BEFORE-SKIP
           INITIALIZE FAKTUR1-IO-AREA.
 
       FAKTUR1-SPACE-BEFORE SECTION.
       FAKTUR1-SPACE-BEFORE-P.
           WRITE FAKTUR1-IO-PRINT       AFTER FAKTUR1-BEFORE-SPACE
                                                                 LINES
           ADD FAKTUR1-BEFORE-SPACE        TO FAKTUR1-LINE-COUNT
           MOVE SPACES TO FAKTUR1-IO-AREA
           INITIALIZE FAKTUR1-IO-AREA
           MOVE 0                          TO FAKTUR1-BEFORE-SPACE.
 
       FAKTUR1-SKIP-AFTER SECTION.
       FAKTUR1-SKIP-AFTER-P.
           WRITE FAKTUR1-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO FAKTUR1-LINE-COUNT
           MOVE 0                          TO FAKTUR1-AFTER-SKIP
           INITIALIZE FAKTUR1-IO-AREA.
 
       FAKTUR1-SPACE-AFTER SECTION.
       FAKTUR1-SPACE-AFTER-P.
           WRITE FAKTUR1-IO-PRINT      BEFORE FAKTUR1-AFTER-SPACE LINES
           ADD FAKTUR1-AFTER-SPACE         TO FAKTUR1-LINE-COUNT
           INITIALIZE FAKTUR1-IO-AREA
           MOVE 0                          TO FAKTUR1-AFTER-SPACE.
 
       FAKTUR2-PRINT-LINE SECTION.
       FAKTUR2-PRINT-LINE-P.
           IF  FAKTUR2-BEFORE-SKIP > 0
               PERFORM FAKTUR2-SKIP-BEFORE
           END-IF
           IF  FAKTUR2-BEFORE-SPACE > 0
               PERFORM FAKTUR2-SPACE-BEFORE
               IF  FAKTUR2-AFTER-SKIP > 0
                   PERFORM FAKTUR2-SKIP-AFTER
               END-IF
               IF  FAKTUR2-AFTER-SPACE > 0
                   PERFORM FAKTUR2-SPACE-AFTER
               END-IF
           ELSE
               IF  FAKTUR2-AFTER-SKIP > 0
                   PERFORM FAKTUR2-SKIP-AFTER
               END-IF
               PERFORM FAKTUR2-SPACE-AFTER
           END-IF
           IF  FAKTUR2-LINE-COUNT NOT < FAKTUR2-MAX-LINES
               MOVE 7                      TO FAKTUR2-AFTER-SKIP
           END-IF.
 
       FAKTUR2-SKIP-BEFORE SECTION.
       FAKTUR2-SKIP-BEFORE-P.
           WRITE FAKTUR2-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO FAKTUR2-LINE-COUNT
           MOVE 0                          TO FAKTUR2-BEFORE-SKIP
           INITIALIZE FAKTUR2-IO-AREA.
 
       FAKTUR2-SPACE-BEFORE SECTION.
       FAKTUR2-SPACE-BEFORE-P.
           WRITE FAKTUR2-IO-PRINT       AFTER FAKTUR2-BEFORE-SPACE
                                                                 LINES
           ADD FAKTUR2-BEFORE-SPACE        TO FAKTUR2-LINE-COUNT
           MOVE SPACES TO FAKTUR2-IO-AREA
           INITIALIZE FAKTUR2-IO-AREA
           MOVE 0                          TO FAKTUR2-BEFORE-SPACE.
 
       FAKTUR2-SKIP-AFTER SECTION.
       FAKTUR2-SKIP-AFTER-P.
           WRITE FAKTUR2-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO FAKTUR2-LINE-COUNT
           MOVE 0                          TO FAKTUR2-AFTER-SKIP
           INITIALIZE FAKTUR2-IO-AREA.
 
       FAKTUR2-SPACE-AFTER SECTION.
       FAKTUR2-SPACE-AFTER-P.
           WRITE FAKTUR2-IO-PRINT      BEFORE FAKTUR2-AFTER-SPACE LINES
           ADD FAKTUR2-AFTER-SPACE         TO FAKTUR2-LINE-COUNT
           INITIALIZE FAKTUR2-IO-AREA
           MOVE 0                          TO FAKTUR2-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-71 AND I-95 AND NOT-I-86)
           OR  (I-39 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE 'K O N T A N T   F A K T' TO FAKTUR1-IO-AREA
                                                               (27:23)
               MOVE ' U R A'               TO FAKTUR1-IO-AREA (50:6)
               MOVE 01                     TO FAKTUR1-BEFORE-SKIP
               MOVE 1                      TO FAKTUR1-BEFORE-SPACE
               MOVE 2                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD1              TO FAKTUR1-IO-AREA (4:50)
               END-IF
               MOVE 'SIDE'                 TO FAKTUR1-IO-AREA (65:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO FAKTUR1-IO-AREA (69:4)
               MOVE 1                      TO FAKTUR1-BEFORE-SPACE
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD2              TO FAKTUR1-IO-AREA (4:50)
               END-IF
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD3              TO FAKTUR1-IO-AREA (4:50)
               END-IF
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD4              TO FAKTUR1-IO-AREA (4:50)
               END-IF
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE 'FORETAKSREGISTERET '  TO FAKTUR1-IO-AREA (4:19)
               MOVE 'NO '                  TO FAKTUR1-IO-AREA (23:3)
               MOVE FTAKNR                 TO XO-90U
               MOVE XO-90U (1:9)           TO FAKTUR1-IO-AREA (28:9)
               MOVE ' MVA'                 TO FAKTUR1-IO-AREA (37:4)
               MOVE 2                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE HLIN1                  TO FAKTUR1-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-72 AND I-95 AND NOT-I-86)
           OR  (I-39 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE HLIN2                  TO FAKTUR1-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-73 AND I-95 AND NOT-I-86)
           OR  (I-39 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE HLIN3                  TO FAKTUR1-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-74 AND I-95 AND NOT-I-86)
           OR  (I-39 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE HLIN4                  TO FAKTUR1-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-75 AND I-95 AND NOT-I-86)
           OR  (I-39 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE HLIN5                  TO FAKTUR1-IO-AREA (1:80)
               MOVE 2                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE 'ORDRENR'              TO FAKTUR1-IO-AREA (4:7)
               MOVE 'KUNDENR'              TO FAKTUR1-IO-AREA (24:7)
               MOVE 'BELØP'                TO FAKTUR1-IO-AREA (68:5)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-39 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE 'TRANSPORT'            TO FAKTUR1-IO-AREA (47:9)
               DIVIDE 100 INTO TRSPTA GIVING EDIT-TRSPTA
               MOVE EDIT-TRSPTA            TO FAKTUR1-IO-AREA (62:12)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-78 AND I-95 AND NOT-I-86)
           OR  (I-79 AND I-95 AND NOT-I-86)
           OR  (I-80 AND I-95 AND NOT-I-86)
           OR  (I-81 AND I-95 AND NOT-I-86)
           OR  (I-82 AND I-95 AND NOT-I-86)
           OR  (I-83 AND I-95 AND NOT-I-86)
           OR  (I-84 AND I-95 AND NOT-I-86)
           OR  (I-85 AND I-95 AND NOT-I-86)
           OR  (I-87 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE LINJE                  TO FAKTUR1-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-38 AND I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE '------------'         TO FAKTUR1-IO-AREA (62:12)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
               MOVE SPACES TO FAKTUR1-IO-AREA
               INITIALIZE FAKTUR1-IO-AREA
               MOVE 'TRANSPORT'            TO FAKTUR1-IO-AREA (47:9)
               DIVIDE 100 INTO TRSPTA GIVING EDIT-TRSPTA
               MOVE EDIT-TRSPTA            TO FAKTUR1-IO-AREA (62:12)
               MOVE 1                      TO FAKTUR1-AFTER-SPACE
               PERFORM FAKTUR1-PRINT-LINE
           END-IF
           IF  (I-71 AND NOT-I-95 AND NOT-I-86)
           OR  (I-39 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE 'K O N T A N T   F A K T' TO FAKTUR2-IO-AREA
                                                               (27:23)
               MOVE ' U R A'               TO FAKTUR2-IO-AREA (50:6)
               MOVE 01                     TO FAKTUR2-BEFORE-SKIP
               MOVE 1                      TO FAKTUR2-BEFORE-SPACE
               MOVE 2                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD1              TO FAKTUR2-IO-AREA (4:50)
               END-IF
               MOVE 'SIDE'                 TO FAKTUR2-IO-AREA (65:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO FAKTUR2-IO-AREA (69:4)
               MOVE 1                      TO FAKTUR2-BEFORE-SPACE
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD2              TO FAKTUR2-IO-AREA (4:50)
               END-IF
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD3              TO FAKTUR2-IO-AREA (4:50)
               END-IF
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               IF  (NOT-I-41)
                   MOVE HEAD4              TO FAKTUR2-IO-AREA (4:50)
               END-IF
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE 'FORETAKSREGISTERET '  TO FAKTUR2-IO-AREA (4:19)
               MOVE 'NO '                  TO FAKTUR2-IO-AREA (23:3)
               IF FTAKNR < 0
                 MOVE FTAKNR               TO XO-90D
                 MOVE XO-90D (1:9)         TO FAKTUR2-IO-AREA (28:9)
               ELSE
                 MOVE FTAKNR               TO XO-90U
                 MOVE XO-90U (1:9)         TO FAKTUR2-IO-AREA (28:9)
               END-IF
               MOVE ' MVA'                 TO FAKTUR2-IO-AREA (37:4)
               MOVE 2                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE HLIN1                  TO FAKTUR2-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF
           IF  (I-72 AND NOT-I-95 AND NOT-I-86)
           OR  (I-39 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE HLIN2                  TO FAKTUR2-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF
           IF  (I-73 AND NOT-I-95 AND NOT-I-86)
           OR  (I-39 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE HLIN3                  TO FAKTUR2-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF
           IF  (I-74 AND NOT-I-95 AND NOT-I-86)
           OR  (I-39 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE HLIN4                  TO FAKTUR2-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF
           IF  (I-75 AND NOT-I-95 AND NOT-I-86)
           OR  (I-39 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE HLIN5                  TO FAKTUR2-IO-AREA (1:80)
               MOVE 2                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE 'ORDRENR'              TO FAKTUR2-IO-AREA (4:7)
               MOVE 'KUNDENR'              TO FAKTUR2-IO-AREA (24:7)
               MOVE 'BELØP'                TO FAKTUR2-IO-AREA (68:5)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF
           IF  (I-39 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE 'TRANSPORT'            TO FAKTUR2-IO-AREA (47:9)
               DIVIDE 100 INTO TRSPTA GIVING EDIT-TRSPTA
               MOVE EDIT-TRSPTA            TO FAKTUR2-IO-AREA (62:12)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF
           IF  (I-78 AND NOT-I-95 AND NOT-I-86)
           OR  (I-79 AND NOT-I-95 AND NOT-I-86)
           OR  (I-80 AND NOT-I-95 AND NOT-I-86)
           OR  (I-81 AND NOT-I-95 AND NOT-I-86)
           OR  (I-82 AND NOT-I-95 AND NOT-I-86)
           OR  (I-83 AND NOT-I-95 AND NOT-I-86)
           OR  (I-84 AND NOT-I-95 AND NOT-I-86)
           OR  (I-85 AND NOT-I-95 AND NOT-I-86)
           OR  (I-87 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE LINJE                  TO FAKTUR2-IO-AREA (1:80)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF
           IF  (I-38 AND NOT-I-95 AND NOT-I-86)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE '------------'         TO FAKTUR2-IO-AREA (62:12)
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE 'TRANSPORT'            TO FAKTUR2-IO-AREA (47:9)
               DIVIDE 100 INTO TRSPTA GIVING EDIT-TRSPTA
               MOVE EDIT-TRSPTA            TO FAKTUR2-IO-AREA (62:12)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 1                      TO FAKTUR2-AFTER-SPACE
               PERFORM FAKTUR2-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-U7 AND I-U8 AND I-U3)
           AND (I-01 AND I-02 AND I-03)
           AND (I-94)
               MOVE SPACES TO FAKTUR2-IO-AREA
               INITIALIZE FAKTUR2-IO-AREA
               MOVE LONR                   TO FAKTUR2-IO-AREA (76:5)
               MOVE LFIRMA                 TO FAKTUR2-IO-AREA (78:3)
               MOVE LUNDGR                 TO FAKTUR2-IO-AREA (78:3)
               MOVE LPROG                  TO FAKTUR2-IO-AREA (73:8)
               MOVE LANTX-IO               TO FAKTUR2-IO-AREA (78:3)
               MOVE LPRIID                 TO FAKTUR2-IO-AREA (77:4)
               MOVE BJOBB                  TO FAKTUR2-IO-AREA (73:8)
               MOVE BBEST                  TO FAKTUR2-IO-AREA (80:1)
               MOVE BFILL1                 TO FAKTUR2-IO-AREA (31:50)
               MOVE BFILL2                 TO FAKTUR2-IO-AREA (31:50)
               MOVE BFILL3                 TO FAKTUR2-IO-AREA (24:57)
               MOVE FIRNVN                 TO FAKTUR2-IO-AREA (51:30)
               MOVE 1                      TO FAKTUR2-BEFORE-SPACE
               PERFORM FAKTUR2-PRINT-LINE
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
           SET ENRLIM-EOF-OFF              TO TRUE
           SET ENRLIM-READ                 TO TRUE
           OPEN INPUT ENRLIM
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE DIVFILE-DATA-FIELDS
           OPEN INPUT DIVFILE
           OPEN OUTPUT FAKTUR1
           INITIALIZE FAKTUR1-IO-AREA
           INITIALIZE FAKTUR1-DATA-FIELDS
           MOVE 57                         TO FAKTUR1-MAX-LINES
           OPEN OUTPUT FAKTUR2
           INITIALIZE FAKTUR2-IO-AREA
           INITIALIZE FAKTUR2-DATA-FIELDS
           MOVE 57                         TO FAKTUR2-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ENRLIM
           CLOSE INFILE
           CLOSE FIRMAF
           CLOSE DIVFILE
           IF FAKTUR1-IO-AREA NOT = SPACES
             WRITE FAKTUR1-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO FAKTUR1-IO-AREA
           END-IF
           CLOSE FAKTUR1
           IF FAKTUR2-IO-AREA NOT = SPACES
             WRITE FAKTUR2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO FAKTUR2-IO-AREA
           END-IF
           CLOSE FAKTUR2.
 
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
