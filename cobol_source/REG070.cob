       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG070R.
      **********************************************  Z-WIN-RPG2   ****
      *PROGRAM REG070 AV ESPEN LARSEN  22.01.2003                     *
      *SELEKSJON AV REGNSKAPSDATA SOM SKAL OPPDATERES I MARKEDS.-     *
      *          UTGIFTER.MASTER                                      *
      * 1: FIRMA 918 MED BILAGSART 0-3, HOVEDBOKSKONTO 73XX, SOM HAR  *
      *    GYLDIG KUNDENR. I TEKSTFELT POS 1-6.                       *
      *                                                               *
      * UPSI 1 : SKRIVER TOTALER PÅ PRINTER.                          *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG070.rpg
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
           SELECT REGFIL
               ASSIGN TO UT-S-REGFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGFIL-STATUS.
           SELECT MUFILE
               ASSIGN TO UT-S-MUFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MUFILE-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KONTOMA
               ASSIGN TO KONTOMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTOMA-STATUS
               RECORD KEY IS KONTOMA-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REGFIL
               BLOCK CONTAINS 240
               RECORD CONTAINS 120.
       01  REGFIL-IO-AREA.
           05  REGFIL-IO-AREA-X            PICTURE X(120).
       FD MUFILE
               BLOCK CONTAINS 100
               RECORD CONTAINS 50.
       01  MUFILE-IO-AREA.
           05  MUFILE-IO-AREA-X            PICTURE X(50).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD KONTOMA
               RECORD CONTAINS 60.
       01  KONTOMA-IO-AREA.
           05  KONTOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KONTOMA-KEY1            PICTURE X(7).
               10  FILLER                  PICTURE X(52).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REGFIL-STATUS               PICTURE 99 VALUE 0.
           10  MUFILE-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFIL-EOF-OFF          VALUE '0'.
               88  REGFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFIL-READ-OFF         VALUE '0'.
               88  REGFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFIL-PROCESS-OFF      VALUE '0'.
               88  REGFIL-PROCESS          VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LISTE-DATA-FIELDS.
               10  LISTE-AFTER-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-AFTER-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-MAX-LINES         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-LINE-COUNT        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-CLR-IO            PICTURE X VALUE 'Y'.
           05  REGFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  BILART                  PICTURE X(1).
               10  BILNR                   PICTURE X(6).
               10  BILAAR                  PICTURE X(2).
               10  BILMND                  PICTURE X(2).
               10  BILDAG                  PICTURE X(2).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REGN1                   PICTURE X(1).
               10  PERAAR                  PICTURE X(2).
               10  HBKTO                   PICTURE X(4).
               10  AVGIFT                  PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  KONTOMA-DATA-FIELDS.
               10  ALTKTO                  PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  TOTREC-IO.
                   15  TOTREC              PICTURE S9(6).
               10  KTOKEY                  PICTURE X(7).
               10  RESKEY                  PICTURE X(9).
               10  BELOP-IO.
                   15  BELOP               PICTURE S9(7)V9(2).
               10  SELREC-IO.
                   15  SELREC              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YNZ                PICTURE ZZZZZZ.
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
               88  NOT-SET-I-OF            VALUE '0'.
               88  SET-I-OF                VALUE '1'.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  REGFIL-PROCESS
               SET REGFIL-PROCESS-OFF      TO TRUE
               SET REGFIL-READ             TO TRUE
           END-IF
 
           IF  REGFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM REGFIL-GET
               SET REGFIL-READ-OFF         TO TRUE
               IF  NOT REGFIL-EOF
                   SET REGFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REGFIL-PROCESS
               PERFORM REGFIL-IDSET
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  REGFIL-PROCESS
               PERFORM REGFIL-FLDOFF
               PERFORM REGFIL-FLDSET
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
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               SET NOT-I-55                TO TRUE
               ADD 1                       TO TOTREC
      *****************************************************************
      * SELEKSJONSRUTINE 1:                                           *
      *    FIRMA 918 MED BILAGSART 0-3, HOVEDBOKSKONTO 73XX, SOM HAR  *
      *    GYLDIG KUNDENR. I TEKSTFELT POS 1-6.                       *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  FIRMA = '918'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO FIRM02-T
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  BILART NOT > '3'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  KUNDNR > '000000'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE FIRMA                  TO KTOKEY (1:3)
               MOVE HBKTO                  TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-89                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-89            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-89)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  ALTKTO NOT < '7300'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51)
               SET NOT-I-52                TO TRUE
               IF  ALTKTO NOT > '7399'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND NOT-I-52)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET I-55                    TO TRUE
               GO TO KUNRUT-T
      *****************************************************************
      * SELEKSJONSRUTINE 1:                                           *
      *    FIRMA 999 MED BILAGSART 0-3, HOVEDBOKSKONTO 4XXX, SOM HAR  *
      *    GYLDIG KUNDENR. I TEKSTFELT POS 1-6.                       *
      *****************************************************************
           END-IF
           .
 
       FIRM02-T.
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  FIRMA = '998'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  FIRMA = '999'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO FIRM03-T
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  BILART NOT > '3'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  HBKTO NOT < '3000'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51)
               SET NOT-I-52                TO TRUE
               IF  HBKTO NOT > '6999'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND NOT-I-52)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  KUNDNR > '000000'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               GO TO KUNRUT-T
      *****************************************************************
           END-IF
           .
 
       FIRM03-T.
           IF  (I-01)
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR Å KUNDENR. KONTROLL                               *
      *****************************************************************
           END-IF
           .
 
       KUNRUT-T.
           IF  (I-01)
               MOVE FIRMA                  TO RESKEY (1:3)
               MOVE KUNDNR                 TO RESKEY (4:6)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-90                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-90            TO TRUE
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-90)
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR Å ENDRE BELØP.                                    *
      *****************************************************************
           END-IF
           IF  (I-01 AND I-10)
               ADD BEL1 TO ZERO        GIVING BELOP
           END-IF
           IF  (I-01 AND NOT-I-10)
               SUBTRACT BEL1 FROM ZERO GIVING BELOP
           END-IF
           IF  (I-01)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND I-50)
               ADD 1                       TO SELREC
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       REGFIL-GET SECTION.
       REGFIL-GET-P.
           IF  REGFIL-EOF-OFF
               READ REGFIL
               AT END
                   SET REGFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REGFIL-FLDOFF SECTION.
       REGFIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-10                TO TRUE
           END-EVALUATE.
 
       REGFIL-FLDSET SECTION.
       REGFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE REGFIL-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE REGFIL-IO-AREA (6:1)   TO BILART (1:1)
               MOVE REGFIL-IO-AREA (7:6)   TO BILNR (1:6)
               MOVE REGFIL-IO-AREA (13:2)  TO BILAAR (1:2)
               MOVE REGFIL-IO-AREA (15:2)  TO BILMND (1:2)
               MOVE REGFIL-IO-AREA (17:2)  TO BILDAG (1:2)
               MOVE REGFIL-IO-AREA (27:5)  TO BEL1-IO
               MOVE REGFIL-IO-AREA (32:1)  TO REGN1 (1:1)
               IF  REGN1 = SPACES
                   SET I-10                TO TRUE
               END-IF
               MOVE REGFIL-IO-AREA (43:2)  TO PERAAR (1:2)
               MOVE REGFIL-IO-AREA (47:4)  TO HBKTO (1:4)
               MOVE REGFIL-IO-AREA (56:1)  TO AVGIFT (1:1)
               MOVE REGFIL-IO-AREA (97:6)  TO KUNDNR (1:6)
           END-EVALUATE.
 
       REGFIL-IDSET SECTION.
       REGFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (45:4) TO ALTKTO (1:4)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       LISTE-PRINT-LINE SECTION.
       LISTE-PRINT-LINE-P.
           IF  LISTE-BEFORE-SKIP > 0
               PERFORM LISTE-SKIP-BEFORE
           END-IF
           IF  LISTE-BEFORE-SPACE > 0
               PERFORM LISTE-SPACE-BEFORE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               IF  LISTE-AFTER-SPACE > 0
                   PERFORM LISTE-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               PERFORM LISTE-SPACE-AFTER
           END-IF
           IF  LISTE-LINE-COUNT NOT < LISTE-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       LISTE-SKIP-BEFORE SECTION.
       LISTE-SKIP-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-BEFORE-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-BEFORE SECTION.
       LISTE-SPACE-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER LISTE-BEFORE-SPACE LINES
           ADD LISTE-BEFORE-SPACE          TO LISTE-LINE-COUNT
           MOVE SPACES TO LISTE-IO-AREA
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-BEFORE-SPACE.
 
       LISTE-SKIP-AFTER SECTION.
       LISTE-SKIP-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-AFTER-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-AFTER SECTION.
       LISTE-SPACE-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE LISTE-AFTER-SPACE LINES
           ADD LISTE-AFTER-SPACE           TO LISTE-LINE-COUNT
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO MUFILE-IO-AREA
               INITIALIZE MUFILE-IO-AREA
               MOVE FIRMA                  TO MUFILE-IO-AREA (1:3)
               MOVE PERAAR                 TO MUFILE-IO-AREA (4:2)
               MOVE KUNDNR                 TO MUFILE-IO-AREA (6:6)
               IF  (NOT-I-55)
                   MOVE HBKTO              TO MUFILE-IO-AREA (12:4)
               END-IF
               IF  (I-55)
                   MOVE ALTKTO             TO MUFILE-IO-AREA (12:4)
               END-IF
               MOVE BILNR                  TO MUFILE-IO-AREA (16:6)
               MOVE BILDAG                 TO MUFILE-IO-AREA (22:2)
               MOVE BILMND                 TO MUFILE-IO-AREA (24:2)
               MOVE BILAAR                 TO MUFILE-IO-AREA (26:2)
               MOVE BELOP-IO               TO MUFILE-IO-AREA (28:9)
               MOVE HBKTO                  TO MUFILE-IO-AREA (37:4)
               WRITE MUFILE-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. REG070 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC PÅ REGFILE'   TO LISTE-IO-AREA (3:18)
               MOVE TOTREC                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (30:6)
               INITIALIZE TOTREC
               MOVE 'DATO'                 TO LISTE-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC SELEKTERT '   TO LISTE-IO-AREA (3:18)
               MOVE SELREC                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (30:6)
               INITIALIZE SELREC
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
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
           INITIALIZE REGFIL-DATA-FIELDS
           SET REGFIL-EOF-OFF              TO TRUE
           SET REGFIL-PROCESS              TO TRUE
           OPEN INPUT REGFIL
           OPEN OUTPUT MUFILE
           OPEN INPUT KUNDEMA
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGFIL
           CLOSE MUFILE
           CLOSE KUNDEMA
           CLOSE KONTOMA
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
