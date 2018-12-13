       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK032R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM FAK032      ESPEN LARSEN    JAN. 1997                 *
      * LESER KUNDEMX OG DANNER TABELL MED FIRMA,KUNDENR,FAKT.KUNDENR.*
      * UPSI 1 LISTE UT HELE TABELLEN MED KUNDENAVN.                  *
      * UPSI 2 LISTE UT OVERFØRING TIL SCANGROSS (GJENNOMFAKTURERING) *
      * 18.11.1998 NY RUTINE FOR Å DANNE GJ.FAKT.TABELL SCANGROSS     *
      *            GML. TEST PÅ POSTNR ER FJERNET.                    *
      * 4.04.2002 SCANGROSS ØNSKER IKKE GJENNOMFAKTURERING.           *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK032.rpg
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
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FKNRTAB
               ASSIGN TO UT-S-FKNRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKNRTAB-STATUS.
           SELECT FGFTAB1
               ASSIGN TO UT-S-FGFTAB1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FGFTAB1-STATUS.
           SELECT TABPUN
               ASSIGN TO UT-S-TABPUN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABPUN-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1.
                   15  KUNDEMX-KEY1N       PICTURE S9(10).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD FKNRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  FKNRTAB-IO-AREA.
           05  FKNRTAB-IO-AREA-X           PICTURE X(40).
       FD FGFTAB1
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  FGFTAB1-IO-AREA.
           05  FGFTAB1-IO-AREA-X           PICTURE X(40).
       FD TABPUN
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABPUN-IO-AREA.
           05  TABPUN-IO-AREA-X            PICTURE X(80).
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
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FKNRTAB-STATUS              PICTURE 99 VALUE 0.
           10  FGFTAB1-STATUS              PICTURE 99 VALUE 0.
           10  TABPUN-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-EOF-OFF         VALUE '0'.
               88  KUNDEMX-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-READ-OFF        VALUE '0'.
               88  KUNDEMX-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-PROCESS-OFF     VALUE '0'.
               88  KUNDEMX-PROCESS         VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  KUNDEMX-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  FAKKNR                  PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  FKNAVN                  PICTURE X(30).
               10  FKSTED                  PICTURE X(15).
               10  FKPNR                   PICTURE X(4).
               10  SLETTS                  PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7).
               10  KNRKEY                  PICTURE X(9).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(7).
               10  ANTS-IO.
                   15  ANTS                PICTURE S9(7).
               10  ANTGF-IO.
                   15  ANTGF               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
 
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KUNDEMX-PROCESS
               SET KUNDEMX-PROCESS-OFF     TO TRUE
               SET KUNDEMX-READ            TO TRUE
           END-IF
 
           IF  KUNDEMX-READ
           AND RECORD-SELECTED-OFF
               PERFORM KUNDEMX-GET
               SET KUNDEMX-READ-OFF        TO TRUE
               IF  NOT KUNDEMX-EOF
                   SET KUNDEMX-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KUNDEMX-PROCESS
               PERFORM KUNDEMX-IDSET
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
           PERFORM HEADING-OVERFLOW
 
           IF  KUNDEMX-PROCESS
               PERFORM KUNDEMX-FLDSET
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
           SET NOT-I-30                    TO TRUE
           SET NOT-I-31                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-35                    TO TRUE
           SET NOT-I-21                    TO TRUE
           IF  RECART = '1'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO ANT
           SET NOT-I-22                    TO TRUE
           IF  FAKKNR > '000000'
               SET I-22                    TO TRUE
           END-IF
           IF  (NOT-I-22)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  FAKKNR = KNR
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               ADD 1                       TO ANTF
               GO TO SLUTT-T
      ******************************************************
      *    SJEKKE OM FAK.KUNDENR. FINNES I KUNDEMASTER.    *
      ******************************************************
           END-IF
           MOVE FNR                        TO KNRKEY (1:3)
           MOVE FAKKNR                     TO KNRKEY (4:6)
           MOVE KNRKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-31                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-31                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  SLETTS = 'S'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-31)
               ADD 1                       TO ANTF
               GO TO SLUTT-T
           END-IF
           SET I-30                        TO TRUE
      *****************************************************************
      * RUTINE FOR Å DANNE TABELL FOR GJENNOMFAKT. SCANGROSS          *
      * VED NYE FIRMA MÅ DENNE RUTINE OPPDATERES.                     *
      *****************************************************************
      *          FNR       COMP "634"                    38 FIRMA 634
      *  38      FAKKNR    COMP "120500"                 35 SCANGROSS GJ.FAKT.
      *  38 35             GOTO ENDSCA                      GJENNOMFAKT.
      *
      *          FNR       COMP "682"                    38 FIRMA 682
      *  38      FAKKNR    COMP "101001"                 35 SCANGROSS GJ.FAKT.
      *  38 35             GOTO ENDSCA                      GJENNOMFAKT.
      *
      *          FNR       COMP "980"                    38 FIRMA 980
      *  38      FAKKNR    COMP "199999"                 35 SCANGROSS GJ.FAKT.
      *  38 35             GOTO ENDSCA                      GJENNOMFAKT.
      *
      *          ENDSCA    TAG
      *****************************************************************
           IF  (I-30)
               ADD 1                       TO ANTS
           END-IF
           IF  (I-30 AND I-35)
               ADD 1                       TO ANTGF
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       KUNDEMX-GET SECTION.
       KUNDEMX-GET-P.
           IF  KUNDEMX-EOF-OFF
               READ KUNDEMX
               AT END
                   SET KUNDEMX-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (1:3)  TO FNR (1:3)
               MOVE KUNDEMX-IO-AREA (4:6)  TO KNR (1:6)
               MOVE KUNDEMX-IO-AREA (10:1) TO RECART (1:1)
               MOVE KUNDEMX-IO-AREA (161:6) TO FAKKNR (1:6)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO FKNAVN (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO FKSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO FKPNR (1:4)
               MOVE KUNDEMA-IO-AREA (189:1) TO SLETTS (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
           IF  (I-02 AND I-30)
               MOVE SPACES TO FKNRTAB-IO-AREA
               INITIALIZE FKNRTAB-IO-AREA
               MOVE FNR                    TO FKNRTAB-IO-AREA (1:3)
               MOVE KNR                    TO FKNRTAB-IO-AREA (4:6)
               MOVE FAKKNR                 TO FKNRTAB-IO-AREA (10:6)
               WRITE FKNRTAB-IO-AREA
           END-IF
           IF  (I-02 AND I-30 AND I-35)
               MOVE SPACES TO FGFTAB1-IO-AREA
               INITIALIZE FGFTAB1-IO-AREA
               MOVE FNR                    TO FGFTAB1-IO-AREA (1:3)
               MOVE KNR                    TO FGFTAB1-IO-AREA (4:6)
               MOVE FAKKNR                 TO FGFTAB1-IO-AREA (10:6)
               WRITE FGFTAB1-IO-AREA
           END-IF
           IF  (I-02 AND I-30 AND I-35)
           AND (I-U3)
               MOVE SPACES TO TABPUN-IO-AREA
               INITIALIZE TABPUN-IO-AREA
               MOVE 'FRA:'                 TO TABPUN-IO-AREA (1:4)
               MOVE FNR                    TO TABPUN-IO-AREA (5:3)
               MOVE ','                    TO TABPUN-IO-AREA (8:1)
               MOVE KNR                    TO TABPUN-IO-AREA (9:6)
               MOVE 'TIL:'                 TO TABPUN-IO-AREA (17:4)
               MOVE '923'                  TO TABPUN-IO-AREA (21:3)
               MOVE ','                    TO TABPUN-IO-AREA (24:1)
               MOVE 'XXXXXX'               TO TABPUN-IO-AREA (25:6)
               IF  I-U3
                   WRITE TABPUN-IO-AREA
               END-IF
           END-IF
           IF  (I-02 AND I-30 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (2:3)
               MOVE KNR                    TO LISTE-IO-AREA (15:6)
               MOVE FAKKNR                 TO LISTE-IO-AREA (35:6)
               MOVE FKNAVN                 TO LISTE-IO-AREA (43:30)
               MOVE FKPNR                  TO LISTE-IO-AREA (74:4)
               MOVE FKSTED                 TO LISTE-IO-AREA (80:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-35 AND I-U2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (2:3)
               MOVE KNR                    TO LISTE-IO-AREA (15:6)
               MOVE FAKKNR                 TO LISTE-IO-AREA (35:6)
               MOVE FKNAVN                 TO LISTE-IO-AREA (43:30)
               MOVE FKPNR                  TO LISTE-IO-AREA (74:4)
               MOVE FKSTED                 TO LISTE-IO-AREA (80:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-31)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (2:3)
               MOVE KNR                    TO LISTE-IO-AREA (15:6)
               MOVE FAKKNR                 TO LISTE-IO-AREA (35:6)
               MOVE 'FEIL FAKT.KUNDENR.'   TO LISTE-IO-AREA (53:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-23)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (2:3)
               MOVE KNR                    TO LISTE-IO-AREA (15:6)
               MOVE FAKKNR                 TO LISTE-IO-AREA (35:6)
               MOVE 'KUNDENR. = FAKT.KUNDENR.' TO LISTE-IO-AREA (53:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE ' FAK032 TAB  FOR OVERFØR' TO LISTE-IO-AREA (25:24)
               MOVE 'ING AV FAKTURA-KUNDENR. ' TO LISTE-IO-AREA (49:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR'                  TO LISTE-IO-AREA (2:3)
               MOVE 'KUNDENR'              TO LISTE-IO-AREA (14:7)
               MOVE 'FAKT.KUNDENR'         TO LISTE-IO-AREA (29:12)
               MOVE 'FEILMELDING.      '   TO LISTE-IO-AREA (53:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE ' FAK032 TAB  FOR OVERFØR' TO LISTE-IO-AREA (25:24)
               MOVE 'ING AV FAKTURA-KUNDENR. ' TO LISTE-IO-AREA (49:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR'                  TO LISTE-IO-AREA (2:3)
               MOVE 'KUNDENR'              TO LISTE-IO-AREA (14:7)
               MOVE 'FAKT.KUNDENR'         TO LISTE-IO-AREA (29:12)
               MOVE 'FEILMELDING.      '   TO LISTE-IO-AREA (53:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT                    TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (32:9)
               MOVE 'REC. LEST.        '   TO LISTE-IO-AREA (42:18)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTF                   TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (32:9)
               MOVE 'FEIL FAKT.KUNDENR.'   TO LISTE-IO-AREA (42:18)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTS                   TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (32:9)
               MOVE 'TABELLREC. DANNET '   TO LISTE-IO-AREA (42:18)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTGF                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (32:9)
               MOVE 'GF.TAB.REC. DANNET '  TO LISTE-IO-AREA (42:19)
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE KUNDEMX-DATA-FIELDS
           SET KUNDEMX-EOF-OFF             TO TRUE
           SET KUNDEMX-PROCESS             TO TRUE
           OPEN INPUT KUNDEMX
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT FKNRTAB
           OPEN OUTPUT FGFTAB1
           IF I-U3
               OPEN OUTPUT TABPUN
           END-IF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KUNDEMX
           CLOSE KUNDEMA
           CLOSE FKNRTAB
           CLOSE FGFTAB1
           IF I-U3
               CLOSE TABPUN
           END-IF
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
