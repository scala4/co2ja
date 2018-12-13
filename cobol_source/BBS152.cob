       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBS152R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: BBS152, AUTOGIRO FEILLISTE.                  *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: DOP12UD                                      *
      *  LAGET DATO....: 26.06.00                                     *
      *  ENDRET........: XX.XX.XX                                     *
      *  RETTET........:                                              *
      *  INPUT.........: AUTOGIRO FRA BBS                             *
      *                  KUNDEMA.                                     *
      *  BEHANDLING....: LISTER AUTOGIROPOSTER MOTTATT FRA BBS.       *
      *  OUTPUT........: KVITTERINGSLISTE.                            *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BBS152.rpg
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
           SELECT BBSREC
               ASSIGN TO UT-S-BBSREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSREC-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT LISTE2
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BBSREC
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  BBSREC-IO-AREA.
           05  BBSREC-IO-AREA-X            PICTURE X(80).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD LISTE2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE2-IO-PRINT.
           05  LISTE2-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE2-IO-AREA.
           05  LISTE2-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BBSREC-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE2-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-EOF-OFF          VALUE '0'.
               88  BBSREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-READ-OFF         VALUE '0'.
               88  BBSREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-PROCESS-OFF      VALUE '0'.
               88  BBSREC-PROCESS          VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LISTE2-DATA-FIELDS.
               10  LISTE2-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-CLR-IO           PICTURE X VALUE 'Y'.
      *PSDS: DATA STRUCTURE FIELDS
           05  PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(28).
               10  R                       PICTURE X(8).
               10  FILLER                  PICTURE X(44).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  P-IO.
                   15  P                   PICTURE S9(3).
               10  FILLER                  PICTURE X(41).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(10).
               10  S-IO.
                   15  S                   PICTURE S9(5).
               10  FILLER                  PICTURE X(65).
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
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  BBSREC-DATA-FIELDS.
               10  OPPTYP                  PICTURE X(2).
               10  BBSKTO                  PICTURE X(11).
               10  FIRMA                   PICTURE X(3).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  KNR                     PICTURE X(6).
               10  PERI                    PICTURE X(2).
               10  AAR                     PICTURE X(1).
               10  FEILKD                  PICTURE X(3).
               10  SUMANT-IO.
                   15  SUMANT              PICTURE S9(8).
               10  BBSDAT                  PICTURE X(6).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(7)V9(2).
           05  KUNDEMA-DATA-FIELDS.
               10  KUNAVN                  PICTURE X(30).
      *
           05  TEMPORARY-FIELDS.
               10  FFNR                    PICTURE X(3).
               10  RESKEY                  PICTURE X(9).
               10  TBEL-IO.
                   15  TBEL                PICTURE S9(7)V9(2).
               10  TANT-IO.
                   15  TANT                PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-50YY9R               PICTURE ZZ.ZZ9-.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-09                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BBSREC-PROCESS
               SET BBSREC-PROCESS-OFF      TO TRUE
               SET BBSREC-READ             TO TRUE
           END-IF
 
           IF  BBSREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM BBSREC-GET
               SET BBSREC-READ-OFF         TO TRUE
               IF  NOT BBSREC-EOF
                   PERFORM BBSREC-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET BBSREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BBSREC-PROCESS
               PERFORM BBSREC-IDSET
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
 
           IF  BBSREC-PROCESS
               PERFORM BBSREC-FLDSET
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
           SET NOT-I-72                    TO TRUE
           IF  (I-01)
               SET NOT-I-72                TO TRUE
               IF  FIRMA NOT = FFNR
                   SET I-72                TO TRUE
               END-IF
               MOVE FIRMA                  TO FFNR
           END-IF
           IF  (I-01 AND I-72)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-01)
               SET NOT-I-73                TO TRUE
               IF  OPPTYP = '25'
                   SET I-73                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               SET NOT-I-40                TO TRUE
               IF  FEILKD = '131'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  FEILKD = '132'
                   SET I-41                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  FEILKD = '133'
                   SET I-42                TO TRUE
               END-IF
               SET NOT-I-43                TO TRUE
               IF  FEILKD = '134'
                   SET I-43                TO TRUE
               END-IF
               SET NOT-I-44                TO TRUE
               IF  FEILKD = '181'
                   SET I-44                TO TRUE
               END-IF
               SET NOT-I-45                TO TRUE
               IF  FEILKD = '221'
                   SET I-45                TO TRUE
               END-IF
               SET NOT-I-46                TO TRUE
               IF  FEILKD = '222'
                   SET I-46                TO TRUE
               END-IF
               SET NOT-I-47                TO TRUE
               IF  FEILKD = '224'
                   SET I-47                TO TRUE
               END-IF
               SET NOT-I-48                TO TRUE
               IF  FEILKD = '225'
                   SET I-48                TO TRUE
               END-IF
               SET NOT-I-49                TO TRUE
               IF  FEILKD = '226'
                   SET I-49                TO TRUE
               END-IF
               SET NOT-I-50                TO TRUE
               IF  FEILKD = '252'
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-51                TO TRUE
               IF  FEILKD = '253'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-05)
               GO TO SLUTT-T
      *
           END-IF
           MOVE FIRMA                      TO RESKEY (1:3)
           MOVE KNR                        TO RESKEY (4:6)
           MOVE RESKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-21                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-21                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           ADD BEL                         TO TBEL
           ADD 1                           TO TANT.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'BBS10'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'BBS152  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               SET I-35                    TO TRUE
           END-IF.
      ******************************************************
 
       BBSREC-GET SECTION.
       BBSREC-GET-P.
           IF  BBSREC-EOF-OFF
               READ BBSREC
               AT END
                   SET BBSREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BBSREC-FLDSET SECTION.
       BBSREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '2'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               MOVE BBSREC-IO-AREA (05:2)  TO OPPTYP (1:2)
               MOVE BBSREC-IO-AREA (70:11) TO BBSKTO (1:11)
               MOVE BBSREC-IO-AREA (57:3)  TO FIRMA (1:3)
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '5' )
               MOVE BBSREC-IO-AREA (41:9)  TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE BBSREC-IO-AREA (56:6)  TO KNR (1:6)
               MOVE BBSREC-IO-AREA (65:2)  TO PERI (1:2)
               MOVE BBSREC-IO-AREA (67:1)  TO AAR (1:1)
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '6' )
               MOVE BBSREC-IO-AREA (76:3)  TO FEILKD (1:3)
           WHEN ( BBSREC-IO-AREA (7:1) = '8'
            AND   BBSREC-IO-AREA (8:1) = '8' )
               MOVE BBSREC-IO-AREA (9:8)   TO SUMANT-IO
               INSPECT SUMANT-IO REPLACING ALL ' ' BY '0'
               MOVE BBSREC-IO-AREA (42:6)  TO BBSDAT (1:6)
               MOVE BBSREC-IO-AREA (33:9)  TO SUMBEL-IO
               INSPECT SUMBEL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       BBSREC-IDCHK SECTION.
       BBSREC-IDCHK-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '2'
            AND   BBSREC-IO-AREA (8:1) = '0' )
             OR ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '5' )
             OR ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '6' )
             OR ( BBSREC-IO-AREA (7:1) = '8'
            AND   BBSREC-IO-AREA (8:1) = '8' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       BBSREC-IDSET SECTION.
       BBSREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '2'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '5' )
               SET I-03                    TO TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '6' )
               SET I-04                    TO TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '8'
            AND   BBSREC-IO-AREA (8:1) = '8' )
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KUNAVN (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-09                        TO TRUE.
 
       LISTE2-PRINT-LINE SECTION.
       LISTE2-PRINT-LINE-P.
           IF  LISTE2-BEFORE-SKIP > 0
               PERFORM LISTE2-SKIP-BEFORE
           END-IF
           IF  LISTE2-BEFORE-SPACE > 0
               PERFORM LISTE2-SPACE-BEFORE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               IF  LISTE2-AFTER-SPACE > 0
                   PERFORM LISTE2-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               PERFORM LISTE2-SPACE-AFTER
           END-IF
           IF  LISTE2-LINE-COUNT NOT < LISTE2-MAX-LINES
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
           END-IF.
 
       LISTE2-SKIP-BEFORE SECTION.
       LISTE2-SKIP-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-BEFORE-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-BEFORE SECTION.
       LISTE2-SPACE-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER LISTE2-BEFORE-SPACE LINES
           ADD LISTE2-BEFORE-SPACE         TO LISTE2-LINE-COUNT
           MOVE SPACES TO LISTE2-IO-AREA
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-BEFORE-SPACE.
 
       LISTE2-SKIP-AFTER SECTION.
       LISTE2-SKIP-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-AFTER-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-AFTER SECTION.
       LISTE2-SPACE-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE LISTE2-AFTER-SPACE LINES
           ADD LISTE2-AFTER-SPACE          TO LISTE2-LINE-COUNT
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE KNR                    TO LISTE2-IO-AREA (12:6)
               IF  (NOT-I-21)
                   MOVE KUNAVN             TO LISTE2-IO-AREA (21:30)
               END-IF
               IF  (I-21)
                   MOVE '** KUNDENR. UKJENT  ** ' TO LISTE2-IO-AREA
                                                               (21:23)
               END-IF
               MOVE 'AUTOGIRO'             TO LISTE2-IO-AREA (53:8)
               MOVE PERI                   TO LISTE2-IO-AREA (64:2)
               MOVE '.'                    TO LISTE2-IO-AREA (66:1)
               MOVE AAR                    TO LISTE2-IO-AREA (67:1)
               MOVE BEL                    TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE2-IO-AREA (71:13)
               IF  (I-73)
                   MOVE '* AVVIST *'       TO LISTE2-IO-AREA (90:10)
               END-IF
               IF  (I-73)
                   MOVE 'UKJENT FEIL             ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-40 AND I-73)
                   MOVE 'FULLMAKT IKKE FUNNET    ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-41 AND I-73)
                   MOVE 'FULLMAKT IKKE AKTIV     ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-42 AND I-73)
                   MOVE 'FULLMAKT SPERRET        ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-43 AND I-73)
                   MOVE 'FULLMAKT AVSLUTTET      ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-44 AND I-73)
                   MOVE 'FULLMAKTSDATO OVERSKRED.' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-45 AND I-73)
                   MOVE 'BELØP AVVIST I BET. BANK' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-46 AND I-73)
                   MOVE 'KONTO IKKE FUNNET       ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-47 AND I-73)
                   MOVE '* OCR-GIRO SKREVET      ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-48 AND I-73)
                   MOVE '* STANDARD-GIRO SKREVET ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-49 AND I-73)
                   MOVE '* BET. PÅMINN. IKKE SKR.' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-50 AND I-73)
                   MOVE '* SENDT TIL REPETERING  ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-51 AND I-73)
                   MOVE '* AVVIST ETTER REPETER. ' TO LISTE2-IO-AREA
                                                              (107:24)
               END-IF
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-05)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TOTALT AVVIST          ' TO LISTE2-IO-AREA (1:23)
               MOVE TBEL                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE2-IO-AREA (83:13)
               INITIALIZE TBEL
               MOVE TANT                   TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE2-IO-AREA (101:7)
               INITIALIZE TANT
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TOTALT FOR FIRMA '    TO LISTE2-IO-AREA (1:17)
               MOVE FIRMA                  TO LISTE2-IO-AREA (18:3)
               MOVE ' BBSDATO '            TO LISTE2-IO-AREA (21:9)
               MOVE BBSDAT                 TO LISTE2-IO-AREA (30:6)
               MOVE SUMBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE2-IO-AREA (83:13)
               MOVE SUMANT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE2-IO-AREA (97:11)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-72)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE FINAVN                 TO LISTE2-IO-AREA (1:30)
               MOVE 'AUTOGIROBETALINGER'   TO LISTE2-IO-AREA (33:18)
               MOVE 'OVERFØRT FRA BBS.  '  TO LISTE2-IO-AREA (52:19)
               MOVE 'KONTO NR.'            TO LISTE2-IO-AREA (75:9)
               MOVE BBSKTO                 TO LISTE2-IO-AREA (86:11)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 2                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '          KUNDENR'    TO LISTE2-IO-AREA (2:17)
               MOVE 'KUNDENAVN'            TO LISTE2-IO-AREA (21:9)
               MOVE 'TYPE'                 TO LISTE2-IO-AREA (53:4)
               MOVE 'PERIODE'              TO LISTE2-IO-AREA (62:7)
               MOVE 'BELØP'                TO LISTE2-IO-AREA (78:5)
               MOVE 'ANMERKNINGER'         TO LISTE2-IO-AREA (90:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE 2                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE FINAVN                 TO LISTE2-IO-AREA (1:30)
               MOVE 'AUTOGIROBETALINGER'   TO LISTE2-IO-AREA (33:18)
               MOVE 'OVERFØRT FRA BBS.  '  TO LISTE2-IO-AREA (52:19)
               MOVE 'KONTO NR.'            TO LISTE2-IO-AREA (75:9)
               MOVE BBSKTO                 TO LISTE2-IO-AREA (86:11)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 2                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '          KUNDENR'    TO LISTE2-IO-AREA (2:17)
               MOVE 'KUNDENAVN'            TO LISTE2-IO-AREA (21:9)
               MOVE 'TYPE'                 TO LISTE2-IO-AREA (53:4)
               MOVE 'PERIODE'              TO LISTE2-IO-AREA (62:7)
               MOVE 'BELØP'                TO LISTE2-IO-AREA (78:5)
               MOVE 'ANMERKNINGER'         TO LISTE2-IO-AREA (90:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE 2                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-U8 AND I-09)
           AND (I-35 AND I-98)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE PSDS                   TO LISTE2-IO-AREA (1:80)
               MOVE R                      TO LISTE2-IO-AREA (73:8)
               MOVE P-IO                   TO LISTE2-IO-AREA (78:3)
               MOVE S-IO                   TO LISTE2-IO-AREA (76:5)
               MOVE LONR                   TO LISTE2-IO-AREA (76:5)
               MOVE LFIRMA                 TO LISTE2-IO-AREA (78:3)
               MOVE LUNDGR                 TO LISTE2-IO-AREA (78:3)
               MOVE LPROG                  TO LISTE2-IO-AREA (73:8)
               MOVE LOPNVN                 TO LISTE2-IO-AREA (46:35)
               MOVE LPRIID                 TO LISTE2-IO-AREA (77:4)
               MOVE BJOBN                  TO LISTE2-IO-AREA (73:8)
               MOVE BBEST                  TO LISTE2-IO-AREA (80:1)
               MOVE BPERS                  TO LISTE2-IO-AREA (51:30)
               MOVE BETTB                  TO LISTE2-IO-AREA (41:40)
               MOVE BFORS                  TO LISTE2-IO-AREA (41:40)
               MOVE BMEMO                  TO LISTE2-IO-AREA (41:40)
               MOVE BANTX-IO               TO LISTE2-IO-AREA (78:3)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
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
           INITIALIZE BBSREC-DATA-FIELDS
           SET BBSREC-EOF-OFF              TO TRUE
           SET BBSREC-PROCESS              TO TRUE
           OPEN INPUT BBSREC
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE2
           INITIALIZE LISTE2-IO-AREA
           INITIALIZE LISTE2-DATA-FIELDS
           MOVE 57                         TO LISTE2-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BBSREC
           CLOSE KUNDEMA
           IF LISTE2-IO-AREA NOT = SPACES
             WRITE LISTE2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE2-IO-AREA
           END-IF
           CLOSE LISTE2.
 
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
