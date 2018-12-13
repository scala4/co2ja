       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR215R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM...: ROR215                                *
      *  PROGRAMERER: ESPEN LARSEN              14.11.1995 *
      *  LESER SELEKTERTE REC.  PRINTER RESTORDRELISTE     *
      *  PÅ ANKOMMENDE VARER PR. VARE.                     *
      *  HENTER ALTERNATIVE ARTIKKELNUMMER FRA VAREMASTER. *
      * 05/02/98 RBS MED AVD.RUTINE.                       *
      * 11/12/98 ORDREBEKREFT. BEHANDLES SOM TILBUDSORDRE  *
      * 05/05/00 BEHOLDNING PÅ FORHÅNDSORDRE SKAL IKKE     *
      *          PRINTES DA DETTE = ANT. BEST. FORH.ORDRE. *
      * 21/06/01 RETTET FEIL VEDR. TEST PÅ OM BEHOLDNING   *
      *          ER MINDRE ENN RESTORDRE.                  *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR215.rpg
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
           SELECT RARKIV
               ASSIGN TO UT-S-RARKIV
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RARKIV-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RARKIV
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  RARKIV-IO-AREA.
           05  RARKIV-IO-AREA-X            PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
           10  RARKIV-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RARKIV-EOF-OFF          VALUE '0'.
               88  RARKIV-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RARKIV-READ-OFF         VALUE '0'.
               88  RARKIV-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RARKIV-PROCESS-OFF      VALUE '0'.
               88  RARKIV-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RARKIV-LEVEL-INIT-OFF   VALUE '0'.
               88  RARKIV-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  RARKIV-LEVEL-01.
               10  RARKIV-01-L4.
                   15  RARKIV-01-L4-FIRM   PICTURE X(3).
               10  RARKIV-01-L3.
                   15  RARKIV-01-L3-AVD    PICTURE X(1).
               10  RARKIV-01-L2.
                   15  RARKIV-01-L2-ALFART PICTURE X(23).
               10  RARKIV-01-L1.
                   15  RARKIV-01-L1-LK     PICTURE X(2).
           05  RARKIV-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  AVD                     PICTURE X(1).
               10  OTYPE                   PICTURE X(1).
               10  RANT-IO.
                   15  RANT                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALFART                  PICTURE X(23).
               10  RALFA                   PICTURE X(3).
               10  RARTNR                  PICTURE X(20).
               10  LKLAR                   PICTURE X(1).
               10  LK                      PICTURE X(2).
               10  VANTLG-IO.
                   15  VANTLG              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALTNR-IO.
                   15  ALTNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
               10  AVDN1                   PICTURE X(10).
               10  AVDN2                   PICTURE X(10).
               10  AVDN3                   PICTURE X(10).
               10  AVDN4                   PICTURE X(10).
               10  AVDN5                   PICTURE X(10).
               10  AVDN6                   PICTURE X(10).
               10  AVDN7                   PICTURE X(10).
               10  AVDN8                   PICTURE X(10).
               10  AVDN9                   PICTURE X(10).
      ******************************************************
      *  H O V E D R U T I N E N                           *
      ******************************************************
           05  VAREMAS-DATA-FIELDS.
               10  VALFA                   PICTURE X(3).
               10  VART                    PICTURE X(20).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(23).
               10  THE-PRIOR-L1            PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  RTANT-IO.
                   15  RTANT               PICTURE S9(7)V9(2).
               10  VARBEH-IO.
                   15  VARBEH              PICTURE S9(7)V9(2).
               10  NALTNR                  PICTURE X(10).
               10  ALTNR-N-IO.
                   15  ALTNR-N             PICTURE S9(7).
               10  AVDNAV                  PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RARKIV-PROCESS
               SET RARKIV-PROCESS-OFF      TO TRUE
               SET RARKIV-READ             TO TRUE
           END-IF
 
           IF  RARKIV-READ
           AND RECORD-SELECTED-OFF
               PERFORM RARKIV-GET
               SET RARKIV-READ-OFF         TO TRUE
               IF  NOT RARKIV-EOF
                   SET RARKIV-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RARKIV-PROCESS
               PERFORM RARKIV-IDSET
           END-IF
 
           IF  RARKIV-PROCESS
               PERFORM RARKIV-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
 
           IF  RARKIV-PROCESS
               PERFORM RARKIV-FLDOFF
               PERFORM RARKIV-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RARKIV-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-87                    TO TRUE
           IF  FIRM = '923'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-L4)
               MOVE FIRM                   TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-08                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-08            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3 AND NOT-I-L4 AND I-87)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3)
               PERFORM AVDRUT-S
      *****************************************************************
      *  RUTINE FOR SUMMERING TOTALT ANTALL I REST PR. VARE PR LK.    *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SUBTRACT RTANT              FROM RTANT
           END-IF
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               IF  OTYPE = 'T'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  OTYPE = 'F'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  OTYPE = 'O'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-21)
               ADD RANT                    TO RTANT
      *****************************************************************
      * RUTINE FOR Å LEGGE UT VAREBEHOLDNING. FORH.ORDRE UTELATES.    *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE 0,00                   TO VARBEH
           END-IF
           IF  (I-01)
               SET NOT-I-22                TO TRUE
               IF  OTYPE = 'F'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-22)
               ADD VANTLG TO ZERO      GIVING VARBEH
      *****************************************************************
      * TEST OM VAREN SKAL LEGGES UT SOM "KAN LEVERES"                *
      *      SELV OM VAREN HAR BEHOLDNING, SKAL DEN IKKE LEGGES UT    *
      *      OM DET IKKE HAR VÆRT VARETILGANG SISTE 7 DAGER (FIRMATEST)
      *      DETTE GJELDER IKKE FORH.ORDRE / TILB.ORDRE               *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-91                TO TRUE
               SET NOT-I-92                TO TRUE
               IF  OTYPE = 'R'
                   SET I-92                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-92)
               SET NOT-I-92                TO TRUE
               IF  OTYPE = 'S'
                   SET I-92                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-92)
               SET NOT-I-91                TO TRUE
               IF  LKLAR = ' '
                   SET I-91                TO TRUE
               END-IF
      *****************************************************************
      *  TEST PÅ OM VAREBEHOLDING ER MINDRE EN ANTALL I REST.         *
      *****************************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'REO03'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           IF  (I-87)
               MOVE AVD                    TO LUNDGR (3:1)
               MOVE '00'                   TO LUNDGR (1:2)
           END-IF
           MOVE 'ROR215  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR HENTING AV AVDELINGSNAVN.         *
      ******************************************************
 
       AVDRUT-S SECTION.
       AVDRUT-S-P.
           SET NOT-I-99                    TO TRUE
           IF  AVD = '1'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN1                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '2'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN2                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '3'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN3                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '4'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN4                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '5'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN5                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '6'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN6                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '7'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN7                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '8'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN8                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '9'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN9                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF.
 
       ENDAVD-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-12                TO TRUE
               IF  VARBEH < RTANT
                   SET I-12                TO TRUE
               END-IF
      *****************************************************************
      *  TEST PÅ OM OM ANTALL I REST ER STØRRE EN 0                   *
      *  DET ER KUN OM ANTALL I REST ER STØRRE EN 0                   *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-55                TO TRUE
               IF  RTANT > 0
                   SET I-55                TO TRUE
               END-IF
      ******************************************************
      *   RUTINE FOR ALTERNATIV ARTIKKEL (EDB.NR)          *
      ******************************************************
           END-IF
           IF  (I-L1 AND NOT-I-75)
               MOVE FIRM                   TO NALTNR (1:3)
               MOVE ALTNR                  TO ALTNR-N
               MOVE ALTNR-N-IO             TO NALTNR (4:7)
               MOVE NALTNR                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-13                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-13            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      *    OPPDATERT MED AVDELING I UNDERGRUPPE.           *
      ******************************************************
           END-IF
           .
 
       RARKIV-GET SECTION.
       RARKIV-GET-P.
           IF  RARKIV-EOF-OFF
               READ RARKIV
               AT END
                   SET RARKIV-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RARKIV-FLDOFF SECTION.
       RARKIV-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-75                TO TRUE
           END-EVALUATE.
 
       RARKIV-FLDSET SECTION.
       RARKIV-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RARKIV-IO-AREA (1:3)   TO FIRM (1:3)
               MOVE RARKIV-IO-AREA (4:1)   TO AVD (1:1)
               MOVE RARKIV-IO-AREA (5:1)   TO OTYPE (1:1)
               MOVE RARKIV-IO-AREA (79:4)  TO RANT-IO
               MOVE RARKIV-IO-AREA (83:23) TO ALFART (1:23)
               MOVE RARKIV-IO-AREA (83:3)  TO RALFA (1:3)
               MOVE RARKIV-IO-AREA (86:20) TO RARTNR (1:20)
               MOVE RARKIV-IO-AREA (156:1) TO LKLAR (1:1)
               MOVE RARKIV-IO-AREA (159:2) TO LK (1:2)
               MOVE RARKIV-IO-AREA (161:5) TO VANTLG-IO
               MOVE RARKIV-IO-AREA (166:4) TO ALTNR-IO
               IF  ALTNR = ZERO
                   SET I-75                TO TRUE
               END-IF
           END-EVALUATE.
 
       RARKIV-IDSET SECTION.
       RARKIV-IDSET-P.
           SET I-01                        TO TRUE.
 
       RARKIV-CHK-LEVEL SECTION.
       RARKIV-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RARKIV-LEVEL-01
               MOVE RARKIV-IO-AREA (1:3)   TO RARKIV-01-L4-FIRM
               MOVE RARKIV-IO-AREA (4:1)   TO RARKIV-01-L3-AVD
               MOVE RARKIV-IO-AREA (83:23) TO RARKIV-01-L2-ALFART
               MOVE RARKIV-IO-AREA (159:2) TO RARKIV-01-L1-LK
               IF  RARKIV-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RARKIV-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RARKIV-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RARKIV-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RARKIV-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RARKIV-01-L4          TO THE-PRIOR-L4
               MOVE  RARKIV-01-L3          TO THE-PRIOR-L3
               MOVE  RARKIV-01-L2          TO THE-PRIOR-L2
               MOVE  RARKIV-01-L1          TO THE-PRIOR-L1
               SET RARKIV-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (405:10) TO AVDN1 (1:10)
               MOVE FIRMAF-IO-AREA (416:10) TO AVDN2 (1:10)
               MOVE FIRMAF-IO-AREA (427:10) TO AVDN3 (1:10)
               MOVE FIRMAF-IO-AREA (438:10) TO AVDN4 (1:10)
               MOVE FIRMAF-IO-AREA (449:10) TO AVDN5 (1:10)
               MOVE FIRMAF-IO-AREA (460:10) TO AVDN6 (1:10)
               MOVE FIRMAF-IO-AREA (471:10) TO AVDN7 (1:10)
               MOVE FIRMAF-IO-AREA (482:10) TO AVDN8 (1:10)
               MOVE FIRMAF-IO-AREA (493:10) TO AVDN9 (1:10)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-09                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO VALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO VART (1:20)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE ' * * * RESTORDRE PR. VAR' TO LISTE-IO-AREA (31:24)
               MOVE 'E PÅ ANKOMNE VARER * * *' TO LISTE-IO-AREA (55:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (88:8)
               MOVE AVDNAV                 TO LISTE-IO-AREA (99:10)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALF    ARTIKKELNR.'   TO LISTE-IO-AREA (3:18)
               MOVE 'LK'                   TO LISTE-IO-AREA (31:2)
               MOVE 'NY LAGERBEHOLDN.'     TO LISTE-IO-AREA (40:16)
               MOVE 'TOTAL RESTORDRE'      TO LISTE-IO-AREA (60:15)
               MOVE 'ALTERNATIV VARE'      TO LISTE-IO-AREA (86:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE ' * * * RESTORDRE PR. VAR' TO LISTE-IO-AREA (31:24)
               MOVE 'E PÅ ANKOMNE VARER * * *' TO LISTE-IO-AREA (55:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (88:8)
               MOVE AVDNAV                 TO LISTE-IO-AREA (99:10)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALF    ARTIKKELNR.'   TO LISTE-IO-AREA (3:18)
               MOVE 'LK'                   TO LISTE-IO-AREA (31:2)
               MOVE 'NY LAGERBEHOLDN.'     TO LISTE-IO-AREA (40:16)
               MOVE 'TOTAL RESTORDRE'      TO LISTE-IO-AREA (60:15)
               MOVE 'ALTERNATIV VARE'      TO LISTE-IO-AREA (86:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-55 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RALFA                  TO LISTE-IO-AREA (3:3)
               MOVE RARTNR                 TO LISTE-IO-AREA (10:20)
               MOVE LK                     TO LISTE-IO-AREA (31:2)
               MOVE VARBEH                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (38:13)
               MOVE RTANT                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (57:13)
               IF  (I-12)
                   MOVE '*****'            TO LISTE-IO-AREA (75:5)
               END-IF
               IF  (NOT-I-75 AND NOT-I-13)
                   MOVE VALFA              TO LISTE-IO-AREA (86:3)
               END-IF
               IF  (NOT-I-75 AND NOT-I-13)
                   MOVE VART               TO LISTE-IO-AREA (90:20)
               END-IF
               IF  (I-91)
                   MOVE 'BLIR IKKE SKREVET UT. ' TO LISTE-IO-AREA
                                                              (111:22)
               END-IF
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
           SET RARKIV-LEVEL-INIT           TO TRUE
           INITIALIZE RARKIV-DATA-FIELDS
           SET RARKIV-EOF-OFF              TO TRUE
           SET RARKIV-PROCESS              TO TRUE
           OPEN INPUT RARKIV
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RARKIV
           CLOSE FIRMAF
           CLOSE VAREMAS
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
