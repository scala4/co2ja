       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR230R.
      **********************************************  Z-WIN-RPG2   ****
      ** OBS Liste som excel på RWeb *********** XX2000XXIRXXEN
      *  LESER DAGENS NOTERTE RESTORDRE OG PRINTER         *
      *  DENNE UT MED KUNDENAVN SAMT HENTER ANTALL I       *
      *  BESTILLING MED FORVENTET ANKOMST FRA VAREMASTER.  *
      * 05/02/98 RBS MED AVD.RUTINE.                       *
      * 11/12/98 ORDREBEKREFTELSE.                         *
      * 03/02/00 KONSERNMODELL FOR KUNDE.MASTER.           *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR230.rpg
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
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RARKIV
               BLOCK CONTAINS 260
               RECORD CONTAINS 130.
       01  RARKIV-IO-AREA.
           05  RARKIV-IO-AREA-X            PICTURE X(130).
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
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
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
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
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
                   15  RARKIV-01-L2-RKNR   PICTURE X(6).
               10  RARKIV-01-L1.
                   15  RARKIV-01-L1-RORDRE PICTURE X(6).
           05  RARKIV-LEVEL-02.
               10  RARKIV-02-L4.
                   15  RARKIV-02-L4-FIRM   PICTURE X(3).
               10  RARKIV-02-L3.
                   15  RARKIV-02-L3-AVD    PICTURE X(1).
               10  RARKIV-02-L2.
                   15  RARKIV-02-L2-RKNR   PICTURE X(6).
               10  RARKIV-02-L1.
                   15  RARKIV-02-L1-RORDRE PICTURE X(6).
           05  RARKIV-LEVEL-03.
               10  RARKIV-03-L4.
                   15  RARKIV-03-L4-FIRM   PICTURE X(3).
               10  RARKIV-03-L3.
                   15  RARKIV-03-L3-AVD    PICTURE X(1).
               10  RARKIV-03-L2.
                   15  RARKIV-03-L2-RKNR   PICTURE X(6).
               10  RARKIV-03-L1.
                   15  RARKIV-03-L1-RORDRE PICTURE X(6).
           05  RARKIV-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  AVD                     PICTURE X(1).
               10  RKNR                    PICTURE X(6).
               10  RORDRE                  PICTURE X(6).
               10  RKREF                   PICTURE X(15).
               10  RARTNR                  PICTURE X(20).
               10  REDBNR                  PICTURE X(7).
               10  BK                      PICTURE X(1).
               10  RDATO-IO.
                   15  RDATO               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RANT-IO.
                   15  RANT                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  AVDN1                   PICTURE X(10).
               10  AVDN2                   PICTURE X(10).
               10  AVDN3                   PICTURE X(10).
               10  AVDN4                   PICTURE X(10).
               10  AVDN5                   PICTURE X(10).
               10  AVDN6                   PICTURE X(10).
               10  AVDN7                   PICTURE X(10).
               10  AVDN8                   PICTURE X(10).
               10  AVDN9                   PICTURE X(10).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  BANT-IO.
                   15  BANT                PICTURE S9(7).
               10  BA-ELGR                 PICTURE X(2).
               10  BUKE                    PICTURE X(2).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KADR                    PICTURE X(30).
               10  KPSTED                  PICTURE X(15).
               10  KPNR                    PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KUNDE                   PICTURE X(9).
               10  NUM6-IO.
                   15  NUM6                PICTURE S9(6).
               10  NEDBNR                  PICTURE X(10).
               10  ALF6                    PICTURE X(6).
               10  AAR                     PICTURE X(2).
               10  DAG                     PICTURE X(2).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  AVDNAV                  PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-52YY9R               PICTURE ZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-21YY9R               PICTURE ZZ,9-.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
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
                   PERFORM RARKIV-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
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
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  RARKIV-PROCESS
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
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  FIRM = '923'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-L4)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3 AND NOT-I-L4 AND I-87)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3)
               PERFORM AVDRUT-S
      **************************************************
      *   HENTE OPPLYSNINGER FRA KUNDEMASTER           *
      **************************************************
           END-IF
           IF  (I-L2)
               MOVE FIRM                   TO KUNDE (1:3)
           END-IF
           IF  (I-L2 AND NOT-I-08)
               SET NOT-I-12                TO TRUE
               IF  KONFNR > '000'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-08 AND I-12)
               MOVE KONFNR                 TO KUNDE (1:3)
           END-IF
           IF  (I-L2)
               MOVE RKNR                   TO KUNDE (4:6)
               MOVE KUNDE                  TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM KUNDEMA-IDCHK
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      **************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-31                TO TRUE
           END-IF
           IF  (I-01)
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-03)
               GO TO SLUTT-T
           END-IF
           ADD RDATO TO ZERO           GIVING NUM6
           SET NOT-I-35                    TO TRUE
           IF  BK = '5'
               SET I-35                    TO TRUE
           END-IF
           IF  (NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  BK = 'F'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           SET NOT-I-37                    TO TRUE
           IF  BK = 'P'
               SET I-37                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  BK = 'T'
               SET I-38                    TO TRUE
           END-IF
           SET NOT-I-39                    TO TRUE
           IF  BK = 'O'
               SET I-39                    TO TRUE
           END-IF
           PERFORM DTORUT-S
      ***************************************************
      *     HENTE OPPLYSNINGER FRA VAREMASTER           *
      ***************************************************
           MOVE FIRM                       TO NEDBNR (1:3)
           MOVE REDBNR                     TO NEDBNR (4:7)
           MOVE NEDBNR                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-10                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-10                TO TRUE
               PERFORM VAREMAS-IDCHK
               PERFORM VAREMAS-FLDOFF
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE SNU DATO TIL DAG,MND,ÅR               *
      ******************************************************
           CONTINUE.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           MOVE NUM6                       TO ALF6
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE ALF6 (6:1)                 TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO ALF6 (6:1)
           MOVE ALF6 (1:2)                 TO AAR
           MOVE ALF6 (5:2)                 TO DAG
           MOVE DAG                        TO ALF6 (1:2)
           MOVE AAR                        TO ALF6 (5:2)
           MOVE ALF6                       TO ORDATO-IO.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      *    OPPDATERT MED AVDELING I UNDERGRUPPE.           *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'REO02'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           IF  (I-87)
               MOVE AVD                    TO LUNDGR (3:1)
               MOVE '00'                   TO LUNDGR (1:2)
           END-IF
           MOVE 'ROR230  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-U2)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 2
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (I-U3)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 3
                   SET I-86                TO TRUE
               END-IF
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
 
       RARKIV-GET SECTION.
       RARKIV-GET-P.
           IF  RARKIV-EOF-OFF
               READ RARKIV
               AT END
                   SET RARKIV-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RARKIV-FLDSET SECTION.
       RARKIV-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RARKIV-IO-AREA (1:1) = '1' )
               MOVE RARKIV-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE RARKIV-IO-AREA (20:1)  TO AVD (1:1)
               MOVE RARKIV-IO-AREA (5:6)   TO RKNR (1:6)
               MOVE RARKIV-IO-AREA (14:6)  TO RORDRE (1:6)
               MOVE RARKIV-IO-AREA (21:15) TO RKREF (1:15)
           WHEN ( RARKIV-IO-AREA (1:1) = '2' )
               MOVE RARKIV-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE RARKIV-IO-AREA (20:1)  TO AVD (1:1)
               MOVE RARKIV-IO-AREA (5:6)   TO RKNR (1:6)
               MOVE RARKIV-IO-AREA (14:6)  TO RORDRE (1:6)
           WHEN ( RARKIV-IO-AREA (1:1) = '3' )
               MOVE RARKIV-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE RARKIV-IO-AREA (20:1)  TO AVD (1:1)
               MOVE RARKIV-IO-AREA (5:6)   TO RKNR (1:6)
               MOVE RARKIV-IO-AREA (31:20) TO RARTNR (1:20)
               MOVE RARKIV-IO-AREA (21:7)  TO REDBNR (1:7)
               MOVE RARKIV-IO-AREA (14:6)  TO RORDRE (1:6)
               MOVE RARKIV-IO-AREA (68:1)  TO BK (1:1)
               MOVE RARKIV-IO-AREA (96:4)  TO RDATO-IO
               MOVE RARKIV-IO-AREA (51:4)  TO RANT-IO
               MOVE RARKIV-IO-AREA (55:5)  TO BEL-IO
               MOVE RARKIV-IO-AREA (60:2)  TO RAB1-IO
               MOVE RARKIV-IO-AREA (62:2)  TO RAB2-IO
               MOVE RARKIV-IO-AREA (64:2)  TO RAB3-IO
           END-EVALUATE.
 
       RARKIV-IDCHK SECTION.
       RARKIV-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RARKIV-IO-AREA (1:1) = '1' )
             OR ( RARKIV-IO-AREA (1:1) = '2' )
             OR ( RARKIV-IO-AREA (1:1) = '3' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RARKIV-IDSET SECTION.
       RARKIV-IDSET-P.
           EVALUATE TRUE
           WHEN ( RARKIV-IO-AREA (1:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( RARKIV-IO-AREA (1:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( RARKIV-IO-AREA (1:1) = '3' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       RARKIV-CHK-LEVEL SECTION.
       RARKIV-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RARKIV-IO-AREA (1:1) = '1' )
               MOVE LOW-VALUES             TO RARKIV-LEVEL-01
               MOVE RARKIV-IO-AREA (2:3)   TO RARKIV-01-L4-FIRM
               MOVE RARKIV-IO-AREA (20:1)  TO RARKIV-01-L3-AVD
               MOVE RARKIV-IO-AREA (5:6)   TO RARKIV-01-L2-RKNR
               MOVE RARKIV-IO-AREA (14:6)  TO RARKIV-01-L1-RORDRE
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
           WHEN ( RARKIV-IO-AREA (1:1) = '2' )
               MOVE LOW-VALUES             TO RARKIV-LEVEL-02
               MOVE RARKIV-IO-AREA (2:3)   TO RARKIV-02-L4-FIRM
               MOVE RARKIV-IO-AREA (20:1)  TO RARKIV-02-L3-AVD
               MOVE RARKIV-IO-AREA (5:6)   TO RARKIV-02-L2-RKNR
               MOVE RARKIV-IO-AREA (14:6)  TO RARKIV-02-L1-RORDRE
               IF  RARKIV-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RARKIV-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RARKIV-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RARKIV-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RARKIV-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RARKIV-02-L4          TO THE-PRIOR-L4
               MOVE  RARKIV-02-L3          TO THE-PRIOR-L3
               MOVE  RARKIV-02-L2          TO THE-PRIOR-L2
               MOVE  RARKIV-02-L1          TO THE-PRIOR-L1
               SET RARKIV-LEVEL-INIT       TO TRUE
           WHEN ( RARKIV-IO-AREA (1:1) = '3' )
               MOVE LOW-VALUES             TO RARKIV-LEVEL-03
               MOVE RARKIV-IO-AREA (2:3)   TO RARKIV-03-L4-FIRM
               MOVE RARKIV-IO-AREA (20:1)  TO RARKIV-03-L3-AVD
               MOVE RARKIV-IO-AREA (5:6)   TO RARKIV-03-L2-RKNR
               MOVE RARKIV-IO-AREA (14:6)  TO RARKIV-03-L1-RORDRE
               IF  RARKIV-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RARKIV-03-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RARKIV-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RARKIV-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RARKIV-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RARKIV-03-L4          TO THE-PRIOR-L4
               MOVE  RARKIV-03-L3          TO THE-PRIOR-L3
               MOVE  RARKIV-03-L2          TO THE-PRIOR-L2
               MOVE  RARKIV-03-L1          TO THE-PRIOR-L1
               SET RARKIV-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
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
 
       VAREMAS-FLDOFF SECTION.
       VAREMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( VAREMAS-IO-AREA (1:1) = '7'
            AND   VAREMAS-IO-AREA (2:1) = '0' )
               SET NOT-I-13                TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VAREMAS-IO-AREA (1:1) = '7'
            AND   VAREMAS-IO-AREA (2:1) = '0' )
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (129:7) TO BANT-IO
               INSPECT BANT-IO REPLACING ALL ' ' BY '0'
               IF  BANT = ZERO
                   SET I-13                TO TRUE
               END-IF
               MOVE VAREMAS-IO-AREA (136:2) TO BA-ELGR (1:2)
               MOVE VAREMAS-IO-AREA (138:2) TO BUKE (1:2)
           END-EVALUATE.
 
       VAREMAS-IDCHK SECTION.
       VAREMAS-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VAREMAS-IO-AREA (1:1) = '7'
            AND   VAREMAS-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           EVALUATE TRUE
           WHEN ( VAREMAS-IO-AREA (1:1) = '7'
            AND   VAREMAS-IO-AREA (2:1) = '0' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               SET NOT-I-70                TO TRUE
               SET NOT-I-71                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KNAVN2 (1:30)
               IF  KNAVN2 = SPACES
                   SET I-70                TO TRUE
               END-IF
               MOVE KUNDEMA-IO-AREA (76:30) TO KADR (1:30)
               IF  KADR = SPACES
                   SET I-71                TO TRUE
               END-IF
               MOVE KUNDEMA-IO-AREA (106:15) TO KPSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO KPNR (1:4)
           END-EVALUATE.
 
       KUNDEMA-IDCHK SECTION.
       KUNDEMA-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               SET I-05                    TO TRUE
           END-EVALUATE.
 
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
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RKNR                   TO LISTE-IO-AREA (3:6)
               MOVE KNAVN1                 TO LISTE-IO-AREA (10:30)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-70 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KNAVN2                 TO LISTE-IO-AREA (10:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-71 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KADR                   TO LISTE-IO-AREA (10:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KPNR                   TO LISTE-IO-AREA (10:4)
               MOVE KPSTED                 TO LISTE-IO-AREA (15:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RANT                   TO XO-52YY9R
               MOVE XO-52YY9R              TO LISTE-IO-AREA (4:10)
               MOVE ALFA                   TO LISTE-IO-AREA (16:3)
               MOVE RARTNR                 TO LISTE-IO-AREA (20:20)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (41:8)
               MOVE RORDRE                 TO LISTE-IO-AREA (52:6)
               MOVE BEL                    TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (60:13)
               MOVE RAB1                   TO XO-21YY9R
               MOVE XO-21YY9R              TO LISTE-IO-AREA (75:5)
               MOVE RAB2                   TO XO-21YY9R
               MOVE XO-21YY9R              TO LISTE-IO-AREA (80:5)
               MOVE RAB3                   TO XO-21YY9R
               MOVE XO-21YY9R              TO LISTE-IO-AREA (85:5)
               IF  (I-31)
                   MOVE RKREF              TO LISTE-IO-AREA (90:15)
               END-IF
               IF  (NOT-I-10 AND NOT-I-13)
                   MOVE BANT               TO XO-70YY9R
                   MOVE XO-70YY9R          TO LISTE-IO-AREA (106:10)
               END-IF
               IF  (NOT-I-10 AND NOT-I-13)
                   MOVE BUKE               TO LISTE-IO-AREA (119:2)
               END-IF
               IF  (NOT-I-10 AND NOT-I-13)
                   MOVE BA-ELGR            TO LISTE-IO-AREA (123:2)
               END-IF
               IF  (I-35)
                   MOVE 'FORH.S.'          TO LISTE-IO-AREA (126:7)
               END-IF
               IF  (I-37)
                   MOVE 'PROD.O.'          TO LISTE-IO-AREA (126:7)
               END-IF
               IF  (I-38)
                   MOVE 'TILB.O.'          TO LISTE-IO-AREA (126:7)
               END-IF
               IF  (I-39)
                   MOVE 'O.BEKR.'          TO LISTE-IO-AREA (126:7)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * * A R T I K L E R' TO LISTE-IO-AREA (31:21)
               MOVE 'N O T E R T  I  R E S T' TO LISTE-IO-AREA (54:23)
               MOVE 'O R D R E * * *'      TO LISTE-IO-AREA (78:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (96:8)
               MOVE AVDNAV                 TO LISTE-IO-AREA (106:10)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDENR NAVN/ADRESSE/POS' TO LISTE-IO-AREA (2:24)
               MOVE 'TSTED'                TO LISTE-IO-AREA (26:5)
               MOVE 'DATO     ORDRE'       TO LISTE-IO-AREA (43:14)
               MOVE 'I  BESTILLING'        TO LISTE-IO-AREA (111:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESTORDRE'            TO LISTE-IO-AREA (2:9)
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (16:18)
               MOVE 'BESTILT     NR.'      TO LISTE-IO-AREA (41:15)
               MOVE 'BELØP'                TO LISTE-IO-AREA (64:5)
               MOVE 'RABATTER       KUNDEREF' TO LISTE-IO-AREA (78:23)
               MOVE 'ANTALL    UKE  ÅR'    TO LISTE-IO-AREA (108:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * * A R T I K L E R' TO LISTE-IO-AREA (31:21)
               MOVE 'N O T E R T  I  R E S T' TO LISTE-IO-AREA (54:23)
               MOVE 'O R D R E * * *'      TO LISTE-IO-AREA (78:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (96:8)
               MOVE AVDNAV                 TO LISTE-IO-AREA (106:10)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDENR NAVN/ADRESSE/POS' TO LISTE-IO-AREA (2:24)
               MOVE 'TSTED'                TO LISTE-IO-AREA (26:5)
               MOVE 'DATO     ORDRE'       TO LISTE-IO-AREA (43:14)
               MOVE 'I  BESTILLING'        TO LISTE-IO-AREA (111:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESTORDRE'            TO LISTE-IO-AREA (2:9)
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (16:18)
               MOVE 'BESTILT     NR.'      TO LISTE-IO-AREA (41:15)
               MOVE 'BELØP'                TO LISTE-IO-AREA (64:5)
               MOVE 'RABATTER       KUNDEREF' TO LISTE-IO-AREA (78:23)
               MOVE 'ANTALL    UKE  ÅR'    TO LISTE-IO-AREA (108:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
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
           CLOSE KUNDEMA
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
