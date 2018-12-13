       IDENTIFICATION DIVISION.
       PROGRAM-ID. RBS101R.
      **********************************************  Z-WIN-RPG2   ****
      *****************************************************************
      *       S T N D .   U T S K R I F T P R O G .   R B S 1 0 1     *
      *                   F O R   F O R M U L A R E R .               *
      *                   S Y S 0 2 1   P R I N T E R .               *
      *       ----------------------------------------------------    *
      *  1. SKRIVER UT FRA FERDIG REDIGERTE LINJER.                   *
      *  2. HENTER FIRMAHEADING FRA RBS-FILE.                         *
      *  3. TESTER PÅ OM FIRMAET SKAL HA UTSKRIFT FRA RBS-FILE.       *
      *                                                               *
      *  4. STND. INNPUT ER:                                          *
      *      -  PRINTLINE  POS.   1-132.                              *
      *      -  FIRMANR    POS. 133-135.                              *
      *      -  SPACEKODE  POS. 136-136.                              *
      *                    F = SPACE TIL NY SIDE FØRST.               *
      *                    E = SPACE TIL NY SIDE ETTER.               *
      *                    G = HOPP TIL KANAL 02 FØRST.               *
      *                    H = HOPP TIL KANAL 02 ETTER.               *
      *                    I = HOPP TIL KANAL 03 FØRST.               *
      *                    J = HOPP TIL KANAL 03 ETTER.               *
      *                    K = HOPP TIL KANAL 04 FØRST.               *
      *                    L = HOPP TIL KANAL 04 ETTER.               *
      *      -  SKIPKODE   POS. 137-137.                              *
      *                    0 = SKIP 0 LINER.                          *
      *                    1 = SKIP 1 LINE  FØRST.                    *
      *                    2 = SKIP 1 LINER FØRST.                    *
      *                    3 = SKIP 1 LINER FØRST.                    *
      *                    6 = SKIP 1 LINE  ETTER.                    *
      *                    7 = SKIP 2 LINER ETTER.                    *
      *                    8 = SKIP 3 LINER ETTER.                    *
      *      -  OPPGAVENR  POS. 138-142.                              *
      *      -  SIDENR,    POS. 149-149.                              *
      *                    N = NEI, IKKE SIDENUMMERERING.             *
      *      -  LINE TYPE  POS. 150-150.                              *
      *                    A = HEADINGLINJE 1.       DENNE SAVES.     *
      *                    B = HEADINGLINJE 2.       DENNE SAVES.     *
      *                    C = HEADINGLINJE 3.       DENNE SAVES.     *
      *                    D = HEADINGLINJE 4.       DENNE SAVES.     *
      *                    E = HEADINGLINJE 5.       DENNE SAVES.     *
      *                    X = DETALJ OG TOTALLINJER.                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RBS101.rpg
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            IS SKIP-CHANNEL-1
            IS SKIP-CHANNEL-3
            IS SKIP-CHANNEL-4
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
           SELECT LISTE
               ASSIGN TO UT-S-LISTE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 8250
               RECORD CONTAINS 150.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(150).
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
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
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
           05  INFILE-LEVEL-01.
               10  INFILE-01-L1.
                   15  INFILE-01-L1-FIRMA  PICTURE X(3).
           05  INFILE-DATA-FIELDS.
               10  LINJEA                  PICTURE X(132).
               10  FIRMA                   PICTURE X(3).
               10  SPACEA                  PICTURE X(1).
               10  SKIPA                   PICTURE X(1).
               10  OPPGNR                  PICTURE X(5).
               10  PROGNR                  PICTURE X(6).
               10  SIDENR                  PICTURE X(1).
               10  LINJEB                  PICTURE X(132).
               10  SKIPB                   PICTURE X(1).
               10  LINJEC                  PICTURE X(132).
               10  SKIPC                   PICTURE X(1).
               10  LINJED                  PICTURE X(132).
               10  SKIPD                   PICTURE X(1).
               10  LINJEE                  PICTURE X(132).
               10  SKIPE                   PICTURE X(1).
               10  LINJEX                  PICTURE X(132).
               10  SPACEX                  PICTURE X(1).
               10  SKIPX                   PICTURE X(1).
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
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
                   PERFORM INFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
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
               PERFORM RBSRUT-S
           END-IF
           IF  (I-01)
               PERFORM RUTA-S
           END-IF
           IF  (I-02)
               PERFORM RUTB-S
           END-IF
           IF  (I-03)
               PERFORM RUTC-S
           END-IF
           IF  (I-04)
               PERFORM RUTD-S
           END-IF
           IF  (I-05)
               PERFORM RUTE-S
           END-IF
           IF  (I-06)
               SET NOT-I-60                TO TRUE
               SET NOT-I-61                TO TRUE
               SET NOT-I-62                TO TRUE
               SET NOT-I-71                TO TRUE
               SET NOT-I-72                TO TRUE
               SET NOT-I-73                TO TRUE
               SET NOT-I-74                TO TRUE
               SET NOT-I-75                TO TRUE
               SET NOT-I-76                TO TRUE
           END-IF
           IF  (I-06 AND NOT-I-95)
               PERFORM RUTX-S
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE    *
      *    FOR FERDIGMELDINGSRAPPORT.                      *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-95                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE OPPGNR                     TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE PROGNR                     TO LPROG (1:6)
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS41E' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-95                    TO TRUE
           IF  LANTX = 0
               SET I-95                    TO TRUE
           END-IF
           IF  (I-U2)
               SET NOT-I-95                TO TRUE
               IF  LANTX < 2
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-U3)
               SET NOT-I-95                TO TRUE
               IF  LANTX < 3
                   SET I-95                TO TRUE
               END-IF
           END-IF.
      *********************************************************************
      *  SUBRUTINE SETTING AV SPACE OG PRINT INDIKATORER  1.HEADINGLINJE. *
      *********************************************************************
 
       RUTA-S SECTION.
       RUTA-S-P.
           SET NOT-I-09                    TO TRUE
           IF  SIDENR = 'N'
               SET I-09                    TO TRUE
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  SPACEA = 'F'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  SPACEA = 'E'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  SKIPA = '0'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  SKIPA = '1'
               SET I-14                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  SKIPA = '2'
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  SKIPA = '3'
               SET I-16                    TO TRUE
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  SKIPA = '6'
               SET I-17                    TO TRUE
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  SKIPA = '7'
               SET I-18                    TO TRUE
           END-IF
           SET NOT-I-19                    TO TRUE
           IF  SKIPA = '8'
               SET I-19                    TO TRUE
           END-IF
           IF  (I-95)
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
               SET NOT-I-14                TO TRUE
               SET NOT-I-15                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-17                TO TRUE
               SET NOT-I-18                TO TRUE
               SET NOT-I-19                TO TRUE
           END-IF.
      *********************************************************************
      *  SUBRUTINE SETTING AV SPACE OG PRINT INDIKATORER  2.HEADINGLINJE. *
      *********************************************************************
 
       RUTB-S SECTION.
       RUTB-S-P.
           SET NOT-I-23                    TO TRUE
           IF  SKIPB = '0'
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  SKIPB = '1'
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  SKIPB = '2'
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  SKIPB = '3'
               SET I-26                    TO TRUE
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  SKIPB = '6'
               SET I-27                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  SKIPB = '7'
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  SKIPB = '8'
               SET I-29                    TO TRUE
           END-IF
           IF  (I-95)
               SET NOT-I-23                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-27                TO TRUE
               SET NOT-I-28                TO TRUE
               SET NOT-I-29                TO TRUE
           END-IF.
      *********************************************************************
      *  SUBRUTINE SETTING AV SPACE OG PRINT INDIKATORER  3.HEADINGLINJE. *
      *********************************************************************
 
       RUTC-S SECTION.
       RUTC-S-P.
           SET NOT-I-33                    TO TRUE
           IF  SKIPC = '0'
               SET I-33                    TO TRUE
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  SKIPC = '1'
               SET I-34                    TO TRUE
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  SKIPC = '2'
               SET I-35                    TO TRUE
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  SKIPC = '3'
               SET I-36                    TO TRUE
           END-IF
           SET NOT-I-37                    TO TRUE
           IF  SKIPC = '6'
               SET I-37                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  SKIPC = '7'
               SET I-38                    TO TRUE
           END-IF
           SET NOT-I-39                    TO TRUE
           IF  SKIPC = '8'
               SET I-39                    TO TRUE
           END-IF
           IF  (I-95)
               SET NOT-I-33                TO TRUE
               SET NOT-I-34                TO TRUE
               SET NOT-I-35                TO TRUE
               SET NOT-I-36                TO TRUE
               SET NOT-I-37                TO TRUE
               SET NOT-I-38                TO TRUE
               SET NOT-I-39                TO TRUE
           END-IF.
      *********************************************************************
      *  SUBRUTINE SETTING AV SPACE OG PRINT INDIKATORER  4.HEADINGLINJE. *
      *********************************************************************
 
       RUTD-S SECTION.
       RUTD-S-P.
           SET NOT-I-43                    TO TRUE
           IF  SKIPD = '0'
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  SKIPD = '1'
               SET I-44                    TO TRUE
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  SKIPD = '2'
               SET I-45                    TO TRUE
           END-IF
           SET NOT-I-46                    TO TRUE
           IF  SKIPD = '3'
               SET I-46                    TO TRUE
           END-IF
           SET NOT-I-47                    TO TRUE
           IF  SKIPD = '6'
               SET I-47                    TO TRUE
           END-IF
           SET NOT-I-48                    TO TRUE
           IF  SKIPD = '7'
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-49                    TO TRUE
           IF  SKIPD = '8'
               SET I-49                    TO TRUE
           END-IF
           IF  (I-95)
               SET NOT-I-43                TO TRUE
               SET NOT-I-44                TO TRUE
               SET NOT-I-45                TO TRUE
               SET NOT-I-46                TO TRUE
               SET NOT-I-47                TO TRUE
               SET NOT-I-48                TO TRUE
               SET NOT-I-49                TO TRUE
           END-IF.
      *********************************************************************
      *  SUBRUTINE SETTING AV SPACE OG PRINT INDIKATORER  5.HEADINGLINJE. *
      *********************************************************************
 
       RUTE-S SECTION.
       RUTE-S-P.
           SET NOT-I-53                    TO TRUE
           IF  SKIPE = '0'
               SET I-53                    TO TRUE
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  SKIPE = '1'
               SET I-54                    TO TRUE
           END-IF
           SET NOT-I-55                    TO TRUE
           IF  SKIPB = '2'
               SET I-55                    TO TRUE
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  SKIPB = '3'
               SET I-56                    TO TRUE
           END-IF
           SET NOT-I-57                    TO TRUE
           IF  SKIPB = '6'
               SET I-57                    TO TRUE
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  SKIPB = '7'
               SET I-58                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  SKIPB = '8'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-95)
               SET NOT-I-53                TO TRUE
               SET NOT-I-54                TO TRUE
               SET NOT-I-55                TO TRUE
               SET NOT-I-56                TO TRUE
               SET NOT-I-57                TO TRUE
               SET NOT-I-58                TO TRUE
               SET NOT-I-59                TO TRUE
           END-IF.
      *********************************************************************
      *  SUBRUTINE SETTING AV SPACE OG PRINT INDIKATORER  DETALJ/TOT LINJE*
      *********************************************************************
 
       RUTX-S SECTION.
       RUTX-S-P.
           SET NOT-I-60                    TO TRUE
           IF  SPACEX = ' '
               SET I-60                    TO TRUE
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  SPACEX = 'F'
               SET I-61                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  SPACEX = 'E'
               SET I-62                    TO TRUE
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  SKIPX = '0'
               SET I-63                    TO TRUE
           END-IF
           SET NOT-I-64                    TO TRUE
           IF  SKIPX = '1'
               SET I-64                    TO TRUE
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  SKIPX = '2'
               SET I-65                    TO TRUE
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  SKIPX = '3'
               SET I-66                    TO TRUE
           END-IF
           SET NOT-I-67                    TO TRUE
           IF  SKIPX = '6'
               SET I-67                    TO TRUE
           END-IF
           SET NOT-I-68                    TO TRUE
           IF  SKIPX = '7'
               SET I-68                    TO TRUE
           END-IF
           SET NOT-I-69                    TO TRUE
           IF  SKIPX = '8'
               SET I-69                    TO TRUE
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  SPACEX = 'G'
               SET I-71                    TO TRUE
           END-IF
           SET NOT-I-72                    TO TRUE
           IF  SPACEX = 'H'
               SET I-72                    TO TRUE
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  SPACEX = 'I'
               SET I-73                    TO TRUE
           END-IF
           SET NOT-I-74                    TO TRUE
           IF  SPACEX = 'J'
               SET I-74                    TO TRUE
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  SPACEX = 'K'
               SET I-75                    TO TRUE
           END-IF
           SET NOT-I-76                    TO TRUE
           IF  SPACEX = 'L'
               SET I-76                    TO TRUE
           END-IF.
      *********************************************************************
 
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
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
               MOVE INFILE-IO-AREA (1:132) TO LINJEA (1:132)
               MOVE INFILE-IO-AREA (133:3) TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (136:1) TO SPACEA (1:1)
               MOVE INFILE-IO-AREA (137:1) TO SKIPA (1:1)
               MOVE INFILE-IO-AREA (138:5) TO OPPGNR (1:5)
               MOVE INFILE-IO-AREA (143:6) TO PROGNR (1:6)
               MOVE INFILE-IO-AREA (149:1) TO SIDENR (1:1)
           WHEN ( INFILE-IO-AREA (150:1) = 'B' )
               MOVE INFILE-IO-AREA (1:132) TO LINJEB (1:132)
               MOVE INFILE-IO-AREA (137:1) TO SKIPB (1:1)
           WHEN ( INFILE-IO-AREA (150:1) = 'C' )
               MOVE INFILE-IO-AREA (1:132) TO LINJEC (1:132)
               MOVE INFILE-IO-AREA (137:1) TO SKIPC (1:1)
           WHEN ( INFILE-IO-AREA (150:1) = 'D' )
               MOVE INFILE-IO-AREA (1:132) TO LINJED (1:132)
               MOVE INFILE-IO-AREA (137:1) TO SKIPD (1:1)
           WHEN ( INFILE-IO-AREA (150:1) = 'E' )
               MOVE INFILE-IO-AREA (1:132) TO LINJEE (1:132)
               MOVE INFILE-IO-AREA (137:1) TO SKIPE (1:1)
           WHEN ( INFILE-IO-AREA (150:1) = 'X' )
               MOVE INFILE-IO-AREA (1:132) TO LINJEX (1:132)
               MOVE INFILE-IO-AREA (136:1) TO SPACEX (1:1)
               MOVE INFILE-IO-AREA (137:1) TO SKIPX (1:1)
           END-EVALUATE.
 
       INFILE-IDCHK SECTION.
       INFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
             OR ( INFILE-IO-AREA (150:1) = 'B' )
             OR ( INFILE-IO-AREA (150:1) = 'C' )
             OR ( INFILE-IO-AREA (150:1) = 'D' )
             OR ( INFILE-IO-AREA (150:1) = 'E' )
             OR ( INFILE-IO-AREA (150:1) = 'X' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
               SET I-01                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'B' )
               SET I-02                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'C' )
               SET I-03                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'D' )
               SET I-04                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'E' )
               SET I-05                    TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'A' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (133:3) TO INFILE-01-L1-FIRMA
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (150:1) = 'B' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'C' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'D' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'E' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (150:1) = 'X' )
               CONTINUE
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
               MOVE 7                      TO LISTE-AFTER-SKIP
           END-IF.
 
       LISTE-SKIP-BEFORE SECTION.
       LISTE-SKIP-BEFORE-P.
           EVALUATE TRUE
           WHEN LISTE-BEFORE-SKIP = 1
               WRITE LISTE-IO-PRINT     AFTER ADVANCING SKIP-CHANNEL-1
           WHEN LISTE-BEFORE-SKIP = 3
               WRITE LISTE-IO-PRINT     AFTER ADVANCING SKIP-CHANNEL-3
           WHEN LISTE-BEFORE-SKIP = 4
               WRITE LISTE-IO-PRINT     AFTER ADVANCING SKIP-CHANNEL-4
           END-EVALUATE
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
           EVALUATE TRUE
           WHEN LISTE-AFTER-SKIP = 1
               WRITE LISTE-IO-PRINT    BEFORE ADVANCING SKIP-CHANNEL-1
           WHEN LISTE-AFTER-SKIP = 3
               WRITE LISTE-IO-PRINT    BEFORE ADVANCING SKIP-CHANNEL-3
           WHEN LISTE-AFTER-SKIP = 4
               WRITE LISTE-IO-PRINT    BEFORE ADVANCING SKIP-CHANNEL-4
           END-EVALUATE
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
           IF  (I-06 AND I-60 AND I-63)
           OR  (I-06 AND I-60 AND I-64)
           OR  (I-06 AND I-60 AND I-65)
           OR  (I-06 AND I-60 AND I-66)
           OR  (I-06 AND I-60 AND I-67)
           OR  (I-06 AND I-60 AND I-68)
           OR  (I-06 AND I-60 AND I-69)
           OR  (I-06 AND I-61 AND I-63)
           OR  (I-06 AND I-61 AND I-64)
           OR  (I-06 AND I-61 AND I-65)
           OR  (I-06 AND I-61 AND I-66)
           OR  (I-06 AND I-61 AND I-67)
           OR  (I-06 AND I-61 AND I-68)
           OR  (I-06 AND I-61 AND I-69)
           OR  (I-06 AND I-62 AND I-63)
           OR  (I-06 AND I-62 AND I-64)
           OR  (I-06 AND I-62 AND I-65)
           OR  (I-06 AND I-62 AND I-66)
           OR  (I-06 AND I-62 AND I-67)
           OR  (I-06 AND I-62 AND I-68)
           OR  (I-06 AND I-62 AND I-69)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJEX                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-06 AND I-60 AND I-63)
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-60 AND I-64)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-60 AND I-65)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-60 AND I-66)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-60 AND I-67)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-60 AND I-68)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-60 AND I-69)
                   MOVE 3                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-61 AND I-63)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-61 AND I-64)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-61 AND I-65)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-61 AND I-66)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-61 AND I-67)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-61 AND I-68)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-61 AND I-69)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-62 AND I-63)
                   MOVE 0                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-62 AND I-64)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-62 AND I-65)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-62 AND I-66)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-62 AND I-67)
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-62 AND I-68)
                   MOVE 2                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-62 AND I-69)
                   MOVE 3                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-06 AND I-73 AND I-63)
           OR  (I-06 AND I-73 AND I-64)
           OR  (I-06 AND I-73 AND I-65)
           OR  (I-06 AND I-73 AND I-66)
           OR  (I-06 AND I-73 AND I-67)
           OR  (I-06 AND I-73 AND I-68)
           OR  (I-06 AND I-73 AND I-69)
           OR  (I-06 AND I-74 AND I-63)
           OR  (I-06 AND I-74 AND I-64)
           OR  (I-06 AND I-74 AND I-65)
           OR  (I-06 AND I-74 AND I-66)
           OR  (I-06 AND I-74 AND I-67)
           OR  (I-06 AND I-74 AND I-68)
           OR  (I-06 AND I-74 AND I-69)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJEX                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-06 AND I-73 AND I-63)
                   MOVE 03                 TO LISTE-BEFORE-SKIP
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-73 AND I-64)
                   MOVE 03                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-73 AND I-65)
                   MOVE 03                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-73 AND I-66)
                   MOVE 03                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-73 AND I-67)
                   MOVE 03                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-73 AND I-68)
                   MOVE 03                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-73 AND I-69)
                   MOVE 03                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-74 AND I-63)
                   MOVE 0                  TO LISTE-AFTER-SPACE
                   MOVE 03                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-74 AND I-64)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   MOVE 03                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-74 AND I-65)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
                   MOVE 03                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-74 AND I-66)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
                   MOVE 03                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-74 AND I-67)
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   MOVE 03                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-74 AND I-68)
                   MOVE 2                  TO LISTE-AFTER-SPACE
                   MOVE 03                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-74 AND I-69)
                   MOVE 3                  TO LISTE-AFTER-SPACE
                   MOVE 03                 TO LISTE-AFTER-SKIP
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-06 AND I-75 AND I-63)
           OR  (I-06 AND I-75 AND I-64)
           OR  (I-06 AND I-75 AND I-65)
           OR  (I-06 AND I-75 AND I-66)
           OR  (I-06 AND I-75 AND I-67)
           OR  (I-06 AND I-75 AND I-68)
           OR  (I-06 AND I-75 AND I-69)
           OR  (I-06 AND I-76 AND I-63)
           OR  (I-06 AND I-76 AND I-64)
           OR  (I-06 AND I-76 AND I-65)
           OR  (I-06 AND I-76 AND I-66)
           OR  (I-06 AND I-76 AND I-67)
           OR  (I-06 AND I-76 AND I-68)
           OR  (I-06 AND I-76 AND I-69)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJEX                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-06 AND I-75 AND I-63)
                   MOVE 04                 TO LISTE-BEFORE-SKIP
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-75 AND I-64)
                   MOVE 04                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-75 AND I-65)
                   MOVE 04                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-75 AND I-66)
                   MOVE 04                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-06 AND I-75 AND I-67)
                   MOVE 04                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-75 AND I-68)
                   MOVE 04                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-75 AND I-69)
                   MOVE 04                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-AFTER-SPACE
               WHEN (I-06 AND I-76 AND I-63)
                   MOVE 0                  TO LISTE-AFTER-SPACE
                   MOVE 04                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-76 AND I-64)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   MOVE 04                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-76 AND I-65)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
                   MOVE 04                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-76 AND I-66)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
                   MOVE 04                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-76 AND I-67)
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   MOVE 04                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-76 AND I-68)
                   MOVE 2                  TO LISTE-AFTER-SPACE
                   MOVE 04                 TO LISTE-AFTER-SKIP
               WHEN (I-06 AND I-76 AND I-69)
                   MOVE 3                  TO LISTE-AFTER-SPACE
                   MOVE 04                 TO LISTE-AFTER-SKIP
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-01 AND I-11 AND I-13)
           OR  (I-01 AND I-11 AND I-14)
           OR  (I-01 AND I-11 AND I-15)
           OR  (I-01 AND I-11 AND I-16)
           OR  (I-01 AND I-11 AND I-17)
           OR  (I-01 AND I-11 AND I-18)
           OR  (I-01 AND I-11 AND I-19)
           OR  (I-01 AND I-12 AND I-13)
           OR  (I-01 AND I-12 AND I-14)
           OR  (I-01 AND I-12 AND I-15)
           OR  (I-01 AND I-12 AND I-16)
           OR  (I-01 AND I-12 AND I-17)
           OR  (I-01 AND I-12 AND I-18)
           OR  (I-01 AND I-12 AND I-19)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJEA                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-01 AND I-11 AND I-13)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-01 AND I-11 AND I-14)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-01 AND I-11 AND I-15)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-01 AND I-11 AND I-16)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-01 AND I-11 AND I-17)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-01 AND I-11 AND I-18)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-01 AND I-11 AND I-19)
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-AFTER-SPACE
               WHEN (I-01 AND I-12 AND I-13)
                   MOVE 0                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-01 AND I-12 AND I-14)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-01 AND I-12 AND I-15)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-01 AND I-12 AND I-16)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-01 AND I-12 AND I-17)
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-01 AND I-12 AND I-18)
                   MOVE 2                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               WHEN (I-01 AND I-12 AND I-19)
                   MOVE 3                  TO LISTE-AFTER-SPACE
                   MOVE 01                 TO LISTE-AFTER-SKIP
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-23)
           OR  (I-02 AND I-24)
           OR  (I-02 AND I-25)
           OR  (I-02 AND I-26)
           OR  (I-02 AND I-27)
           OR  (I-02 AND I-28)
           OR  (I-02 AND I-29)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJEB                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-02 AND I-23)
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-02 AND I-24)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-02 AND I-25)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-02 AND I-26)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-02 AND I-27)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-02 AND I-28)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-02 AND I-29)
                   MOVE 3                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-33)
           OR  (I-03 AND I-34)
           OR  (I-03 AND I-35)
           OR  (I-03 AND I-36)
           OR  (I-03 AND I-37)
           OR  (I-03 AND I-38)
           OR  (I-03 AND I-39)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJEC                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-03 AND I-33)
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-03 AND I-34)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-03 AND I-35)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-03 AND I-36)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-03 AND I-37)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-03 AND I-38)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-03 AND I-39)
                   MOVE 3                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-04 AND I-43)
           OR  (I-04 AND I-44)
           OR  (I-04 AND I-45)
           OR  (I-04 AND I-46)
           OR  (I-04 AND I-47)
           OR  (I-04 AND I-48)
           OR  (I-04 AND I-49)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJED                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-04 AND I-43)
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-04 AND I-44)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-04 AND I-45)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-04 AND I-46)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-04 AND I-47)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-04 AND I-48)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-04 AND I-49)
                   MOVE 3                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-05 AND I-53)
           OR  (I-05 AND I-54)
           OR  (I-05 AND I-55)
           OR  (I-05 AND I-56)
           OR  (I-05 AND I-57)
           OR  (I-05 AND I-58)
           OR  (I-05 AND I-59)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LINJEE                 TO LISTE-IO-AREA (1:132)
               EVALUATE TRUE
               WHEN (I-05 AND I-53)
                   MOVE 0                  TO LISTE-AFTER-SPACE
               WHEN (I-05 AND I-54)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
               WHEN (I-05 AND I-55)
                   MOVE 2                  TO LISTE-BEFORE-SPACE
               WHEN (I-05 AND I-56)
                   MOVE 3                  TO LISTE-BEFORE-SPACE
               WHEN (I-05 AND I-57)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-05 AND I-58)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-05 AND I-59)
                   MOVE 3                  TO LISTE-AFTER-SPACE
               END-EVALUATE
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
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
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
