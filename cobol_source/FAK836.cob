       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK836R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK836 INPUTKONTROLL AV UKONTROLLERTE REGNSKAPS OG   *        *
      *                 RESKONTRODATA.                                *        *
      *                 KANSELLERER I NESTE STEP, PROGRAM RSK837,     *        *
      *                 HVIS DET ER FEIL I DATAENE.                   *        *
      * PROGR. : MORTEN TUVRØNNINGEN                                  *
      * ENDRING: 25.09.98 KANSELLERER VED TOM FILE, EGEN MELDING HVIS *
      *                   MISTANKE OM FEIL FRA AUTO DATA (RECORDART). *
      * ENDRING: 31.07.06 NY FEILMELDING                              *
      ***************************************************************** ********
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK836.rpg
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
           SELECT REGFILE
               ASSIGN TO UT-S-REGFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGFILE-STATUS.
           SELECT RESFILE
               ASSIGN TO UT-S-RESFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILE-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT LISTE2
               ASSIGN TO UT-S-LISTE2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE2-STATUS.
           SELECT FEILFIL
               ASSIGN TO UT-S-FEILFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FEILFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REGFILE
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  REGFILE-IO-AREA.
           05  REGFILE-IO-AREA-X           PICTURE X(80).
       FD RESFILE
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  RESFILE-IO-AREA.
           05  RESFILE-IO-AREA-X           PICTURE X(70).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE2-IO-PRINT.
           05  LISTE2-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE2-IO-AREA.
           05  LISTE2-IO-AREA-X            PICTURE X(132).
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
       FD FEILFIL
               BLOCK CONTAINS 40
               RECORD CONTAINS 20.
       01  FEILFIL-IO-AREA.
           05  FEILFIL-IO-AREA-X           PICTURE X(20).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REGFILE-STATUS              PICTURE 99 VALUE 0.
           10  RESFILE-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LISTE2-STATUS               PICTURE 99 VALUE 0.
           10  FEILFIL-STATUS              PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFILE-EOF-OFF         VALUE '0'.
               88  REGFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFILE-READ-OFF        VALUE '0'.
               88  REGFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFILE-PROCESS-OFF     VALUE '0'.
               88  REGFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  REGFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  REGFILE-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-EOF-OFF         VALUE '0'.
               88  RESFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-READ-OFF        VALUE '0'.
               88  RESFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-PROCESS-OFF     VALUE '0'.
               88  RESFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  RESFILE-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  CONSOLE-IO-AREA.
               10  CONSOLE-IO-AREA-X       PICTURE X(800).
      *DSDS: DATA STRUCTURE FIELDS
           05  DTOPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DTODIV                  PICTURE X(73).
               10  FILLER                  PICTURE X(177).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  PSDS REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(28).
               10  R                       PICTURE X(8).
               10  FILLER                  PICTURE X(221).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  P-IO.
                   15  P                   PICTURE S9(3).
               10  FILLER                  PICTURE X(218).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(10).
               10  S-IO.
                   15  S                   PICTURE S9(5).
               10  FILLER                  PICTURE X(242).
           05  LDATA-XX REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
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
           05  REGFILE-LEVEL-01.
               10  REGFILE-01-L4.
                   15  REGFILE-01-L4-REGART PICTURE X(2).
               10  REGFILE-01-L3.
                   15  REGFILE-01-L3-REGFNR PICTURE X(3).
           05  REGFILE-LEVEL-05.
               10  REGFILE-05-L4.
                   15  REGFILE-05-L4-REGART PICTURE X(2).
               10  REGFILE-05-L3.
                   15  REGFILE-05-L3-REGFNR PICTURE X(3).
           05  REGFILE-DATA-FIELDS.
               10  REGREC                  PICTURE X(80).
               10  REGART                  PICTURE X(2).
               10  REGBNR                  PICTURE X(6).
               10  REGBAR                  PICTURE X(2).
               10  REGBMN                  PICTURE X(2).
               10  REGBDA                  PICTURE X(2).
               10  REGKNR                  PICTURE X(6).
               10  REGFNR                  PICTURE X(3).
               10  REGFDA                  PICTURE X(2).
               10  REGFMN                  PICTURE X(2).
               10  REGFAR                  PICTURE X(2).
           05  RESFILE-LEVEL-02.
               10  RESFILE-02-L4.
                   15  RESFILE-02-L4-RESART PICTURE X(2).
               10  RESFILE-02-L3.
                   15  RESFILE-02-L3-RESFNR PICTURE X(3).
           05  RESFILE-LEVEL-06.
               10  RESFILE-06-L4.
                   15  RESFILE-06-L4-RESART PICTURE X(2).
               10  RESFILE-06-L3.
                   15  RESFILE-06-L3-RESFNR PICTURE X(3).
           05  RESFILE-DATA-FIELDS.
               10  RESART                  PICTURE X(2).
               10  RESREC                  PICTURE X(70).
               10  RESKNR                  PICTURE X(6).
               10  RESBAR                  PICTURE X(2).
               10  RESBMN                  PICTURE X(2).
               10  RESBDA                  PICTURE X(2).
               10  RESBNR                  PICTURE X(6).
               10  RESFAR                  PICTURE X(2).
               10  RESFMN                  PICTURE X(2).
               10  RESFDA                  PICTURE X(2).
               10  RESFNR                  PICTURE X(3).
           05  KUNDEMA-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  FIRMAF-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(2).
               10  THE-PRIOR-L3            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  HJFNR                   PICTURE X(3).
               10  HJKNR                   PICTURE X(6).
               10  ANTREG-IO.
                   15  ANTREG              PICTURE S9(4).
               10  ANTRES-IO.
                   15  ANTRES              PICTURE S9(4).
               10  HJKKEY                  PICTURE X(9).
               10  HJ4X                    PICTURE X(4).
               10  HJ6X                    PICTURE X(6).
               10  HJBNR                   PICTURE X(6).
               10  FEIREG-IO.
                   15  FEIREG              PICTURE S9(4).
               10  FEIRES-IO.
                   15  FEIRES              PICTURE S9(4).
               10  HJANT-IO.
                   15  HJANT               PICTURE S9(4).
           05  EDITTING-FIELDS.
               10  XO-40YY9                PICTURE Z.ZZ9.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  REGFILE-PROCESS
               SET REGFILE-PROCESS-OFF     TO TRUE
               SET REGFILE-READ            TO TRUE
           END-IF
 
           IF  REGFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM REGFILE-GET
               SET REGFILE-READ-OFF        TO TRUE
               IF  NOT REGFILE-EOF
                   PERFORM REGFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET REGFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RESFILE-PROCESS
               SET RESFILE-PROCESS-OFF     TO TRUE
               SET RESFILE-READ            TO TRUE
           END-IF
 
           IF  RESFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESFILE-GET
               SET RESFILE-READ-OFF        TO TRUE
               IF  NOT RESFILE-EOF
                   PERFORM RESFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RESFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REGFILE-PROCESS
               PERFORM REGFILE-IDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-IDSET
           END-IF
 
           IF  REGFILE-PROCESS
               PERFORM REGFILE-CHK-LEVEL
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  REGFILE-PROCESS
               PERFORM REGFILE-FLDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  REGFILE-PROCESS
           OR  RESFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-24                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-96                    TO TRUE
           IF  (I-05 AND NOT-I-80)
               SET I-80                    TO TRUE
               SET I-96                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-06 AND NOT-I-80)
               SET I-80                    TO TRUE
               SET I-96                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-01)
               OR  (I-05)
               MOVE REGFNR                 TO HJFNR
               MOVE REGKNR                 TO HJKNR
               ADD 1                       TO ANTREG
           END-IF
           IF  (I-02)
               OR  (I-06)
               MOVE RESFNR                 TO HJFNR
               MOVE RESKNR                 TO HJKNR
               ADD 1                       TO ANTRES
           END-IF
           MOVE HJFNR                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-21                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-21                TO TRUE
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (I-21)
               SET I-96                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-L3)
               PERFORM RBSRUT-S
      *
           END-IF
           MOVE HJFNR                      TO HJKKEY (1:3)
           MOVE HJKNR                      TO HJKKEY (4:6)
           MOVE HJKKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-22                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-22                TO TRUE
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (I-22)
               SET I-96                    TO TRUE
               SET I-99                    TO TRUE
      *
           END-IF
           IF  (I-01)
               OR  (I-05)
               MOVE REGBAR                 TO HJ4X (1:2)
               MOVE REGBMN                 TO HJ4X (3:2)
               MOVE HJ4X                   TO HJ6X (1:4)
               MOVE REGBDA                 TO HJ6X (5:2)
           END-IF
           IF  (I-02)
               OR  (I-02)
               MOVE RESBAR                 TO HJ4X (1:2)
               MOVE RESBMN                 TO HJ4X (3:2)
               MOVE HJ4X                   TO HJ6X (1:4)
               MOVE RESBDA                 TO HJ6X (5:2)
           END-IF
           MOVE HJ6X                       TO DTODTO
           MOVE 'B'                        TO DTOKOD
           PERFORM DTORUT-S
           IF  (I-98)
               SET I-23                    TO TRUE
               SET I-96                    TO TRUE
               SET I-99                    TO TRUE
      *
           END-IF
           IF  (I-01)
               OR  (I-05)
               MOVE REGFAR                 TO HJ4X (1:2)
               MOVE REGFMN                 TO HJ4X (3:2)
               MOVE HJ4X                   TO HJ6X (1:4)
               MOVE REGFDA                 TO HJ6X (5:2)
           END-IF
           IF  (I-02)
               OR  (I-06)
               MOVE RESFAR                 TO HJ4X (1:2)
               MOVE RESFMN                 TO HJ4X (3:2)
               MOVE HJ4X                   TO HJ6X (1:4)
               MOVE RESFDA                 TO HJ6X (5:2)
           END-IF
           MOVE HJ6X                       TO DTODTO
           MOVE 'B'                        TO DTOKOD
           PERFORM DTORUT-S
           IF  (I-98)
               SET I-24                    TO TRUE
               SET I-96                    TO TRUE
               SET I-99                    TO TRUE
      *                    MOVE "DTOKOD  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGBUGFILO   DTOKOD           VIS INDIKATOR
      *
           END-IF
           IF  (I-01)
               OR  (I-05)
               MOVE REGBNR                 TO HJBNR
           END-IF
           IF  (I-02)
               OR  (I-06)
               MOVE RESBNR                 TO HJBNR
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  HJBNR NOT > '000000'
               SET I-25                    TO TRUE
           END-IF
           IF  (NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  HJBNR > '999999'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-25)
               SET I-96                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-01 AND I-96)
               OR  (I-05)
               ADD 1                       TO FEIREG
           END-IF
           IF  (I-02 AND I-96)
               OR  (I-06)
               ADD 1                       TO FEIRES
           END-IF.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-98                    TO TRUE
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-98                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-98                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'FAK54'                    TO LONR
           MOVE HJFNR                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK836  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD ANTREG TO ZERO          GIVING HJANT
           SET NOT-I-81                    TO TRUE
           IF  HJANT = 0
               SET I-81                    TO TRUE
           END-IF
           ADD ANTRES TO ZERO          GIVING HJANT
           SET NOT-I-82                    TO TRUE
           IF  HJANT = 0
               SET I-82                    TO TRUE
           END-IF
           IF  (I-80)
               SET I-99                    TO TRUE
           END-IF
           IF  (I-81)
               SET I-99                    TO TRUE
               SET I-83                    TO TRUE
           END-IF
           IF  (I-82)
               SET I-99                    TO TRUE
               SET I-83                    TO TRUE
      *
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
           END-IF
           .
 
       REGFILE-GET SECTION.
       REGFILE-GET-P.
           IF  REGFILE-EOF-OFF
               READ REGFILE
               AT END
                   SET REGFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REGFILE-FLDSET SECTION.
       REGFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( REGFILE-IO-AREA (1:1) = '0'
            AND   REGFILE-IO-AREA (2:1) = '1' )
               MOVE REGFILE-IO-AREA (1:80) TO REGREC (1:80)
               MOVE REGFILE-IO-AREA (1:2)  TO REGART (1:2)
               MOVE REGFILE-IO-AREA (3:6)  TO REGBNR (1:6)
               MOVE REGFILE-IO-AREA (10:2) TO REGBAR (1:2)
               MOVE REGFILE-IO-AREA (12:2) TO REGBMN (1:2)
               MOVE REGFILE-IO-AREA (14:2) TO REGBDA (1:2)
               MOVE REGFILE-IO-AREA (16:6) TO REGKNR (1:6)
               MOVE REGFILE-IO-AREA (22:3) TO REGFNR (1:3)
               MOVE REGFILE-IO-AREA (63:2) TO REGFDA (1:2)
               MOVE REGFILE-IO-AREA (65:2) TO REGFMN (1:2)
               MOVE REGFILE-IO-AREA (67:2) TO REGFAR (1:2)
           WHEN ( REGFILE-IO-AREA (1:1) NOT = '0' )
               MOVE REGFILE-IO-AREA (1:80) TO REGREC (1:80)
               MOVE REGFILE-IO-AREA (1:2)  TO REGART (1:2)
               MOVE REGFILE-IO-AREA (3:6)  TO REGBNR (1:6)
               MOVE REGFILE-IO-AREA (10:2) TO REGBAR (1:2)
               MOVE REGFILE-IO-AREA (12:2) TO REGBMN (1:2)
               MOVE REGFILE-IO-AREA (14:2) TO REGBDA (1:2)
               MOVE REGFILE-IO-AREA (16:6) TO REGKNR (1:6)
               MOVE REGFILE-IO-AREA (22:3) TO REGFNR (1:3)
               MOVE REGFILE-IO-AREA (63:2) TO REGFDA (1:2)
               MOVE REGFILE-IO-AREA (65:2) TO REGFMN (1:2)
               MOVE REGFILE-IO-AREA (67:2) TO REGFAR (1:2)
           END-EVALUATE.
 
       REGFILE-IDCHK SECTION.
       REGFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( REGFILE-IO-AREA (1:1) = '0'
            AND   REGFILE-IO-AREA (2:1) = '1' )
             OR ( REGFILE-IO-AREA (1:1) NOT = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       REGFILE-IDSET SECTION.
       REGFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( REGFILE-IO-AREA (1:1) = '0'
            AND   REGFILE-IO-AREA (2:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( REGFILE-IO-AREA (1:1) NOT = '0' )
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       REGFILE-CHK-LEVEL SECTION.
       REGFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( REGFILE-IO-AREA (1:1) = '0'
            AND   REGFILE-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO REGFILE-LEVEL-01
               MOVE REGFILE-IO-AREA (1:2)  TO REGFILE-01-L4-REGART
               MOVE REGFILE-IO-AREA (22:3) TO REGFILE-01-L3-REGFNR
               IF  REGFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  REGFILE-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  REGFILE-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   END-EVALUATE
               END-IF
               MOVE  REGFILE-01-L4         TO THE-PRIOR-L4
               MOVE  REGFILE-01-L3         TO THE-PRIOR-L3
               SET REGFILE-LEVEL-INIT      TO TRUE
           WHEN ( REGFILE-IO-AREA (1:1) NOT = '0' )
               MOVE LOW-VALUES             TO REGFILE-LEVEL-05
               MOVE REGFILE-IO-AREA (1:2)  TO REGFILE-05-L4-REGART
               MOVE REGFILE-IO-AREA (22:3) TO REGFILE-05-L3-REGFNR
               IF  REGFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  REGFILE-05-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  REGFILE-05-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   END-EVALUATE
               END-IF
               MOVE  REGFILE-05-L4         TO THE-PRIOR-L4
               MOVE  REGFILE-05-L3         TO THE-PRIOR-L3
               SET REGFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESFILE-GET SECTION.
       RESFILE-GET-P.
           IF  RESFILE-EOF-OFF
               READ RESFILE
               AT END
                   SET RESFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESFILE-FLDSET SECTION.
       RESFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
               MOVE RESFILE-IO-AREA (1:2)  TO RESART (1:2)
               MOVE RESFILE-IO-AREA (1:70) TO RESREC (1:70)
               MOVE RESFILE-IO-AREA (5:6)  TO RESKNR (1:6)
               MOVE RESFILE-IO-AREA (11:2) TO RESBAR (1:2)
               MOVE RESFILE-IO-AREA (13:2) TO RESBMN (1:2)
               MOVE RESFILE-IO-AREA (15:2) TO RESBDA (1:2)
               MOVE RESFILE-IO-AREA (23:6) TO RESBNR (1:6)
               MOVE RESFILE-IO-AREA (29:2) TO RESFAR (1:2)
               MOVE RESFILE-IO-AREA (31:2) TO RESFMN (1:2)
               MOVE RESFILE-IO-AREA (33:2) TO RESFDA (1:2)
               MOVE RESFILE-IO-AREA (44:3) TO RESFNR (1:3)
           WHEN ( RESFILE-IO-AREA (1:1) NOT = '0' )
               MOVE RESFILE-IO-AREA (1:2)  TO RESART (1:2)
               MOVE RESFILE-IO-AREA (1:70) TO RESREC (1:70)
               MOVE RESFILE-IO-AREA (5:6)  TO RESKNR (1:6)
               MOVE RESFILE-IO-AREA (11:2) TO RESBAR (1:2)
               MOVE RESFILE-IO-AREA (13:2) TO RESBMN (1:2)
               MOVE RESFILE-IO-AREA (15:2) TO RESBDA (1:2)
               MOVE RESFILE-IO-AREA (23:6) TO RESBNR (1:6)
               MOVE RESFILE-IO-AREA (29:2) TO RESFAR (1:2)
               MOVE RESFILE-IO-AREA (31:2) TO RESFMN (1:2)
               MOVE RESFILE-IO-AREA (33:2) TO RESFDA (1:2)
               MOVE RESFILE-IO-AREA (44:3) TO RESFNR (1:3)
           END-EVALUATE.
 
       RESFILE-IDCHK SECTION.
       RESFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
             OR ( RESFILE-IO-AREA (1:1) NOT = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESFILE-IDSET SECTION.
       RESFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( RESFILE-IO-AREA (1:1) NOT = '0' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       RESFILE-CHK-LEVEL SECTION.
       RESFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO RESFILE-LEVEL-02
               MOVE RESFILE-IO-AREA (1:2)  TO RESFILE-02-L4-RESART
               MOVE RESFILE-IO-AREA (44:3) TO RESFILE-02-L3-RESFNR
               IF  RESFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESFILE-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RESFILE-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   END-EVALUATE
               END-IF
               MOVE  RESFILE-02-L4         TO THE-PRIOR-L4
               MOVE  RESFILE-02-L3         TO THE-PRIOR-L3
               SET RESFILE-LEVEL-INIT      TO TRUE
           WHEN ( RESFILE-IO-AREA (1:1) NOT = '0' )
               MOVE LOW-VALUES             TO RESFILE-LEVEL-06
               MOVE RESFILE-IO-AREA (1:2)  TO RESFILE-06-L4-RESART
               MOVE RESFILE-IO-AREA (44:3) TO RESFILE-06-L3-RESFNR
               IF  RESFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESFILE-06-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RESFILE-06-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   END-EVALUATE
               END-IF
               MOVE  RESFILE-06-L4         TO THE-PRIOR-L4
               MOVE  RESFILE-06-L3         TO THE-PRIOR-L3
               SET RESFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
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
           IF  (I-96)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-01)
                   MOVE '=> FEIL REGNSKAPS-TRANS:' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-01)
                   MOVE REGREC             TO LISTE-IO-AREA (31:80)
               END-IF
               IF  (I-05)
                   MOVE '=> FEIL REGNSKAPS-TRANS:' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-05)
                   MOVE REGREC             TO LISTE-IO-AREA (31:80)
               END-IF
               IF  (I-02)
                   MOVE '=> FEIL RESKONTRO-TRANS:' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-02)
                   MOVE RESREC             TO LISTE-IO-AREA (31:70)
               END-IF
               IF  (I-06)
                   MOVE '=> FEIL RESKONTRO-TRANS:' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-06)
                   MOVE RESREC             TO LISTE-IO-AREA (31:70)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-01)
           OR  (I-96 AND I-05)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '===> POSISJON:'       TO LISTE-IO-AREA (11:14)
               IF  (I-05)
                   MOVE '++'               TO LISTE-IO-AREA (31:2)
               END-IF
               IF  (I-21)
                   MOVE '---'              TO LISTE-IO-AREA (52:3)
               END-IF
               IF  (I-22)
                   MOVE '++++++'           TO LISTE-IO-AREA (46:6)
               END-IF
               IF  (I-23)
                   MOVE '------'           TO LISTE-IO-AREA (40:6)
               END-IF
               IF  (I-24)
                   MOVE '++++++'           TO LISTE-IO-AREA (93:6)
               END-IF
               IF  (I-25)
                   MOVE '------'           TO LISTE-IO-AREA (33:6)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '===> DATA    :'       TO LISTE-IO-AREA (11:14)
               IF  (I-05)
                   MOVE 'A '               TO LISTE-IO-AREA (31:2)
               END-IF
               IF  (I-21)
                   MOVE ' F '              TO LISTE-IO-AREA (52:3)
               END-IF
               IF  (I-22)
                   MOVE ' RNR  '           TO LISTE-IO-AREA (46:6)
               END-IF
               IF  (I-23)
                   MOVE ' BDTO '           TO LISTE-IO-AREA (40:6)
               END-IF
               IF  (I-24)
                   MOVE ' FDTO '           TO LISTE-IO-AREA (93:6)
               END-IF
               IF  (I-25)
                   MOVE ' BNR  '           TO LISTE-IO-AREA (33:6)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-02)
           OR  (I-96 AND I-06)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '===> POSISJON:'       TO LISTE-IO-AREA (11:14)
               IF  (I-06)
                   MOVE '++'               TO LISTE-IO-AREA (31:2)
               END-IF
               IF  (I-21)
                   MOVE '---'              TO LISTE-IO-AREA (74:3)
               END-IF
               IF  (I-22)
                   MOVE '++++++'           TO LISTE-IO-AREA (35:6)
               END-IF
               IF  (I-23)
                   MOVE '------'           TO LISTE-IO-AREA (41:6)
               END-IF
               IF  (I-24)
                   MOVE '++++++'           TO LISTE-IO-AREA (59:6)
               END-IF
               IF  (I-25)
                   MOVE '------'           TO LISTE-IO-AREA (53:6)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '===> DATA    :'       TO LISTE-IO-AREA (11:14)
               IF  (I-06)
                   MOVE 'A '               TO LISTE-IO-AREA (31:2)
               END-IF
               IF  (I-21)
                   MOVE ' F '              TO LISTE-IO-AREA (74:3)
               END-IF
               IF  (I-22)
                   MOVE ' RNR  '           TO LISTE-IO-AREA (35:6)
               END-IF
               IF  (I-23)
                   MOVE ' BDTO '           TO LISTE-IO-AREA (41:6)
               END-IF
               IF  (I-24)
                   MOVE ' FDTO '           TO LISTE-IO-AREA (59:6)
               END-IF
               IF  (I-25)
                   MOVE ' BNR  '           TO LISTE-IO-AREA (53:6)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U7 AND I-U8 AND I-03)
           AND (I-04 AND I-86 AND I-97)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DTODTO                 TO LISTE-IO-AREA (115:6)
               MOVE DTODIV                 TO LISTE-IO-AREA (48:73)
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE LONR                   TO LISTE-IO-AREA (116:5)
               MOVE LFIRMA                 TO LISTE-IO-AREA (118:3)
               MOVE LUNDGR                 TO LISTE-IO-AREA (118:3)
               MOVE LPROG                  TO LISTE-IO-AREA (113:8)
               MOVE LOPNVN                 TO LISTE-IO-AREA (86:35)
               MOVE LANTX-IO               TO LISTE-IO-AREA (118:3)
               MOVE LPRIID                 TO LISTE-IO-AREA (117:4)
               MOVE BJOBN                  TO LISTE-IO-AREA (113:8)
               MOVE BBEST                  TO LISTE-IO-AREA (120:1)
               MOVE BPERS                  TO LISTE-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE-IO-AREA (118:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-05)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE FINAVN                 TO LISTE2-IO-AREA (1:30)
               MOVE '=> FEIL REGNSKAPS-TRANS:' TO LISTE2-IO-AREA (1:24)
               MOVE REGREC                 TO LISTE2-IO-AREA (31:80)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-06)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '=> FEIL RESKONTRO-TRANS:' TO LISTE2-IO-AREA (1:24)
               MOVE RESREC                 TO LISTE2-IO-AREA (31:70)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-05)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '===> POSISJON:'       TO LISTE2-IO-AREA (11:14)
               MOVE '++'                   TO LISTE2-IO-AREA (31:2)
               IF  (I-21)
                   MOVE '---'              TO LISTE2-IO-AREA (52:3)
               END-IF
               IF  (I-22)
                   MOVE '++++++'           TO LISTE2-IO-AREA (46:6)
               END-IF
               IF  (I-23)
                   MOVE '------'           TO LISTE2-IO-AREA (40:6)
               END-IF
               IF  (I-24)
                   MOVE '++++++'           TO LISTE2-IO-AREA (93:6)
               END-IF
               IF  (I-25)
                   MOVE '------'           TO LISTE2-IO-AREA (33:6)
               END-IF
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '===> DATA    :'       TO LISTE2-IO-AREA (11:14)
               MOVE 'A '                   TO LISTE2-IO-AREA (31:2)
               IF  (I-21)
                   MOVE ' F '              TO LISTE2-IO-AREA (52:3)
               END-IF
               IF  (I-22)
                   MOVE ' RNR  '           TO LISTE2-IO-AREA (46:6)
               END-IF
               IF  (I-23)
                   MOVE ' BDTO '           TO LISTE2-IO-AREA (40:6)
               END-IF
               IF  (I-24)
                   MOVE ' FDTO '           TO LISTE2-IO-AREA (93:6)
               END-IF
               IF  (I-25)
                   MOVE ' BNR  '           TO LISTE2-IO-AREA (33:6)
               END-IF
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-06)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '===> POSISJON:'       TO LISTE2-IO-AREA (11:14)
               MOVE '++'                   TO LISTE2-IO-AREA (31:2)
               IF  (I-21)
                   MOVE '---'              TO LISTE2-IO-AREA (74:3)
               END-IF
               IF  (I-22)
                   MOVE '++++++'           TO LISTE2-IO-AREA (35:6)
               END-IF
               IF  (I-23)
                   MOVE '------'           TO LISTE2-IO-AREA (41:6)
               END-IF
               IF  (I-24)
                   MOVE '++++++'           TO LISTE2-IO-AREA (59:6)
               END-IF
               IF  (I-25)
                   MOVE '------'           TO LISTE2-IO-AREA (53:6)
               END-IF
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '===> DATA    :'       TO LISTE2-IO-AREA (11:14)
               MOVE 'A '                   TO LISTE2-IO-AREA (31:2)
               IF  (I-21)
                   MOVE ' F '              TO LISTE2-IO-AREA (74:3)
               END-IF
               IF  (I-22)
                   MOVE ' RNR  '           TO LISTE2-IO-AREA (35:6)
               END-IF
               IF  (I-23)
                   MOVE ' BDTO '           TO LISTE2-IO-AREA (41:6)
               END-IF
               IF  (I-24)
                   MOVE ' FDTO '           TO LISTE2-IO-AREA (59:6)
               END-IF
               IF  (I-25)
                   MOVE ' BNR  '           TO LISTE2-IO-AREA (53:6)
               END-IF
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-U7 AND I-U8 AND I-03)
           AND (I-04 AND I-86 AND I-97)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE DTODTO                 TO LISTE2-IO-AREA (115:6)
               MOVE DTODIV                 TO LISTE2-IO-AREA (48:73)
               MOVE PSDS                   TO LISTE2-IO-AREA (41:80)
               MOVE R                      TO LISTE2-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE2-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE2-IO-AREA (116:5)
               MOVE LONR                   TO LISTE2-IO-AREA (116:5)
               MOVE LFIRMA                 TO LISTE2-IO-AREA (118:3)
               MOVE LUNDGR                 TO LISTE2-IO-AREA (118:3)
               MOVE LPROG                  TO LISTE2-IO-AREA (113:8)
               MOVE LOPNVN                 TO LISTE2-IO-AREA (86:35)
               MOVE LANTX-IO               TO LISTE2-IO-AREA (118:3)
               MOVE LPRIID                 TO LISTE2-IO-AREA (117:4)
               MOVE BJOBN                  TO LISTE2-IO-AREA (113:8)
               MOVE BBEST                  TO LISTE2-IO-AREA (120:1)
               MOVE BPERS                  TO LISTE2-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE2-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE2-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE2-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE2-IO-AREA (118:3)
               MOVE BPCLAS                 TO LISTE2-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE2-IO-AREA (118:3)
      *****************************************************************
      * FEILMELDINGER PÅ KONSOLLET:                                   *
      *****************************************************************
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** KONTROLL AV PC-OVERFØ' TO LISTE-IO-AREA (11:24)
               MOVE 'RTE REGNSKAPS OG RESKONT' TO LISTE-IO-AREA (35:24)
               MOVE 'RODATA **   PROGRAM FAK8' TO LISTE-IO-AREA (59:24)
               MOVE '36 '                  TO LISTE-IO-AREA (83:3)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (125:8)
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
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
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
           END-IF
           IF  (I-L4)
           OR  (I-83)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '** KONTROLL AV PC-OVERFØ' TO LISTE2-IO-AREA
                                                               (11:24)
               MOVE 'RTE REGNSKAPS OG RESKONT' TO LISTE2-IO-AREA
                                                               (35:24)
               MOVE 'RODATA **   PROGRAM FAK8' TO LISTE2-IO-AREA
                                                               (59:24)
               MOVE '36 '                  TO LISTE2-IO-AREA (83:3)
               MOVE 'FRAMSTILT'            TO LISTE2-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (125:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
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
               MOVE '------------'         TO LISTE2-IO-AREA (121:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** KONTROLL AV PC-OVERFØ' TO LISTE-IO-AREA (11:24)
               MOVE 'RTE REGNSKAPS OG RESKONT' TO LISTE-IO-AREA (35:24)
               MOVE 'RODATA **   PROGRAM FAK8' TO LISTE-IO-AREA (59:24)
               MOVE '36 '                  TO LISTE-IO-AREA (83:3)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (125:8)
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
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
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
           END-IF
           IF  (I-OV)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '** KONTROLL AV PC-OVERFØ' TO LISTE2-IO-AREA
                                                               (11:24)
               MOVE 'RTE REGNSKAPS OG RESKONT' TO LISTE2-IO-AREA
                                                               (35:24)
               MOVE 'RODATA **   PROGRAM FAK8' TO LISTE2-IO-AREA
                                                               (59:24)
               MOVE '36 '                  TO LISTE2-IO-AREA (83:3)
               MOVE 'FRAMSTILT'            TO LISTE2-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (125:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
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
               MOVE '------------'         TO LISTE2-IO-AREA (121:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-83)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** KONTROLL AV PC-OVERFØ' TO LISTE-IO-AREA (11:24)
               MOVE 'RTE REGNSKAPS OG RESKONT' TO LISTE-IO-AREA (35:24)
               MOVE 'RODATA **   PROGRAM FAK8' TO LISTE-IO-AREA (59:24)
               MOVE '36 '                  TO LISTE-IO-AREA (83:3)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (125:8)
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
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99 AND NOT-I-83)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' A     = RECORDART      ' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' F     = FIRMANR        ' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' RNR   = RESKONTRONR    ' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' BDTO  = BILAGSDATO     ' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' FDTO  = FORFALLSDATO   ' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' BNR   = BILAGSNR       ' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-81)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** TOM REGNSKAPSFILE  **' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-82)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** TOM RESKONTROFILE  **' TO LISTE-IO-AREA (1:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL REGN.-TRANSER  : ' TO LISTE-IO-AREA (1:24)
               MOVE ANTREG                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (26:5)
               MOVE 'ANTALL FEIL-TRANSER   : ' TO LISTE-IO-AREA (35:24)
               MOVE FEIREG                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (60:5)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL RESK.-TRANSER  : ' TO LISTE-IO-AREA (1:24)
               MOVE ANTRES                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (26:5)
               MOVE 'ANTALL FEIL-TRANSER   : ' TO LISTE-IO-AREA (35:24)
               MOVE FEIRES                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (60:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '========================' TO LISTE-IO-AREA (1:24)
               MOVE '========================' TO LISTE-IO-AREA (25:24)
               MOVE '========================' TO LISTE-IO-AREA (49:24)
               MOVE '========================' TO LISTE-IO-AREA (73:24)
               MOVE '========================' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99 AND NOT-I-83)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '===> ALLE TRANSAKSJONER ' TO LISTE-IO-AREA (1:24)
               MOVE 'ER AVVIST. VED OMKJØRING' TO LISTE-IO-AREA (25:24)
               MOVE ' MÅ OVENSTÅENDE FEIL RET' TO LISTE-IO-AREA (49:24)
               MOVE 'TES OG HELE SENDINGEN SE' TO LISTE-IO-AREA (73:24)
               MOVE 'NDES PÅ NYTT.       <===' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99 AND I-83)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '===> KJØRING STOPPET. TR' TO LISTE-IO-AREA (1:24)
               MOVE 'OLIG FEIL I AUTO DATA. S' TO LISTE-IO-AREA (25:24)
               MOVE 'JEKK OM FAKTURAER ER SEN' TO LISTE-IO-AREA (49:24)
               MOVE 'DT, EVT OM SENDINGEN INN' TO LISTE-IO-AREA (73:24)
               MOVE 'EHOLDER FAKTURAER.  <===' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '========================' TO LISTE-IO-AREA (1:24)
               MOVE '========================' TO LISTE-IO-AREA (25:24)
               MOVE '========================' TO LISTE-IO-AREA (49:24)
               MOVE '========================' TO LISTE-IO-AREA (73:24)
               MOVE '========================' TO LISTE-IO-AREA (97:24)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-83)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '** KONTROLL AV PC-OVERFØ' TO LISTE2-IO-AREA
                                                               (11:24)
               MOVE 'RTE REGNSKAPS OG RESKONT' TO LISTE2-IO-AREA
                                                               (35:24)
               MOVE 'RODATA **   PROGRAM FAK8' TO LISTE2-IO-AREA
                                                               (59:24)
               MOVE '36 '                  TO LISTE2-IO-AREA (83:3)
               MOVE 'FRAMSTILT'            TO LISTE2-IO-AREA (115:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (125:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
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
               MOVE '------------'         TO LISTE2-IO-AREA (121:12)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-81)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '** TOM REGNSKAPSFILE  **' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-82)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '** TOM RESKONTROFILE  **' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-80)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE ' A     = RECORDART      ' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE ' F     = FIRMANR        ' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE ' RNR   = RESKONTRONR    ' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE ' BDTO  = BILAGSDATO     ' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE ' FDTO  = FORFALLSDATO   ' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE ' BNR   = BILAGSNR       ' TO LISTE2-IO-AREA (1:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'ANTALL REGN.-TRANSER  : ' TO LISTE2-IO-AREA (1:24)
               MOVE ANTREG                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE2-IO-AREA (26:5)
               MOVE 'ANTALL FEIL-TRANSER   : ' TO LISTE2-IO-AREA
                                                               (35:24)
               MOVE FEIREG                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE2-IO-AREA (60:5)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'ANTALL RESK.-TRANSER  : ' TO LISTE2-IO-AREA (1:24)
               MOVE ANTRES                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE2-IO-AREA (26:5)
               MOVE 'ANTALL FEIL-TRANSER   : ' TO LISTE2-IO-AREA
                                                               (35:24)
               MOVE FEIRES                 TO XO-40YY9
               MOVE XO-40YY9               TO LISTE2-IO-AREA (60:5)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '========================' TO LISTE2-IO-AREA (1:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99 AND NOT-I-83)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '===> ALLE TRANSAKSJONER ' TO LISTE2-IO-AREA (1:24)
               MOVE 'ER AVVIST. VED OMKJØRING' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE ' MÅ OVENSTÅENDE FEIL RET' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE 'TES OG HELE SENDINGEN SE' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE 'NDES PÅ NYTT.       <===' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99 AND I-80)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '===> KJØRING STOPPET. TR' TO LISTE2-IO-AREA (1:24)
               MOVE 'OLIG FEIL I AUTO DATA. S' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 'JEKK ÅRSAKEN TIL AT DET ' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE 'ER FEIL I RECORDART.    ' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '                    <===' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99 AND I-83)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '===> KJØRING STOPPET. TR' TO LISTE2-IO-AREA (1:24)
               MOVE 'OLIG FEIL I AUTO DATA. S' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 'JEKK ÅRSAKEN TIL TOM FIL' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE 'E.                      ' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '                    <===' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '========================' TO LISTE2-IO-AREA (1:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '========================' TO LISTE2-IO-AREA
                                                               (97:24)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-99)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> CANCEL FAK836. FEIL ' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE 'I REGNSKAPS-/RESKONTRO-D' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE 'ATA.     <=='         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-LR AND I-99 AND NOT-I-83)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> KUNDEN FÅR FEILLISTE' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE ' TILSENDT OG RETTER FEIL' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE 'EN SELV. <=='         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-LR AND I-99 AND I-83)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> TOM(ME) INPUT-FILE(R' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE ')'                    TO CONSOLE-IO-AREA (25:1)
               MOVE '         <=='         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO FEILFIL-IO-AREA
               INITIALIZE FEILFIL-IO-AREA
               IF  (NOT-I-99)
                   MOVE '***      OK      ***' TO FEILFIL-IO-AREA
                                                                (1:20)
               END-IF
               IF  (I-99)
                   MOVE '***    CANCEL    ***' TO FEILFIL-IO-AREA
                                                                (1:20)
               END-IF
               WRITE FEILFIL-IO-AREA
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
           MOVE 2                          TO LR-CHECK
           SET REGFILE-LEVEL-INIT          TO TRUE
           INITIALIZE REGFILE-DATA-FIELDS
           SET REGFILE-EOF-OFF             TO TRUE
           SET REGFILE-PROCESS             TO TRUE
           OPEN INPUT REGFILE
           SET RESFILE-LEVEL-INIT          TO TRUE
           INITIALIZE RESFILE-DATA-FIELDS
           SET RESFILE-EOF-OFF             TO TRUE
           SET RESFILE-PROCESS             TO TRUE
           OPEN INPUT RESFILE
           OPEN INPUT KUNDEMA
           OPEN INPUT FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT LISTE2
           INITIALIZE LISTE2-IO-AREA
           INITIALIZE LISTE2-DATA-FIELDS
           MOVE 57                         TO LISTE2-MAX-LINES
           OPEN OUTPUT FEILFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGFILE
           CLOSE RESFILE
           CLOSE KUNDEMA
           CLOSE FIRMAF
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           IF LISTE2-IO-AREA NOT = SPACES
             WRITE LISTE2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE2-IO-AREA
           END-IF
           CLOSE LISTE2
           CLOSE FEILFIL.
 
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
