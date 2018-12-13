       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK428R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM FAK428           ESPEN LARSEN 28.01.2005              *
      * PROGRAM FOR Å DANNE FAKTURADATA FRA ETT FIRMA TIL ETT ANNET.  *
      * KONTANTSALGOPPGJØR I FIRMA 633 SKAL DANNE FAKTURA FRA FIRMA   *
      * 608 TIL SELVKOSTPRIS + 10%.                                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK428.rpg
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
           SELECT FAKFIN
               ASSIGN TO UT-S-FAKFIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKFIN-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FAKFGF
               ASSIGN TO UT-S-FAKFGF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKFGF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKFIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKFIN-IO-AREA.
           05  FAKFIN-IO-AREA-X            PICTURE X(200).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD FAKFGF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKFGF-IO-AREA.
           05  FAKFGF-IO-AREA-X            PICTURE X(200).
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
           10  FAKFIN-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FAKFGF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  VLFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKFIN-EOF-OFF          VALUE '0'.
               88  FAKFIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKFIN-READ-OFF         VALUE '0'.
               88  FAKFIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKFIN-PROCESS-OFF      VALUE '0'.
               88  FAKFIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKFIN-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKFIN-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  VLFELT-XX-DATA-FIELDS.
               10  VLANT-IO.
                   15  VLANT               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(248).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  VLBEL-IO.
                   15  VLBEL               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(237).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(20).
               10  VLPTIL-IO.
                   15  VLPTIL              PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(226).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(31).
               10  VLRAB1-IO.
                   15  VLRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(223).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(34).
               10  VLRAB2-IO.
                   15  VLRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(220).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(37).
               10  VLRAB3-IO.
                   15  VLRAB3              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(217).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(40).
               10  VLEDBN-IO.
                   15  VLEDBN              PICTURE S9(7).
               10  FILLER                  PICTURE X(210).
           05  LDATA-XX REDEFINES VLFELT-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
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
           05  FAKFIN-LEVEL-03.
               10  FAKFIN-03-L2.
                   15  FAKFIN-03-L2-FIRMNR PICTURE X(3).
               10  FAKFIN-03-L1.
                   15  FAKFIN-03-L1-KORDNR PICTURE X(6).
           05  FAKFIN-DATA-FIELDS.
               10  REC2                    PICTURE X(200).
               10  FIRMNR                  PICTURE X(3).
               10  FAKTYP                  PICTURE X(1).
               10  BETBET                  PICTURE X(2).
               10  KORDNR                  PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  LAGERK                  PICTURE X(2).
               10  ORDDTO-IO.
                   15  ORDDTO              PICTURE S9(6).
               10  VGR                     PICTURE X(5).
               10  ARTNR                   PICTURE X(20).
               10  ARTN6F                  PICTURE X(6).
               10  VAREB6                  PICTURE X(5).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  FAKP                    PICTURE X(4).
               10  SKOST-IO.
                   15  SKOST               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KRTYPE                  PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
           05  FAKPAR-DATA-FIELDS.
               10  FOMGNR                  PICTURE X(1).
               10  PAAR                    PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  FAKPER-IO.
                   15  FAKPER              PICTURE S9(6).
           05  FIRMAF-DATA-FIELDS.
               10  FFNAVN                  PICTURE X(30).
               10  FFPNR                   PICTURE X(4).
               10  FFSTED                  PICTURE X(26).
      *****************************************************************
      *  FAKTURAPARAMETER RUTINE.                                     *
      *****************************************************************
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KADR                    PICTURE X(30).
               10  KPSTED                  PICTURE X(15).
               10  KPNR                    PICTURE X(4).
               10  KBETM                   PICTURE X(2).
      *                                     170 170 KFAKFS
               10  KFAKMT                  PICTURE X(1).
               10  HND                     PICTURE X(3).
               10  ALFAK                   PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(6).
               10  TILFNR                  PICTURE X(3).
               10  TILKNR                  PICTURE X(6).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(6).
               10  NYKOST-IO.
                   15  NYKOST              PICTURE S9(7)V9(2).
               10  NYPRIS-IO.
                   15  NYPRIS              PICTURE S9(7)V9(2).
               10  NYRAB-IO.
                   15  NYRAB               PICTURE S9(2)V9(1).
               10  SUML1-IO.
                   15  SUML1               PICTURE S9(7)V9(2).
               10  SUML2-IO.
                   15  SUML2               PICTURE S9(7)V9(2).
               10  SUMLR-IO.
                   15  SUMLR               PICTURE S9(7)V9(2).
               10  SUML1N-IO.
                   15  SUML1N              PICTURE S9(7)V9(2).
               10  SUML2N-IO.
                   15  SUML2N              PICTURE S9(7)V9(2).
               10  SUMLRN-IO.
                   15  SUMLRN              PICTURE S9(7)V9(2).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(6).
               10  KNRKEY                  PICTURE X(9).
               10  FBETM                   PICTURE X(2).
               10  FFAKMT                  PICTURE X(1).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-21YN9                PICTURE ZZ,9.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKFIN-PROCESS
               SET FAKFIN-PROCESS-OFF      TO TRUE
               SET FAKFIN-READ             TO TRUE
           END-IF
 
           IF  FAKFIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKFIN-GET
               SET FAKFIN-READ-OFF         TO TRUE
               IF  NOT FAKFIN-EOF
                   SET FAKFIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKFIN-PROCESS
               PERFORM FAKFIN-IDSET
           END-IF
 
           IF  FAKFIN-PROCESS
               PERFORM FAKFIN-CHK-LEVEL
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
 
           IF  FAKFIN-PROCESS
               PERFORM FAKFIN-FLDOFF
               PERFORM FAKFIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKFIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-10)
               GO TO ENDPAR-T
           END-IF
           READ FAKPAR
           AT END
               SET I-99                    TO TRUE
           NOT AT END
               SET NOT-I-99                TO TRUE
               PERFORM FAKPAR-FLDSET
               PERFORM FAKPAR-IDSET
           END-READ
           SET I-10                        TO TRUE.
 
       ENDPAR-T.
           IF  (NOT-I-03)
               GO TO SLUTT-T
      *****************************************************************
      *  HOVED RUTINE.                                                *
      *****************************************************************
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTINN
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  (I-61)
               SET NOT-I-60                TO TRUE
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L2)
               SET I-60                    TO TRUE
               SET NOT-I-65                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-12                TO TRUE
               SUBTRACT SUML1              FROM SUML1
               SUBTRACT SUML1N             FROM SUML1N
           END-IF
           IF  (I-L2)
               SUBTRACT SUML2              FROM SUML2
               SUBTRACT SUML2N             FROM SUML2N
      *  L1      BETBET    COMP "07"                     47 KONTANT
      *  L1N47   BETBET    COMP "47"                     47 KONTANT
      *  L1N47   BETBET    COMP "14"                     47 OPPKRAV
      *  47                GOTO SLUTT                       UAKTUELL
      *****************************************************************
      * SKAL DENNE ORDRE GJENNOMFAKTURERES ?                          *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-11                TO TRUE
               IF  FIRMNR = 'XXX'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-U1)
               SET NOT-I-11                TO TRUE
               IF  FIRMNR = '608'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-11)
               MOVE '608'                  TO TILFNR
               MOVE '300003'               TO TILKNR
               PERFORM KNRRUT-S
           END-IF
           IF  (NOT-I-11)
               GO TO SLUTT-T
           END-IF
           IF  (I-60)
               SET I-61                    TO TRUE
               SET I-65                    TO TRUE
           END-IF
           IF  (I-61)
               MOVE TILFNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-61 AND NOT-I-25)
               MOVE FFNAVN                 TO FINAVN
           END-IF
           IF  (I-61)
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
      *****************************************************************
      * TEST OM DET ER EN ORDRE SOM IKKE SKAL VÆRE MED.               *
      * KREDITNOTA SOM IKKE ER RETUR AV VARER SKAL IKKE VÆRE MED.     *
      * VARELINJER MED 0 I LEVERT SKAL IKKE VÆRE MED.                 *
      *****************************************************************
           END-IF
           IF  (I-03)
               SET NOT-I-31                TO TRUE
               IF  FAKTYP = 'K'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-31)
               SET NOT-I-32                TO TRUE
               IF  KRTYPE = '2'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-31 AND NOT-I-32)
               GO TO SLUTT-T
           END-IF
           IF  (I-03 AND I-08)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET I-50                    TO TRUE
      *****************************************************************
      *  DIVERSE                                                      *
      *****************************************************************
           END-IF
           IF  (I-03 AND I-11)
               ADD 1                       TO ANTNYE
           END-IF
           IF  (I-03)
               SET NOT-I-21                TO TRUE
               IF  RECART = 'A'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  RECART = 'L'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-58)
               PERFORM RBSRUT-S
           END-IF
           SET I-58                        TO TRUE
      *****************************************************************
      * LAG ORDREPRIS: SELVKOST + 10 % PÅ VANLIGE VARER.              *
      * ARBEIDE FAKTURERES IKKE.                                      *
      * FRAKT PORTO FAKTURERS IKKE.                                   *
      * SKAFFEVARER: 10% FORTJENESTE.                                 *
      *****************************************************************
           IF  (I-22)
               SET NOT-I-18                TO TRUE
               IF  VGR = '80100'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  VGR = '80150'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  VGR = '80200'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  VGR = '99030'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  VGR = '22201'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-22)
               SET NOT-I-19                TO TRUE
               IF  VGR = '13199'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-19)
               SET NOT-I-19                TO TRUE
               IF  LAGERK = 'PT'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-19)
               SET NOT-I-19                TO TRUE
               IF  ARTN6F = 'PANT  '
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-22)
               ADD SKOST TO ZERO       GIVING NYKOST
           END-IF
           IF  (I-22 AND NOT-I-09)
               MULTIPLY 1,1 BY SKOST   GIVING NYPRIS ROUNDED
           END-IF
           IF  (I-22 AND I-09)
               MULTIPLY 0,7 BY BEL     GIVING NYKOST
               MULTIPLY 1,1 BY NYKOST  GIVING NYPRIS
           END-IF
           IF  (I-22 AND I-18)
               MOVE 0,00                   TO NYPRIS
               MOVE 0,00                   TO NYKOST
           END-IF
           IF  (I-22 AND I-19)
               ADD SKOST TO ZERO       GIVING NYKOST
               ADD BEL TO ZERO         GIVING NYPRIS
           END-IF
           IF  (I-22 AND I-09)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-22)
               MOVE 0,0                    TO NYRAB
      *****************************************************************
      *    RUTINE FOR BEREGNING AV NETTO VARELINJE BELØP              *
      *    OG NETTOSUM PR.VGR(SUML1) PR.KUNDE(SUML2) PR.FIRMA(SUML3)  *
      *****************************************************************
           END-IF
           IF  (NOT-I-22)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  FAKTYP = 'K'
               SET I-24                    TO TRUE
           END-IF
      *****************************************************************
      * SUMMERING AV OPPRINNELIG FAKTURA.                             *
      *****************************************************************
           ADD ANT TO ZERO             GIVING VLANT
           ADD BEL TO ZERO             GIVING VLBEL
           MOVE 0                          TO VLPTIL
           ADD RAB1 TO ZERO            GIVING VLRAB1
           ADD RAB2 TO ZERO            GIVING VLRAB2
           ADD RAB3 TO ZERO            GIVING VLRAB3
           ADD EDBNR TO ZERO           GIVING VLEDBN
           CALL 'NETTOSUM' USING VLFELT-XX-DATA-FIELDS
           ADD VLBEL                       TO SUML1
           IF  (I-24)
               SUBTRACT VLBEL              FROM SUML1
           END-IF
           IF  (NOT-I-24)
               ADD VLBEL                   TO SUML2
           END-IF
           IF  (I-24)
               SUBTRACT VLBEL              FROM SUML2
           END-IF
           IF  (NOT-I-24)
               ADD VLBEL                   TO SUMLR
           END-IF
           IF  (I-24)
               SUBTRACT VLBEL              FROM SUMLR
      *****************************************************************
      * SUMMERING AV NY FAKTURA.                                      *
      *****************************************************************
           END-IF
           ADD ANT TO ZERO             GIVING VLANT
           ADD NYPRIS TO ZERO          GIVING VLBEL
           MOVE 0                          TO VLPTIL
           ADD NYRAB TO ZERO           GIVING VLRAB1
           ADD NYRAB TO ZERO           GIVING VLRAB2
           ADD NYRAB TO ZERO           GIVING VLRAB3
           ADD EDBNR TO ZERO           GIVING VLEDBN
           CALL 'NETTOSUM' USING VLFELT-XX-DATA-FIELDS
           ADD VLBEL                       TO SUML1N
           IF  (I-24)
               SUBTRACT VLBEL              FROM SUML1N
           END-IF
           IF  (NOT-I-24)
               ADD VLBEL                   TO SUML2N
           END-IF
           IF  (I-24)
               SUBTRACT VLBEL              FROM SUML2N
           END-IF
           IF  (NOT-I-24)
               ADD VLBEL                   TO SUMLRN
           END-IF
           IF  (I-24)
               SUBTRACT VLBEL              FROM SUMLRN
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           IF  (I-03)
               ADD 1                       TO ANTUT
      ******************************************************
      * ---> SUBRUTINE FOR Å                               *
      *    HENTE FAKTURADATA PÅ NYTT KUNDENUMMER.          *
      ******************************************************
           END-IF
           .
 
       KNRRUT-S SECTION.
       KNRRUT-S-P.
           MOVE TILFNR                     TO KNRKEY (1:3)
           MOVE TILKNR                     TO KNRKEY (4:6)
           MOVE KNRKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-84                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-84                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           MOVE KBETM                      TO FBETM
      *                    MOVE KFAKFS    FFAKFS  1         FAKT.FL.SIDE
           MOVE KFAKMT                     TO FFAKMT
           SET NOT-I-87                    TO TRUE
           IF  FFAKMT = '5'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE ' '                    TO FFAKMT
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  FFAKMT = '6'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE '1'                    TO FFAKMT
           END-IF.
 
       KNREND-T.
           CONTINUE.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'FAK28'                    TO LONR
           MOVE TILFNR                     TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK428  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      *****************************************************************
      * RECORDS KOPIERT TIL GJENNOMFAKTURERING.                       *
      *****************************************************************
 
       FAKFIN-GET SECTION.
       FAKFIN-GET-P.
           IF  FAKFIN-EOF-OFF
               READ FAKFIN
               AT END
                   SET FAKFIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKFIN-FLDOFF SECTION.
       FAKFIN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-08                TO TRUE
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       FAKFIN-FLDSET SECTION.
       FAKFIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKFIN-IO-AREA (1:200) TO REC2 (1:200)
               MOVE FAKFIN-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKFIN-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKFIN-IO-AREA (14:2)  TO BETBET (1:2)
               MOVE FAKFIN-IO-AREA (19:6)  TO KORDNR (1:6)
               MOVE FAKFIN-IO-AREA (25:1)  TO RECART (1:1)
               MOVE FAKFIN-IO-AREA (62:2)  TO LAGERK (1:2)
               MOVE FAKFIN-IO-AREA (71:6)  TO ORDDTO-IO
               INSPECT ORDDTO-IO REPLACING ALL ' ' BY '0'
               MOVE FAKFIN-IO-AREA (77:5)  TO VGR (1:5)
               MOVE FAKFIN-IO-AREA (82:20) TO ARTNR (1:20)
               MOVE FAKFIN-IO-AREA (82:6)  TO ARTN6F (1:6)
               MOVE FAKFIN-IO-AREA (102:5) TO VAREB6 (1:5)
               MOVE FAKFIN-IO-AREA (137:4) TO ANT-IO
               IF  ANT = ZERO
                   SET I-08                TO TRUE
               END-IF
               MOVE FAKFIN-IO-AREA (141:7) TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKFIN-IO-AREA (148:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKFIN-IO-AREA (151:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKFIN-IO-AREA (154:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKFIN-IO-AREA (157:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKFIN-IO-AREA (167:4) TO FAKP (1:4)
               MOVE FAKFIN-IO-AREA (171:5) TO SKOST-IO
               IF  SKOST = ZERO
                   SET I-09                TO TRUE
               END-IF
               MOVE FAKFIN-IO-AREA (179:1) TO KRTYPE (1:1)
               MOVE FAKFIN-IO-AREA (184:6) TO KUNDNR (1:6)
           END-EVALUATE.
 
       FAKFIN-IDSET SECTION.
       FAKFIN-IDSET-P.
           SET I-03                        TO TRUE.
 
       FAKFIN-CHK-LEVEL SECTION.
       FAKFIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKFIN-LEVEL-03
               MOVE FAKFIN-IO-AREA (1:3)   TO FAKFIN-03-L2-FIRMNR
               MOVE FAKFIN-IO-AREA (19:6)  TO FAKFIN-03-L1-KORDNR
               IF  FAKFIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKFIN-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKFIN-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKFIN-03-L2          TO THE-PRIOR-L2
               MOVE  FAKFIN-03-L1          TO THE-PRIOR-L1
               SET FAKFIN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (3:1)   TO FOMGNR (1:1)
               MOVE FAKPAR-IO-AREA (6:2)   TO PAAR (1:2)
               MOVE FAKPAR-IO-AREA (8:2)   TO PMND (1:2)
               MOVE FAKPAR-IO-AREA (12:6)  TO FAKPER-IO
               INSPECT FAKPER-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FFNAVN (1:30)
               MOVE FIRMAF-IO-AREA (534:4) TO FFPNR (1:4)
               MOVE FIRMAF-IO-AREA (538:26) TO FFSTED (1:26)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KNAVN2 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO KADR (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO KPSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO KPNR (1:4)
               MOVE KUNDEMA-IO-AREA (127:2) TO KBETM (1:2)
               MOVE KUNDEMA-IO-AREA (171:1) TO KFAKMT (1:1)
               MOVE KUNDEMA-IO-AREA (185:3) TO HND (1:3)
               MOVE KUNDEMA-IO-AREA (190:3) TO ALFAK (1:3)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-11 AND I-50)
               MOVE SPACES TO FAKFGF-IO-AREA
               INITIALIZE FAKFGF-IO-AREA
               MOVE REC2                   TO FAKFGF-IO-AREA (1:200)
               MOVE TILFNR                 TO FAKFGF-IO-AREA (1:3)
               MOVE TILKNR                 TO FAKFGF-IO-AREA (4:6)
      *                        KFAKFS    11
               MOVE 'F'                    TO FAKFGF-IO-AREA (11:1)
               MOVE KFAKMT                 TO FAKFGF-IO-AREA (12:1)
               MOVE KBETM                  TO FAKFGF-IO-AREA (14:2)
               MOVE 'X'                    TO FAKFGF-IO-AREA (40:1)
      *                      22          63 "99"
               IF  (I-22)
                   MOVE HND                TO FAKFGF-IO-AREA (68:3)
               END-IF
               IF  (I-22)
                   MOVE NYRAB-IO           TO FAKFGF-IO-AREA (148:3)
               END-IF
               IF  (I-22)
                   MOVE NYRAB-IO           TO FAKFGF-IO-AREA (151:3)
               END-IF
               IF  (I-22)
                   MOVE NYRAB-IO           TO FAKFGF-IO-AREA (154:3)
               END-IF
               IF  (I-22)
                   MOVE NYPRIS-IO          TO FAKFGF-IO-AREA (157:9)
      *                                 166 "4"
               END-IF
               IF  (I-22)
                   MOVE NYKOST             TO XO-72P
                   MOVE XO-72P-EF          TO FAKFGF-IO-AREA (171:5)
               END-IF
               IF  (I-22)
                   MOVE ' '                TO FAKFGF-IO-AREA (176:1)
               END-IF
               MOVE 'N'                    TO FAKFGF-IO-AREA (178:1)
               MOVE TILKNR                 TO FAKFGF-IO-AREA (184:6)
      *                                 192 "FGF"
               MOVE FIRMNR                 TO FAKFGF-IO-AREA (198:3)
               WRITE FAKFGF-IO-AREA
           END-IF
           IF  (I-03 AND I-11 AND I-50)
           AND (I-22)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (1:3)
               MOVE KUNDNR                 TO LISTE-IO-AREA (5:6)
               MOVE KORDNR                 TO LISTE-IO-AREA (12:6)
               MOVE ORDDTO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (19:8)
               MOVE FAKP                   TO LISTE-IO-AREA (28:4)
               MOVE VGR                    TO LISTE-IO-AREA (33:5)
               MOVE KRTYPE                 TO LISTE-IO-AREA (39:1)
               MOVE ALFAK                  TO LISTE-IO-AREA (41:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (45:20)
               MOVE EDBNR-IO               TO LISTE-IO-AREA (66:7)
               MOVE ANT                    TO XO-52YN9
               MOVE XO-52YN9               TO LISTE-IO-AREA (74:8)
               MOVE SKOST                  TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (83:10)
               MOVE BEL                    TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (94:10)
               MOVE RAB1                   TO XO-21YN9
               MOVE XO-21YN9               TO LISTE-IO-AREA (105:4)
               MOVE RAB2                   TO XO-21YN9
               MOVE XO-21YN9               TO LISTE-IO-AREA (110:4)
               MOVE RAB3                   TO XO-21YN9
               MOVE XO-21YN9               TO LISTE-IO-AREA (115:4)
               MOVE LAGERK                 TO LISTE-IO-AREA (120:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TILFNR                 TO LISTE-IO-AREA (1:3)
               MOVE TILKNR                 TO LISTE-IO-AREA (5:6)
               MOVE NYKOST                 TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (83:10)
               MOVE NYPRIS                 TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (94:10)
      *       T  1     L1 11
      *                        KUNDNR     7
      *                        KORDNR    14
      *                        TILKNR    21
      *                        SUML1 J   34
      *                        KNAVN1    65
      *                        KPNR      70
      *                        KPSTED    86
      *                        SUML1NJ  100
      *                      12         120 "VARE UTEN SELVKOST"
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-61)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (71:2)
               MOVE PAAR                   TO LISTE-IO-AREA (74:2)
               MOVE PMND                   TO LISTE-IO-AREA (76:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (80:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FRA FIRMA'            TO LISTE-IO-AREA (32:9)
               MOVE FIRMNR                 TO LISTE-IO-AREA (42:3)
               IF  (NOT-I-25)
                   MOVE FFNAVN             TO LISTE-IO-AREA (46:30)
               END-IF
               IF  (NOT-I-25)
                   MOVE FFPNR              TO LISTE-IO-AREA (77:4)
               END-IF
               IF  (NOT-I-25)
                   MOVE FFSTED             TO LISTE-IO-AREA (82:26)
               END-IF
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
               MOVE 'FNR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (5:6)
               MOVE 'ORDRE '               TO LISTE-IO-AREA (12:6)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (19:8)
               MOVE 'FPER'                 TO LISTE-IO-AREA (28:4)
               MOVE 'VGR. '                TO LISTE-IO-AREA (33:5)
               MOVE 'KT'                   TO LISTE-IO-AREA (38:2)
               MOVE 'ALF'                  TO LISTE-IO-AREA (41:3)
               MOVE 'ARTIKKELNUMMER      ' TO LISTE-IO-AREA (45:20)
               MOVE 'EDB-NR.'              TO LISTE-IO-AREA (66:7)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (76:6)
               MOVE 'KOSTPRIS'             TO LISTE-IO-AREA (85:8)
               MOVE 'ORDREPRIS'            TO LISTE-IO-AREA (95:9)
               MOVE 'RAB1'                 TO LISTE-IO-AREA (105:4)
               MOVE 'RAB2'                 TO LISTE-IO-AREA (110:4)
               MOVE 'RAB3'                 TO LISTE-IO-AREA (115:4)
               MOVE 'PT'                   TO LISTE-IO-AREA (120:2)
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
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (71:2)
               MOVE PAAR                   TO LISTE-IO-AREA (74:2)
               MOVE PMND                   TO LISTE-IO-AREA (76:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (80:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FRA FIRMA'            TO LISTE-IO-AREA (32:9)
               MOVE FIRMNR                 TO LISTE-IO-AREA (42:3)
               IF  (NOT-I-25)
                   MOVE FFNAVN             TO LISTE-IO-AREA (46:30)
               END-IF
               IF  (NOT-I-25)
                   MOVE FFPNR              TO LISTE-IO-AREA (77:4)
               END-IF
               IF  (NOT-I-25)
                   MOVE FFSTED             TO LISTE-IO-AREA (82:26)
               END-IF
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
               MOVE 'FNR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (5:6)
               MOVE 'ORDRE '               TO LISTE-IO-AREA (12:6)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (19:8)
               MOVE 'FPER'                 TO LISTE-IO-AREA (28:4)
               MOVE 'VGR. '                TO LISTE-IO-AREA (33:5)
               MOVE 'KT'                   TO LISTE-IO-AREA (38:2)
               MOVE 'ALF'                  TO LISTE-IO-AREA (41:3)
               MOVE 'ARTIKKELNUMMER      ' TO LISTE-IO-AREA (45:20)
               MOVE 'EDB-NR.'              TO LISTE-IO-AREA (66:7)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (76:6)
               MOVE 'KOSTPRIS'             TO LISTE-IO-AREA (85:8)
               MOVE 'ORDREPRIS'            TO LISTE-IO-AREA (95:9)
               MOVE 'RAB1'                 TO LISTE-IO-AREA (105:4)
               MOVE 'RAB2'                 TO LISTE-IO-AREA (110:4)
               MOVE 'RAB3'                 TO LISTE-IO-AREA (115:4)
               MOVE 'PT'                   TO LISTE-IO-AREA (120:2)
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
           IF  (I-L2 AND I-65)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** SUM FIRMA   ***'   TO LISTE-IO-AREA (4:18)
               MOVE FIRMNR                 TO LISTE-IO-AREA (19:3)
               MOVE SUML2                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (22:13)
               MOVE SUML2N                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (88:13)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** GRANDTOTAL ***'   TO LISTE-IO-AREA (4:18)
               MOVE SUMLR                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (22:13)
               MOVE SUMLRN                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (88:13)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '   --- FAK428 ---    ***' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTINN                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'RECORDS BEHANDLET.     ' TO LISTE-IO-AREA (12:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTNYE                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'NYE GJENNOMFAKTURA REC.' TO LISTE-IO-AREA (12:23)
               MOVE ' DANNET.         '    TO LISTE-IO-AREA (36:17)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTUT                  TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'RECORDS TIL FAKTURERING' TO LISTE-IO-AREA (12:23)
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
           SET FAKFIN-LEVEL-INIT           TO TRUE
           INITIALIZE FAKFIN-DATA-FIELDS
           SET FAKFIN-EOF-OFF              TO TRUE
           SET FAKFIN-PROCESS              TO TRUE
           OPEN INPUT FAKFIN
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT FAKFGF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKFIN
           CLOSE FAKPAR
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE FAKFGF
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
