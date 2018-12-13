       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAL304R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: SAL304                                              *
      *  PROGR. : Elin                                                *
      *  VISER  : SALG KONT./OPPKRAV/ENGROS                           *
      *           SALGSDATA TAS FRA INNSLÅTT BET.MÅTE PÅ ORRE.        *
      * FIRMA 977 (AMUNDSEN) VIL ATT KONTANT SKAL VÆRE KUNDENR:
      *           500190-500200
      * ENDRING : 21.11.96 - LAGT INN PROSENT KONTANT,OPPKRAV,ENGRO   *
      * FIRMA 977 (AMUNDSEN) FJERNER SPESIALTESTEN.
      * Laget til SAL.XSAL83A2- bestilling av SAL30 på pc- fil        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: SAL304.rpg
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
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT PCFILE
               ASSIGN TO PCFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PCFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREREC
               BLOCK CONTAINS 820
               RECORD CONTAINS 82.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(82).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD PCFILE
               RECORD CONTAINS 200.
       01  PCFILE-IO-AREA.
           05  PCFILE-IO-AREA-X            PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  PCFILE-STATUS               PICTURE 99 VALUE 0.
           10  VLFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-EOF-OFF         VALUE '0'.
               88  VAREREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-READ-OFF        VALUE '0'.
               88  VAREREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-PROCESS-OFF     VALUE '0'.
               88  VAREREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREREC-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREREC-LEVEL-INIT      VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
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
           05  VAREREC-LEVEL-02.
               10  VAREREC-02-L4.
                   15  VAREREC-02-L4-FIRMA PICTURE X(3).
               10  VAREREC-02-L3.
                   15  VAREREC-02-L3-RAVD  PICTURE X(1).
               10  VAREREC-02-L2.
                   15  VAREREC-02-L2-MND   PICTURE X(2).
               10  VAREREC-02-L1.
                   15  VAREREC-02-L1-VGR   PICTURE X(5).
           05  VAREREC-DATA-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDB2F                   PICTURE X(2).
               10  EDB3F                   PICTURE X(3).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  FAKT                    PICTURE X(1).
               10  MND                     PICTURE X(2).
               10  RAVD                    PICTURE X(1).
               10  KUNDE                   PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  BETM                    PICTURE X(2).
      **************************************************************
      **************************************************************
           05  VAGRMAS-DATA-FIELDS.
               10  TILKTO                  PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  KEY-X                   PICTURE X(8).
               10  VLSUM-IO.
                   15  VLSUM               PICTURE S9(7)V9(2).
               10  SAL07-IO.
                   15  SAL07               PICTURE S9(9)V9(2).
               10  TOT07-IO.
                   15  TOT07               PICTURE S9(9)V9(2).
               10  L4T07-IO.
                   15  L4T07               PICTURE S9(9)V9(2).
               10  LRT07-IO.
                   15  LRT07               PICTURE S9(11)V9(2).
               10  SAL14-IO.
                   15  SAL14               PICTURE S9(9)V9(2).
               10  TOT14-IO.
                   15  TOT14               PICTURE S9(9)V9(2).
               10  L4T14-IO.
                   15  L4T14               PICTURE S9(9)V9(2).
               10  LRT14-IO.
                   15  LRT14               PICTURE S9(11)V9(2).
               10  SALG-IO.
                   15  SALG                PICTURE S9(9)V9(2).
               10  TOTAL-IO.
                   15  TOTAL               PICTURE S9(9)V9(2).
               10  L4TAL-IO.
                   15  L4TAL               PICTURE S9(9)V9(2).
               10  LRTAL-IO.
                   15  LRTAL               PICTURE S9(11)V9(2).
               10  MNDSAL-IO.
                   15  MNDSAL              PICTURE S9(9)V9(2).
               10  PRO1-IO.
                   15  PRO1                PICTURE S9(1)V9(4).
               10  PROK-IO.
                   15  PROK                PICTURE S9(2)V9(2).
               10  PROO-IO.
                   15  PROO                PICTURE S9(2)V9(2).
               10  PROE-IO.
                   15  PROE                PICTURE S9(2)V9(2).
               10  TOTSAL-IO.
                   15  TOTSAL              PICTURE S9(9)V9(2).
               10  L3PROK-IO.
                   15  L3PROK              PICTURE S9(2)V9(2).
               10  L3PROO-IO.
                   15  L3PROO              PICTURE S9(2)V9(2).
               10  L3PROE-IO.
                   15  L3PROE              PICTURE S9(2)V9(2).
               10  L4TSAL-IO.
                   15  L4TSAL              PICTURE S9(9)V9(2).
               10  L4PROK-IO.
                   15  L4PROK              PICTURE S9(2)V9(2).
               10  L4PROO-IO.
                   15  L4PROO              PICTURE S9(2)V9(2).
               10  L4PROE-IO.
                   15  L4PROE              PICTURE S9(2)V9(2).
           05  EDITTING-FIELDS.
               10  XO-92YYZR               PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  XO-22YYZR               PICTURE ZZ,ZZ-.
               10  XO-40YNZ                PICTURE ZZZZ.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREREC-PROCESS
               SET VAREREC-PROCESS-OFF     TO TRUE
               SET VAREREC-READ            TO TRUE
           END-IF
 
           IF  VAREREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREREC-GET
               SET VAREREC-READ-OFF        TO TRUE
               IF  NOT VAREREC-EOF
                   SET VAREREC-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-IDSET
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-CHK-LEVEL
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
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-FLDOFF
               PERFORM VAREREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREREC-PROCESS
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
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               SET NOT-I-30                TO TRUE
               IF  MND = '01'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  MND = '02'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  MND = '03'
                   SET I-32                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               IF  MND = '04'
                   SET I-33                TO TRUE
               END-IF
               SET NOT-I-34                TO TRUE
               IF  MND = '05'
                   SET I-34                TO TRUE
               END-IF
               SET NOT-I-35                TO TRUE
               IF  MND = '06'
                   SET I-35                TO TRUE
               END-IF
               SET NOT-I-36                TO TRUE
               IF  MND = '07'
                   SET I-36                TO TRUE
               END-IF
               SET NOT-I-37                TO TRUE
               IF  MND = '08'
                   SET I-37                TO TRUE
               END-IF
               SET NOT-I-38                TO TRUE
               IF  MND = '09'
                   SET I-38                TO TRUE
               END-IF
               SET NOT-I-39                TO TRUE
               IF  MND = '10'
                   SET I-39                TO TRUE
               END-IF
               SET NOT-I-40                TO TRUE
               IF  MND = '11'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  MND = '12'
                   SET I-41                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO KEY-X (1:3)
               MOVE VGR                    TO KEY-X (4:5)
               MOVE KEY-X                  TO VAGRMAS-KEY1
               READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
               INVALID KEY
                   SET I-45                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-45            TO TRUE
                   PERFORM VAGRMAS-FLDSET
                   PERFORM VAGRMAS-IDSET
               END-READ
      *
           END-IF
           IF  (I-L1 AND NOT-I-45)
               SET NOT-I-91                TO TRUE
               IF  TILKTO > '    '
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-91)
               GO TO SLUTT-T
      *
      *****************************************************************
      *    RUTINE FOR BEREGNING AV NETTO VARELINJE BELØP              *
      *    OG NETTOSUM PÅ ALLE LEVELS.                                *
      *    Subrutinen snur ntosum om det er edb-nr. 995xxxx eller     *
      *        edb-nr. 94xxxxx. Dette er allerede gjort i FAK075      *
      *****************************************************************
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  EDB3F = '995'
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  EDB2F = '94'
                   SET I-42                TO TRUE
               END-IF
           END-IF
           ADD ANT TO ZERO             GIVING VLANT
           ADD BEL TO ZERO             GIVING VLBEL
           MOVE 0                          TO VLPTIL
           ADD RAB1 TO ZERO            GIVING VLRAB1
           ADD RAB2 TO ZERO            GIVING VLRAB2
           ADD RAB3 TO ZERO            GIVING VLRAB3
           ADD EDBNR TO ZERO           GIVING VLEDBN
           IF  (I-42)
               MOVE 0000000                TO VLEDBN
           END-IF
           CALL 'NETTOSUM' USING VLFELT-XX-DATA-FIELDS
           ADD VLBEL TO ZERO           GIVING VLSUM ROUNDED
      *****************************************************************
      * FAKTURA/KREDITNOTA RUTINE. SNU BELØP TIL MINUS.               *
      *****************************************************************
           SET NOT-I-20                    TO TRUE
           IF  FAKT = '2'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               DIVIDE VLSUM BY -1      GIVING VLSUM
      *
           END-IF
           IF  (I-02)
               SET NOT-I-62                TO TRUE
      *  02      FIRMA     COMP "977"                    77 AMUNDSEN
      *  02 77   KUNDE     COMP "500190"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500191"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500192"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500193"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500194"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500195"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500196"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500197"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500198"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500199"                 60 KONTANT
      *  02 77N60KUNDE     COMP "500200"                 60 KONTANT
      *  02N77   BETM      COMP "07"                     60 KONTANT
           END-IF
           IF  (I-02)
               SET NOT-I-60                TO TRUE
               IF  BETM = '07'
                   SET I-60                TO TRUE
               END-IF
               SET NOT-I-61                TO TRUE
               IF  BETM = '14'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-60 AND NOT-I-61)
               SET I-62                    TO TRUE
      *
           END-IF
           IF  (I-02 AND I-60)
               ADD VLSUM                   TO SAL07
               ADD VLSUM                   TO TOT07
               ADD VLSUM                   TO L4T07
               ADD VLSUM                   TO LRT07
      *
           END-IF
           IF  (I-02 AND I-61)
               ADD VLSUM                   TO SAL14
               ADD VLSUM                   TO TOT14
               ADD VLSUM                   TO L4T14
               ADD VLSUM                   TO LRT14
      *
           END-IF
           IF  (I-02 AND I-62)
               ADD VLSUM                   TO SALG
               ADD VLSUM                   TO TOTAL
               ADD VLSUM                   TO L4TAL
               ADD VLSUM                   TO LRTAL
      *
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'SAL30'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'SAL304  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L2)
               ADD SAL07                   TO MNDSAL
               ADD SAL14                   TO MNDSAL
               ADD SALG                    TO MNDSAL
               SET NOT-I-70                TO TRUE
               IF  MNDSAL = 0
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-70)
               DIVIDE SAL07 BY MNDSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING PROK ROUNDED
               DIVIDE SAL14 BY MNDSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING PROO ROUNDED
               DIVIDE SALG BY MNDSAL   GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING PROE ROUNDED
      *
           END-IF
           IF  (I-L3)
               ADD TOT07                   TO TOTSAL
               ADD TOT14                   TO TOTSAL
               ADD TOTAL                   TO TOTSAL
               SET NOT-I-71                TO TRUE
               IF  TOTSAL = 0
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-71)
               DIVIDE TOT07 BY TOTSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING L3PROK ROUNDED
               DIVIDE TOT14 BY TOTSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING L3PROO ROUNDED
               DIVIDE TOTAL BY TOTSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING L3PROE ROUNDED
      *
           END-IF
           IF  (I-L4)
               ADD L4T07                   TO L4TSAL
               ADD L4T14                   TO L4TSAL
               ADD L4TAL                   TO L4TSAL
               SET NOT-I-72                TO TRUE
               IF  L4TSAL = 0
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-72)
               DIVIDE L4T07 BY L4TSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING L4PROK ROUNDED
               DIVIDE L4T14 BY L4TSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING L4PROO ROUNDED
               DIVIDE L4TAL BY L4TSAL  GIVING PRO1
               MULTIPLY 100 BY PRO1    GIVING L4PROE ROUNDED
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       VAREREC-GET SECTION.
       VAREREC-GET-P.
           IF  VAREREC-EOF-OFF
               READ VAREREC
               AT END
                   SET VAREREC-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREREC-FLDOFF SECTION.
       VAREREC-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       VAREREC-FLDSET SECTION.
       VAREREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (12:4) TO ANT-IO
               IF  ANT = ZERO
                   SET I-09                TO TRUE
               END-IF
               MOVE VAREREC-IO-AREA (16:2) TO EDB2F (1:2)
               MOVE VAREREC-IO-AREA (16:3) TO EDB3F (1:3)
               MOVE VAREREC-IO-AREA (16:7) TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE VAREREC-IO-AREA (32:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE VAREREC-IO-AREA (23:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE VAREREC-IO-AREA (26:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE VAREREC-IO-AREA (29:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE VAREREC-IO-AREA (41:1) TO FAKT (1:1)
               MOVE VAREREC-IO-AREA (42:2) TO MND (1:2)
               MOVE VAREREC-IO-AREA (2:1)  TO RAVD (1:1)
               MOVE VAREREC-IO-AREA (45:6) TO KUNDE (1:6)
               MOVE VAREREC-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE VAREREC-IO-AREA (60:5) TO VGR (1:5)
               MOVE VAREREC-IO-AREA (81:2) TO BETM (1:2)
           END-EVALUATE.
 
       VAREREC-IDSET SECTION.
       VAREREC-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREREC-CHK-LEVEL SECTION.
       VAREREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREREC-LEVEL-02
               MOVE VAREREC-IO-AREA (51:3) TO VAREREC-02-L4-FIRMA
               MOVE VAREREC-IO-AREA (2:1)  TO VAREREC-02-L3-RAVD
               MOVE VAREREC-IO-AREA (42:2) TO VAREREC-02-L2-MND
               MOVE VAREREC-IO-AREA (60:5) TO VAREREC-02-L1-VGR
               IF  VAREREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREREC-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  VAREREC-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  VAREREC-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREREC-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREREC-02-L4         TO THE-PRIOR-L4
               MOVE  VAREREC-02-L3         TO THE-PRIOR-L3
               MOVE  VAREREC-02-L2         TO THE-PRIOR-L2
               MOVE  VAREREC-02-L1         TO THE-PRIOR-L1
               SET VAREREC-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (69:4) TO TILKTO (1:4)
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
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
           IF  (I-1P)
               MOVE SPACES TO PCFILE-IO-AREA
               INITIALIZE PCFILE-IO-AREA
               MOVE 'FNR'                  TO PCFILE-IO-AREA (1:3)
               MOVE ';'                    TO PCFILE-IO-AREA (4:1)
               MOVE 'MND'                  TO PCFILE-IO-AREA (5:3)
               MOVE ';'                    TO PCFILE-IO-AREA (8:1)
               MOVE 'KONTANT'              TO PCFILE-IO-AREA (17:7)
               MOVE ';'                    TO PCFILE-IO-AREA (24:1)
               MOVE '%'                    TO PCFILE-IO-AREA (30:1)
               MOVE ';'                    TO PCFILE-IO-AREA (31:1)
               MOVE 'OPPKRAV'              TO PCFILE-IO-AREA (40:7)
               MOVE ';'                    TO PCFILE-IO-AREA (47:1)
               MOVE '%'                    TO PCFILE-IO-AREA (53:1)
               MOVE ';'                    TO PCFILE-IO-AREA (54:1)
               MOVE 'ENGROS'               TO PCFILE-IO-AREA (64:6)
               MOVE ';'                    TO PCFILE-IO-AREA (70:1)
               MOVE '%'                    TO PCFILE-IO-AREA (76:1)
               MOVE ';'                    TO PCFILE-IO-AREA (77:1)
               MOVE 'TOTAL'                TO PCFILE-IO-AREA (88:5)
               MOVE ';'                    TO PCFILE-IO-AREA (93:1)
               WRITE PCFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'REG.AVDELING'         TO LISTE-IO-AREA (68:12)
               MOVE RAVD                   TO LISTE-IO-AREA (81:1)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
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
               MOVE 'MND'                  TO LISTE-IO-AREA (1:3)
               MOVE 'K O N T A N T'        TO LISTE-IO-AREA (13:13)
               MOVE '%.'                   TO LISTE-IO-AREA (32:2)
               MOVE 'O P P K R A V'        TO LISTE-IO-AREA (38:13)
               MOVE '%.'                   TO LISTE-IO-AREA (57:2)
               MOVE 'E N G R O S  '        TO LISTE-IO-AREA (63:13)
               MOVE '%.'                   TO LISTE-IO-AREA (82:2)
               MOVE 'T O T A L T  '        TO LISTE-IO-AREA (98:13)
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
           IF  (I-OF AND NOT-I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'REG.AVDELING'         TO LISTE-IO-AREA (68:12)
               MOVE RAVD                   TO LISTE-IO-AREA (81:1)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
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
               MOVE 'MND'                  TO LISTE-IO-AREA (1:3)
               MOVE 'K O N T A N T'        TO LISTE-IO-AREA (13:13)
               MOVE '%.'                   TO LISTE-IO-AREA (32:2)
               MOVE 'O P P K R A V'        TO LISTE-IO-AREA (38:13)
               MOVE '%.'                   TO LISTE-IO-AREA (57:2)
               MOVE 'E N G R O S  '        TO LISTE-IO-AREA (63:13)
               MOVE '%.'                   TO LISTE-IO-AREA (82:2)
               MOVE 'T O T A L T  '        TO LISTE-IO-AREA (98:13)
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
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L4)
               MOVE SPACES TO PCFILE-IO-AREA
               INITIALIZE PCFILE-IO-AREA
               MOVE FIRMA                  TO PCFILE-IO-AREA (1:3)
               MOVE ';'                    TO PCFILE-IO-AREA (4:1)
               IF  (I-30)
                   MOVE 'JAN'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-31)
                   MOVE 'FEB'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-32)
                   MOVE 'MAR'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-33)
                   MOVE 'APR'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-34)
                   MOVE 'MAI'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-35)
                   MOVE 'JUN'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-36)
                   MOVE 'JUL'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-37)
                   MOVE 'AUG'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-38)
                   MOVE 'SEP'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-39)
                   MOVE 'OKT'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-40)
                   MOVE 'NOV'              TO PCFILE-IO-AREA (5:3)
               END-IF
               IF  (I-41)
                   MOVE 'DES'              TO PCFILE-IO-AREA (5:3)
               END-IF
               MOVE ';'                    TO PCFILE-IO-AREA (8:1)
               MOVE L4T07                  TO XO-92YYZR
               MOVE XO-92YYZR              TO PCFILE-IO-AREA (9:15)
               MOVE ';'                    TO PCFILE-IO-AREA (24:1)
               MOVE L4PROK                 TO XO-22YYZR
               MOVE XO-22YYZR              TO PCFILE-IO-AREA (25:6)
               MOVE ';'                    TO PCFILE-IO-AREA (31:1)
               MOVE L4T14                  TO XO-92YYZR
               MOVE XO-92YYZR              TO PCFILE-IO-AREA (32:15)
               MOVE ';'                    TO PCFILE-IO-AREA (47:1)
               MOVE L4PROO                 TO XO-22YYZR
               MOVE XO-22YYZR              TO PCFILE-IO-AREA (48:6)
               MOVE ';'                    TO PCFILE-IO-AREA (54:1)
               MOVE L4TAL                  TO XO-92YYZR
               MOVE XO-92YYZR              TO PCFILE-IO-AREA (55:15)
               MOVE ';'                    TO PCFILE-IO-AREA (70:1)
               MOVE L4PROE                 TO XO-22YYZR
               MOVE XO-22YYZR              TO PCFILE-IO-AREA (71:6)
               MOVE ';'                    TO PCFILE-IO-AREA (77:1)
               MOVE L4TSAL                 TO XO-92YYZR
               MOVE XO-92YYZR              TO PCFILE-IO-AREA (78:15)
               MOVE ';'                    TO PCFILE-IO-AREA (93:1)
               WRITE PCFILE-IO-AREA
           END-IF
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-30)
                   MOVE 'JAN'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-31)
                   MOVE 'FEB'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-32)
                   MOVE 'MAR'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-33)
                   MOVE 'APR'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-34)
                   MOVE 'MAI'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-35)
                   MOVE 'JUN'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-36)
                   MOVE 'JUL'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-37)
                   MOVE 'AUG'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-38)
                   MOVE 'SEP'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-39)
                   MOVE 'OKT'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-40)
                   MOVE 'NOV'              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (I-41)
                   MOVE 'DES'              TO LISTE-IO-AREA (1:3)
               END-IF
               MOVE SAL07                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (11:15)
               INITIALIZE SAL07
               MOVE PROK                   TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (28:6)
               INITIALIZE PROK
               MOVE SAL14                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (36:15)
               INITIALIZE SAL14
               MOVE PROO                   TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (53:6)
               INITIALIZE PROO
               MOVE SALG                   TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (61:15)
               INITIALIZE SALG
               MOVE PROE                   TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (78:6)
               INITIALIZE PROE
               MOVE MNDSAL                 TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (96:15)
               INITIALIZE MNDSAL
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOT. AVD'             TO LISTE-IO-AREA (1:8)
               MOVE RAVD                   TO LISTE-IO-AREA (10:1)
               MOVE TOT07                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (11:15)
               INITIALIZE TOT07
               MOVE L3PROK                 TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (28:6)
               INITIALIZE L3PROK
               MOVE TOT14                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (36:15)
               INITIALIZE TOT14
               MOVE L3PROO                 TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (53:6)
               INITIALIZE L3PROO
               MOVE TOTAL                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (61:15)
               INITIALIZE TOTAL
               MOVE L3PROE                 TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (78:6)
               INITIALIZE L3PROE
               MOVE TOTSAL                 TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (96:15)
               INITIALIZE TOTSAL
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMATOTAL'           TO LISTE-IO-AREA (1:10)
               MOVE L4T07                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (11:15)
               INITIALIZE L4T07
               MOVE L4PROK                 TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (28:6)
               INITIALIZE L4PROK
               MOVE L4T14                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (36:15)
               INITIALIZE L4T14
               MOVE L4PROO                 TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (53:6)
               INITIALIZE L4PROO
               MOVE L4TAL                  TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (61:15)
               INITIALIZE L4TAL
               MOVE L4PROE                 TO XO-22YYZR
               MOVE XO-22YYZR              TO LISTE-IO-AREA (78:6)
               INITIALIZE L4PROE
               MOVE L4TSAL                 TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (96:15)
               INITIALIZE L4TSAL
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           SET VAREREC-LEVEL-INIT          TO TRUE
           INITIALIZE VAREREC-DATA-FIELDS
           SET VAREREC-EOF-OFF             TO TRUE
           SET VAREREC-PROCESS             TO TRUE
           OPEN INPUT VAREREC
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT PCFILE.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREREC
           CLOSE VAGRMAS
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE PCFILE.
 
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
