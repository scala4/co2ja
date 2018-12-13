       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD825R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring i excel på Report Web *****************
      *       O R D R E R U T I N E P R O G R A M   O R D 8 2 5       *
      *       -------------------------------------------------       *
      *  1. SKRIVE UT DAGLIG ORDRESTATISTIKK PR. ORDREMOTAGER.        *
      *      31/1-1991 AV ESPEN LARSEN.                               *
      *  2. 22/11-91  LAGT INN TABELL MED ORDREMOTAGER NAVN.          *
      *  3. 13/ 5-92  PRINTER IKKE NÅR SALG IÅR OG IFJOR = 0          *
      *  4.  3/11-94  ØKET FELTLENGER PÅ FIRMATOTALER.                *
      *  5  09.10.97  HENTER DATO FRA AUTOPAR.                        *
      *  6  21.10.98  KLARGJORT FOR ÅR2000. AUTOPAR 8 SIFFERET DATO.  *
      *  7   5.01.99  TOTALSUMMER ANT. VARELINJER I ÅR OG I FJOR.     *
      *  8  13.04.99  HENTER NAVN FRA STATTAB I STEDENFOR TABELL.     *
      *  9  25.07.00  UPSI 1 LISTER KUN UT IDENTER SOM ER I STATTAB.  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD825.rpg
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
           SELECT MASTINN
               ASSIGN TO UT-S-MASTINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTINN-STATUS.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT STATTAB
               ASSIGN TO STATTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS STATTAB-STATUS
               RECORD KEY IS STATTAB-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MASTINN
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  MASTINN-IO-AREA.
           05  MASTINN-IO-AREA-X           PICTURE X(100).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD STATTAB
               RECORD CONTAINS 40.
       01  STATTAB-IO-AREA.
           05  STATTAB-IO-AREA-X.
               10  STATTAB-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(32).
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
           10  MASTINN-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  STATTAB-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-EOF-OFF         VALUE '0'.
               88  MASTINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-READ-OFF        VALUE '0'.
               88  MASTINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-PROCESS-OFF     VALUE '0'.
               88  MASTINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MASTINN-LEVEL-INIT-OFF  VALUE '0'.
               88  MASTINN-LEVEL-INIT      VALUE '1'.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  STATTAB-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  MASTINN-LEVEL-01.
               10  MASTINN-01-L2.
                   15  MASTINN-01-L2-FIRMA PICTURE X(3).
               10  MASTINN-01-L1.
                   15  MASTINN-01-L1-OM    PICTURE X(2).
           05  MASTINN-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  OM                      PICTURE X(2).
               10  DAG-IO.
                   15  DAG                 PICTURE S9(2).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  RUT                     PICTURE X(1).
               10  LK                      PICTURE X(2).
               10  DAGENS                  PICTURE X(1).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ORDBEL-IO.
                   15  ORDBEL              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORDSVS-IO.
                   15  ORDSVS              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REOBEL-IO.
                   15  REOBEL              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTVL-IO.
                   15  ANTVL               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  ANTVLR-IO.
                   15  ANTVLR              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  AUTOPAR-DATA-FIELDS.
               10  APDATO-IO.
                   15  APDATO              PICTURE S9(6).
               10  APDAG-IO.
                   15  APDAG               PICTURE S9(2).
               10  APMND-IO.
                   15  APMND               PICTURE S9(2).
               10  APAAR-IO.
                   15  APAAR               PICTURE S9(4).
           05  STATTAB-DATA-FIELDS.
               10  ANAVN                   PICTURE X(30).
      *****************************************************************
      * OPPSLAG MOT PARAMFIL.AUTODATA FOR HENTING AV AKTUELL DATO.    *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  APKEY                   PICTURE X(3).
               10  FAAR-IO.
                   15  FAAR                PICTURE S9(4).
               10  OAAR-IO.
                   15  OAAR                PICTURE S9(4).
               10  NTOBEL-IO.
                   15  NTOBEL              PICTURE S9(7)V9(2).
               10  SVSBEL-IO.
                   15  SVSBEL              PICTURE S9(7)V9(2).
               10  KEY5                    PICTURE X(5).
               10  OMKEY                   PICTURE X(8).
               10  KEY3                    PICTURE X(3).
               10  TABNAV                  PICTURE X(9).
               10  AODAG1-IO.
                   15  AODAG1              PICTURE S9(3).
               10  AODAG2-IO.
                   15  AODAG2              PICTURE S9(3).
               10  OBDAG-IO.
                   15  OBDAG               PICTURE S9(7)V9(2).
               10  RBDAG-IO.
                   15  RBDAG               PICTURE S9(7)V9(2).
               10  KBDAG-IO.
                   15  KBDAG               PICTURE S9(7)V9(2).
               10  NODAG-IO.
                   15  NODAG               PICTURE S9(7)V9(2).
               10  SVSDAG-IO.
                   15  SVSDAG              PICTURE S9(7)V9(2).
               10  AOMND1-IO.
                   15  AOMND1              PICTURE S9(5).
               10  AOMND2-IO.
                   15  AOMND2              PICTURE S9(5).
               10  VLMND1-IO.
                   15  VLMND1              PICTURE S9(6).
               10  VLMND2-IO.
                   15  VLMND2              PICTURE S9(6).
               10  VRMND1-IO.
                   15  VRMND1              PICTURE S9(6).
               10  VRMND2-IO.
                   15  VRMND2              PICTURE S9(6).
               10  NOMND-IO.
                   15  NOMND               PICTURE S9(8)V9(2).
               10  NOAAR-IO.
                   15  NOAAR               PICTURE S9(9)V9(2).
               10  NOFJO-IO.
                   15  NOFJO               PICTURE S9(9)V9(2).
               10  SVSMND-IO.
                   15  SVSMND              PICTURE S9(8)V9(2).
               10  SVSAAR-IO.
                   15  SVSAAR              PICTURE S9(9)V9(2).
               10  SVSFJO-IO.
                   15  SVSFJO              PICTURE S9(9)V9(2).
               10  VLA-ELGTOT-IO.
                   15  VLA-ELGTOT          PICTURE S9(7).
               10  VLA-ELGT10-IO.
                   15  VLA-ELGT10          PICTURE S9(7).
               10  VLA-ELGT13-IO.
                   15  VLA-ELGT13          PICTURE S9(7).
               10  VLA-ELGT15-IO.
                   15  VLA-ELGT15          PICTURE S9(7).
               10  VLA-ELGT17-IO.
                   15  VLA-ELGT17          PICTURE S9(7).
               10  VLA-ELGT18-IO.
                   15  VLA-ELGT18          PICTURE S9(7).
               10  VLA-ELGT92-IO.
                   15  VLA-ELGT92          PICTURE S9(7).
               10  VLA-ELGT93-IO.
                   15  VLA-ELGT93          PICTURE S9(7).
               10  VLFTOT-IO.
                   15  VLFTOT              PICTURE S9(7).
               10  VLFT10-IO.
                   15  VLFT10              PICTURE S9(7).
               10  VLFT13-IO.
                   15  VLFT13              PICTURE S9(7).
               10  VLFT15-IO.
                   15  VLFT15              PICTURE S9(7).
               10  VLFT17-IO.
                   15  VLFT17              PICTURE S9(7).
               10  VLFT18-IO.
                   15  VLFT18              PICTURE S9(7).
               10  VLFT92-IO.
                   15  VLFT92              PICTURE S9(7).
               10  VLFT93-IO.
                   15  VLFT93              PICTURE S9(7).
               10  OBDAG1-IO.
                   15  OBDAG1              PICTURE S9(7).
               10  OBDAG2-IO.
                   15  OBDAG2              PICTURE S9(7).
               10  RBDAG1-IO.
                   15  RBDAG1              PICTURE S9(7).
               10  RBDAG2-IO.
                   15  RBDAG2              PICTURE S9(7).
               10  KBDAG1-IO.
                   15  KBDAG1              PICTURE S9(7).
               10  KBDAG2-IO.
                   15  KBDAG2              PICTURE S9(7).
               10  NODAG1-IO.
                   15  NODAG1              PICTURE S9(7).
               10  NODAG2-IO.
                   15  NODAG2              PICTURE S9(7).
               10  BRFDG-IO.
                   15  BRFDG               PICTURE S9(7)V9(2).
               10  BRFDG1-IO.
                   15  BRFDG1              PICTURE S9(7).
               10  BRFDAG-IO.
                   15  BRFDAG              PICTURE S9(7).
               10  NOMND1-IO.
                   15  NOMND1              PICTURE S9(8).
               10  NOMND2-IO.
                   15  NOMND2              PICTURE S9(8).
               10  BRFMD-IO.
                   15  BRFMD               PICTURE S9(7)V9(2).
               10  BRFMD1-IO.
                   15  BRFMD1              PICTURE S9(8).
               10  BRFMND-IO.
                   15  BRFMND              PICTURE S9(8).
               10  NOAAR1-IO.
                   15  NOAAR1              PICTURE S9(9).
               10  NOAAR2-IO.
                   15  NOAAR2              PICTURE S9(9).
               10  BRFAR-IO.
                   15  BRFAR               PICTURE S9(9)V9(2).
               10  BRFAR1-IO.
                   15  BRFAR1              PICTURE S9(9).
               10  BRFAAR-IO.
                   15  BRFAAR              PICTURE S9(9).
               10  NOFJO1-IO.
                   15  NOFJO1              PICTURE S9(9).
               10  NOFJO2-IO.
                   15  NOFJO2              PICTURE S9(9).
               10  BRFFJ-IO.
                   15  BRFFJ               PICTURE S9(9)V9(2).
               10  BRFFJ1-IO.
                   15  BRFFJ1              PICTURE S9(9).
               10  BRFFJO-IO.
                   15  BRFFJO              PICTURE S9(9).
               10  DIFF1-IO.
                   15  DIFF1               PICTURE S9(6).
               10  DIFFX-IO.
                   15  DIFFX               PICTURE S9(8).
               10  SERVP-IO.
                   15  SERVP               PICTURE S9(3)V9(1).
               10  SERVP1-IO.
                   15  SERVP1              PICTURE S9(2)V9(1).
               10  SERVP8-IO.
                   15  SERVP8              PICTURE S9(3).
               10  OSNIT1-IO.
                   15  OSNIT1              PICTURE S9(5).
               10  DIFF2-IO.
                   15  DIFF2               PICTURE S9(6).
               10  SERVP2-IO.
                   15  SERVP2              PICTURE S9(2)V9(1).
               10  SERVP9-IO.
                   15  SERVP9              PICTURE S9(3).
               10  OSNIT2-IO.
                   15  OSNIT2              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-30YY9                PICTURE ZZ9.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
               10  XO-50YN9                PICTURE ZZZZ9.
               10  XO-60YN9                PICTURE ZZZZZ9.
               10  XO-21YYZ                PICTURE ZZ,Z.
               10  XO-30YYZ                PICTURE ZZZ.
               10  XO-50YYZ                PICTURE ZZ.ZZZ.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
               10  XO-90YY9R               PICTURE ZZZ.ZZZ.ZZ9-.
               10  XO-90YYZR               PICTURE ZZZ.ZZZ.ZZZ-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-92YYZR               PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  MASTINN-PROCESS
               SET MASTINN-PROCESS-OFF     TO TRUE
               SET MASTINN-READ            TO TRUE
           END-IF
 
           IF  MASTINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM MASTINN-GET
               SET MASTINN-READ-OFF        TO TRUE
               IF  NOT MASTINN-EOF
                   SET MASTINN-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-IDSET
           END-IF
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-CHK-LEVEL
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
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  MASTINN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-09)
               MOVE 'B01'                  TO APKEY
               MOVE APKEY                  TO AUTOPAR-KEY1
               READ AUTOPAR RECORD KEY IS AUTOPAR-KEY1
               INVALID KEY
                   SET I-99                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-99            TO TRUE
                   PERFORM AUTOPAR-FLDSET
                   PERFORM AUTOPAR-IDSET
               END-READ
               SET I-09                    TO TRUE
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           END-IF
           IF  (I-L2)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2)
               SUBTRACT 1 FROM APAAR   GIVING FAAR
               SET NOT-I-79                TO TRUE
               IF  FIRMA = '923'
                   SET I-79                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '658'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           SET NOT-I-11                    TO TRUE
           IF  DAGENS = '*'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  OM = '**'
               SET I-17                    TO TRUE
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  OM = 'QQ'
               SET I-18                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  MND < APMND
               SET I-16                    TO TRUE
           END-IF
           IF  MND = APMND
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  AAR NOT < 80
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               ADD AAR TO 1900         GIVING OAAR
           END-IF
           IF  (NOT-I-80)
               ADD AAR TO 2000         GIVING OAAR
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  OAAR = APAAR
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  RUT = 'K'
               SET I-14                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  OAAR = FAAR
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           IF  MND < APMND
               SET I-22                    TO TRUE
           END-IF
           IF  MND = APMND
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  DAG NOT > APDAG
               SET I-24                    TO TRUE
           END-IF
           IF  (I-21 AND I-22)
               OR  (I-21 AND I-23 AND I-24)
               SET I-15                    TO TRUE
           END-IF
           ADD ORDBEL TO ZERO          GIVING NTOBEL
           IF  (I-31)
               ADD ORDSVS TO ZERO      GIVING SVSBEL
      *****************************************************************
      *          RUTINE FOR Å NULLSTILLE SUMMER.                      *
      *****************************************************************
           END-IF
           IF  (I-L2)
               MOVE 0                      TO AODAG2
               MOVE 0                      TO OBDAG2
               MOVE 0                      TO RBDAG2
               MOVE 0                      TO KBDAG2
               MOVE 0                      TO NODAG2
               MOVE 0                      TO AOMND2
               MOVE 0                      TO VLMND2
               MOVE 0                      TO VRMND2
               MOVE 0                      TO NOMND2
               MOVE 0                      TO NOAAR2
               MOVE 0                      TO NOFJO2
      *****************************************************************
      *  NULLSTILLING FIRMATOTALER ANT. VARELINJER.                   *
      *****************************************************************
           END-IF
           IF  (I-L2)
               MOVE 0                      TO VLFTOT
               MOVE 0                      TO VLFT10
               MOVE 0                      TO VLFT13
               MOVE 0                      TO VLFT15
               MOVE 0                      TO VLFT17
               MOVE 0                      TO VLFT18
               MOVE 0                      TO VLFT92
               MOVE 0                      TO VLFT93
               MOVE 0                      TO VLA-ELGTOT
               MOVE 0                      TO VLA-ELGT10
               MOVE 0                      TO VLA-ELGT13
               MOVE 0                      TO VLA-ELGT15
               MOVE 0                      TO VLA-ELGT17
               MOVE 0                      TO VLA-ELGT18
               MOVE 0                      TO VLA-ELGT92
               MOVE 0                      TO VLA-ELGT93
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE 0                      TO AODAG1
               MOVE 0                      TO OBDAG
               MOVE 0                      TO RBDAG
               MOVE 0                      TO KBDAG
               MOVE 0                      TO NODAG
               MOVE 0                      TO SVSDAG
               MOVE 0                      TO BRFDAG
               MOVE 0                      TO AOMND1
               MOVE 0                      TO VLMND1
               MOVE 0                      TO VRMND1
               MOVE 0                      TO NOMND
               MOVE 0                      TO NOAAR
               MOVE 0                      TO NOFJO
               MOVE 0                      TO SVSMND
               MOVE 0                      TO SVSAAR
               MOVE 0                      TO SVSFJO
               MOVE 0                      TO BRFMND
               MOVE 0                      TO BRFAAR
               MOVE 0                      TO BRFFJO
      *****************************************************************
      *          RUTINE FOR Å HENTE ORDREMOTAGERS NAVN.               *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO KEY5 (1:3)
               MOVE '07'                   TO KEY5 (4:2)
               MOVE KEY5                   TO OMKEY (1:5)
               MOVE OM                     TO KEY3 (1:2)
               MOVE KEY3                   TO OMKEY (6:3)
               MOVE OMKEY                  TO STATTAB-KEY1
               READ STATTAB RECORD KEY IS STATTAB-KEY1
               INVALID KEY
                   SET I-88                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-88            TO TRUE
                   PERFORM STATTAB-FLDSET
                   PERFORM STATTAB-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-88)
               MOVE ANAVN (1:9)            TO TABNAV
      *****************************************************************
      *          RUTINE FOR Å DANNE SUMMER.                           *
      *****************************************************************
           END-IF
           IF  (I-11 AND NOT-I-14)
               ADD ANTORD                  TO AODAG1
               ADD ANTORD                  TO AODAG2
               ADD ORDBEL                  TO OBDAG
               ADD REOBEL                  TO RBDAG
           END-IF
           IF  (I-11 AND I-14)
               ADD ORDBEL                  TO KBDAG
           END-IF
           IF  (I-11)
               ADD NTOBEL                  TO NODAG
           END-IF
           IF  (I-11 AND I-31)
               ADD SVSBEL                  TO SVSDAG
           END-IF
           IF  (I-12 AND I-13 AND NOT-I-14)
               ADD ANTORD                  TO AOMND1
               ADD ANTORD                  TO AOMND2
               ADD ANTVL                   TO VLMND1
               ADD ANTVL                   TO VLMND2
               ADD ANTVLR                  TO VRMND1
               ADD ANTVLR                  TO VRMND2
           END-IF
           IF  (I-12 AND I-13)
               ADD NTOBEL                  TO NOMND
           END-IF
           IF  (I-13)
               ADD NTOBEL                  TO NOAAR
           END-IF
           IF  (I-15)
               ADD NTOBEL                  TO NOFJO
           END-IF
           IF  (I-12 AND I-13 AND I-31)
               ADD SVSBEL                  TO SVSMND
           END-IF
           IF  (I-13 AND I-31)
               ADD SVSBEL                  TO SVSAAR
           END-IF
           IF  (I-15 AND I-31)
               ADD SVSBEL                  TO SVSFJO
      *****************************************************************
      * RUTINE TOTALSUMMER VARELINJER.                                *
      *****************************************************************
           END-IF
           IF  (NOT-I-79)
               GO TO ENDVLR-T
           END-IF
           IF  (I-14)
               GO TO ENDVLR-T
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  LK NOT > '10'
               SET I-70                    TO TRUE
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  LK = '13'
               SET I-73                    TO TRUE
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  LK = '15'
               SET I-75                    TO TRUE
           END-IF
           SET NOT-I-77                    TO TRUE
           IF  LK = '17'
               SET I-77                    TO TRUE
           END-IF
           SET NOT-I-78                    TO TRUE
           IF  LK = '18'
               SET I-78                    TO TRUE
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  LK = '92'
               SET I-71                    TO TRUE
           END-IF
           SET NOT-I-72                    TO TRUE
           IF  LK = '93'
               SET I-72                    TO TRUE
           END-IF
           IF  (I-13)
               ADD ANTVL                   TO VLA-ELGTOT
           END-IF
           IF  (I-13 AND I-70)
               ADD ANTVL                   TO VLA-ELGT10
           END-IF
           IF  (I-13 AND I-73)
               ADD ANTVL                   TO VLA-ELGT13
           END-IF
           IF  (I-13 AND I-75)
               ADD ANTVL                   TO VLA-ELGT15
           END-IF
           IF  (I-13 AND I-77)
               ADD ANTVL                   TO VLA-ELGT17
           END-IF
           IF  (I-13 AND I-78)
               ADD ANTVL                   TO VLA-ELGT18
           END-IF
           IF  (I-13 AND I-71)
               ADD ANTVL                   TO VLA-ELGT92
           END-IF
           IF  (I-13 AND I-72)
               ADD ANTVL                   TO VLA-ELGT93
           END-IF
           IF  (I-21)
               ADD ANTVL                   TO VLFTOT
           END-IF
           IF  (I-21 AND I-70)
               ADD ANTVL                   TO VLFT10
           END-IF
           IF  (I-21 AND I-73)
               ADD ANTVL                   TO VLFT13
           END-IF
           IF  (I-21 AND I-75)
               ADD ANTVL                   TO VLFT15
           END-IF
           IF  (I-21 AND I-77)
               ADD ANTVL                   TO VLFT17
           END-IF
           IF  (I-21 AND I-78)
               ADD ANTVL                   TO VLFT18
           END-IF
           IF  (I-21 AND I-71)
               ADD ANTVL                   TO VLFT92
           END-IF
           IF  (I-21 AND I-72)
               ADD ANTVL                   TO VLFT93
           END-IF.
 
       ENDVLR-T.
      *****************************************************************
      *          RUTINE FOR Å DANNE SUMMER VED L1 BRUDD (ORDREMOTAGER)*
      *          AVRUNDING OG TOTALSUMMER PR. FIRMA.                  *
      *****************************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'ORD80'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD825  '                 TO LPROG
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
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
               ADD OBDAG TO ZERO       GIVING OBDAG1 ROUNDED
               ADD OBDAG1                  TO OBDAG2
               ADD RBDAG TO ZERO       GIVING RBDAG1 ROUNDED
               ADD RBDAG1                  TO RBDAG2
               ADD KBDAG TO ZERO       GIVING KBDAG1 ROUNDED
               ADD KBDAG1                  TO KBDAG2
               ADD NODAG TO ZERO       GIVING NODAG1 ROUNDED
               ADD NODAG1                  TO NODAG2
               SUBTRACT SVSDAG FROM NODAG GIVING BRFDG
               ADD BRFDG TO ZERO       GIVING BRFDG1 ROUNDED
               ADD BRFDG1                  TO BRFDAG
               ADD NOMND TO ZERO       GIVING NOMND1 ROUNDED
               ADD NOMND1                  TO NOMND2
               SUBTRACT SVSMND FROM NOMND GIVING BRFMD
               ADD BRFMD TO ZERO       GIVING BRFMD1 ROUNDED
               ADD BRFMD1                  TO BRFMND
               ADD NOAAR TO ZERO       GIVING NOAAR1 ROUNDED
               ADD NOAAR1                  TO NOAAR2
               SUBTRACT SVSAAR FROM NOAAR GIVING BRFAR
               ADD BRFAR TO ZERO       GIVING BRFAR1 ROUNDED
               ADD BRFAR1                  TO BRFAAR
               ADD NOFJO TO ZERO       GIVING NOFJO1 ROUNDED
               ADD NOFJO1                  TO NOFJO2
               SUBTRACT SVSFJO FROM NOFJO GIVING BRFFJ
               ADD BRFFJ TO ZERO       GIVING BRFFJ1 ROUNDED
               ADD BRFFJ1                  TO BRFFJO
               SUBTRACT VRMND1 FROM VLMND1 GIVING DIFF1
               MULTIPLY 100 BY DIFF1   GIVING DIFFX
               SET NOT-I-61                TO TRUE
               IF  VLMND1 = 0
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-61)
               DIVIDE DIFFX BY VLMND1  GIVING SERVP ROUNDED
           END-IF
           IF  (I-L1 AND I-61)
               MOVE 0                      TO SERVP
           END-IF
           IF  (I-L1)
               SET NOT-I-51                TO TRUE
               IF  SERVP NOT < 100
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-51)
               ADD SERVP TO ZERO       GIVING SERVP1
           END-IF
           IF  (I-L1 AND I-51)
               ADD SERVP TO ZERO       GIVING SERVP8
           END-IF
           IF  (I-L1)
               SET NOT-I-61                TO TRUE
               IF  AOMND1 = 0
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-61)
               DIVIDE NOMND BY AOMND1  GIVING OSNIT1 ROUNDED
           END-IF
           IF  (I-L1 AND I-61)
               MOVE 0                      TO OSNIT1
           END-IF
           IF  (I-L1)
               SET NOT-I-48                TO TRUE
               IF  NOAAR1 = 0
                   SET I-48                TO TRUE
               END-IF
               SET NOT-I-49                TO TRUE
               IF  NOFJO1 = 0
                   SET I-49                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-48 AND I-49)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-L1 AND I-U1 AND I-88 AND NOT-I-17)
               SET I-50                    TO TRUE
      *****************************************************************
      *          RUTINE FOR Å DANNE SUMMER VED L2 BRUDD (FIRMA)       *
      *          AVRUNDING OG TOTALSUMMER PR. FIRMA.                  *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SUBTRACT VRMND2 FROM VLMND2 GIVING DIFF2
               MULTIPLY 100 BY DIFF2   GIVING DIFFX
           END-IF
           IF  (I-L1)
               SET NOT-I-61                TO TRUE
               IF  VLMND2 = 0
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-61)
               DIVIDE DIFFX BY VLMND2  GIVING SERVP ROUNDED
           END-IF
           IF  (I-L2 AND I-61)
               MOVE 0                      TO SERVP
           END-IF
           IF  (I-L2)
               SET NOT-I-52                TO TRUE
               IF  SERVP NOT < 100
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-52)
               ADD SERVP TO ZERO       GIVING SERVP2
           END-IF
           IF  (I-L2 AND I-52)
               ADD SERVP TO ZERO       GIVING SERVP9
           END-IF
           IF  (I-L1)
               SET NOT-I-61                TO TRUE
               IF  AOMND2 = 0
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-61)
               DIVIDE NOMND2 BY AOMND2 GIVING OSNIT2 ROUNDED
           END-IF
           IF  (I-L2 AND I-61)
               MOVE 0                      TO OSNIT2
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       MASTINN-GET SECTION.
       MASTINN-GET-P.
           IF  MASTINN-EOF-OFF
               READ MASTINN
               AT END
                   SET MASTINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MASTINN-FLDSET SECTION.
       MASTINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MASTINN-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE MASTINN-IO-AREA (17:2) TO OM (1:2)
               MOVE MASTINN-IO-AREA (11:2) TO DAG-IO
               INSPECT DAG-IO REPLACING ALL ' ' BY '0'
               MOVE MASTINN-IO-AREA (13:2) TO MND-IO
               INSPECT MND-IO REPLACING ALL ' ' BY '0'
               MOVE MASTINN-IO-AREA (15:2) TO AAR-IO
               INSPECT AAR-IO REPLACING ALL ' ' BY '0'
               MOVE MASTINN-IO-AREA (26:1) TO RUT (1:1)
               MOVE MASTINN-IO-AREA (29:2) TO LK (1:2)
               MOVE MASTINN-IO-AREA (41:1) TO DAGENS (1:1)
               MOVE MASTINN-IO-AREA (51:4) TO ANTORD-IO
               MOVE MASTINN-IO-AREA (55:6) TO ORDBEL-IO
               MOVE MASTINN-IO-AREA (61:6) TO ORDSVS-IO
               MOVE MASTINN-IO-AREA (67:6) TO REOBEL-IO
               MOVE MASTINN-IO-AREA (73:5) TO ANTVL-IO
               MOVE MASTINN-IO-AREA (78:5) TO ANTVLR-IO
           END-EVALUATE.
 
       MASTINN-IDSET SECTION.
       MASTINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       MASTINN-CHK-LEVEL SECTION.
       MASTINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO MASTINN-LEVEL-01
               MOVE MASTINN-IO-AREA (2:3)  TO MASTINN-01-L2-FIRMA
               MOVE MASTINN-IO-AREA (17:2) TO MASTINN-01-L1-OM
               IF  MASTINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MASTINN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  MASTINN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MASTINN-01-L2         TO THE-PRIOR-L2
               MOVE  MASTINN-01-L1         TO THE-PRIOR-L1
               SET MASTINN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (4:6)  TO APDATO-IO
               INSPECT APDATO-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (4:2)  TO APDAG-IO
               INSPECT APDAG-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (6:2)  TO APMND-IO
               INSPECT APMND-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (26:4) TO APAAR-IO
               INSPECT APAAR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       STATTAB-FLDSET SECTION.
       STATTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATTAB-IO-AREA (11:30) TO ANAVN (1:30)
           END-EVALUATE.
 
       STATTAB-IDSET SECTION.
       STATTAB-IDSET-P.
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'DAGLIG ORDRESTATISTIKK' TO LISTE-IO-AREA (32:22)
               MOVE 'PR. ORDREMOTAGER'     TO LISTE-IO-AREA (55:16)
               MOVE 'ORDREDATO'            TO LISTE-IO-AREA (92:9)
               MOVE APDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (102:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT'                  TO LISTE-IO-AREA (15:3)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (23:5)
               MOVE 'REST-'                TO LISTE-IO-AREA (33:5)
               MOVE 'KRED.'                TO LISTE-IO-AREA (43:5)
               MOVE 'NETTO ANT.'           TO LISTE-IO-AREA (53:10)
               MOVE 'ANT.'                 TO LISTE-IO-AREA (65:4)
               MOVE 'V.LIN SERV  SNITT'    TO LISTE-IO-AREA (72:17)
               MOVE 'AKK.NETTO'            TO LISTE-IO-AREA (93:9)
               MOVE 'AKK.NETTO'            TO LISTE-IO-AREA (105:9)
               MOVE 'AKK.NETTO'            TO LISTE-IO-AREA (117:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OM NAVN'              TO LISTE-IO-AREA (1:7)
               MOVE 'ORD'                  TO LISTE-IO-AREA (15:3)
               MOVE 'BELØP'                TO LISTE-IO-AREA (23:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (33:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (43:5)
               MOVE 'BELØP ORDRE'          TO LISTE-IO-AREA (53:11)
               MOVE 'V.LIN.'               TO LISTE-IO-AREA (65:6)
               MOVE 'M/RES PROS O.BEL.'    TO LISTE-IO-AREA (72:17)
               MOVE 'ORDRESUM.'            TO LISTE-IO-AREA (93:9)
               MOVE 'ORDRESUM.'            TO LISTE-IO-AREA (105:9)
               MOVE 'ORDRESUM.'            TO LISTE-IO-AREA (117:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'IDAG'                 TO LISTE-IO-AREA (14:4)
               MOVE 'IDAG'                 TO LISTE-IO-AREA (24:4)
               MOVE 'IDAG'                 TO LISTE-IO-AREA (34:4)
               MOVE 'IDAG'                 TO LISTE-IO-AREA (44:4)
               MOVE ' IDAG D.MND'          TO LISTE-IO-AREA (53:11)
               MOVE 'D.MND'                TO LISTE-IO-AREA (65:5)
               MOVE 'D.MND  MND  D.MND'    TO LISTE-IO-AREA (72:17)
               MOVE 'D.MND'                TO LISTE-IO-AREA (97:5)
               MOVE 'I ÅR'                 TO LISTE-IO-AREA (110:4)
               MOVE 'I FJOR'               TO LISTE-IO-AREA (120:6)
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
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'DAGLIG ORDRESTATISTIKK' TO LISTE-IO-AREA (32:22)
               MOVE 'PR. ORDREMOTAGER'     TO LISTE-IO-AREA (55:16)
               MOVE 'ORDREDATO'            TO LISTE-IO-AREA (92:9)
               MOVE APDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (102:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT'                  TO LISTE-IO-AREA (15:3)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (23:5)
               MOVE 'REST-'                TO LISTE-IO-AREA (33:5)
               MOVE 'KRED.'                TO LISTE-IO-AREA (43:5)
               MOVE 'NETTO ANT.'           TO LISTE-IO-AREA (53:10)
               MOVE 'ANT.'                 TO LISTE-IO-AREA (65:4)
               MOVE 'V.LIN SERV  SNITT'    TO LISTE-IO-AREA (72:17)
               MOVE 'AKK.NETTO'            TO LISTE-IO-AREA (93:9)
               MOVE 'AKK.NETTO'            TO LISTE-IO-AREA (105:9)
               MOVE 'AKK.NETTO'            TO LISTE-IO-AREA (117:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OM NAVN'              TO LISTE-IO-AREA (1:7)
               MOVE 'ORD'                  TO LISTE-IO-AREA (15:3)
               MOVE 'BELØP'                TO LISTE-IO-AREA (23:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (33:5)
               MOVE 'BELØP'                TO LISTE-IO-AREA (43:5)
               MOVE 'BELØP ORDRE'          TO LISTE-IO-AREA (53:11)
               MOVE 'V.LIN.'               TO LISTE-IO-AREA (65:6)
               MOVE 'M/RES PROS O.BEL.'    TO LISTE-IO-AREA (72:17)
               MOVE 'ORDRESUM.'            TO LISTE-IO-AREA (93:9)
               MOVE 'ORDRESUM.'            TO LISTE-IO-AREA (105:9)
               MOVE 'ORDRESUM.'            TO LISTE-IO-AREA (117:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'IDAG'                 TO LISTE-IO-AREA (14:4)
               MOVE 'IDAG'                 TO LISTE-IO-AREA (24:4)
               MOVE 'IDAG'                 TO LISTE-IO-AREA (34:4)
               MOVE 'IDAG'                 TO LISTE-IO-AREA (44:4)
               MOVE ' IDAG D.MND'          TO LISTE-IO-AREA (53:11)
               MOVE 'D.MND'                TO LISTE-IO-AREA (65:5)
               MOVE 'D.MND  MND  D.MND'    TO LISTE-IO-AREA (72:17)
               MOVE 'D.MND'                TO LISTE-IO-AREA (97:5)
               MOVE 'I ÅR'                 TO LISTE-IO-AREA (110:4)
               MOVE 'I FJOR'               TO LISTE-IO-AREA (120:6)
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
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86 AND NOT-I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE OM                     TO LISTE-IO-AREA (1:2)
               IF  (NOT-I-88)
                   MOVE TABNAV             TO LISTE-IO-AREA (4:9)
               END-IF
               IF  (I-17)
                   MOVE 'DIR.ORDRE'        TO LISTE-IO-AREA (4:9)
               END-IF
               IF  (I-18)
                   MOVE 'INT.ORDRE'        TO LISTE-IO-AREA (4:9)
               END-IF
               MOVE AODAG1                 TO XO-30YY9
               MOVE XO-30YY9               TO LISTE-IO-AREA (15:3)
               INITIALIZE AODAG1
               MOVE OBDAG1                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (19:9)
               MOVE RBDAG1                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (29:9)
               MOVE KBDAG1                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               MOVE NODAG1                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (49:10)
               MOVE AOMND1                 TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (59:5)
               INITIALIZE AOMND1
               MOVE VLMND1                 TO XO-60YN9
               MOVE XO-60YN9               TO LISTE-IO-AREA (65:6)
               INITIALIZE VLMND1
               MOVE VRMND1                 TO XO-60YN9
               MOVE XO-60YN9               TO LISTE-IO-AREA (71:6)
               INITIALIZE VRMND1
               IF  (NOT-I-51)
                   MOVE SERVP1             TO XO-21YYZ
                   MOVE XO-21YYZ           TO LISTE-IO-AREA (78:4)
               END-IF
               IF  (I-51)
                   MOVE SERVP8             TO XO-30YYZ
                   MOVE XO-30YYZ           TO LISTE-IO-AREA (78:3)
               END-IF
               MOVE OSNIT1                 TO XO-50YYZ
               MOVE XO-50YYZ               TO LISTE-IO-AREA (83:6)
               MOVE NOMND1                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (91:11)
               MOVE NOAAR1                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (102:12)
               MOVE NOFJO1                 TO XO-90YYZR
               MOVE XO-90YYZR              TO LISTE-IO-AREA (114:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND NOT-I-86 AND NOT-I-50)
           AND (I-31)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE OM                     TO LISTE-IO-AREA (1:2)
               IF  (NOT-I-88)
                   MOVE TABNAV             TO LISTE-IO-AREA (4:9)
               END-IF
               IF  (I-17)
                   MOVE 'DIR.ORDRE'        TO LISTE-IO-AREA (4:9)
               END-IF
               IF  (I-18)
                   MOVE 'INT.ORDRE'        TO LISTE-IO-AREA (4:9)
               END-IF
               MOVE 'SELVKOST         '    TO LISTE-IO-AREA (16:17)
               MOVE SVSDAG                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (46:13)
               MOVE SVSMND                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (88:14)
               MOVE SVSAAR                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (99:15)
               MOVE SVSFJO                 TO XO-92YYZR
               MOVE XO-92YYZR              TO LISTE-IO-AREA (111:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE OM                     TO LISTE-IO-AREA (1:2)
               IF  (NOT-I-88)
                   MOVE TABNAV             TO LISTE-IO-AREA (4:9)
               END-IF
               IF  (I-17)
                   MOVE 'DIR.ORDRE'        TO LISTE-IO-AREA (4:9)
               END-IF
               IF  (I-18)
                   MOVE 'INT.ORDRE'        TO LISTE-IO-AREA (4:9)
               END-IF
               MOVE 'BRUTTOFORTJENESTE'    TO LISTE-IO-AREA (16:17)
               MOVE BRFDAG                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (49:10)
               MOVE BRFMND                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (91:11)
               MOVE BRFAAR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (102:12)
               MOVE BRFFJO                 TO XO-90YYZR
               MOVE XO-90YYZR              TO LISTE-IO-AREA (114:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA TOTAL'          TO LISTE-IO-AREA (1:11)
               MOVE AODAG2                 TO XO-30YY9
               MOVE XO-30YY9               TO LISTE-IO-AREA (15:3)
               INITIALIZE AODAG2
               MOVE OBDAG2                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (19:9)
               INITIALIZE OBDAG2
               MOVE RBDAG2                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (29:9)
               INITIALIZE RBDAG2
               MOVE KBDAG2                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE KBDAG2
               MOVE NODAG2                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (49:10)
               INITIALIZE NODAG2
               MOVE AOMND2                 TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (59:5)
               INITIALIZE AOMND2
               MOVE VLMND2                 TO XO-60YN9
               MOVE XO-60YN9               TO LISTE-IO-AREA (65:6)
               INITIALIZE VLMND2
               MOVE VRMND2                 TO XO-60YN9
               MOVE XO-60YN9               TO LISTE-IO-AREA (71:6)
               INITIALIZE VRMND2
               IF  (NOT-I-52)
                   MOVE SERVP2             TO XO-21YYZ
                   MOVE XO-21YYZ           TO LISTE-IO-AREA (78:4)
               END-IF
               IF  (I-52)
                   MOVE SERVP9             TO XO-30YYZ
                   MOVE XO-30YYZ           TO LISTE-IO-AREA (78:3)
               END-IF
               MOVE OSNIT2                 TO XO-50YYZ
               MOVE XO-50YYZ               TO LISTE-IO-AREA (83:6)
               MOVE NOMND2                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (91:11)
               INITIALIZE NOMND2
               MOVE NOAAR2                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (102:12)
               INITIALIZE NOAAR2
               MOVE NOFJO2                 TO XO-90YYZR
               MOVE XO-90YYZR              TO LISTE-IO-AREA (114:12)
               INITIALIZE NOFJO2
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86 AND I-79)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ANTALL VARELINJER' TO LISTE-IO-AREA (11:24)
               MOVE 'HITTIL I ÅR'          TO LISTE-IO-AREA (40:11)
               MOVE 'TOTALT IFJOR'         TO LISTE-IO-AREA (54:12)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT HELE FIRMA       ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGTOT             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFTOT                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT LAGER 10         ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGT10             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFT10                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT LAGER 13         ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGT13             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFT13                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT LAGER 15         ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGT15             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFT15                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT LAGER 17         ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGT17             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFT17                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT LAGER 18         ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGT18             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFT18                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT LAGER 92         ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGT92             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFT92                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT LAGER 93         ' TO LISTE-IO-AREA (11:24)
               MOVE VLA-ELGT93             TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (42:9)
               MOVE VLFT93                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (57:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           SET MASTINN-LEVEL-INIT          TO TRUE
           INITIALIZE MASTINN-DATA-FIELDS
           SET MASTINN-EOF-OFF             TO TRUE
           SET MASTINN-PROCESS             TO TRUE
           OPEN INPUT MASTINN
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           INITIALIZE STATTAB-DATA-FIELDS
           OPEN INPUT STATTAB
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE MASTINN
           CLOSE AUTOPAR
           CLOSE STATTAB
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
