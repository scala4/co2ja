       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD063R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring excel på Report Web *****************
      *       O R D R E R U T I N E P R O G R A M   O R D 0 6 3       *
      *       -------------------------------------------------       *
      *  1. UTSKRIFT AV ORDRESTATISTIKK DIR.REG. KUNDER.              *
      *      14/3-1997 AV ESPEN LARSEN.                               *
      *     14/12-2004 UPSI 1 GIR WWW-SUMMER (TEKST)                  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD063.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT PFILE
               ASSIGN TO UT-S-PFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PFILE-STATUS.
           SELECT ORDSUMF
               ASSIGN TO UT-S-ORDSUMF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSUMF-STATUS.
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
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD PFILE
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  PFILE-IO-AREA.
           05  PFILE-IO-AREA-X             PICTURE X(100).
       FD ORDSUMF
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  ORDSUMF-IO-AREA.
           05  ORDSUMF-IO-AREA-X           PICTURE X(80).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
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
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  PFILE-STATUS                PICTURE 99 VALUE 0.
           10  ORDSUMF-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-EOF-OFF           VALUE '0'.
               88  PFILE-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-READ-OFF          VALUE '0'.
               88  PFILE-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-PROCESS-OFF       VALUE '0'.
               88  PFILE-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMF-EOF-OFF         VALUE '0'.
               88  ORDSUMF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMF-READ-OFF        VALUE '0'.
               88  ORDSUMF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMF-PROCESS-OFF     VALUE '0'.
               88  ORDSUMF-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDSUMF-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDSUMF-LEVEL-INIT      VALUE '1'.
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
           05  PARAM-DATA-FIELDS.
               10  PJOBN                   PICTURE X(8).
               10  PRKODE                  PICTURE X(1).
               10  PPERS                   PICTURE X(30).
               10  PANTX-IO.
                   15  PANTX               PICTURE S9(3).
               10  PETTB                   PICTURE X(40).
               10  PFORS                   PICTURE X(40).
               10  PMEMO                   PICTURE X(40).
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  PFILE-DATA-FIELDS.
               10  PAR4                    PICTURE X(4).
               10  MNDNAV                  PICTURE X(9).
           05  ORDSUMF-LEVEL-01.
               10  ORDSUMF-01-L2.
                   15  ORDSUMF-01-L2-FIRMA PICTURE X(3).
               10  ORDSUMF-01-L1.
                   15  ORDSUMF-01-L1-KUNDE PICTURE X(6).
           05  ORDSUMF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
               10  KNRKEY                  PICTURE X(9).
               10  AODMT-IO.
                   15  AODMT               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  AOA-ELGRT-IO.
                   15  AOA-ELGRT           PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  AOFJT-IO.
                   15  AOFJT               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  AODMDR-IO.
                   15  AODMDR              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  AOA-ELGRDR-IO.
                   15  AOA-ELGRDR          PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  AOFJDR-IO.
                   15  AOFJDR              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BODMT-IO.
                   15  BODMT               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BOA-ELGRT-IO.
                   15  BOA-ELGRT           PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BOFJT-IO.
                   15  BOFJT               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BODMDR-IO.
                   15  BODMDR              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BOA-ELGRDR-IO.
                   15  BOA-ELGRDR          PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BOFJDR-IO.
                   15  BOFJDR              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BKDMT-IO.
                   15  BKDMT               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BKA-ELGRT-IO.
                   15  BKA-ELGRT           PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  BKFJT-IO.
                   15  BKFJT               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN1                   PICTURE X(30).
               10  PSTED                   PICTURE X(15).
               10  PNR                     PICTURE X(4).
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FADMT-IO.
                   15  FADMT               PICTURE S9(5).
               10  FAA-ELGRT-IO.
                   15  FAA-ELGRT           PICTURE S9(5).
               10  FAFJT-IO.
                   15  FAFJT               PICTURE S9(5).
               10  FADMDR-IO.
                   15  FADMDR              PICTURE S9(5).
               10  FAA-ELGRDR-IO.
                   15  FAA-ELGRDR          PICTURE S9(5).
               10  FAFJDR-IO.
                   15  FAFJDR              PICTURE S9(5).
               10  FBDMT-IO.
                   15  FBDMT               PICTURE S9(9).
               10  FBA-ELGRT-IO.
                   15  FBA-ELGRT           PICTURE S9(9).
               10  FBFJT-IO.
                   15  FBFJT               PICTURE S9(9).
               10  FBDMTK-IO.
                   15  FBDMTK              PICTURE S9(9).
               10  FBA-ELGRTK-IO.
                   15  FBA-ELGRTK          PICTURE S9(9).
               10  FBFJTK-IO.
                   15  FBFJTK              PICTURE S9(9).
               10  FBDMDR-IO.
                   15  FBDMDR              PICTURE S9(9).
               10  FBA-ELGRDR-IO.
                   15  FBA-ELGRDR          PICTURE S9(9).
               10  FBFJDR-IO.
                   15  FBFJDR              PICTURE S9(9).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9).
               10  BNDMT-IO.
                   15  BNDMT               PICTURE S9(9).
               10  FAKTOR-IO.
                   15  FAKTOR              PICTURE S9(2)V9(4).
               10  BKDMDR-IO.
                   15  BKDMDR              PICTURE S9(9).
               10  BNDMDR-IO.
                   15  BNDMDR              PICTURE S9(9).
               10  BNA-ELGRT-IO.
                   15  BNA-ELGRT           PICTURE S9(9).
               10  BKA-ELGRDR-IO.
                   15  BKA-ELGRDR          PICTURE S9(9).
               10  BNA-ELGRDR-IO.
                   15  BNA-ELGRDR          PICTURE S9(9).
               10  BNFJT-IO.
                   15  BNFJT               PICTURE S9(9).
               10  BKFJDR-IO.
                   15  BKFJDR              PICTURE S9(9).
               10  BNFJDR-IO.
                   15  BNFJDR              PICTURE S9(9).
               10  FKDMDR-IO.
                   15  FKDMDR              PICTURE S9(9).
               10  FKA-ELGRDR-IO.
                   15  FKA-ELGRDR          PICTURE S9(9).
               10  FKFJDR-IO.
                   15  FKFJDR              PICTURE S9(9).
               10  FNDMT-IO.
                   15  FNDMT               PICTURE S9(9).
               10  FNA-ELGRT-IO.
                   15  FNA-ELGRT           PICTURE S9(9).
               10  FNFJT-IO.
                   15  FNFJT               PICTURE S9(9).
               10  FNDMTD-IO.
                   15  FNDMTD              PICTURE S9(9).
               10  FNA-ELGRTD-IO.
                   15  FNA-ELGRTD          PICTURE S9(9).
               10  FNFJTD-IO.
                   15  FNFJTD              PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-90YY9R               PICTURE ZZZ.ZZZ.ZZ9-.
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
           SET NOT-I-81                    TO TRUE
           SET NOT-I-82                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  PFILE-PROCESS
               SET PFILE-PROCESS-OFF       TO TRUE
               SET PFILE-READ              TO TRUE
           END-IF
 
           IF  PFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM PFILE-GET
               SET PFILE-READ-OFF          TO TRUE
               IF  NOT PFILE-EOF
                   PERFORM PFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PFILE-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  ORDSUMF-PROCESS
               SET ORDSUMF-PROCESS-OFF     TO TRUE
               SET ORDSUMF-READ            TO TRUE
           END-IF
 
           IF  ORDSUMF-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDSUMF-GET
               SET ORDSUMF-READ-OFF        TO TRUE
               IF  NOT ORDSUMF-EOF
                   SET ORDSUMF-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  PFILE-PROCESS
               PERFORM PFILE-IDSET
           END-IF
 
           IF  ORDSUMF-PROCESS
               PERFORM ORDSUMF-IDSET
           END-IF
 
           IF  ORDSUMF-PROCESS
               PERFORM ORDSUMF-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  PFILE-PROCESS
               PERFORM PFILE-FLDSET
           END-IF
 
           IF  ORDSUMF-PROCESS
               PERFORM ORDSUMF-FLDOFF
               PERFORM ORDSUMF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDSUMF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-81)
               MOVE PJOBN                  TO BJOBN
               SET NOT-I-89                TO TRUE
               IF  PRKODE = 'B'
                   SET I-89                TO TRUE
               END-IF
               MOVE PRKODE                 TO BBEST
           END-IF
           IF  (I-81 AND I-89)
               MOVE PPERS                  TO BPERS
               MOVE PANTX                  TO BANTX-IO
           END-IF
           IF  (I-82 AND I-89)
               MOVE PETTB                  TO BETTB
           END-IF
           IF  (I-83 AND I-89)
               MOVE PFORS                  TO BFORS
           END-IF
           IF  (I-84 AND I-89)
               MOVE PMEMO                  TO BMEMO
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           END-IF
           IF  (I-03)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '950'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           MOVE KNRKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-10                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-10                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
      *****************************************************************
      * NULLSTILLING AV SUMFELTER.                                    *
      *****************************************************************
           IF  (I-L2)
               SUBTRACT FADMT              FROM FADMT
               SUBTRACT FAA-ELGRT          FROM FAA-ELGRT
               SUBTRACT FAFJT              FROM FAFJT
               SUBTRACT FADMDR             FROM FADMDR
               SUBTRACT FAA-ELGRDR         FROM FAA-ELGRDR
               SUBTRACT FAFJDR             FROM FAFJDR
               SUBTRACT FBDMT              FROM FBDMT
               SUBTRACT FBA-ELGRT          FROM FBA-ELGRT
               SUBTRACT FBFJT              FROM FBFJT
               SUBTRACT FBDMTK             FROM FBDMTK
               SUBTRACT FBA-ELGRTK         FROM FBA-ELGRTK
               SUBTRACT FBFJTK             FROM FBFJTK
               SUBTRACT FBDMDR             FROM FBDMDR
               SUBTRACT FBA-ELGRDR         FROM FBA-ELGRDR
               SUBTRACT FBFJDR             FROM FBFJDR
      *****************************************************************
      * SUMMERING PR. FIRMA.                                          *
      *****************************************************************
           END-IF
           ADD AODMT                       TO FADMT
           ADD AOA-ELGRT                   TO FAA-ELGRT
           ADD AOFJT                       TO FAFJT
           ADD AODMDR                      TO FADMDR
           ADD AOA-ELGRDR                  TO FAA-ELGRDR
           ADD AOFJDR                      TO FAFJDR
      *
           ADD BODMT                       TO FBDMT
           ADD BOA-ELGRT                   TO FBA-ELGRT
           ADD BOFJT                       TO FBFJT
           ADD BKDMT                       TO FBDMTK
           ADD BKA-ELGRT                   TO FBA-ELGRTK
           ADD BKFJT                       TO FBFJTK
           ADD BODMDR                      TO FBDMDR
           ADD BOA-ELGRDR                  TO FBA-ELGRDR
           ADD BOFJDR                      TO FBFJDR.
 
       SLUTT-T.
      *****************************************************************
      * TOTALBEREGNINGER FOR HAFNOR.                                  *
      *   KR.NOTA DIR.REG. BEREGNES UTIFRA TOT.SALG OG TOT.KR.NOTA.   *
      *   NETTO SALG.      BEREGNES UTIFRA TOT.SALG - TOT.KR.NOTA.    *
      *   NETTO DIR.REG.   BEREGNES UTIFRA DIR.SALG - KR.NOTA DIR.REG.*
      *****************************************************************
           IF  (NOT-I-95)
               GO TO END95-T
           END-IF
           DIVIDE BKDMT BY -1          GIVING SUM1
           SUBTRACT SUM1 FROM BODMT    GIVING BNDMT
           IF  (NOT-I-61)
               DIVIDE SUM1 BY BODMT    GIVING FAKTOR ROUNDED
               MULTIPLY FAKTOR BY BODMDR GIVING BKDMDR ROUNDED
               SUBTRACT BKDMDR FROM BODMDR GIVING BNDMDR
               DIVIDE BKDMDR BY -1     GIVING BKDMDR
      *****************************************************************
           END-IF
           DIVIDE BKA-ELGRT BY -1      GIVING SUM1
           SUBTRACT SUM1 FROM BOA-ELGRT GIVING BNA-ELGRT
           IF  (NOT-I-62)
               DIVIDE SUM1 BY BOA-ELGRT GIVING FAKTOR ROUNDED
               MULTIPLY FAKTOR BY BOA-ELGRDR GIVING BKA-ELGRDR ROUNDED
               SUBTRACT BKA-ELGRDR FROM BOA-ELGRDR GIVING BNA-ELGRDR
               DIVIDE BKA-ELGRDR BY -1 GIVING BKA-ELGRDR
      *****************************************************************
           END-IF
           DIVIDE BKFJT BY -1          GIVING SUM1
           SUBTRACT SUM1 FROM BOFJT    GIVING BNFJT
           IF  (NOT-I-63)
               DIVIDE SUM1 BY BOFJT    GIVING FAKTOR ROUNDED
               MULTIPLY FAKTOR BY BOFJDR GIVING BKFJDR ROUNDED
               SUBTRACT BKFJDR FROM BOFJDR GIVING BNFJDR
               DIVIDE BKFJDR BY -1     GIVING BKFJDR
      *****************************************************************
           END-IF
           ADD BKDMDR                      TO FKDMDR
           ADD BKA-ELGRDR                  TO FKA-ELGRDR
           ADD BKFJDR                      TO FKFJDR
      *****************************************************************
           ADD BNDMT                       TO FNDMT
           ADD BNA-ELGRT                   TO FNA-ELGRT
           ADD BNFJT                       TO FNFJT
      *****************************************************************
           ADD BNDMDR                      TO FNDMTD
           ADD BNA-ELGRDR                  TO FNA-ELGRTD
           ADD BNFJDR                      TO FNFJTD.
 
       END95-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'ORD47'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD063  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               MOVE PARAM-IO-AREA (8:8)    TO PJOBN (1:8)
               MOVE PARAM-IO-AREA (19:1)   TO PRKODE (1:1)
               MOVE PARAM-IO-AREA (32:30)  TO PPERS (1:30)
               MOVE PARAM-IO-AREA (69:3)   TO PANTX-IO
               INSPECT PANTX-IO REPLACING ALL ' ' BY '0'
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               MOVE PARAM-IO-AREA (21:40)  TO PETTB (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               MOVE PARAM-IO-AREA (21:40)  TO PFORS (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               MOVE PARAM-IO-AREA (21:40)  TO PMEMO (1:40)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               SET I-81                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               SET I-82                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               SET I-83                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               SET I-84                    TO TRUE
           END-EVALUATE.
 
       PFILE-GET SECTION.
       PFILE-GET-P.
           IF  PFILE-EOF-OFF
               READ PFILE
               AT END
                   SET PFILE-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PFILE-FLDSET SECTION.
       PFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PFILE-IO-AREA (1:1) = '9'
            AND   PFILE-IO-AREA (2:1) = '0' )
               MOVE PFILE-IO-AREA (3:4)    TO PAR4 (1:4)
               MOVE PFILE-IO-AREA (9:9)    TO MNDNAV (1:9)
           END-EVALUATE.
 
       PFILE-IDCHK SECTION.
       PFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PFILE-IO-AREA (1:1) = '9'
            AND   PFILE-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PFILE-IDSET SECTION.
       PFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( PFILE-IO-AREA (1:1) = '9'
            AND   PFILE-IO-AREA (2:1) = '0' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       ORDSUMF-GET SECTION.
       ORDSUMF-GET-P.
           IF  ORDSUMF-EOF-OFF
               READ ORDSUMF
               AT END
                   SET ORDSUMF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDSUMF-FLDOFF SECTION.
       ORDSUMF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-61                TO TRUE
               SET NOT-I-62                TO TRUE
               SET NOT-I-63                TO TRUE
           END-EVALUATE.
 
       ORDSUMF-FLDSET SECTION.
       ORDSUMF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDSUMF-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDSUMF-IO-AREA (5:6)  TO KUNDE (1:6)
               MOVE ORDSUMF-IO-AREA (2:9)  TO KNRKEY (1:9)
               MOVE ORDSUMF-IO-AREA (11:3) TO AODMT-IO
               MOVE ORDSUMF-IO-AREA (14:3) TO AOA-ELGRT-IO
               MOVE ORDSUMF-IO-AREA (17:3) TO AOFJT-IO
               MOVE ORDSUMF-IO-AREA (20:3) TO AODMDR-IO
               MOVE ORDSUMF-IO-AREA (23:3) TO AOA-ELGRDR-IO
               MOVE ORDSUMF-IO-AREA (26:3) TO AOFJDR-IO
               MOVE ORDSUMF-IO-AREA (29:5) TO BODMT-IO
               IF  BODMT = ZERO
                   SET I-61                TO TRUE
               END-IF
               MOVE ORDSUMF-IO-AREA (34:5) TO BOA-ELGRT-IO
               IF  BOA-ELGRT = ZERO
                   SET I-62                TO TRUE
               END-IF
               MOVE ORDSUMF-IO-AREA (39:5) TO BOFJT-IO
               IF  BOFJT = ZERO
                   SET I-63                TO TRUE
               END-IF
               MOVE ORDSUMF-IO-AREA (44:5) TO BODMDR-IO
               MOVE ORDSUMF-IO-AREA (49:5) TO BOA-ELGRDR-IO
               MOVE ORDSUMF-IO-AREA (54:5) TO BOFJDR-IO
               MOVE ORDSUMF-IO-AREA (59:5) TO BKDMT-IO
               MOVE ORDSUMF-IO-AREA (64:5) TO BKA-ELGRT-IO
               MOVE ORDSUMF-IO-AREA (69:5) TO BKFJT-IO
           END-EVALUATE.
 
       ORDSUMF-IDSET SECTION.
       ORDSUMF-IDSET-P.
           SET I-01                        TO TRUE.
 
       ORDSUMF-CHK-LEVEL SECTION.
       ORDSUMF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDSUMF-LEVEL-01
               MOVE ORDSUMF-IO-AREA (2:3)  TO ORDSUMF-01-L2-FIRMA
               MOVE ORDSUMF-IO-AREA (5:6)  TO ORDSUMF-01-L1-KUNDE
               IF  ORDSUMF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSUMF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDSUMF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSUMF-01-L2         TO THE-PRIOR-L2
               MOVE  ORDSUMF-01-L1         TO THE-PRIOR-L1
               SET ORDSUMF-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO PSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO PNR (1:4)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-06                        TO TRUE.
 
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
           IF  (I-01 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KUNDE                  TO LISTE-IO-AREA (2:6)
               IF  (NOT-I-10)
                   MOVE NAVN1              TO LISTE-IO-AREA (9:30)
               END-IF
               IF  (I-10)
                   MOVE 'UKJENT KUNDENUMMER' TO LISTE-IO-AREA (9:18)
               END-IF
               MOVE '* KUNDE TOT.SALG*'    TO LISTE-IO-AREA (40:17)
               MOVE AODMT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (59:6)
               MOVE AOA-ELGRT              TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (67:6)
               MOVE AOFJT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (75:6)
               MOVE BODMT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE BOA-ELGRT              TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE BOFJT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-10)
                   MOVE PNR                TO LISTE-IO-AREA (9:4)
               END-IF
               IF  (NOT-I-10)
                   MOVE PSTED              TO LISTE-IO-AREA (14:15)
               END-IF
               IF  (NOT-I-U1)
                   MOVE '* HERAV DIR.REG.*' TO LISTE-IO-AREA (40:17)
               END-IF
               IF  (I-U1)
                   MOVE '* HERAV WWW.REG.*' TO LISTE-IO-AREA (40:17)
               END-IF
               MOVE AODMDR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (59:6)
               MOVE AOA-ELGRDR             TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (67:6)
               MOVE AOFJDR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (75:6)
               MOVE BODMDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE BOA-ELGRDR             TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE BOFJDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86 AND NOT-I-95)
           OR  (I-01 AND NOT-I-86 AND I-95)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* KUNDE TOT.KRED*'    TO LISTE-IO-AREA (40:17)
               MOVE BKDMT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE BKA-ELGRT              TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE BKFJT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               EVALUATE TRUE
               WHEN (I-01 AND NOT-I-86 AND NOT-I-95)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-01 AND NOT-I-86 AND I-95)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86 AND I-95)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* KRED. NTO.REG.*'    TO LISTE-IO-AREA (40:17)
               MOVE BKDMDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE BKA-ELGRDR             TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE BKFJDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* KUNDE NTO.TOT.*'    TO LISTE-IO-AREA (40:17)
               MOVE BNDMT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE BNA-ELGRT              TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE BNFJT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* KUNDE NTO.DIR.*'    TO LISTE-IO-AREA (40:17)
               MOVE BNDMDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE BNA-ELGRDR             TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE BNFJDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'ORDRESTATISTIKK'      TO LISTE-IO-AREA (32:15)
               IF  (NOT-I-U1)
                   MOVE 'DIREKTE'          TO LISTE-IO-AREA (48:7)
               END-IF
               IF  (I-U1)
                   MOVE 'FRA WWW'          TO LISTE-IO-AREA (48:7)
               END-IF
               MOVE 'REGISTRERINGS'        TO LISTE-IO-AREA (56:13)
               MOVE 'KUNDER  PR.'          TO LISTE-IO-AREA (70:11)
               MOVE MNDNAV                 TO LISTE-IO-AREA (82:9)
               MOVE PAR4                   TO LISTE-IO-AREA (92:4)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (98:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (108:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (118:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (123:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL ORDRE'         TO LISTE-IO-AREA (60:12)
               MOVE 'FAKTURERT'            TO LISTE-IO-AREA (73:9)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (85:8)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (98:8)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (111:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDENAVN OG POSTADR.' TO LISTE-IO-AREA (9:21)
               MOVE 'D.MND'                TO LISTE-IO-AREA (60:5)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (66:7)
               MOVE 'AKK IFJ'              TO LISTE-IO-AREA (74:7)
               MOVE 'D.MND'                TO LISTE-IO-AREA (88:5)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (99:7)
               MOVE 'AKK IFJ'              TO LISTE-IO-AREA (112:7)
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
               MOVE 'ORDRESTATISTIKK'      TO LISTE-IO-AREA (32:15)
               IF  (NOT-I-U1)
                   MOVE 'DIREKTE'          TO LISTE-IO-AREA (48:7)
               END-IF
               IF  (I-U1)
                   MOVE 'FRA WWW'          TO LISTE-IO-AREA (48:7)
               END-IF
               MOVE 'REGISTRERINGS'        TO LISTE-IO-AREA (56:13)
               MOVE 'KUNDER  PR.'          TO LISTE-IO-AREA (70:11)
               MOVE MNDNAV                 TO LISTE-IO-AREA (82:9)
               MOVE PAR4                   TO LISTE-IO-AREA (92:4)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (98:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (108:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (118:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (123:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL ORDRE'         TO LISTE-IO-AREA (60:12)
               MOVE 'FAKTURERT'            TO LISTE-IO-AREA (73:9)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (85:8)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (98:8)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (111:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDENAVN OG POSTADR.' TO LISTE-IO-AREA (9:21)
               MOVE 'D.MND'                TO LISTE-IO-AREA (60:5)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (66:7)
               MOVE 'AKK IFJ'              TO LISTE-IO-AREA (74:7)
               MOVE 'D.MND'                TO LISTE-IO-AREA (88:5)
               MOVE 'AKK IÅR'              TO LISTE-IO-AREA (99:7)
               MOVE 'AKK IFJ'              TO LISTE-IO-AREA (112:7)
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
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***'                  TO LISTE-IO-AREA (5:3)
               MOVE 'FIRMATOTAL'           TO LISTE-IO-AREA (9:10)
               MOVE '* FIRMA TOT.SALG*'    TO LISTE-IO-AREA (40:17)
               MOVE FADMT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (59:6)
               MOVE FAA-ELGRT              TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (67:6)
               MOVE FAFJT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (75:6)
               MOVE FBDMT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE FBA-ELGRT              TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE FBFJT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-U1)
                   MOVE '* HERAV DIR.REG.*' TO LISTE-IO-AREA (40:17)
               END-IF
               IF  (I-U1)
                   MOVE '* HERAV WWW.REG.*' TO LISTE-IO-AREA (40:17)
               END-IF
               MOVE FADMDR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (59:6)
               MOVE FAA-ELGRDR             TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (67:6)
               MOVE FAFJDR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (75:6)
               MOVE FBDMDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE FBA-ELGRDR             TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE FBFJDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* FIRMA TOT.KRED*'    TO LISTE-IO-AREA (40:17)
               MOVE FBDMTK                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               MOVE FBA-ELGRTK             TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               MOVE FBFJTK                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86 AND I-95)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-U1)
                   MOVE '* FIRMA DIR.KRED*' TO LISTE-IO-AREA (40:17)
               END-IF
               IF  (I-U1)
                   MOVE '* FIRMA WWW.KRED*' TO LISTE-IO-AREA (40:17)
               END-IF
               MOVE FKDMDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               INITIALIZE FKDMDR
               MOVE FKA-ELGRDR             TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               INITIALIZE FKA-ELGRDR
               MOVE FKFJDR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               INITIALIZE FKFJDR
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* FIRMA NTO.TOT.*'    TO LISTE-IO-AREA (40:17)
               MOVE FNDMT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               INITIALIZE FNDMT
               MOVE FNA-ELGRT              TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               INITIALIZE FNA-ELGRT
               MOVE FNFJT                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               INITIALIZE FNFJT
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-U1)
                   MOVE '* FIRMA NTO.DIR.*' TO LISTE-IO-AREA (40:17)
               END-IF
               IF  (I-U1)
                   MOVE '* FIRMA NTO.WWW.*' TO LISTE-IO-AREA (40:17)
               END-IF
               MOVE FNDMTD                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (82:12)
               INITIALIZE FNDMTD
               MOVE FNA-ELGRTD             TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (95:12)
               INITIALIZE FNA-ELGRTD
               MOVE FNFJTD                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (108:12)
               INITIALIZE FNFJTD
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
           MOVE 3                          TO LR-CHECK
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           INITIALIZE PFILE-DATA-FIELDS
           SET PFILE-EOF-OFF               TO TRUE
           SET PFILE-PROCESS               TO TRUE
           OPEN INPUT PFILE
           SET ORDSUMF-LEVEL-INIT          TO TRUE
           INITIALIZE ORDSUMF-DATA-FIELDS
           SET ORDSUMF-EOF-OFF             TO TRUE
           SET ORDSUMF-PROCESS             TO TRUE
           OPEN INPUT ORDSUMF
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
           CLOSE PARAM
           CLOSE PFILE
           CLOSE ORDSUMF
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
