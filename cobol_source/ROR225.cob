       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR225R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM ROR225                    ESPEN LARSEN  14.11.1995           *
      *   UTLISTING AV RESTORDRE PR KUNDE PÅ ANKOMMENDE VARER                  *
      * 05/02/98 RBS MED AVD.RUTINE.                       *
      * 11/12/98 ORDREBEKREFT. BEHANDLES SOM TILBUDSORDRE  *
      * 03/02/00 KONSERNMODELL FOR KUNDE.MASTER.                      *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR225.rpg
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
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT RESTEXT
               ASSIGN TO RESTEXT
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RESTEXT-STATUS
               RECORD KEY IS RESTEXT-KEY1.
           SELECT RABMAST
               ASSIGN TO RABMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RABMAST-STATUS
               RECORD KEY IS RABMAST-KEY1.
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
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD RESTEXT
               RECORD CONTAINS 130.
       01  RESTEXT-IO-AREA.
           05  RESTEXT-IO-AREA-X.
               10  RESTEXT-KEY1            PICTURE X(13).
               10  FILLER                  PICTURE X(117).
       FD RABMAST
               RECORD CONTAINS 40.
       01  RABMAST-IO-AREA.
           05  RABMAST-IO-AREA-X.
               10  RABMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(20).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 9               PICTURE 9(4) USAGE BINARY.
       01  TABLES.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 9 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(1).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RARKIV-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  RESTEXT-STATUS              PICTURE 99 VALUE 0.
           10  RABMAST-STATUS              PICTURE 99 VALUE 0.
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
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RESTEXT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RABMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  RARKIV-LEVEL-03.
               10  RARKIV-03-L4.
                   15  RARKIV-03-L4-FIRM   PICTURE X(3).
               10  RARKIV-03-L3.
                   15  RARKIV-03-L3-AVD    PICTURE X(1).
                   15  RARKIV-03-L3-OTYPE  PICTURE X(1).
               10  RARKIV-03-L2.
                   15  RARKIV-03-L2-RKNR   PICTURE X(6).
               10  RARKIV-03-L1.
                   15  RARKIV-03-L1-BRUDD  PICTURE X(11).
           05  RARKIV-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  AVD                     PICTURE X(1).
               10  OTYPE                   PICTURE X(1).
               10  RKNR                    PICTURE X(6).
               10  BRUDD                   PICTURE X(11).
               10  BETBET                  PICTURE X(2).
               10  EDB                     PICTURE X(7).
               10  RKREF                   PICTURE X(15).
               10  RORDRE                  PICTURE X(6).
               10  RDATO-IO.
                   15  RDATO               PICTURE S9(6).
               10  RANT-IO.
                   15  RANT                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RALFA                   PICTURE X(3).
               10  RARTNR                  PICTURE X(20).
               10  RVABET                  PICTURE X(30).
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
               10  INIT                    PICTURE X(2).
               10  HND                     PICTURE X(3).
               10  VAIND                   PICTURE X(1).
               10  TLIND                   PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  LAGERK                  PICTURE X(2).
               10  ALTNR-IO.
                   15  ALTNR               PICTURE S9(7) USAGE
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
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KADR                    PICTURE X(30).
               10  KPSTED                  PICTURE X(15).
               10  KPNR                    PICTURE X(4).
           05  RESTEXT-DATA-FIELDS.
               10  VADR1                   PICTURE X(30).
               10  VADR2                   PICTURE X(30).
               10  VADR3                   PICTURE X(30).
               10  VADR4                   PICTURE X(20).
               10  TEKST1                  PICTURE X(20).
               10  TEKST2                  PICTURE X(30).
           05  RABMAST-DATA-FIELDS.
               10  RABRA                   PICTURE X(1).
               10  RABM1-IO.
                   15  RABM1               PICTURE S9(2)V9(1).
               10  RABM2-IO.
                   15  RABM2               PICTURE S9(2)V9(1).
               10  RABM3-IO.
                   15  RABM3               PICTURE S9(2)V9(1).
               10  SLETT                   PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  VARTNR                  PICTURE X(20).
               10  UPRIS-IO.
                   15  UPRIS               PICTURE S9(7)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(1).
               10  VGR                     PICTURE X(5).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(2).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(11).
           05  TEMPORARY-FIELDS.
               10  F16                     PICTURE X(16).
               10  F14                     PICTURE X(14).
               10  F30                     PICTURE X(30).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(6)V9(2).
               10  BEL-N-IO.
                   15  BEL-N               PICTURE S9(7)V9(2).
               10  KUNDE                   PICTURE X(9).
               10  TKEY4                   PICTURE X(4).
               10  TKEY10                  PICTURE X(10).
               10  TKEY                    PICTURE X(13).
               10  NUM6-IO.
                   15  NUM6                PICTURE S9(6).
               10  NEDBNR                  PICTURE X(10).
               10  ALTNR-N-IO.
                   15  ALTNR-N             PICTURE S9(7).
               10  HEAD11                  PICTURE X(6).
               10  HEAD12                  PICTURE X(30).
               10  F7                      PICTURE X(7).
               10  F8                      PICTURE X(8).
               10  F15                     PICTURE X(15).
               10  HEAD13                  PICTURE X(30).
               10  HEAD21                  PICTURE X(30).
               10  HEAD22                  PICTURE X(30).
               10  HEAD31                  PICTURE X(30).
               10  HEAD32                  PICTURE X(30).
               10  HEAD41                  PICTURE X(4).
               10  HEAD42                  PICTURE X(15).
               10  HEAD43                  PICTURE X(30).
               10  HEAD44                  PICTURE X(7).
               10  ALF6                    PICTURE X(6).
               10  AAR                     PICTURE X(2).
               10  DAG                     PICTURE X(2).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  AVDNAV                  PICTURE X(10).
               10  VARKEY                  PICTURE X(10).
               10  F11                     PICTURE X(11).
               10  F6                      PICTURE X(6).
               10  F17                     PICTURE X(17).
               10  RABKEY                  PICTURE X(20).
               10  RKOD                    PICTURE X(1).
               10  F1                      PICTURE X(2).
               10  F2                      PICTURE X(4).
               10  F3                      PICTURE X(6).
               10  ANTBEL-IO.
                   15  ANTBEL              PICTURE S9(8)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(8)V9(2).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(6)V9(2).
               10  KUNSUM-IO.
                   15  KUNSUM              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-52YY9R               PICTURE ZZ.ZZZ,99-.
               10  XO-62YY9                PICTURE ZZZ.ZZZ,99.
               10  XO-21YY9                PICTURE ZZ,9.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-06                    TO TRUE
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
           SET NOT-I-50                    TO TRUE
           SET NOT-I-51                    TO TRUE
           SET NOT-I-52                    TO TRUE
           SET NOT-I-53                    TO TRUE
           SET NOT-I-54                    TO TRUE
           SET NOT-I-61                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-63                    TO TRUE
           SET NOT-I-64                    TO TRUE
           SET NOT-I-87                    TO TRUE
           IF  FIRM = '923'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-L4)
               SET I-44                    TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-L4 AND I-87)
               SET I-44                    TO TRUE
           END-IF
           IF  (I-L4)
               MOVE '* UTEN V'             TO F16 (1:8)
               MOVE 'AREADRES'             TO F16 (9:8)
               MOVE 'SE. *   '             TO F14 (1:8)
               MOVE F16                    TO F30 (1:16)
               MOVE F14                    TO F30 (17:14)
           END-IF
           IF  (I-L2)
               SET NOT-I-60                TO TRUE
               SET NOT-I-58                TO TRUE
           END-IF
           IF  (I-L3)
               SET I-43                    TO TRUE
           END-IF
           IF  (I-L2)
               SET I-42                    TO TRUE
           END-IF
           IF  (I-L1)
               SET I-41                    TO TRUE
               SET NOT-I-47                TO TRUE
           END-IF
           IF  (NOT-I-03)
               GO TO SLUTT-T
           END-IF
           MOVE BEL                        TO BEL-N
           MOVE BEL-N-IO (2:8)             TO BEL2-IO
           SET NOT-I-48                    TO TRUE
           IF  VAIND = '*'
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  STATUS-X = 'P'
               SET I-50                    TO TRUE
           END-IF
           IF  (NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  STATUS-X = 'B'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-50)
               SET I-58                    TO TRUE
           END-IF
           IF  (NOT-I-50)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  OTYPE = 'F'
               SET I-35                    TO TRUE
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  OTYPE = 'R'
               SET I-36                    TO TRUE
           END-IF
           SET NOT-I-37                    TO TRUE
           IF  OTYPE = 'S'
               SET I-37                    TO TRUE
           END-IF
           IF  (I-44)
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
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-43)
               PERFORM AVDRUT-S
      **************************************************
      *   HENTE OPPLYSNINGER FRA KUNDEMASTER           *
      **************************************************
           END-IF
           IF  (I-42)
               MOVE FIRM                   TO KUNDE (1:3)
           END-IF
           IF  (I-42 AND NOT-I-08)
               SET NOT-I-12                TO TRUE
               IF  KONFNR > '000'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-42 AND NOT-I-08 AND I-12)
               MOVE KONFNR                 TO KUNDE (1:3)
           END-IF
           IF  (I-42)
               MOVE RKNR                   TO KUNDE (4:6)
               MOVE KUNDE                  TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      *****************************************************************
      * HENTE VAREADRESSE OM DETTE FINNES PÅ DETTE BRUDD.             *
      *****************************************************************
           END-IF
           IF  (NOT-I-41)
               GO TO ENDVAD-T
           END-IF
           IF  (NOT-I-48)
               GO TO ENDVAD-T
           END-IF
           MOVE FIRM                       TO TKEY4 (1:3)
           MOVE OTYPE                      TO TKEY4 (4:1)
           MOVE TKEY4                      TO TKEY10 (1:4)
           MOVE RORDRE                     TO TKEY10 (5:6)
           MOVE TKEY10                     TO TKEY (1:10)
           MOVE '000'                      TO TKEY (11:3)
           MOVE TKEY                       TO RESTEXT-KEY1
           READ RESTEXT RECORD KEY IS RESTEXT-KEY1
           INVALID KEY
               SET I-13                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-13                TO TRUE
               PERFORM RESTEXT-FLDOFF
               PERFORM RESTEXT-FLDSET
               PERFORM RESTEXT-IDSET
           END-READ
           IF  (NOT-I-13)
               SET I-47                    TO TRUE
           END-IF.
 
       ENDVAD-T.
      **************************************************
           ADD RDATO TO ZERO           GIVING NUM6
           PERFORM DTORUT-S
           PERFORM RABRUT-S
      ***************************************************
      *     HENTE OPPLYSNINGER OM ALTERNATIV VARE.      *
      ***************************************************
           IF  (NOT-I-75)
               MOVE FIRM                   TO NEDBNR (1:3)
               MOVE ALTNR                  TO ALTNR-N
               MOVE ALTNR-N-IO             TO NEDBNR (4:7)
               MOVE NEDBNR                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
      *******************************************
      *  INDIKATORSETTING FOR LEVELBRUDD        *
      *******************************************
           END-IF
           IF  (I-44)
               SET I-54                    TO TRUE
           END-IF
           IF  (I-43)
               SET I-53                    TO TRUE
           END-IF
           IF  (I-42)
               SET I-52                    TO TRUE
           END-IF
           IF  (I-41)
               SET I-51                    TO TRUE
           END-IF
           IF  (I-54)
               SET NOT-I-44                TO TRUE
           END-IF
           IF  (I-53)
               SET NOT-I-43                TO TRUE
           END-IF
           IF  (I-52)
               SET NOT-I-42                TO TRUE
           END-IF
           IF  (I-51)
               SET NOT-I-41                TO TRUE
      **********************************************************
      * HEADINGKONTROLLRUTINE KUNDENAVN OG VAREADRESSE OG HND. *
      * * * * *  HEADINGLINJE 1  * * * * *
           END-IF
           IF  (NOT-I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-52)
               MOVE RKNR                   TO HEAD11
           END-IF
           IF  (I-52 AND NOT-I-12)
               MOVE KNAVN1                 TO HEAD12
           END-IF
           IF  (I-52 AND I-12)
               MOVE 'KUNDEN '              TO F7
               MOVE 'ER IKKE '             TO F8
               MOVE F7                     TO F15 (1:7)
               MOVE F8                     TO F15 (8:8)
               MOVE F15                    TO HEAD12 (1:15)
               MOVE 'I KUNDE'              TO F7
               MOVE '-ARKIV. '             TO F8
               MOVE F7                     TO F15 (1:7)
               MOVE F8                     TO F15 (8:8)
               MOVE F15                    TO HEAD12 (16:15)
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-80)
               MOVE VADR1                  TO HEAD13
           END-IF
           IF  (I-52)
               SET I-61                    TO TRUE
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-80)
               SET I-61                    TO TRUE
               SET I-60                    TO TRUE
      * * * * *  HEADINGLINJE 2  * * * * *
           END-IF
           IF  (I-52 AND NOT-I-12 AND NOT-I-70)
               MOVE KNAVN2                 TO HEAD21
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-81)
               MOVE VADR2                  TO HEAD22
           END-IF
           IF  (I-52 AND NOT-I-12 AND NOT-I-70)
               SET I-62                    TO TRUE
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-81)
               SET I-62                    TO TRUE
               SET I-60                    TO TRUE
      * * * * *  HEADINGLINJE 3  * * * * *
           END-IF
           IF  (I-52 AND NOT-I-12 AND NOT-I-71)
               MOVE KADR                   TO HEAD31
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-82)
               MOVE VADR3                  TO HEAD32
           END-IF
           IF  (I-52 AND NOT-I-12 AND NOT-I-71)
               SET I-63                    TO TRUE
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-82)
               SET I-63                    TO TRUE
               SET I-60                    TO TRUE
      * * * * *  HEADINGLINJE 4  * * * * *
           END-IF
           IF  (I-52 AND NOT-I-12)
               MOVE KPNR                   TO HEAD41
               MOVE KPSTED                 TO HEAD42
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-83)
               MOVE VADR4                  TO HEAD43 (1:20)
           END-IF
           IF  (I-51 AND NOT-I-09)
               MOVE 'HND.'                 TO HEAD44 (1:4)
               MOVE HND                    TO HEAD44 (5:3)
           END-IF
           IF  (I-51 AND NOT-I-09)
               OR  (I-52 AND NOT-I-12)
               SET I-64                    TO TRUE
           END-IF
           IF  (I-51 AND I-47 AND NOT-I-83)
               SET I-64                    TO TRUE
               SET I-60                    TO TRUE
           END-IF
           IF  (I-51 AND NOT-I-47 AND I-60)
               PERFORM NOVADR-S
           END-IF.
 
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
      *    SUBRUTINE MARKERING AV NY ORDRE UTEN VAREADRESSE*
      ******************************************************
 
       NOVADR-S SECTION.
       NOVADR-S-P.
           MOVE F30                        TO HEAD13
           SET I-61                        TO TRUE
           SET NOT-I-60                    TO TRUE.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      *    OPPDATERT MED AVDELING I UNDERGRUPPE.           *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'REO04'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           IF  (I-87)
               MOVE AVD                    TO LUNDGR (3:1)
               MOVE '00'                   TO LUNDGR (1:2)
           END-IF
           MOVE 'ROR225  '                 TO LPROG
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
      **********************************************************
      *    RUTINE FOR Å SJEKKE OM RABATTER ER OVERSTYRT I      *
      *    "OREG" KUN OVERSTYRTE RABATTER SKAL FREM PÅ LISTEN. *
      **********************************************************
 
       RABRUT-S SECTION.
       RABRUT-S-P.
           SET NOT-I-77                    TO TRUE
           SET NOT-I-78                    TO TRUE
           SET NOT-I-79                    TO TRUE
           SET NOT-I-84                    TO TRUE
           MOVE FIRM                       TO VARKEY (1:3)
           MOVE EDB                        TO VARKEY (4:7)
           MOVE VARKEY                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-76                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-76                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (I-76)
               GO TO SLUTSR-T
           END-IF
           MOVE RKNR                       TO F11 (1:6)
           MOVE VGR                        TO F11 (7:5)
           MOVE RALFA                      TO F6 (1:3)
           MOVE '   '                      TO F6 (4:3)
           MOVE F11                        TO F17 (1:11)
           MOVE F6                         TO F17 (12:6)
           MOVE F17                        TO RABKEY (4:17)
           MOVE FIRM                       TO RABKEY (1:3)
           MOVE RABKEY                     TO RABMAST-KEY1
           READ RABMAST RECORD KEY IS RABMAST-KEY1
           INVALID KEY
               SET I-30                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-30                TO TRUE
               PERFORM RABMAST-IDCHK
               PERFORM RABMAST-FLDOFF
               PERFORM RABMAST-FLDSET
               PERFORM RABMAST-IDSET
           END-READ
           IF  (NOT-I-30 AND NOT-I-28)
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  RABRA = '9'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               GO TO STAND-T
           END-IF
           SET NOT-I-77                    TO TRUE
           IF  RAB1 = RABM1
               SET I-77                    TO TRUE
           END-IF
           IF  (I-77)
               SET NOT-I-77                TO TRUE
               IF  RAB2 = RABM2
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-77)
               SET NOT-I-77                TO TRUE
               IF  RAB3 = RABM3
                   SET I-77                TO TRUE
               END-IF
           END-IF
           GO TO BUNN-T.
 
       STAND-T.
           IF  (I-12)
               GO TO BUNN-T
           END-IF
           SET NOT-I-89                    TO TRUE
           IF  X = 0
               SET I-89                    TO TRUE
           END-IF
           IF  (I-89)
               ADD 1                       TO X
           END-IF
           MOVE ARA (X)                    TO RKOD
           MOVE RKOD                       TO F1 (1:1)
           MOVE RKOD                       TO F1 (2:1)
           MOVE F1                         TO F2 (1:2)
           MOVE F1                         TO F2 (3:2)
           MOVE F2                         TO F3 (1:4)
           MOVE F2                         TO F3 (3:4)
           MOVE F3                         TO F11 (1:6)
           MOVE VGR                        TO F11 (7:5)
           MOVE RALFA                      TO F6 (1:3)
           MOVE '   '                      TO F6 (4:3)
           MOVE F11                        TO F17 (1:11)
           MOVE F6                         TO F17 (12:6)
           MOVE F17                        TO RABKEY (4:17)
           MOVE FIRM                       TO RABKEY (1:3)
           MOVE RABKEY                     TO RABMAST-KEY1
           READ RABMAST RECORD KEY IS RABMAST-KEY1
           INVALID KEY
               SET I-30                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-30                TO TRUE
               PERFORM RABMAST-IDCHK
               PERFORM RABMAST-FLDOFF
               PERFORM RABMAST-FLDSET
               PERFORM RABMAST-IDSET
           END-READ
           IF  (NOT-I-30 AND NOT-I-28)
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  RABRA = '9'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               GO TO BUNN-T
           END-IF
           SET NOT-I-77                    TO TRUE
           IF  RAB1 = RABM1
               SET I-77                    TO TRUE
           END-IF
           IF  (I-77)
               SET NOT-I-77                TO TRUE
               IF  RAB2 = RABM2
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-77)
               SET NOT-I-77                TO TRUE
               IF  RAB3 = RABM3
                   SET I-77                TO TRUE
               END-IF
           END-IF.
 
       BUNN-T.
           IF  (I-30 AND I-55 AND I-56)
               SET I-79                    TO TRUE
           END-IF
           IF  (I-79 AND I-57)
               SET I-77                    TO TRUE
           END-IF
           IF  (NOT-I-77)
               SET I-78                    TO TRUE
           END-IF
           SET NOT-I-84                    TO TRUE
           IF  BEL NOT = UPRIS
               SET I-84                    TO TRUE
           END-IF
           MULTIPLY RANT BY BEL        GIVING ANTBEL
           IF  (NOT-I-55)
               MULTIPLY RAB1 BY ANTBEL GIVING SUM1
               DIVIDE SUM1 BY 100      GIVING SUM2
               SUBTRACT SUM2               FROM ANTBEL
      *
           END-IF
           IF  (NOT-I-56)
               MULTIPLY RAB2 BY ANTBEL GIVING SUM1
               DIVIDE SUM1 BY 100      GIVING SUM2
               SUBTRACT SUM2               FROM ANTBEL
      *
           END-IF
           IF  (NOT-I-57)
               MULTIPLY RAB3 BY ANTBEL GIVING SUM1
               DIVIDE SUM1 BY 100      GIVING SUM2
               SUBTRACT SUM2               FROM ANTBEL
      *
           END-IF
           ADD ANTBEL                      TO KUNSUM.
 
       SLUTSR-T.
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
 
       RARKIV-FLDOFF SECTION.
       RARKIV-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-55                TO TRUE
               SET NOT-I-56                TO TRUE
               SET NOT-I-57                TO TRUE
               SET NOT-I-09                TO TRUE
               SET NOT-I-75                TO TRUE
           END-EVALUATE.
 
       RARKIV-FLDSET SECTION.
       RARKIV-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RARKIV-IO-AREA (1:3)   TO FIRM (1:3)
               MOVE RARKIV-IO-AREA (4:1)   TO AVD (1:1)
               MOVE RARKIV-IO-AREA (5:1)   TO OTYPE (1:1)
               MOVE RARKIV-IO-AREA (6:6)   TO RKNR (1:6)
               MOVE RARKIV-IO-AREA (24:11) TO BRUDD (1:11)
               MOVE RARKIV-IO-AREA (30:2)  TO BETBET (1:2)
               MOVE RARKIV-IO-AREA (41:7)  TO EDB (1:7)
               MOVE RARKIV-IO-AREA (48:15) TO RKREF (1:15)
               MOVE RARKIV-IO-AREA (64:6)  TO RORDRE (1:6)
               MOVE RARKIV-IO-AREA (73:6)  TO RDATO-IO
               INSPECT RDATO-IO REPLACING ALL ' ' BY '0'
               MOVE RARKIV-IO-AREA (79:4)  TO RANT-IO
               MOVE RARKIV-IO-AREA (83:3)  TO RALFA (1:3)
               MOVE RARKIV-IO-AREA (86:20) TO RARTNR (1:20)
               MOVE RARKIV-IO-AREA (106:30) TO RVABET (1:30)
               MOVE RARKIV-IO-AREA (137:5) TO BEL-IO
               MOVE RARKIV-IO-AREA (142:2) TO RAB1-IO
               IF  RAB1 = ZERO
                   SET I-55                TO TRUE
               END-IF
               MOVE RARKIV-IO-AREA (144:2) TO RAB2-IO
               IF  RAB2 = ZERO
                   SET I-56                TO TRUE
               END-IF
               MOVE RARKIV-IO-AREA (146:2) TO RAB3-IO
               IF  RAB3 = ZERO
                   SET I-57                TO TRUE
               END-IF
               MOVE RARKIV-IO-AREA (149:2) TO INIT (1:2)
               MOVE RARKIV-IO-AREA (151:3) TO HND (1:3)
               IF  HND = SPACES
                   SET I-09                TO TRUE
               END-IF
               MOVE RARKIV-IO-AREA (154:1) TO VAIND (1:1)
               MOVE RARKIV-IO-AREA (155:1) TO TLIND (1:1)
               MOVE RARKIV-IO-AREA (156:1) TO STATUS-X (1:1)
               MOVE RARKIV-IO-AREA (159:2) TO LAGERK (1:2)
               MOVE RARKIV-IO-AREA (166:4) TO ALTNR-IO
               IF  ALTNR = ZERO
                   SET I-75                TO TRUE
               END-IF
           END-EVALUATE.
 
       RARKIV-IDSET SECTION.
       RARKIV-IDSET-P.
           SET I-03                        TO TRUE.
 
       RARKIV-CHK-LEVEL SECTION.
       RARKIV-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RARKIV-LEVEL-03
               MOVE RARKIV-IO-AREA (1:3)   TO RARKIV-03-L4-FIRM
               MOVE RARKIV-IO-AREA (4:1)   TO RARKIV-03-L3-AVD
               MOVE RARKIV-IO-AREA (5:1)   TO RARKIV-03-L3-OTYPE
               MOVE RARKIV-IO-AREA (6:6)   TO RARKIV-03-L2-RKNR
               MOVE RARKIV-IO-AREA (24:11) TO RARKIV-03-L1-BRUDD
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
           SET I-06                        TO TRUE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-70                TO TRUE
               SET NOT-I-71                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
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
               MOVE 199                    TO BW-A
               PERFORM VARYING ARA-I FROM ARA-MAX BY -1
                         UNTIL ARA-I < 1
                   SUBTRACT 1            FROM BW-A
                   MOVE KUNDEMA-IO-AREA (BW-A:1) TO ARA-ENTRY (ARA-I)
               END-PERFORM
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       RESTEXT-FLDOFF SECTION.
       RESTEXT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-80                TO TRUE
               SET NOT-I-81                TO TRUE
               SET NOT-I-82                TO TRUE
               SET NOT-I-83                TO TRUE
           END-EVALUATE.
 
       RESTEXT-FLDSET SECTION.
       RESTEXT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESTEXT-IO-AREA (21:30) TO VADR1 (1:30)
               IF  VADR1 = SPACES
                   SET I-80                TO TRUE
               END-IF
               MOVE RESTEXT-IO-AREA (51:30) TO VADR2 (1:30)
               IF  VADR2 = SPACES
                   SET I-81                TO TRUE
               END-IF
               MOVE RESTEXT-IO-AREA (81:30) TO VADR3 (1:30)
               IF  VADR3 = SPACES
                   SET I-82                TO TRUE
               END-IF
               MOVE RESTEXT-IO-AREA (111:20) TO VADR4 (1:20)
               IF  VADR4 = SPACES
                   SET I-83                TO TRUE
               END-IF
               MOVE RESTEXT-IO-AREA (21:20) TO TEKST1 (1:20)
               MOVE RESTEXT-IO-AREA (41:30) TO TEKST2 (1:30)
           END-EVALUATE.
 
       RESTEXT-IDSET SECTION.
       RESTEXT-IDSET-P.
           SET I-01                        TO TRUE.
 
       RABMAST-FLDOFF SECTION.
       RABMAST-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( RABMAST-IO-AREA (1:1) = '8' )
             OR ( RABMAST-IO-AREA (1:1) = '9' )
               SET NOT-I-31                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-33                TO TRUE
               SET NOT-I-28                TO TRUE
           END-EVALUATE.
 
       RABMAST-FLDSET SECTION.
       RABMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RABMAST-IO-AREA (1:1) = '8' )
             OR ( RABMAST-IO-AREA (1:1) = '9' )
               MOVE RABMAST-IO-AREA (1:1)  TO RABRA (1:1)
               MOVE RABMAST-IO-AREA (22:3) TO RABM1-IO
               INSPECT RABM1-IO REPLACING ALL ' ' BY '0'
               IF  RABM1 = ZERO
                   SET I-31                TO TRUE
               END-IF
               MOVE RABMAST-IO-AREA (25:3) TO RABM2-IO
               INSPECT RABM2-IO REPLACING ALL ' ' BY '0'
               IF  RABM2 = ZERO
                   SET I-32                TO TRUE
               END-IF
               MOVE RABMAST-IO-AREA (28:3) TO RABM3-IO
               INSPECT RABM3-IO REPLACING ALL ' ' BY '0'
               IF  RABM3 = ZERO
                   SET I-33                TO TRUE
               END-IF
               MOVE RABMAST-IO-AREA (40:1) TO SLETT (1:1)
               IF  SLETT = SPACES
                   SET I-28                TO TRUE
               END-IF
           END-EVALUATE.
 
       RABMAST-IDCHK SECTION.
       RABMAST-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RABMAST-IO-AREA (1:1) = '8' )
             OR ( RABMAST-IO-AREA (1:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RABMAST-IDSET SECTION.
       RABMAST-IDSET-P.
           EVALUATE TRUE
           WHEN ( RABMAST-IO-AREA (1:1) = '8' )
             OR ( RABMAST-IO-AREA (1:1) = '9' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (16:20) TO VARTNR (1:20)
               MOVE VAREMAS-IO-AREA (75:9) TO UPRIS-IO
               INSPECT UPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (118:1) TO X-IO
               INSPECT X-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-05                        TO TRUE.
 
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
           IF  (I-61 AND NOT-I-52 AND NOT-I-86)
           OR  (I-61 AND I-52 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE HEAD11                 TO LISTE-IO-AREA (2:6)
               INITIALIZE HEAD11
               MOVE HEAD12                 TO LISTE-IO-AREA (10:30)
               INITIALIZE HEAD12
               MOVE HEAD13                 TO LISTE-IO-AREA (56:30)
               INITIALIZE HEAD13
               EVALUATE TRUE
               WHEN (I-61 AND NOT-I-52 AND NOT-I-86)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-61 AND I-52 AND NOT-I-86)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   MOVE 1                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-62 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE HEAD21                 TO LISTE-IO-AREA (10:30)
               INITIALIZE HEAD21
               MOVE HEAD22                 TO LISTE-IO-AREA (56:30)
               INITIALIZE HEAD22
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-63 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE HEAD31                 TO LISTE-IO-AREA (10:30)
               INITIALIZE HEAD31
               MOVE HEAD32                 TO LISTE-IO-AREA (56:30)
               INITIALIZE HEAD32
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-64 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE HEAD41                 TO LISTE-IO-AREA (10:4)
               INITIALIZE HEAD41
               MOVE HEAD42                 TO LISTE-IO-AREA (15:15)
               INITIALIZE HEAD42
               MOVE HEAD43                 TO LISTE-IO-AREA (46:30)
               INITIALIZE HEAD43
               MOVE HEAD44                 TO LISTE-IO-AREA (79:7)
               INITIALIZE HEAD44
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RANT                   TO XO-52YY9R
               MOVE XO-52YY9R              TO LISTE-IO-AREA (1:10)
               MOVE RALFA                  TO LISTE-IO-AREA (11:3)
               MOVE RARTNR                 TO LISTE-IO-AREA (15:20)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE RORDRE                 TO LISTE-IO-AREA (45:6)
               IF  (I-84)
                   MOVE BEL2               TO XO-62YY9
                   MOVE XO-62YY9           TO LISTE-IO-AREA (52:10)
               END-IF
               IF  (I-78)
                   MOVE RAB1               TO XO-21YY9
                   MOVE XO-21YY9           TO LISTE-IO-AREA (63:4)
               END-IF
               IF  (I-78)
                   MOVE RAB2               TO XO-21YY9
                   MOVE XO-21YY9           TO LISTE-IO-AREA (68:4)
               END-IF
               IF  (I-78)
                   MOVE RAB3               TO XO-21YY9
                   MOVE XO-21YY9           TO LISTE-IO-AREA (73:4)
               END-IF
               MOVE BETBET                 TO LISTE-IO-AREA (78:2)
               MOVE INIT                   TO LISTE-IO-AREA (81:2)
               MOVE LAGERK                 TO LISTE-IO-AREA (84:2)
               MOVE RKREF                  TO LISTE-IO-AREA (87:15)
               MOVE '*'                    TO LISTE-IO-AREA (102:1)
               IF  (NOT-I-75 AND NOT-I-10)
                   MOVE VARTNR             TO LISTE-IO-AREA (103:20)
               END-IF
               IF  (NOT-I-75 AND NOT-I-10)
                   MOVE UPRIS              TO XO-72YY9R
                   MOVE XO-72YY9R          TO LISTE-IO-AREA (120:13)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-53 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               IF  (I-35)
                   MOVE '* * * FORHÅNDSALG SOM N' TO LISTE-IO-AREA
                                                               (32:23)
               END-IF
               IF  (I-35)
                   MOVE 'Å SKAL LEVERES.     * *' TO LISTE-IO-AREA
                                                               (55:23)
               END-IF
               IF  (I-35)
                   MOVE ' *'               TO LISTE-IO-AREA (79:2)
               END-IF
               IF  (I-36)
                   MOVE '* * * RESTORDRE PR. KUN' TO LISTE-IO-AREA
                                                               (32:23)
               END-IF
               IF  (I-36)
                   MOVE 'DE PÅ ANKOMNE VARER * *' TO LISTE-IO-AREA
                                                               (55:23)
               END-IF
               IF  (I-36)
                   MOVE ' *'               TO LISTE-IO-AREA (79:2)
               END-IF
               IF  (I-37)
                   MOVE '* * * SKAFFEVARER SOM N' TO LISTE-IO-AREA
                                                               (32:23)
               END-IF
               IF  (I-37)
                   MOVE 'Å KAN LEVERES       * *' TO LISTE-IO-AREA
                                                               (55:23)
               END-IF
               IF  (I-37)
                   MOVE ' *'               TO LISTE-IO-AREA (79:2)
               END-IF
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (83:8)
               MOVE AVDNAV                 TO LISTE-IO-AREA (94:10)
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
               MOVE '----------------------- ' TO LISTE-IO-AREA (1:24)
               MOVE '----------------------- ' TO LISTE-IO-AREA (25:24)
               MOVE '----------------------- ' TO LISTE-IO-AREA (49:24)
               MOVE '----------------------- ' TO LISTE-IO-AREA (73:24)
               MOVE '-----*----------------- ' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDENR NAVN/ADRESSE/POS' TO LISTE-IO-AREA (2:24)
               MOVE 'TSTED'                TO LISTE-IO-AREA (26:5)
               MOVE 'BESTILT   ORDRE'      TO LISTE-IO-AREA (36:15)
               MOVE 'BELØP'                TO LISTE-IO-AREA (57:5)
               MOVE 'RABATTER      '       TO LISTE-IO-AREA (63:14)
               MOVE 'BM'                   TO LISTE-IO-AREA (78:2)
               MOVE 'OM'                   TO LISTE-IO-AREA (81:2)
               MOVE 'LK'                   TO LISTE-IO-AREA (84:2)
               MOVE '*'                    TO LISTE-IO-AREA (102:1)
               MOVE 'ALTERNATIV LEVERING'  TO LISTE-IO-AREA (109:19)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL    ALF ARTIKKELNU' TO LISTE-IO-AREA (2:24)
               MOVE 'MMER'                 TO LISTE-IO-AREA (26:4)
               MOVE 'DATO      NR.'        TO LISTE-IO-AREA (37:13)
               MOVE 'VAREADRESSE'          TO LISTE-IO-AREA (66:11)
               MOVE 'KUNDEREF.'            TO LISTE-IO-AREA (87:9)
               MOVE '*'                    TO LISTE-IO-AREA (102:1)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (109:14)
               MOVE 'BELØP'                TO LISTE-IO-AREA (126:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '-----*------------------' TO LISTE-IO-AREA (97:24)
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
               IF  (I-35)
                   MOVE '* * * FORHÅNDSALG SOM N' TO LISTE-IO-AREA
                                                               (32:23)
               END-IF
               IF  (I-35)
                   MOVE 'Å SKAL LEVERES.     * *' TO LISTE-IO-AREA
                                                               (55:23)
               END-IF
               IF  (I-35)
                   MOVE ' *'               TO LISTE-IO-AREA (79:2)
               END-IF
               IF  (I-36)
                   MOVE '* * * RESTORDRE PR. KUN' TO LISTE-IO-AREA
                                                               (32:23)
               END-IF
               IF  (I-36)
                   MOVE 'DE PÅ ANKOMNE VARER * *' TO LISTE-IO-AREA
                                                               (55:23)
               END-IF
               IF  (I-36)
                   MOVE ' *'               TO LISTE-IO-AREA (79:2)
               END-IF
               IF  (I-37)
                   MOVE '* * * SKAFFEVARER SOM N' TO LISTE-IO-AREA
                                                               (32:23)
               END-IF
               IF  (I-37)
                   MOVE 'Å KAN LEVERES       * *' TO LISTE-IO-AREA
                                                               (55:23)
               END-IF
               IF  (I-37)
                   MOVE ' *'               TO LISTE-IO-AREA (79:2)
               END-IF
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (83:8)
               MOVE AVDNAV                 TO LISTE-IO-AREA (94:10)
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
               MOVE '----------------------- ' TO LISTE-IO-AREA (1:24)
               MOVE '----------------------- ' TO LISTE-IO-AREA (25:24)
               MOVE '----------------------- ' TO LISTE-IO-AREA (49:24)
               MOVE '----------------------- ' TO LISTE-IO-AREA (73:24)
               MOVE '-----*----------------- ' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDENR NAVN/ADRESSE/POS' TO LISTE-IO-AREA (2:24)
               MOVE 'TSTED'                TO LISTE-IO-AREA (26:5)
               MOVE 'BESTILT   ORDRE'      TO LISTE-IO-AREA (36:15)
               MOVE 'BELØP'                TO LISTE-IO-AREA (57:5)
               MOVE 'RABATTER      '       TO LISTE-IO-AREA (63:14)
               MOVE 'BM'                   TO LISTE-IO-AREA (78:2)
               MOVE 'OM'                   TO LISTE-IO-AREA (81:2)
               MOVE 'LK'                   TO LISTE-IO-AREA (84:2)
               MOVE '*'                    TO LISTE-IO-AREA (102:1)
               MOVE 'ALTERNATIV LEVERING'  TO LISTE-IO-AREA (109:19)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL    ALF ARTIKKELNU' TO LISTE-IO-AREA (2:24)
               MOVE 'MMER'                 TO LISTE-IO-AREA (26:4)
               MOVE 'DATO      NR.'        TO LISTE-IO-AREA (37:13)
               MOVE 'VAREADRESSE'          TO LISTE-IO-AREA (66:11)
               MOVE 'KUNDEREF.'            TO LISTE-IO-AREA (87:9)
               MOVE '*'                    TO LISTE-IO-AREA (102:1)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (109:14)
               MOVE 'BELØP'                TO LISTE-IO-AREA (126:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '-----*------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND I-58 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALSUM PR KUNDE'    TO LISTE-IO-AREA (34:17)
               MOVE KUNSUM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (53:14)
               INITIALIZE KUNSUM
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
           SET RARKIV-LEVEL-INIT           TO TRUE
           INITIALIZE RARKIV-DATA-FIELDS
           SET RARKIV-EOF-OFF              TO TRUE
           SET RARKIV-PROCESS              TO TRUE
           OPEN INPUT RARKIV
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE RESTEXT-DATA-FIELDS
           OPEN INPUT RESTEXT
           INITIALIZE RABMAST-DATA-FIELDS
           OPEN INPUT RABMAST
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RARKIV
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE RESTEXT
           CLOSE RABMAST
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
