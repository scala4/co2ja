       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR801R.
      **********************************************  Z-WIN-RPG2   ****
      *                                          XX2000XXIRXXTF     *
      * ALLE ARTIKKLER MED LIKE ARTIKKLENR, HVOR EDB-NR IKKE STEMMER*
      * MED EDBNR. SOM FINNES I OPPSLAGSARKIVET BLIR LAGT UT FOR    *
      * SLETTING. BEHOLDNING FRA VAREN SOM SKAL SLETTES BLIR LAGT   *
      * INN I VAREN SOM SKAL BEHOLDES.                              *
      ***************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR801.rpg
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
           SELECT KOPINN
               ASSIGN TO UT-S-KOPINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPINN-STATUS.
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS OPPSMAS-STATUS
               RECORD KEY IS OPPSMAS-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT KOPIUT
               ASSIGN TO UT-S-KOPIUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPIUT-STATUS.
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
       FD KOPINN
               BLOCK CONTAINS 9600
               RECORD CONTAINS 200.
       01  KOPINN-IO-AREA.
           05  KOPINN-IO-AREA-X            PICTURE X(200).
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X.
               10  OPPSMAS-KEY1            PICTURE X(21).
               10  FILLER                  PICTURE X(9).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD KOPIUT
               BLOCK CONTAINS 9600
               RECORD CONTAINS 200.
       01  KOPIUT-IO-AREA.
           05  KOPIUT-IO-AREA-X            PICTURE X(200).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARO-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       77  ARA-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARO-TABLE.
               10  ARO-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARO-I
                                                      ARO-S.
                   15  ARO                 PICTURE X(1).
      **************************************************************            ¶±éŽ
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(1).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  KOPINN-STATUS               PICTURE 99 VALUE 0.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  KOPIUT-STATUS               PICTURE 99 VALUE 0.
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
               88  KOPINN-EOF-OFF          VALUE '0'.
               88  KOPINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPINN-READ-OFF         VALUE '0'.
               88  KOPINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPINN-PROCESS-OFF      VALUE '0'.
               88  KOPINN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOPINN-LEVEL-INIT-OFF   VALUE '0'.
               88  KOPINN-LEVEL-INIT       VALUE '1'.
           05  OPPSMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
      **************************************************************
           05  KOPINN-LEVEL-01.
               10  KOPINN-01-L3.
                   15  KOPINN-01-L3-FNR    PICTURE X(3).
               10  KOPINN-01-L2.
                   15  KOPINN-01-L2-ALF    PICTURE X(3).
               10  KOPINN-01-L1.
                   15  KOPINN-01-L1-ANUM   PICTURE X(20).
           05  KOPINN-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  ENUM                    PICTURE X(7).
               10  ALF                     PICTURE X(3).
               10  ANUM                    PICTURE X(20).
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LAG13-IO.
                   15  LAG13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG93-IO.
                   15  LAG93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG15-IO.
                   15  LAG15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG17-IO.
                   15  LAG17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG92-IO.
                   15  LAG92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG18-IO.
                   15  LAG18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LOCGML                  PICTURE X(6).
               10  REC                     PICTURE X(200).
           05  OPPSMAS-DATA-FIELDS.
               10  EDBNR                   PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  VATIN-IO.
                   15  VATIN               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VATUT-IO.
                   15  VATUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEH13-IO.
                   15  BEH13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BEH93-IO.
                   15  BEH93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BEH15-IO.
                   15  BEH15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BEH17-IO.
                   15  BEH17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BEH92-IO.
                   15  BEH92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BEH18-IO.
                   15  BEH18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(20).
           05  TEMPORARY-FIELDS.
               10  XENUM                   PICTURE X(7).
               10  XAIN-IO.
                   15  XAIN                PICTURE S9(7)V9(2).
               10  XAUT-IO.
                   15  XAUT                PICTURE S9(7)V9(2).
               10  MAXANT-IO.
                   15  MAXANT              PICTURE S9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  Z-IO.
                   15  Z                   PICTURE S9(2).
               10  ANR                     PICTURE X(1).
               10  KKEY                    PICTURE X(6).
               10  OKEY                    PICTURE X(15).
               10  ARO14                   PICTURE X(14).
               10  KEY-X                   PICTURE X(21).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(5).
               10  VKEY                    PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-72YNZR               PICTURE ZZZZZZZ,ZZ-.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           05  MOVEA-COUNT                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE1                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA1                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE2                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA2                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-LENGTH                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-OFFSET                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-TEMP                  PICTURE X(256).
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
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
 
           IF  KOPINN-PROCESS
               SET KOPINN-PROCESS-OFF      TO TRUE
               SET KOPINN-READ             TO TRUE
           END-IF
 
           IF  KOPINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM KOPINN-GET
               SET KOPINN-READ-OFF         TO TRUE
               IF  NOT KOPINN-EOF
                   SET KOPINN-PROCESS      TO TRUE
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
 
           IF  KOPINN-PROCESS
               PERFORM KOPINN-IDSET
           END-IF
 
           IF  KOPINN-PROCESS
               PERFORM KOPINN-CHK-LEVEL
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
 
           IF  KOPINN-PROCESS
               PERFORM KOPINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KOPINN-PROCESS
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
      **************************************************************
           END-IF
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           SET NOT-I-12                    TO TRUE
           SET NOT-I-14                    TO TRUE
           IF  (NOT-I-L1 AND I-01)
               GO TO KOMRUT-T
           END-IF
           IF  (I-01)
               MOVE ENUM                   TO XENUM
               ADD ANTIN TO ZERO       GIVING XAIN
               ADD ANTUT TO ZERO       GIVING XAUT
           END-IF
           IF  (I-L1 AND I-01)
               GO TO SLUTT-T
      ****************************************************
      *     RUTINE FOR KONTROLL AV ARTIKKELNUMMER        *
      ****************************************************
           END-IF
           .
 
       KOMRUT-T.
           IF  (I-01)
               MOVE 20                     TO MAXANT
      *
           END-IF
           IF  (I-01)
               PERFORM VARYING ARO-I FROM 1 BY 1
                         UNTIL ARO-I > ARO-MAX
                   MOVE ' '                TO ARO (ARO-I)
               END-PERFORM
               MOVE 1                      TO MOVEA-SA1 MOVEA-SA2
               MOVE 20                     TO MOVEA-SIZE1
               MULTIPLY ARA-MAX BY 1 GIVING MOVEA-SIZE2
               IF  MOVEA-SIZE1 > MOVEA-SIZE2
                   MOVE MOVEA-SIZE2        TO MOVEA-SIZE1
               END-IF
               MOVE ANUM
                        TO ARA-TABLE (MOVEA-SA2:MOVEA-SIZE2)
               SUBTRACT X                  FROM X
               SUBTRACT Z                  FROM Z
      *
           END-IF
           .
 
       RUTA-T.
           IF  (I-01)
               ADD 1                       TO X
               SET NOT-I-21                TO TRUE
               IF  X > MAXANT
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-21 AND I-01)
               GO TO PRINT-X-T
           END-IF
           IF  (I-01)
               MOVE ARA (X)                TO ANR
               SET NOT-I-40                TO TRUE
               IF  ANR = '.'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = '&'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = ','
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = '+'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = '-'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = '/'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = ')'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = '('
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-01)
               SET NOT-I-40                TO TRUE
               IF  ANR = ' '
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40 AND I-01)
               GO TO RUTA-T
      **********************************************************
      *      RUTINE FOR OPPBYGGING AV OPPSLAGSNUMMER           *
      **********************************************************
           END-IF
           IF  (I-01)
               ADD 1                       TO Z
               SET NOT-I-22                TO TRUE
               IF  Z > MAXANT
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND I-01)
               GO TO PRINT-X-T
           END-IF
           IF  (I-01)
               MOVE ANR                    TO ARO (Z)
               GO TO RUTA-T
           END-IF.
 
       PRINT-X-T.
      **************************************************************
           IF  (I-01)
               MOVE FNR                    TO KKEY (1:3)
               MOVE ALF                    TO KKEY (4:3)
               MOVE ' '                    TO OKEY (1:1)
               MOVE 1                      TO MOVEA-SA1 MOVEA-SA2
               MULTIPLY ARO-MAX BY 1 GIVING MOVEA-SIZE1
               MOVE 14                     TO MOVEA-SIZE2
               IF  MOVEA-SIZE1 > MOVEA-SIZE2
                   MOVE MOVEA-SIZE2        TO MOVEA-SIZE1
               END-IF
               MOVE ARO-TABLE (MOVEA-SA1:MOVEA-SIZE1)
                        TO ARO14
               MOVE ARO14                  TO OKEY (2:14)
               MOVE KKEY                   TO KEY-X (1:6)
               MOVE OKEY                   TO KEY-X (7:15)
               MOVE KEY-X                  TO OPPSMAS-KEY1
               READ OPPSMAS RECORD KEY IS OPPSMAS-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM OPPSMAS-FLDSET
                   PERFORM OPPSMAS-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-12                TO TRUE
               SET NOT-I-14                TO TRUE
               IF  ENUM NOT = EDBNR
                   SET I-12                TO TRUE
               END-IF
               IF  ENUM = EDBNR
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-L1)
               ADD 1                       TO ANTKOR
      ********************************************************
           END-IF
           IF  (I-01 AND I-12)
               MOVE FNR                    TO VKEY (1:3)
               MOVE EDBNR                  TO VKEY (4:7)
               MOVE VKEY                   TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-12 AND NOT-I-11)
               ADD ANTIN                   TO VATIN
               ADD ANTUT                   TO VATUT
               ADD LAG13                   TO BEH13
               ADD LAG93                   TO BEH93
               ADD LAG15                   TO BEH15
               ADD LAG17                   TO BEH17
               ADD LAG92                   TO BEH92
               ADD LAG18                   TO BEH18
           END-IF
           IF  (I-01 AND I-12)
               GO TO SLUTT-T
      ********************************************************
           END-IF
           IF  (I-01 AND I-14)
               MOVE FNR                    TO VKEY (1:3)
               MOVE ENUM                   TO VKEY (4:7)
               MOVE VKEY                   TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-14 AND NOT-I-11)
               ADD XAIN                    TO VATIN
               ADD XAUT                    TO VATUT
           END-IF.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE '  '                   TO BBEST
           END-IF
           MOVE 'VAR39'                    TO LONR
           MOVE FNR                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'VAR801  '                 TO LPROG
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
 
       KOPINN-GET SECTION.
       KOPINN-GET-P.
           IF  KOPINN-EOF-OFF
               READ KOPINN
               AT END
                   SET KOPINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KOPINN-FLDSET SECTION.
       KOPINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPINN-IO-AREA (3:3)   TO FNR (1:3)
               MOVE KOPINN-IO-AREA (6:7)   TO ENUM (1:7)
               MOVE KOPINN-IO-AREA (13:3)  TO ALF (1:3)
               MOVE KOPINN-IO-AREA (16:20) TO ANUM (1:20)
               MOVE KOPINN-IO-AREA (97:5)  TO ANTIN-IO
               MOVE KOPINN-IO-AREA (102:5) TO ANTUT-IO
               MOVE KOPINN-IO-AREA (179:3) TO LAG13-IO
               MOVE KOPINN-IO-AREA (182:3) TO LAG93-IO
               MOVE KOPINN-IO-AREA (185:3) TO LAG15-IO
               MOVE KOPINN-IO-AREA (188:3) TO LAG17-IO
               MOVE KOPINN-IO-AREA (191:3) TO LAG92-IO
               MOVE KOPINN-IO-AREA (194:3) TO LAG18-IO
               MOVE KOPINN-IO-AREA (140:6) TO LOCGML (1:6)
               MOVE KOPINN-IO-AREA (1:200) TO REC (1:200)
           END-EVALUATE.
 
       KOPINN-IDSET SECTION.
       KOPINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       KOPINN-CHK-LEVEL SECTION.
       KOPINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KOPINN-LEVEL-01
               MOVE KOPINN-IO-AREA (3:3)   TO KOPINN-01-L3-FNR
               MOVE KOPINN-IO-AREA (13:3)  TO KOPINN-01-L2-ALF
               MOVE KOPINN-IO-AREA (16:20) TO KOPINN-01-L1-ANUM
               IF  KOPINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOPINN-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  KOPINN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KOPINN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOPINN-01-L3          TO THE-PRIOR-L3
               MOVE  KOPINN-01-L2          TO THE-PRIOR-L2
               MOVE  KOPINN-01-L1          TO THE-PRIOR-L1
               SET KOPINN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       OPPSMAS-FLDSET SECTION.
       OPPSMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (23:7) TO EDBNR (1:7)
           END-EVALUATE.
 
       OPPSMAS-IDSET SECTION.
       OPPSMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (97:5) TO VATIN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO VATUT-IO
               MOVE VAREMAS-IO-AREA (179:3) TO BEH13-IO
               MOVE VAREMAS-IO-AREA (182:3) TO BEH93-IO
               MOVE VAREMAS-IO-AREA (185:3) TO BEH15-IO
               MOVE VAREMAS-IO-AREA (188:3) TO BEH17-IO
               MOVE VAREMAS-IO-AREA (191:3) TO BEH92-IO
               MOVE VAREMAS-IO-AREA (194:3) TO BEH18-IO
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-12 AND NOT-I-11)
           OR  (I-01 AND I-14 AND NOT-I-11)
               MOVE VATIN                  TO XO-72P
               MOVE XO-72P-EF              TO VAREMAS-IO-AREA (97:5)
               MOVE VATUT                  TO XO-72P
               MOVE XO-72P-EF              TO VAREMAS-IO-AREA (102:5)
               MOVE BEH13                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (179:3)
               MOVE BEH93                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (182:3)
               MOVE BEH15                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (185:3)
               MOVE BEH17                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (188:3)
               MOVE BEH92                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (191:3)
               MOVE BEH18                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (194:3)
               MOVE LOCGML                 TO VAREMAS-IO-AREA (140:6)
               REWRITE VAREMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAREMAS'
               END-REWRITE
           END-IF
           IF  (I-01 AND NOT-I-L1)
               MOVE SPACES TO KOPIUT-IO-AREA
               INITIALIZE KOPIUT-IO-AREA
               MOVE REC                    TO KOPIUT-IO-AREA (1:200)
               IF  (I-12)
                   MOVE ENUM               TO KOPIUT-IO-AREA (146:7)
               END-IF
               IF  (I-14)
                   MOVE XENUM              TO KOPIUT-IO-AREA (146:7)
               END-IF
               IF  (I-12)
                   MOVE 'K'                TO KOPIUT-IO-AREA (200:1)
               END-IF
               IF  (I-14)
                   MOVE 'R'                TO KOPIUT-IO-AREA (200:1)
               END-IF
               WRITE KOPIUT-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALF                    TO LISTE-IO-AREA (1:3)
               MOVE ANUM                   TO LISTE-IO-AREA (6:20)
               MOVE ENUM                   TO LISTE-IO-AREA (29:7)
               MOVE XENUM                  TO LISTE-IO-AREA (39:7)
               MOVE XAIN                   TO XO-72YNZR
               MOVE XO-72YNZR              TO LISTE-IO-AREA (50:11)
               MOVE XAUT                   TO XO-72YNZR
               MOVE XO-72YNZR              TO LISTE-IO-AREA (65:11)
               IF  (I-12)
                   MOVE 'KORRIGERT.   '    TO LISTE-IO-AREA (108:13)
               END-IF
               IF  (I-14)
                   MOVE 'VAR RIKTIG.  '    TO LISTE-IO-AREA (108:13)
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
               MOVE 'KVITTERINGSLISTE'     TO LISTE-IO-AREA (39:16)
               MOVE 'ENDRING AV VAREARKIV' TO LISTE-IO-AREA (58:20)
               MOVE 'OPPDATERT'            TO LISTE-IO-AREA (92:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (111:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (121:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (126:4)
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
               MOVE 'ALFA     ARTIKKELNUMMER' TO LISTE-IO-AREA (1:23)
               MOVE 'EDB.NR.'              TO LISTE-IO-AREA (29:7)
               MOVE 'F.EDB.NR.'            TO LISTE-IO-AREA (37:9)
               MOVE 'F.ANT.INN'            TO LISTE-IO-AREA (52:9)
               MOVE 'F.ANT.UT'             TO LISTE-IO-AREA (68:8)
               MOVE 'MERKNAD.'             TO LISTE-IO-AREA (113:8)
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
               MOVE 'KVITTERINGSLISTE'     TO LISTE-IO-AREA (39:16)
               MOVE 'ENDRING AV VAREARKIV' TO LISTE-IO-AREA (58:20)
               MOVE 'OPPDATERT'            TO LISTE-IO-AREA (92:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (111:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (121:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (126:4)
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
               MOVE 'ALFA     ARTIKKELNUMMER' TO LISTE-IO-AREA (1:23)
               MOVE 'EDB.NR.'              TO LISTE-IO-AREA (29:7)
               MOVE 'F.EDB.NR.'            TO LISTE-IO-AREA (37:9)
               MOVE 'F.ANT.INN'            TO LISTE-IO-AREA (52:9)
               MOVE 'F.ANT.UT'             TO LISTE-IO-AREA (68:8)
               MOVE 'MERKNAD.'             TO LISTE-IO-AREA (113:8)
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
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTKOR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE 'DUBLETTER.'           TO LISTE-IO-AREA (12:10)
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET KOPINN-LEVEL-INIT           TO TRUE
           INITIALIZE KOPINN-DATA-FIELDS
           SET KOPINN-EOF-OFF              TO TRUE
           SET KOPINN-PROCESS              TO TRUE
           OPEN INPUT KOPINN
           INITIALIZE OPPSMAS-DATA-FIELDS
           OPEN INPUT OPPSMAS
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN I-O VAREMAS
           OPEN OUTPUT KOPIUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               INITIALIZE ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE KOPINN
           CLOSE OPPSMAS
           CLOSE VAREMAS
           CLOSE KOPIUT
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
