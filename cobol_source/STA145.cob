       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA145R.
      **OBS ved endring excel på Report Web *****************
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA145.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
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
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD INNPUT
               BLOCK CONTAINS 230
               RECORD CONTAINS 115.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(115).
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
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
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
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNPUT-LEVEL-INIT-OFF   VALUE '0'.
               88  INNPUT-LEVEL-INIT       VALUE '1'.
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
           05  FAKPAR-DATA-FIELDS.
               10  FAKTOM                  PICTURE X(1).
               10  MNDNAV                  PICTURE X(9).
           05  INNPUT-LEVEL-04.
               10  INNPUT-04-L3.
                   15  INNPUT-04-L3-FNR    PICTURE X(3).
               10  INNPUT-04-L2.
                   15  INNPUT-04-L2-KNR    PICTURE X(6).
               10  INNPUT-04-L1.
                   15  INNPUT-04-L1-VGR    PICTURE X(5).
           05  INNPUT-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  HND                     PICTURE X(3).
               10  ORDRE-IO.
                   15  ORDRE               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KNR                     PICTURE X(6).
               10  VGR                     PICTURE X(5).
               10  FAKT                    PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  AVD                     PICTURE X(1).
               10  EDBNR                   PICTURE X(7).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  FK                      PICTURE X(1).
               10  MND                     PICTURE X(2).
               10  A-ELGR                  PICTURE X(2).
               10  KRETYP                  PICTURE X(1).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  LKODE                   PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  ORD1-IO.
                   15  ORD1                PICTURE S9(7).
               10  ANT1-IO.
                   15  ANT1                PICTURE S9(5)V9(2).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(7)V9(2).
               10  KJSUM1-IO.
                   15  KJSUM1              PICTURE S9(9)V9(2).
               10  KJSUM2-IO.
                   15  KJSUM2              PICTURE S9(9)V9(2).
               10  KJSUM3-IO.
                   15  KJSUM3              PICTURE S9(9)V9(2).
               10  VSUM1A-IO.
                   15  VSUM1A              PICTURE S9(9).
               10  VSUM1B-IO.
                   15  VSUM1B              PICTURE S9(9)V9(2).
               10  VSUMVL-IO.
                   15  VSUMVL              PICTURE S9(5).
               10  KSUM1A-IO.
                   15  KSUM1A              PICTURE S9(9).
               10  KSUM1B-IO.
                   15  KSUM1B              PICTURE S9(9)V9(2).
               10  KSUMVL-IO.
                   15  KSUMVL              PICTURE S9(5).
               10  SUM1A-IO.
                   15  SUM1A               PICTURE S9(9).
               10  SUM1B-IO.
                   15  SUM1B               PICTURE S9(9)V9(2).
               10  SUMVL-IO.
                   15  SUMVL               PICTURE S9(5).
               10  SUM5-IO.
                   15  SUM5                PICTURE S9(9)V9(2).
               10  RPROS1-IO.
                   15  RPROS1              PICTURE S9(3)V9(1).
               10  SUM4-IO.
                   15  SUM4                PICTURE S9(9)V9(2).
               10  RPROS2-IO.
                   15  RPROS2              PICTURE S9(3)V9(1).
               10  SUM3-IO.
                   15  SUM3                PICTURE S9(9)V9(2).
               10  RPROS3-IO.
                   15  RPROS3              PICTURE S9(3)V9(1).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-70YN9                PICTURE ZZZZZZ9.
               10  XO-52YY9R               PICTURE ZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-21YY9R               PICTURE ZZ,9-.
               10  XO-50YN9                PICTURE ZZZZ9.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-31YY9R               PICTURE ZZZ,9-.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
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
 
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   SET INNPUT-PROCESS      TO TRUE
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
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDOFF
               PERFORM INNPUT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNPUT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-15                    TO TRUE
           IF  (I-L1)
               SET NOT-I-16                TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-17                TO TRUE
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           END-IF
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
      ****************************************************************
           END-IF
           IF  (I-L3 AND NOT-I-30)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3)
               SET I-30                    TO TRUE
               MOVE 0                      TO SUM1A
               MOVE 0                      TO SUM1B
               MOVE 0                      TO SUMVL
               MOVE 0                      TO KJSUM3
               MOVE 0                      TO SUM3
               MOVE 0                      TO RPROS3
           END-IF
           IF  (I-L2)
               MOVE 0                      TO KSUM1A
               MOVE 0                      TO KSUM1B
               MOVE 0                      TO KSUMVL
               MOVE 0                      TO KJSUM2
               MOVE 0                      TO SUM4
               MOVE 0                      TO RPROS2
           END-IF
           IF  (I-L1)
               MOVE 0                      TO VSUM1A
               MOVE 0                      TO SUM5
               MOVE 0                      TO VSUM1B
               MOVE 0                      TO VSUMVL
               MOVE 0                      TO KJSUM1
               MOVE 0                      TO RPROS1
      *************************************************************
      *      RUTINE FOR BEREGNING AV FAKTURABELØP.                *
      *************************************************************
           END-IF
           IF  (I-04)
               ADD ORDRE TO ZERO       GIVING ORD1
               ADD ANT TO ZERO         GIVING ANT1
           END-IF
           IF  (NOT-I-10)
               MULTIPLY ANT BY PRIS    GIVING BEL ROUNDED
           END-IF
           IF  (I-10)
               ADD PRIS TO ZERO        GIVING BEL
           END-IF
           IF  (NOT-I-11)
               MULTIPLY RAB1 BY BEL    GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BEL
           END-IF
           IF  (NOT-I-12)
               MULTIPLY RAB2 BY BEL    GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BEL
           END-IF
           IF  (NOT-I-13)
               MULTIPLY RAB3 BY BEL    GIVING SUM1 ROUNDED
               DIVIDE SUM1 BY 100      GIVING SUM2 ROUNDED
               SUBTRACT SUM2               FROM BEL
      **
           END-IF
           IF  (I-04)
               SET NOT-I-31                TO TRUE
               IF  LKODE = 'PT'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-31)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-04)
               SET NOT-I-22                TO TRUE
               IF  FK = '2'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-22)
               SET NOT-I-32                TO TRUE
               IF  KRETYP = '5'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-22)
               ADD BEL                     TO KJSUM1
               ADD BEL                     TO KJSUM2
               ADD BEL                     TO KJSUM3
           END-IF
           IF  (I-04 AND I-22 AND NOT-I-32)
               SUBTRACT BEL                FROM KJSUM1
               SUBTRACT BEL                FROM KJSUM2
               SUBTRACT BEL                FROM KJSUM3
           END-IF
           IF  (I-22)
               SET NOT-I-22                TO TRUE
               IF  KRETYP = '5'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-22)
               GO TO SLUTT-T
      ****************************************************************
      * *  TOTALT SUM FOR VAREGRUPPE                                 *
      ****************************************************************
           END-IF
           IF  (I-04 AND I-22)
               ADD ANT                     TO VSUM1A
               ADD BEL                     TO VSUM1B
               ADD 1                       TO VSUMVL
      ****************************************************************
      * *  TOTALT SUM FOR KUNDENUMMER                                *
      ****************************************************************
           END-IF
           IF  (I-04 AND I-22)
               ADD ANT                     TO KSUM1A
               ADD BEL                     TO KSUM1B
               ADD 1                       TO KSUMVL
      ****************************************************************
      * *  TOTALT NETTOBELØP FOR FIRMA                               *
      ****************************************************************
           END-IF
           IF  (I-04 AND I-22)
               ADD ANT                     TO SUM1A
               ADD BEL                     TO SUM1B
               ADD 1                       TO SUMVL
      *
           END-IF
           SET I-15                        TO TRUE
           SET I-16                        TO TRUE
           SET I-17                        TO TRUE.
 
       SLUTT-T.
      *1         VSUMVL    COMP 0                        26
      *
      * UTREGNING AV REKLAMASJONSPROSENT (AV TOT.KJØP)
      *
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           SET NOT-I-35                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'STA70'                    TO LONR
           MOVE FNR                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'STA145  '                 TO LPROG
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
               SET NOT-I-26                TO TRUE
               IF  KSUMVL = 0
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-25                TO TRUE
               IF  KJSUM1 = 0
                   SET I-25                TO TRUE
               END-IF
               MULTIPLY 100 BY VSUM1B  GIVING SUM5
           END-IF
           IF  (I-L1 AND NOT-I-25)
               DIVIDE SUM5 BY KJSUM1   GIVING RPROS1
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-23                TO TRUE
               IF  KJSUM2 = 0
                   SET I-23                TO TRUE
               END-IF
               MULTIPLY 100 BY KSUM1B  GIVING SUM4
           END-IF
           IF  (I-L2 AND NOT-I-23)
               DIVIDE SUM4 BY KJSUM2   GIVING RPROS2
      *
           END-IF
           IF  (I-L3)
               SET NOT-I-24                TO TRUE
               IF  KJSUM3 = 0
                   SET I-24                TO TRUE
               END-IF
               MULTIPLY 100 BY SUM1B   GIVING SUM3
           END-IF
           IF  (I-L3 AND NOT-I-24)
               DIVIDE SUM3 BY KJSUM3   GIVING RPROS3
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
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
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (3:1)   TO FAKTOM (1:1)
               MOVE FAKPAR-IO-AREA (74:9)  TO MNDNAV (1:9)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT-FLDOFF SECTION.
       INNPUT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-10                TO TRUE
               SET NOT-I-14                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
           END-EVALUATE.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (51:3)  TO FNR (1:3)
               MOVE INNPUT-IO-AREA (54:3)  TO HND (1:3)
               MOVE INNPUT-IO-AREA (71:4)  TO ORDRE-IO
               MOVE INNPUT-IO-AREA (45:6)  TO KNR (1:6)
               MOVE INNPUT-IO-AREA (60:5)  TO VGR (1:5)
               MOVE INNPUT-IO-AREA (68:1)  TO FAKT (1:1)
               MOVE INNPUT-IO-AREA (12:4)  TO ANT-IO
               IF  ANT = ZERO
                   SET I-10                TO TRUE
               END-IF
               MOVE INNPUT-IO-AREA (32:9)  TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               IF  PRIS = ZERO
                   SET I-14                TO TRUE
               END-IF
               MOVE INNPUT-IO-AREA (60:1)  TO AVD (1:1)
               MOVE INNPUT-IO-AREA (16:7)  TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (23:3)  TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               IF  RAB1 = ZERO
                   SET I-11                TO TRUE
               END-IF
               MOVE INNPUT-IO-AREA (26:3)  TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               IF  RAB2 = ZERO
                   SET I-12                TO TRUE
               END-IF
               MOVE INNPUT-IO-AREA (29:3)  TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               IF  RAB3 = ZERO
                   SET I-13                TO TRUE
               END-IF
               MOVE INNPUT-IO-AREA (41:1)  TO FK (1:1)
               MOVE INNPUT-IO-AREA (42:2)  TO MND (1:2)
               MOVE INNPUT-IO-AREA (58:2)  TO A-ELGR (1:2)
               MOVE INNPUT-IO-AREA (66:1)  TO KRETYP (1:1)
               MOVE INNPUT-IO-AREA (83:3)  TO ALFA (1:3)
               MOVE INNPUT-IO-AREA (86:20) TO ARTNR (1:20)
               MOVE INNPUT-IO-AREA (69:2)  TO LKODE (1:2)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-04                        TO TRUE.
 
       INNPUT-CHK-LEVEL SECTION.
       INNPUT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNPUT-LEVEL-04
               MOVE INNPUT-IO-AREA (51:3)  TO INNPUT-04-L3-FNR
               MOVE INNPUT-IO-AREA (45:6)  TO INNPUT-04-L2-KNR
               MOVE INNPUT-IO-AREA (60:5)  TO INNPUT-04-L1-VGR
               IF  INNPUT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNPUT-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNPUT-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNPUT-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNPUT-04-L3          TO THE-PRIOR-L3
               MOVE  INNPUT-04-L2          TO THE-PRIOR-L2
               MOVE  INNPUT-04-L1          TO THE-PRIOR-L1
               SET INNPUT-LEVEL-INIT       TO TRUE
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
           IF  (I-15 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALFA                   TO LISTE-IO-AREA (11:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (17:20)
               MOVE ORDRE                  TO XO-70YN9
               MOVE XO-70YN9               TO LISTE-IO-AREA (40:7)
               MOVE ANT1                   TO XO-52YY9R
               MOVE XO-52YY9R              TO LISTE-IO-AREA (55:10)
               MOVE PRIS                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (68:13)
               IF  (NOT-I-11)
                   MOVE RAB1               TO XO-21YY9R
                   MOVE XO-21YY9R          TO LISTE-IO-AREA (86:5)
               END-IF
               IF  (NOT-I-12)
                   MOVE RAB2               TO XO-21YY9R
                   MOVE XO-21YY9R          TO LISTE-IO-AREA (91:5)
               END-IF
               IF  (NOT-I-13)
                   MOVE RAB3               TO XO-21YY9R
                   MOVE XO-21YY9R          TO LISTE-IO-AREA (96:5)
               END-IF
               MOVE KNR                    TO LISTE-IO-AREA (1:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'REKLAMASJONER PR. KUNDE ' TO LISTE-IO-AREA (32:24)
               MOVE 'HITTIL I ÅR'          TO LISTE-IO-AREA (57:11)
               MOVE 'DATO'                 TO LISTE-IO-AREA (106:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (128:4)
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
               MOVE 'KUNDENR'              TO LISTE-IO-AREA (1:7)
               MOVE 'ALFA  ARTIKKELNUMMER' TO LISTE-IO-AREA (11:20)
               MOVE 'ORDRENR'              TO LISTE-IO-AREA (40:7)
               MOVE 'ANTALL LEV.'          TO LISTE-IO-AREA (54:11)
               MOVE 'ORDREPRIS'            TO LISTE-IO-AREA (72:9)
               MOVE 'RABATTER'             TO LISTE-IO-AREA (88:8)
               MOVE 'KJØP'                 TO LISTE-IO-AREA (115:4)
               MOVE 'REKL.%'               TO LISTE-IO-AREA (126:6)
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
               MOVE 'REKLAMASJONER PR. KUNDE ' TO LISTE-IO-AREA (32:24)
               MOVE 'HITTIL I ÅR'          TO LISTE-IO-AREA (57:11)
               MOVE 'DATO'                 TO LISTE-IO-AREA (106:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (128:4)
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
               MOVE 'KUNDENR'              TO LISTE-IO-AREA (1:7)
               MOVE 'ALFA  ARTIKKELNUMMER' TO LISTE-IO-AREA (11:20)
               MOVE 'ORDRENR'              TO LISTE-IO-AREA (40:7)
               MOVE 'ANTALL LEV.'          TO LISTE-IO-AREA (54:11)
               MOVE 'ORDREPRIS'            TO LISTE-IO-AREA (72:9)
               MOVE 'RABATTER'             TO LISTE-IO-AREA (88:8)
               MOVE 'KJØP'                 TO LISTE-IO-AREA (115:4)
               MOVE 'REKL.%'               TO LISTE-IO-AREA (126:6)
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
           IF  (I-L1 AND I-16 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT.VARELINJER='   TO LISTE-IO-AREA (7:18)
               MOVE VSUMVL                 TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (26:5)
               INITIALIZE VSUMVL
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VAREGRUPPE SUMMER='   TO LISTE-IO-AREA (19:18)
               MOVE VGR                    TO LISTE-IO-AREA (48:5)
               MOVE VSUM1B                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (66:15)
               INITIALIZE VSUM1B
               MOVE KJSUM1                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (104:15)
               MOVE RPROS1                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (126:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-17 AND NOT-I-86)
           AND (NOT-I-26)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** ANT.VARELINJER='   TO LISTE-IO-AREA (7:18)
               MOVE KSUMVL                 TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (26:5)
               INITIALIZE KSUMVL
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDENS NETTOBELØP='  TO LISTE-IO-AREA (17:19)
               MOVE KSUM1B                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (66:15)
               INITIALIZE KSUM1B
               MOVE KJSUM2                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (104:15)
               MOVE RPROS2                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (126:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL VARELINJER='   TO LISTE-IO-AREA (7:18)
               MOVE SUMVL                  TO XO-50YN9
               MOVE XO-50YN9               TO LISTE-IO-AREA (26:5)
               INITIALIZE SUMVL
               MOVE 'TOTALT NETTOBELØP='   TO LISTE-IO-AREA (37:18)
               MOVE SUM1B                  TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (55:15)
               INITIALIZE SUM1B
               MOVE KJSUM3                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (104:15)
               MOVE RPROS3                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (126:6)
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
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET INNPUT-LEVEL-INIT           TO TRUE
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           OPEN INPUT INNPUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FAKPAR
           CLOSE INNPUT
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
