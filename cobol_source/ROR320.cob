       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR320R.
      **********************************************  Z-WIN-RPG2   ****
      *   REST OG FORHÅNDSORDRE PR. VGR PR. AVD.                      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR320.rpg
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
           SELECT REOINF
               ASSIGN TO UT-S-REOINF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REOINF-STATUS.
           SELECT FIRMAK
               ASSIGN TO FIRMAK
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAK-STATUS
               RECORD KEY IS FIRMAK-KEY1.
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
       DATA DIVISION.
       FILE SECTION.
       FD REOINF
               BLOCK CONTAINS 100
               RECORD CONTAINS 25.
       01  REOINF-IO-AREA.
           05  REOINF-IO-AREA-X            PICTURE X(25).
       FD FIRMAK
               RECORD CONTAINS 1000.
       01  FIRMAK-IO-AREA.
           05  FIRMAK-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAK-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
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
           10  REOINF-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAK-STATUS               PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  REOINF-EOF-OFF          VALUE '0'.
               88  REOINF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REOINF-READ-OFF         VALUE '0'.
               88  REOINF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REOINF-PROCESS-OFF      VALUE '0'.
               88  REOINF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  REOINF-LEVEL-INIT-OFF   VALUE '0'.
               88  REOINF-LEVEL-INIT       VALUE '1'.
           05  FIRMAK-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  REOINF-LEVEL-01.
               10  REOINF-01-L3.
                   15  REOINF-01-L3-FIRM   PICTURE X(3).
               10  REOINF-01-L2.
                   15  REOINF-01-L2-AVD    PICTURE X(1).
               10  REOINF-01-L1.
                   15  REOINF-01-L1-VGR    PICTURE X(5).
           05  REOINF-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  AVD                     PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5)V9(2) USAGE
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
           05  FIRMAK-DATA-FIELDS.
               10  AVDN1                   PICTURE X(10).
               10  AVDN2                   PICTURE X(10).
               10  AVDN3                   PICTURE X(10).
               10  AVDN4                   PICTURE X(10).
               10  AVDN5                   PICTURE X(10).
               10  AVDN6                   PICTURE X(10).
               10  AVDN7                   PICTURE X(10).
               10  AVDN8                   PICTURE X(10).
               10  AVDN9                   PICTURE X(10).
           05  VAGRMAS-DATA-FIELDS.
               10  VGNAVN                  PICTURE X(40).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  FNRVGR                  PICTURE X(8).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(8)V9(2).
               10  RAB-IO.
                   15  RAB                 PICTURE S9(2)V9(1).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(10)V9(2).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(8)V9(2).
               10  SUML1-IO.
                   15  SUML1               PICTURE S9(8)V9(2).
               10  SUML2-IO.
                   15  SUML2               PICTURE S9(8)V9(2).
               10  SUML3-IO.
                   15  SUML3               PICTURE S9(8)V9(2).
               10  AVDNAV                  PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-82YY9                PICTURE ZZ.ZZZ.ZZZ,99.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  REOINF-PROCESS
               SET REOINF-PROCESS-OFF      TO TRUE
               SET REOINF-READ             TO TRUE
           END-IF
 
           IF  REOINF-READ
           AND RECORD-SELECTED-OFF
               PERFORM REOINF-GET
               SET REOINF-READ-OFF         TO TRUE
               IF  NOT REOINF-EOF
                   PERFORM REOINF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET REOINF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REOINF-PROCESS
               PERFORM REOINF-IDSET
           END-IF
 
           IF  REOINF-PROCESS
               PERFORM REOINF-CHK-LEVEL
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
 
           IF  REOINF-PROCESS
               PERFORM REOINF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  REOINF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           IF  (I-L3)
               MOVE FIRM                   TO FIRMAK-KEY1
               READ FIRMAK RECORD KEY IS FIRMAK-KEY1
               INVALID KEY
                   SET I-08                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-08            TO TRUE
                   PERFORM FIRMAK-FLDSET
                   PERFORM FIRMAK-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-08)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               PERFORM AVDRUT-S
           END-IF
           IF  (I-L1)
               MOVE FIRM                   TO FNRVGR (1:3)
               MOVE VGR                    TO FNRVGR (4:5)
               MOVE FNRVGR                 TO VAGRMAS-KEY1
               READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM VAGRMAS-FLDSET
                   PERFORM VAGRMAS-IDSET
               END-READ
      *
      * NETTOPRISUTREGNING.  PRIS X ANTALL - RAB.
           END-IF
           MULTIPLY ANT BY BEL         GIVING SUM-X
           ADD RAB1 TO ZERO            GIVING RAB.
 
       STEP1-T.
           MULTIPLY RAB BY SUM-X       GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING SUM2 ROUNDED
           SUBTRACT SUM2                   FROM SUM-X
           IF  (NOT-I-22)
               GO TO STEP2-T
           END-IF
           IF  (NOT-I-23)
               GO TO STEP3-T
           END-IF
           GO TO STEP4-T.
 
       STEP2-T.
           SET I-22                        TO TRUE
           ADD RAB2 TO ZERO            GIVING RAB
           GO TO STEP1-T.
 
       STEP3-T.
           SET I-23                        TO TRUE
           ADD RAB3 TO ZERO            GIVING RAB
           GO TO STEP1-T.
 
       STEP4-T.
      * TOTALSUMMERINGSRUTINE.
           ADD SUM-X                       TO SUML1
           ADD SUM-X                       TO SUML2
           ADD SUM-X                       TO SUML3.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'ROR04'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ROR320  '                 TO LPROG
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
 
       REOINF-GET SECTION.
       REOINF-GET-P.
           IF  REOINF-EOF-OFF
               READ REOINF
               AT END
                   SET REOINF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REOINF-FLDSET SECTION.
       REOINF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( REOINF-IO-AREA (1:1) NOT = ' ' )
               MOVE REOINF-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE REOINF-IO-AREA (21:5)  TO VGR (1:5)
               MOVE REOINF-IO-AREA (1:1)   TO AVD (1:1)
               MOVE REOINF-IO-AREA (5:4)   TO ANT-IO
               MOVE REOINF-IO-AREA (9:5)   TO BEL-IO
               MOVE REOINF-IO-AREA (14:2)  TO RAB1-IO
               MOVE REOINF-IO-AREA (16:2)  TO RAB2-IO
               MOVE REOINF-IO-AREA (18:2)  TO RAB3-IO
           END-EVALUATE.
 
       REOINF-IDCHK SECTION.
       REOINF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( REOINF-IO-AREA (1:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       REOINF-IDSET SECTION.
       REOINF-IDSET-P.
           EVALUATE TRUE
           WHEN ( REOINF-IO-AREA (1:1) NOT = ' ' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       REOINF-CHK-LEVEL SECTION.
       REOINF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( REOINF-IO-AREA (1:1) NOT = ' ' )
               MOVE LOW-VALUES             TO REOINF-LEVEL-01
               MOVE REOINF-IO-AREA (2:3)   TO REOINF-01-L3-FIRM
               MOVE REOINF-IO-AREA (1:1)   TO REOINF-01-L2-AVD
               MOVE REOINF-IO-AREA (21:5)  TO REOINF-01-L1-VGR
               IF  REOINF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  REOINF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  REOINF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  REOINF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  REOINF-01-L3          TO THE-PRIOR-L3
               MOVE  REOINF-01-L2          TO THE-PRIOR-L2
               MOVE  REOINF-01-L1          TO THE-PRIOR-L1
               SET REOINF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAK-FLDSET SECTION.
       FIRMAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAK-IO-AREA (405:10) TO AVDN1 (1:10)
               MOVE FIRMAK-IO-AREA (416:10) TO AVDN2 (1:10)
               MOVE FIRMAK-IO-AREA (427:10) TO AVDN3 (1:10)
               MOVE FIRMAK-IO-AREA (438:10) TO AVDN4 (1:10)
               MOVE FIRMAK-IO-AREA (449:10) TO AVDN5 (1:10)
               MOVE FIRMAK-IO-AREA (460:10) TO AVDN6 (1:10)
               MOVE FIRMAK-IO-AREA (471:10) TO AVDN7 (1:10)
               MOVE FIRMAK-IO-AREA (482:10) TO AVDN8 (1:10)
               MOVE FIRMAK-IO-AREA (493:10) TO AVDN9 (1:10)
           END-EVALUATE.
 
       FIRMAK-IDSET SECTION.
       FIRMAK-IDSET-P.
           SET I-06                        TO TRUE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (11:40) TO VGNAVN (1:40)
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
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
               MOVE 'REST OG FORHÅNDSORDRE ' TO LISTE-IO-AREA (36:22)
               MOVE 'PR. AVD. FOR '        TO LISTE-IO-AREA (58:13)
               MOVE AVDNAV                 TO LISTE-IO-AREA (72:10)
               MOVE 'PR.'                  TO LISTE-IO-AREA (84:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (87:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (103:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (107:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VGR.  VAREGRUPPENAVN' TO LISTE-IO-AREA (10:20)
               MOVE 'BELØP'                TO LISTE-IO-AREA (65:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (10:24)
               MOVE '------------------------' TO LISTE-IO-AREA (34:24)
               MOVE '------------------------' TO LISTE-IO-AREA (58:24)
               MOVE '------------------------' TO LISTE-IO-AREA (82:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'REST OG FORHÅNDSORDRE ' TO LISTE-IO-AREA (36:22)
               MOVE 'PR. AVD. FOR '        TO LISTE-IO-AREA (58:13)
               MOVE AVDNAV                 TO LISTE-IO-AREA (72:10)
               MOVE 'PR.'                  TO LISTE-IO-AREA (84:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (87:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (103:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (107:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VGR.  VAREGRUPPENAVN' TO LISTE-IO-AREA (10:20)
               MOVE 'BELØP'                TO LISTE-IO-AREA (65:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (10:24)
               MOVE '------------------------' TO LISTE-IO-AREA (34:24)
               MOVE '------------------------' TO LISTE-IO-AREA (58:24)
               MOVE '------------------------' TO LISTE-IO-AREA (82:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VGR                    TO LISTE-IO-AREA (10:5)
               IF  (NOT-I-12)
                   MOVE VGNAVN             TO LISTE-IO-AREA (16:40)
               END-IF
               MOVE SUML1                  TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (57:13)
               INITIALIZE SUML1
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT'               TO LISTE-IO-AREA (16:6)
               MOVE AVDNAV                 TO LISTE-IO-AREA (46:10)
               MOVE SUML2                  TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (57:13)
               INITIALIZE SUML2
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMATOTALT'          TO LISTE-IO-AREA (16:11)
               MOVE SUML3                  TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (57:13)
               INITIALIZE SUML3
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           SET REOINF-LEVEL-INIT           TO TRUE
           INITIALIZE REOINF-DATA-FIELDS
           SET REOINF-EOF-OFF              TO TRUE
           SET REOINF-PROCESS              TO TRUE
           OPEN INPUT REOINF
           INITIALIZE FIRMAK-DATA-FIELDS
           OPEN INPUT FIRMAK
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REOINF
           CLOSE FIRMAK
           CLOSE VAGRMAS
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
