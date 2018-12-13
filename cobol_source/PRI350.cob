       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI350R.
      **********************************************  Z-WIN-RPG2   ****
      *  UTSKRIFT AV PRISENDRINGER P� PRISFILE.    ****
      *************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI350.rpg
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
           SELECT TABELL
               ASSIGN TO UT-S-TABELL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABELL-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT PRISF
               ASSIGN TO UT-S-PRISF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRISF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TABELL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABELL-IO-AREA.
           05  TABELL-IO-AREA-X            PICTURE X(80).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD PRISF
               BLOCK CONTAINS 4080
               RECORD CONTAINS 85.
       01  PRISF-IO-AREA.
           05  PRISF-IO-AREA-X             PICTURE X(85).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABFIR-MAX   VALUE 10           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
           05  TABFIR-TABLE.
               10  TABFIR-ENTRY
                                           OCCURS 10 TIMES
                                           INDEXED BY TABFIR-I
                                                      TABFIR-S.
                   15  TABFIR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TABELL-STATUS               PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  PRISF-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TABELL-EOF-OFF          VALUE '0'.
               88  TABELL-EOF              VALUE '1'.
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
               88  PRISF-EOF-OFF           VALUE '0'.
               88  PRISF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISF-READ-OFF          VALUE '0'.
               88  PRISF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISF-PROCESS-OFF       VALUE '0'.
               88  PRISF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PRISF-LEVEL-INIT-OFF    VALUE '0'.
               88  PRISF-LEVEL-INIT        VALUE '1'.
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
           05  PRISF-LEVEL-01.
               10  PRISF-01-L3.
                   15  PRISF-01-L3-FIRMA   PICTURE X(3).
               10  PRISF-01-L2.
                   15  PRISF-01-L2-REFNR   PICTURE X(5).
               10  PRISF-01-L1.
                   15  PRISF-01-L1-VGRALF  PICTURE X(8).
           05  PRISF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  AVD                     PICTURE X(1).
               10  REFNR                   PICTURE X(5).
               10  VGR                     PICTURE X(5).
               10  VGRALF                  PICTURE X(8).
               10  ALF                     PICTURE X(3).
               10  REGDAT-IO.
                   15  REGDAT              PICTURE S9(6).
               10  KORDAT-IO.
                   15  KORDAT              PICTURE S9(6).
               10  TYP                     PICTURE X(1).
               10  REF                     PICTURE X(2).
               10  FAKSEL-IO.
                   15  FAKSEL              PICTURE S9(5)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  FAKUTS-IO.
                   15  FAKUTS              PICTURE S9(5)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  UPDAT                   PICTURE X(1).
      *RISF                                FIRMA S3A
      *                                    REFNR S2A
      *                                    VGRALFS1A
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(5).
               10  THE-PRIOR-L1            PICTURE X(8).
           05  TEMPORARY-FIELDS.
               10  TOT-IO.
                   15  TOT                 PICTURE S9(6).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  TOT3-IO.
                   15  TOT3                PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-54YYZ                PICTURE ZZ.ZZZ,ZZZZ.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
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
 
           IF  PRISF-PROCESS
               SET PRISF-PROCESS-OFF       TO TRUE
               SET PRISF-READ              TO TRUE
           END-IF
 
           IF  PRISF-READ
           AND RECORD-SELECTED-OFF
               PERFORM PRISF-GET
               SET PRISF-READ-OFF          TO TRUE
               IF  NOT PRISF-EOF
                   SET PRISF-PROCESS       TO TRUE
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
 
           IF  PRISF-PROCESS
               PERFORM PRISF-IDSET
           END-IF
 
           IF  PRISF-PROCESS
               PERFORM PRISF-CHK-LEVEL
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
 
           IF  PRISF-PROCESS
               PERFORM PRISF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  PRISF-PROCESS
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
      *                    SETOF                     30
           END-IF
           IF  (I-U6)
               GO TO ALLE-T
           END-IF
           IF  (I-L3)
               SET NOT-I-35                TO TRUE
               SET NOT-I-99                TO TRUE
               SET TABFIR-S                TO TABFIR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFIR-I FROM 1 BY 1
                         UNTIL TABFIR-I >= TABFIR-MAX
                            OR I-99
                   IF  FIRMA = TABFIR (TABFIR-I)
                       SET I-99            TO TRUE
                       SET TABFIR-S        TO TABFIR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (NOT-I-99)
               GO TO END-X-T
           END-IF
           IF  (I-L3)
               SET I-35                    TO TRUE
           END-IF.
 
       ALLE-T.
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-01)
               ADD 1                       TO TOT
               ADD 1                       TO ANT
               ADD 1                       TO TOT3
               SET I-30                    TO TRUE
           END-IF.
 
       END-X-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBSH01 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'PRI66'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'PRI350  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               SET I-35                    TO TRUE
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
 
       PRISF-GET SECTION.
       PRISF-GET-P.
           IF  PRISF-EOF-OFF
               READ PRISF
               AT END
                   SET PRISF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PRISF-FLDSET SECTION.
       PRISF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRISF-IO-AREA (3:3)    TO FIRMA (1:3)
               MOVE PRISF-IO-AREA (13:1)   TO AVD (1:1)
               MOVE PRISF-IO-AREA (14:5)   TO REFNR (1:5)
               MOVE PRISF-IO-AREA (19:5)   TO VGR (1:5)
               MOVE PRISF-IO-AREA (19:8)   TO VGRALF (1:8)
               MOVE PRISF-IO-AREA (24:3)   TO ALF (1:3)
               MOVE PRISF-IO-AREA (27:6)   TO REGDAT-IO
               INSPECT REGDAT-IO REPLACING ALL ' ' BY '0'
               MOVE PRISF-IO-AREA (33:6)   TO KORDAT-IO
               INSPECT KORDAT-IO REPLACING ALL ' ' BY '0'
               MOVE PRISF-IO-AREA (59:1)   TO TYP (1:1)
               MOVE PRISF-IO-AREA (56:2)   TO REF (1:2)
               MOVE PRISF-IO-AREA (60:5)   TO FAKSEL-IO
               MOVE PRISF-IO-AREA (65:5)   TO FAKUTS-IO
               MOVE PRISF-IO-AREA (72:1)   TO UPDAT (1:1)
           END-EVALUATE.
 
       PRISF-IDSET SECTION.
       PRISF-IDSET-P.
           SET I-01                        TO TRUE.
 
       PRISF-CHK-LEVEL SECTION.
       PRISF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PRISF-LEVEL-01
               MOVE PRISF-IO-AREA (3:3)    TO PRISF-01-L3-FIRMA
               MOVE PRISF-IO-AREA (14:5)   TO PRISF-01-L2-REFNR
               MOVE PRISF-IO-AREA (19:8)   TO PRISF-01-L1-VGRALF
               IF  PRISF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PRISF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  PRISF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PRISF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PRISF-01-L3           TO THE-PRIOR-L3
               MOVE  PRISF-01-L2           TO THE-PRIOR-L2
               MOVE  PRISF-01-L1           TO THE-PRIOR-L1
               SET PRISF-LEVEL-INIT        TO TRUE
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
 
       TABELL-LOAD SECTION.
       TABELL-LOAD-P.
           OPEN INPUT TABELL
           SET TABFIR-I                    TO 1
           PERFORM UNTIL TABELL-EOF
               READ TABELL
               AT END
                   SET TABELL-EOF          TO TRUE
               NOT AT END
                   MOVE TABELL-IO-AREA (1:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (4:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (7:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (10:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (13:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (16:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (19:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (22:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (25:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE TABELL-IO-AREA (28:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABELL.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FREMKJ�RT'            TO LISTE-IO-AREA (68:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'REF.NR'               TO LISTE-IO-AREA (2:6)
               MOVE 'VGR.'                 TO LISTE-IO-AREA (10:4)
               MOVE 'ALF'                  TO LISTE-IO-AREA (16:3)
               MOVE 'REG.DATO'             TO LISTE-IO-AREA (21:8)
               MOVE 'KOR.DATO'             TO LISTE-IO-AREA (31:8)
               MOVE 'REF.'                 TO LISTE-IO-AREA (41:4)
               MOVE 'TYP'                  TO LISTE-IO-AREA (46:3)
               MOVE 'FAKT.KOST'            TO LISTE-IO-AREA (52:9)
               MOVE 'FAKT.UTS'             TO LISTE-IO-AREA (65:8)
               MOVE 'UD'                   TO LISTE-IO-AREA (74:2)
               MOVE 'ANTALL ART.'          TO LISTE-IO-AREA (80:11)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FREMKJ�RT'            TO LISTE-IO-AREA (68:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'REF.NR'               TO LISTE-IO-AREA (2:6)
               MOVE 'VGR.'                 TO LISTE-IO-AREA (10:4)
               MOVE 'ALF'                  TO LISTE-IO-AREA (16:3)
               MOVE 'REG.DATO'             TO LISTE-IO-AREA (21:8)
               MOVE 'KOR.DATO'             TO LISTE-IO-AREA (31:8)
               MOVE 'REF.'                 TO LISTE-IO-AREA (41:4)
               MOVE 'TYP'                  TO LISTE-IO-AREA (46:3)
               MOVE 'FAKT.KOST'            TO LISTE-IO-AREA (52:9)
               MOVE 'FAKT.UTS'             TO LISTE-IO-AREA (65:8)
               MOVE 'UD'                   TO LISTE-IO-AREA (74:2)
               MOVE 'ANTALL ART.'          TO LISTE-IO-AREA (80:11)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-30)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REFNR                  TO LISTE-IO-AREA (2:5)
               MOVE VGR                    TO LISTE-IO-AREA (9:5)
               MOVE ALF                    TO LISTE-IO-AREA (16:3)
               MOVE REGDAT                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (21:8)
               MOVE KORDAT                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (31:8)
               MOVE REF                    TO LISTE-IO-AREA (42:2)
               MOVE TYP                    TO LISTE-IO-AREA (47:1)
               MOVE FAKSEL                 TO XO-54YYZ
               MOVE XO-54YYZ               TO LISTE-IO-AREA (50:11)
               MOVE FAKUTS                 TO XO-54YYZ
               MOVE XO-54YYZ               TO LISTE-IO-AREA (62:11)
               MOVE UPDAT                  TO LISTE-IO-AREA (75:1)
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (81:7)
               INITIALIZE ANT
               MOVE '*'                    TO LISTE-IO-AREA (90:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-30)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (81:7)
               INITIALIZE TOT
               MOVE '**'                   TO LISTE-IO-AREA (89:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-30)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOT3                   TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (81:7)
               INITIALIZE TOT3
               MOVE '***'                  TO LISTE-IO-AREA (88:3)
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
           MOVE 2                          TO LR-CHECK
           PERFORM TABELL-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET PRISF-LEVEL-INIT            TO TRUE
           INITIALIZE PRISF-DATA-FIELDS
           SET PRISF-EOF-OFF               TO TRUE
           SET PRISF-PROCESS               TO TRUE
           OPEN INPUT PRISF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABFIR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE PRISF
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