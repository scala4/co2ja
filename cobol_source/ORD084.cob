       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD084R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring excel på Report Web *****************
      * PROGRAM ORD084 ,UTLISTING AV LAGEROVERFØRINGER      *
      * PR. FIRMA, PR. ALFA ARTIKKELNR ELLER KUN TOTALER.   *
      * LESER SELEKTERTE LAGER.OVERF. RECORDS               *
      * UPSI 1 PRINTER IKKE ALFAKODE SUMMER.                *
      *                                                     *
      * PROGR.  ESPEN LARSEN                                *
      * DATO    29.03.01                                    *
      *******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD084.rpg
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
           SELECT LAGOVF
               ASSIGN TO UT-S-LAGOVF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LAGOVF-STATUS.
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
       FD LAGOVF
               BLOCK CONTAINS 240
               RECORD CONTAINS 120.
       01  LAGOVF-IO-AREA.
           05  LAGOVF-IO-AREA-X            PICTURE X(120).
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
           10  LAGOVF-STATUS               PICTURE 99 VALUE 0.
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
               88  LAGOVF-EOF-OFF          VALUE '0'.
               88  LAGOVF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGOVF-READ-OFF         VALUE '0'.
               88  LAGOVF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGOVF-PROCESS-OFF      VALUE '0'.
               88  LAGOVF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  LAGOVF-LEVEL-INIT-OFF   VALUE '0'.
               88  LAGOVF-LEVEL-INIT       VALUE '1'.
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
           05  LAGOVF-LEVEL-01.
               10  LAGOVF-01-L3.
                   15  LAGOVF-01-L3-FIRMA  PICTURE X(3).
               10  LAGOVF-01-L2.
                   15  LAGOVF-01-L2-LAGER  PICTURE X(2).
               10  LAGOVF-01-L1.
                   15  LAGOVF-01-L1-ALFA   PICTURE X(3).
           05  LAGOVF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  LAGER                   PICTURE X(2).
               10  FRATIL                  PICTURE X(1).
               10  RECART                  PICTURE X(1).
               10  FFNR                    PICTURE X(3).
               10  TFNR                    PICTURE X(3).
               10  KUNDNR                  PICTURE X(6).
               10  KUNFRA                  PICTURE X(2).
               10  KUNTIL                  PICTURE X(2).
               10  ORDNR                   PICTURE X(6).
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(6).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  OM                      PICTURE X(2).
               10  AVD                     PICTURE X(1).
               10  VERDI-IO.
                   15  VERDI               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTFL1-IO.
                   15  ANTFL1              PICTURE S9(6)V9(2).
               10  ANTTL1-IO.
                   15  ANTTL1              PICTURE S9(6)V9(2).
               10  BELFL1-IO.
                   15  BELFL1              PICTURE S9(8)V9(2).
               10  BELTL1-IO.
                   15  BELTL1              PICTURE S9(8)V9(2).
               10  ANTFL2-IO.
                   15  ANTFL2              PICTURE S9(6)V9(2).
               10  ANTFL3-IO.
                   15  ANTFL3              PICTURE S9(6)V9(2).
               10  ANTTL2-IO.
                   15  ANTTL2              PICTURE S9(6)V9(2).
               10  ANTTL3-IO.
                   15  ANTTL3              PICTURE S9(6)V9(2).
               10  BELFL2-IO.
                   15  BELFL2              PICTURE S9(8)V9(2).
               10  BELFL3-IO.
                   15  BELFL3              PICTURE S9(8)V9(2).
               10  BELTL2-IO.
                   15  BELTL2              PICTURE S9(8)V9(2).
               10  BELTL3-IO.
                   15  BELTL3              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-52YY9R               PICTURE ZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-62YY9R               PICTURE ZZZ.ZZZ,99-.
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
 
           IF  LAGOVF-PROCESS
               SET LAGOVF-PROCESS-OFF      TO TRUE
               SET LAGOVF-READ             TO TRUE
           END-IF
 
           IF  LAGOVF-READ
           AND RECORD-SELECTED-OFF
               PERFORM LAGOVF-GET
               SET LAGOVF-READ-OFF         TO TRUE
               IF  NOT LAGOVF-EOF
                   SET LAGOVF-PROCESS      TO TRUE
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
 
           IF  LAGOVF-PROCESS
               PERFORM LAGOVF-IDSET
           END-IF
 
           IF  LAGOVF-PROCESS
               PERFORM LAGOVF-CHK-LEVEL
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
           PERFORM DETAIL-OVERFLOW
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  LAGOVF-PROCESS
               PERFORM LAGOVF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  LAGOVF-PROCESS
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
           IF  (NOT-I-01)
               GO TO SLUTT-T
      *                    SETOF                         80
      *****************************************************************
      * NULLSTILLING.                                                 *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SUBTRACT ANTFL1             FROM ANTFL1
               SUBTRACT ANTTL1             FROM ANTTL1
               SUBTRACT BELFL1             FROM BELFL1
               SUBTRACT BELTL1             FROM BELTL1
      *****************************************************************
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  FRATIL = 'F'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  FRATIL = 'T'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  RECART = 'L'
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  RECART = 'V'
               SET I-22                    TO TRUE
           END-IF
      *****************************************************************
      * SUMMERING AV ANTALL FRA-LAGER OG TIL-LAGER.                   *
      *****************************************************************
           IF  (I-11)
               ADD ANTLEV                  TO ANTFL1
               ADD ANTLEV                  TO ANTFL2
               ADD ANTLEV                  TO ANTFL3
      *
           END-IF
           IF  (I-12)
               ADD ANTLEV                  TO ANTTL1
               ADD ANTLEV                  TO ANTTL2
               ADD ANTLEV                  TO ANTTL3
      *
      *****************************************************************
      * SUMMERING AV VERDI  FRA-LAGER OG TIL-LAGER.                   *
      *****************************************************************
           END-IF
           IF  (I-11)
               ADD VERDI                   TO BELFL1
               ADD VERDI                   TO BELFL2
               ADD VERDI                   TO BELFL3
      *
           END-IF
           IF  (I-12)
               ADD VERDI                   TO BELTL1
               ADD VERDI                   TO BELTL2
               ADD VERDI                   TO BELTL3
      *
           END-IF
           .
 
       SLUTT-T.
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
           MOVE 'ORD42'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD084  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      *****************************************************************
 
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
 
       LAGOVF-GET SECTION.
       LAGOVF-GET-P.
           IF  LAGOVF-EOF-OFF
               READ LAGOVF
               AT END
                   SET LAGOVF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LAGOVF-FLDSET SECTION.
       LAGOVF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LAGOVF-IO-AREA (106:3) TO FIRMA (1:3)
               MOVE LAGOVF-IO-AREA (109:2) TO LAGER (1:2)
               MOVE LAGOVF-IO-AREA (120:1) TO FRATIL (1:1)
               MOVE LAGOVF-IO-AREA (1:1)   TO RECART (1:1)
               MOVE LAGOVF-IO-AREA (2:3)   TO FFNR (1:3)
               MOVE LAGOVF-IO-AREA (5:3)   TO TFNR (1:3)
               MOVE LAGOVF-IO-AREA (8:6)   TO KUNDNR (1:6)
               MOVE LAGOVF-IO-AREA (10:2)  TO KUNFRA (1:2)
               MOVE LAGOVF-IO-AREA (12:2)  TO KUNTIL (1:2)
               MOVE LAGOVF-IO-AREA (14:6)  TO ORDNR (1:6)
               MOVE LAGOVF-IO-AREA (20:6)  TO ODATO-IO
               INSPECT ODATO-IO REPLACING ALL ' ' BY '0'
               MOVE LAGOVF-IO-AREA (26:4)  TO ANTLEV-IO
               MOVE LAGOVF-IO-AREA (54:3)  TO ALFA (1:3)
               MOVE LAGOVF-IO-AREA (57:20) TO ARTNR (1:20)
               MOVE LAGOVF-IO-AREA (93:2)  TO OM (1:2)
               MOVE LAGOVF-IO-AREA (95:1)  TO AVD (1:1)
               MOVE LAGOVF-IO-AREA (101:5) TO VERDI-IO
           END-EVALUATE.
 
       LAGOVF-IDSET SECTION.
       LAGOVF-IDSET-P.
           SET I-01                        TO TRUE.
 
       LAGOVF-CHK-LEVEL SECTION.
       LAGOVF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO LAGOVF-LEVEL-01
               MOVE LAGOVF-IO-AREA (106:3) TO LAGOVF-01-L3-FIRMA
               MOVE LAGOVF-IO-AREA (109:2) TO LAGOVF-01-L2-LAGER
               MOVE LAGOVF-IO-AREA (54:3)  TO LAGOVF-01-L1-ALFA
               IF  LAGOVF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  LAGOVF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  LAGOVF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  LAGOVF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  LAGOVF-01-L3          TO THE-PRIOR-L3
               MOVE  LAGOVF-01-L2          TO THE-PRIOR-L2
               MOVE  LAGOVF-01-L1          TO THE-PRIOR-L1
               SET LAGOVF-LEVEL-INIT       TO TRUE
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
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LK'                   TO LISTE-IO-AREA (2:2)
               MOVE 'ALF'                  TO LISTE-IO-AREA (5:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (9:14)
               MOVE 'ORD.NR'               TO LISTE-IO-AREA (30:6)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (37:8)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (46:6)
               MOVE 'OM'                   TO LISTE-IO-AREA (53:2)
               MOVE 'AVD'                  TO LISTE-IO-AREA (56:3)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (63:6)
               MOVE 'VERDI'                TO LISTE-IO-AREA (79:5)
               MOVE 'MERKNADER'            TO LISTE-IO-AREA (88:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LAGER                  TO LISTE-IO-AREA (2:2)
               MOVE ALFA                   TO LISTE-IO-AREA (5:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (9:20)
               MOVE ORDNR                  TO LISTE-IO-AREA (30:6)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (37:8)
               MOVE KUNDNR                 TO LISTE-IO-AREA (46:6)
               MOVE OM                     TO LISTE-IO-AREA (53:2)
               MOVE AVD                    TO LISTE-IO-AREA (57:1)
               MOVE ANTLEV                 TO XO-52YY9R
               MOVE XO-52YY9R              TO LISTE-IO-AREA (60:10)
               MOVE VERDI                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (72:13)
               IF  (I-11)
                   MOVE 'OVERF. FRA LAGER' TO LISTE-IO-AREA (88:16)
               END-IF
               IF  (I-21 AND I-11)
                   MOVE KUNFRA             TO LISTE-IO-AREA (106:2)
               END-IF
               IF  (I-22 AND I-11)
                   MOVE FFNR               TO LISTE-IO-AREA (105:3)
               END-IF
               IF  (I-11)
                   MOVE 'TIL'              TO LISTE-IO-AREA (109:3)
               END-IF
               IF  (I-21 AND I-11)
                   MOVE KUNTIL             TO LISTE-IO-AREA (114:2)
               END-IF
               IF  (I-22 AND I-11)
                   MOVE TFNR               TO LISTE-IO-AREA (113:3)
               END-IF
               IF  (I-12)
                   MOVE 'OVERF. TIL LAGER' TO LISTE-IO-AREA (88:16)
               END-IF
               IF  (I-21 AND I-12)
                   MOVE KUNTIL             TO LISTE-IO-AREA (106:2)
               END-IF
               IF  (I-22 AND I-12)
                   MOVE TFNR               TO LISTE-IO-AREA (105:3)
               END-IF
               IF  (I-12)
                   MOVE 'FRA'              TO LISTE-IO-AREA (109:3)
               END-IF
               IF  (I-21 AND I-12)
                   MOVE KUNFRA             TO LISTE-IO-AREA (114:2)
               END-IF
               IF  (I-22 AND I-12)
                   MOVE FFNR               TO LISTE-IO-AREA (113:3)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86 AND NOT-I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LK'                   TO LISTE-IO-AREA (2:2)
               MOVE 'ALF'                  TO LISTE-IO-AREA (5:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (9:14)
               MOVE 'ORD.NR'               TO LISTE-IO-AREA (30:6)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (37:8)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (46:6)
               MOVE 'OM'                   TO LISTE-IO-AREA (53:2)
               MOVE 'AVD'                  TO LISTE-IO-AREA (56:3)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (63:6)
               MOVE 'VERDI'                TO LISTE-IO-AREA (79:5)
               MOVE 'MERKNADER'            TO LISTE-IO-AREA (88:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (6:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (54:35)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (92:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (123:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86 AND NOT-I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (6:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (54:35)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (92:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (123:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86 AND NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LAGER                  TO LISTE-IO-AREA (2:2)
               MOVE ALFA                   TO LISTE-IO-AREA (5:3)
               MOVE '*   SUM ALFA-KODE'    TO LISTE-IO-AREA (9:17)
               MOVE ANTTL1                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (59:11)
               MOVE BELTL1                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (71:14)
               MOVE 'SUM TIL LAGER *  '    TO LISTE-IO-AREA (88:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LAGER                  TO LISTE-IO-AREA (2:2)
               MOVE ALFA                   TO LISTE-IO-AREA (5:3)
               MOVE '*   SUM ALFA-KODE'    TO LISTE-IO-AREA (9:17)
               MOVE ANTFL1                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (59:11)
               MOVE BELFL1                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (71:14)
               MOVE 'SUM FRA LAGER *  '    TO LISTE-IO-AREA (88:17)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LAGER                  TO LISTE-IO-AREA (2:2)
               MOVE '**  SUM LAGERKODE'    TO LISTE-IO-AREA (9:17)
               MOVE ANTTL2                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (59:11)
               INITIALIZE ANTTL2
               MOVE BELTL2                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (71:14)
               INITIALIZE BELTL2
               MOVE 'SUM TIL LAGER ** '    TO LISTE-IO-AREA (88:17)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LAGER                  TO LISTE-IO-AREA (2:2)
               MOVE '**  SUM LAGERKODE'    TO LISTE-IO-AREA (9:17)
               MOVE ANTFL2                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (59:11)
               INITIALIZE ANTFL2
               MOVE BELFL2                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (71:14)
               INITIALIZE BELFL2
               MOVE 'SUM FRA LAGER ** '    TO LISTE-IO-AREA (88:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** FIRMA TOTAL  '    TO LISTE-IO-AREA (9:17)
               MOVE ANTTL3                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (59:11)
               INITIALIZE ANTTL3
               MOVE BELTL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (71:14)
               INITIALIZE BELTL3
               MOVE 'SUM TIL LAGER ***'    TO LISTE-IO-AREA (88:17)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** FIRMA TOTAL  '    TO LISTE-IO-AREA (9:17)
               MOVE ANTFL3                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (59:11)
               INITIALIZE ANTFL3
               MOVE BELFL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (71:14)
               INITIALIZE BELFL3
               MOVE 'SUM FRA LAGER ***'    TO LISTE-IO-AREA (88:17)
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET LAGOVF-LEVEL-INIT           TO TRUE
           INITIALIZE LAGOVF-DATA-FIELDS
           SET LAGOVF-EOF-OFF              TO TRUE
           SET LAGOVF-PROCESS              TO TRUE
           OPEN INPUT LAGOVF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE LAGOVF
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
