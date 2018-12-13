       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDI062R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: EDI062                                          *
      *  PROGRAMERER: STEIN SANDVOLD                                  *
      *  PROGRAMT...: 22.11.99                                        *
      *  RETTET.....: 22.11.99                                        *
      *                                                               *
      *  DANNE KVITTERINGSLISTE OVER UKENS FRAKTBREV TIL LINJEGODS    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: EDI062.rpg
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
           SELECT FRAKTED
               ASSIGN TO FRAKTED
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FRAKTED-STATUS
               RECORD KEY IS FRAKTED-KEY1.
           SELECT FRAKTED-TMP
               ASSIGN TO UT-S-FRAKTED-TMP
               ACCESS MODE  IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRAKTED-STATUS.
           SELECT SORT-WK1
               ASSIGN TO UT-S-SORT-WK1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
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
       FD FRAKTED
               RECORD CONTAINS 2000.
       01  FRAKTED-IO-AREA.
           05  FRAKTED-IO-AREA-X.
               10  FRAKTED-KEY1.
                   15  FRAKTED-KEY1N       PICTURE S9(17).
               10  FILLER                  PICTURE X(1983).
       SD SORT-WK1
           DATA RECORD IS SORT-REC1.
       01  SORT-REC1.
           05  FILLER                  PIC X(3).
           05  SORT-WK1-K5             PIC X(3).
           05  FILLER                  PIC X(395).
           05  SORT-WK1-K4             PIC X(2).
           05  FILLER                  PIC X(656).
           05  SORT-WK1-K3             PIC X(1).
           05  SORT-WK1-K2             PIC X(1).
           05  SORT-WK1-K1             PIC X(1).
           05  FILLER                  PIC X(941).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
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
           10  FRAKTED-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
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
           05  FRAKTED-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTED-EOF-OFF         VALUE '0'.
               88  FRAKTED-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTED-READ-OFF        VALUE '0'.
               88  FRAKTED-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTED-PROCESS-OFF     VALUE '0'.
               88  FRAKTED-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FRAKTED-LEVEL-INIT-OFF  VALUE '0'.
               88  FRAKTED-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
               10  PFNR                    PICTURE X(3).
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           05  FRAKTED-LEVEL-01.
               10  FRAKTED-01-L1.
                   15  FRAKTED-01-L1-FNR   PICTURE X(3).
           05  FRAKTED-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  FBNR                    PICTURE X(6).
               10  MKNR                    PICTURE X(10).
               10  MNAVN                   PICTURE X(30).
               10  MPNR                    PICTURE X(4).
               10  KUNDE                   PICTURE X(6).
               10  SHENT                   PICTURE X(1).
               10  SFRAKT                  PICTURE X(1).
               10  SUTKJ                   PICTURE X(1).
               10  SINTET                  PICTURE X(1).
               10  ORD1                    PICTURE X(6).
               10  ORD2                    PICTURE X(6).
               10  ORD3                    PICTURE X(6).
               10  ORD4                    PICTURE X(6).
               10  ORD5                    PICTURE X(6).
               10  ORD6                    PICTURE X(6).
               10  ORD7                    PICTURE X(6).
               10  ORD8                    PICTURE X(6).
               10  ORD9                    PICTURE X(6).
               10  ORD10                   PICTURE X(6).
               10  ORD11                   PICTURE X(6).
               10  ORD12                   PICTURE X(6).
               10  ORD13                   PICTURE X(6).
               10  AKOL1-IO.
                   15  AKOL1               PICTURE S9(4).
               10  VEKT1-IO.
                   15  VEKT1               PICTURE S9(4).
               10  AKOL2-IO.
                   15  AKOL2               PICTURE S9(4).
               10  VEKT2-IO.
                   15  VEKT2               PICTURE S9(4).
               10  AKOL3-IO.
                   15  AKOL3               PICTURE S9(4).
               10  VEKT3-IO.
                   15  VEKT3               PICTURE S9(4).
               10  AKOL4-IO.
                   15  AKOL4               PICTURE S9(4).
               10  VEKT4-IO.
                   15  VEKT4               PICTURE S9(4).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(5).
               10  RAB-IO.
                   15  RAB                 PICTURE S9(5).
               10  FNUMM                   PICTURE X(10).
               10  STATUS-X                PICTURE X(1).
               10  RUTENR                  PICTURE X(2).
               10  SORT-X                  PICTURE X(3).
               10  SORT1                   PICTURE X(1).
               10  SORT2                   PICTURE X(1).
               10  SORT3                   PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  FNAVN                   PICTURE X(30).
      **************************************************************
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTFR-IO.
                   15  ANTFR               PICTURE S9(5).
               10  L1PRIS-IO.
                   15  L1PRIS              PICTURE S9(7).
               10  L1RAB-IO.
                   15  L1RAB               PICTURE S9(7).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(5).
               10  TOTANT-IO.
                   15  TOTANT              PICTURE S9(4).
               10  TOTKG-IO.
                   15  TOTKG               PICTURE S9(6).
               10  L1NETT-IO.
                   15  L1NETT              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
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
 
           IF  FRAKTED-PROCESS
               SET FRAKTED-PROCESS-OFF     TO TRUE
               SET FRAKTED-READ            TO TRUE
           END-IF
 
           IF  FRAKTED-READ
           AND RECORD-SELECTED-OFF
               PERFORM FRAKTED-GET
               SET FRAKTED-READ-OFF        TO TRUE
               IF  NOT FRAKTED-EOF
                   SET FRAKTED-PROCESS     TO TRUE
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
 
           IF  FRAKTED-PROCESS
               PERFORM FRAKTED-IDSET
           END-IF
 
           IF  FRAKTED-PROCESS
               PERFORM FRAKTED-CHK-LEVEL
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
 
           IF  FRAKTED-PROCESS
               PERFORM FRAKTED-FLDOFF
               PERFORM FRAKTED-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FRAKTED-PROCESS
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
           IF  (I-L1)
               SET NOT-I-90                TO TRUE
               IF  FNR = PFNR
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-90)
               GO TO END-X-T
           END-IF
           IF  (I-L1)
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-60                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-60            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L1)
               MOVE 0                      TO ANTFR
      *  01      STATUS    COMP "O"                      91
      *  01N91   STATUS    COMP "P"                      91
      *  01N91   STATUS    COMP "X"                      91
      *  01N91             GOTO END
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTFR
               ADD PRIS                    TO L1PRIS
               ADD RAB                     TO L1RAB
               SUBTRACT RAB FROM PRIS  GIVING NETTO
               ADD 1                       TO TOTANT
               ADD VEKT1                   TO TOTKG
           END-IF
           IF  (I-01 AND NOT-I-25)
               ADD VEKT2                   TO TOTKG
           END-IF
           IF  (I-01 AND NOT-I-26)
               ADD VEKT3                   TO TOTKG
           END-IF
           IF  (I-01 AND NOT-I-27)
               ADD VEKT4                   TO TOTKG
           END-IF.
 
       END-X-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'EDI60'                    TO LONR
           MOVE FNR                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'EDI062  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *RAKTED D        01 90 91
      *                                 974 "X"
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SUBTRACT L1RAB FROM L1PRIS GIVING L1NETT
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
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               MOVE PARAM-IO-AREA (3:3)    TO PFNR (1:3)
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
             OR ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
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
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       FRAKTED-GET SECTION.
       FRAKTED-GET-P.
           IF  FRAKTED-EOF-OFF
               READ FRAKTED-TMP
               AT END
                   SET FRAKTED-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FRAKTED-FLDOFF SECTION.
       FRAKTED-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
               SET NOT-I-14                TO TRUE
               SET NOT-I-15                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-17                TO TRUE
               SET NOT-I-18                TO TRUE
               SET NOT-I-19                TO TRUE
               SET NOT-I-20                TO TRUE
               SET NOT-I-21                TO TRUE
               SET NOT-I-22                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-27                TO TRUE
           END-EVALUATE.
 
       FRAKTED-FLDSET SECTION.
       FRAKTED-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FRAKTED-IO-AREA (4:3)  TO FNR (1:3)
               MOVE FRAKTED-IO-AREA (12:6) TO ORDNR (1:6)
               MOVE FRAKTED-IO-AREA (129:6) TO FBNR (1:6)
               MOVE FRAKTED-IO-AREA (201:10) TO MKNR (1:10)
               MOVE FRAKTED-IO-AREA (211:30) TO MNAVN (1:30)
               MOVE FRAKTED-IO-AREA (271:4) TO MPNR (1:4)
               MOVE FRAKTED-IO-AREA (342:6) TO KUNDE (1:6)
               MOVE FRAKTED-IO-AREA (431:1) TO SHENT (1:1)
               MOVE FRAKTED-IO-AREA (432:1) TO SFRAKT (1:1)
               MOVE FRAKTED-IO-AREA (433:1) TO SUTKJ (1:1)
               MOVE FRAKTED-IO-AREA (434:1) TO SINTET (1:1)
               MOVE FRAKTED-IO-AREA (440:6) TO ORD1 (1:6)
               IF  ORD1 = SPACES
                   SET I-11                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (450:6) TO ORD2 (1:6)
               IF  ORD2 = SPACES
                   SET I-12                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (460:6) TO ORD3 (1:6)
               IF  ORD3 = SPACES
                   SET I-13                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (470:6) TO ORD4 (1:6)
               IF  ORD4 = SPACES
                   SET I-14                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (480:6) TO ORD5 (1:6)
               IF  ORD5 = SPACES
                   SET I-15                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (490:6) TO ORD6 (1:6)
               IF  ORD6 = SPACES
                   SET I-16                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (500:6) TO ORD7 (1:6)
               IF  ORD7 = SPACES
                   SET I-17                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (510:6) TO ORD8 (1:6)
               IF  ORD8 = SPACES
                   SET I-18                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (520:6) TO ORD9 (1:6)
               IF  ORD9 = SPACES
                   SET I-19                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (530:6) TO ORD10 (1:6)
               IF  ORD10 = SPACES
                   SET I-20                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (540:6) TO ORD11 (1:6)
               IF  ORD11 = SPACES
                   SET I-21                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (550:6) TO ORD12 (1:6)
               IF  ORD12 = SPACES
                   SET I-22                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (560:6) TO ORD13 (1:6)
               IF  ORD13 = SPACES
                   SET I-23                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (601:4) TO AKOL1-IO
               INSPECT AKOL1-IO REPLACING ALL ' ' BY '0'
               IF  AKOL1 = ZERO
                   SET I-24                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (631:4) TO VEKT1-IO
               INSPECT VEKT1-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (650:4) TO AKOL2-IO
               INSPECT AKOL2-IO REPLACING ALL ' ' BY '0'
               IF  AKOL2 = ZERO
                   SET I-25                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (680:4) TO VEKT2-IO
               INSPECT VEKT2-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (699:4) TO AKOL3-IO
               INSPECT AKOL3-IO REPLACING ALL ' ' BY '0'
               IF  AKOL3 = ZERO
                   SET I-26                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (729:4) TO VEKT3-IO
               INSPECT VEKT3-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (748:4) TO AKOL4-IO
               INSPECT AKOL4-IO REPLACING ALL ' ' BY '0'
               IF  AKOL4 = ZERO
                   SET I-27                TO TRUE
               END-IF
               MOVE FRAKTED-IO-AREA (778:4) TO VEKT4-IO
               INSPECT VEKT4-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (1001:5) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (1006:5) TO RAB-IO
               INSPECT RAB-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (1011:10) TO FNUMM (1:10)
               MOVE FRAKTED-IO-AREA (974:1) TO STATUS-X (1:1)
               MOVE FRAKTED-IO-AREA (401:2) TO RUTENR (1:2)
               MOVE FRAKTED-IO-AREA (1058:3) TO SORT-X (1:3)
               MOVE FRAKTED-IO-AREA (1058:1) TO SORT1 (1:1)
               MOVE FRAKTED-IO-AREA (1059:1) TO SORT2 (1:1)
               MOVE FRAKTED-IO-AREA (1060:1) TO SORT3 (1:1)
           END-EVALUATE.
 
       FRAKTED-IDSET SECTION.
       FRAKTED-IDSET-P.
           SET I-01                        TO TRUE.
 
       FRAKTED-CHK-LEVEL SECTION.
       FRAKTED-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FRAKTED-LEVEL-01
               MOVE FRAKTED-IO-AREA (4:3)  TO FRAKTED-01-L1-FNR
               IF  FRAKTED-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FRAKTED-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FRAKTED-01-L1         TO THE-PRIOR-L1
               SET FRAKTED-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FNAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
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
           IF  (I-01 AND I-90 AND I-91)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDNR                  TO LISTE-IO-AREA (1:6)
               MOVE KUNDE                  TO LISTE-IO-AREA (8:6)
               MOVE MNAVN                  TO LISTE-IO-AREA (15:30)
               MOVE MPNR                   TO LISTE-IO-AREA (46:4)
               MOVE FNUMM                  TO LISTE-IO-AREA (51:10)
               MOVE PRIS                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (70:6)
               MOVE RAB                    TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (85:6)
               MOVE NETTO                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (100:6)
               MOVE RUTENR                 TO LISTE-IO-AREA (108:2)
               MOVE SORT-X                 TO LISTE-IO-AREA (110:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1 AND I-90)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
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
               MOVE 'ORDRE  KUNDE'         TO LISTE-IO-AREA (1:12)
               MOVE 'NAVN'                 TO LISTE-IO-AREA (16:4)
               MOVE 'PNR. FRAKTBREVNR.'    TO LISTE-IO-AREA (46:17)
               MOVE 'BRUTTO        RABATT' TO LISTE-IO-AREA (68:20)
               MOVE 'NETTO'                TO LISTE-IO-AREA (99:5)
               MOVE 'RUTE '                TO LISTE-IO-AREA (106:5)
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
           IF  (I-OF AND NOT-I-L1 AND I-90)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
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
               MOVE 'ORDRE  KUNDE'         TO LISTE-IO-AREA (1:12)
               MOVE 'NAVN'                 TO LISTE-IO-AREA (16:4)
               MOVE 'PNR. FRAKTBREVNR.'    TO LISTE-IO-AREA (46:17)
               MOVE 'BRUTTO        RABATT' TO LISTE-IO-AREA (68:20)
               MOVE 'NETTO'                TO LISTE-IO-AREA (99:5)
               MOVE 'RUTE '                TO LISTE-IO-AREA (106:5)
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
           IF  (I-L1 AND I-90)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FRAKTBREV DENNE UKE'  TO LISTE-IO-AREA (2:19)
               MOVE ANTFR                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (23:6)
               INITIALIZE ANTFR
               MOVE L1PRIS                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (67:9)
               MOVE L1RAB                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (82:9)
               MOVE L1NETT                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (97:9)
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
           SET FRAKTED-LEVEL-INIT          TO TRUE
           INITIALIZE FRAKTED-DATA-FIELDS
           SET FRAKTED-EOF-OFF             TO TRUE
           SET FRAKTED-PROCESS             TO TRUE
           SORT SORT-WK1
               ASCENDING  SORT-WK1-K1
               ASCENDING  SORT-WK1-K2
               ASCENDING  SORT-WK1-K3
               ASCENDING  SORT-WK1-K4
               ASCENDING  SORT-WK1-K5
               USING  FRAKTED
               GIVING FRAKTED-TMP.
           OPEN INPUT  FRAKTED-TMP.
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FRAKTED-TMP
           CLOSE FIRMAF
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
