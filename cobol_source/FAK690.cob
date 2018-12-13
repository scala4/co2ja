       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK690R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM FAK690 AV STEIN SANDVOLD 10.05.2007                   *
      * DANNER NY FAKTURAFILE som skal sende ut pdf faktura           *
      *        OBS OBS OBS                                            *
      * LEGGES DET INN NYE FIRMANR RETT OGSÅ ANTALL I RBS1 - FAK17    *
      * husk å rette fak693,fak694,fak695                            *
      * 10.9.14 Fjernet firmatest på 258,789 - EN                    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK690.rpg
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
           SELECT TABMAIL
               ASSIGN TO UT-S-TABMAIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABMAIL-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT INNFILE
               ASSIGN TO UT-S-INNFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFILE-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT STATTAB
               ASSIGN TO STATTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS STATTAB-STATUS
               RECORD KEY IS STATTAB-KEY1.
           SELECT FKREC2
               ASSIGN TO UT-S-FKREC2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKREC2-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TABMAIL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABMAIL-IO-AREA.
           05  TABMAIL-IO-AREA-X           PICTURE X(80).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD INNFILE
               BLOCK CONTAINS 256
               RECORD CONTAINS 128.
       01  INNFILE-IO-AREA.
           05  INNFILE-IO-AREA-X           PICTURE X(128).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD STATTAB
               RECORD CONTAINS 40.
       01  STATTAB-IO-AREA.
           05  STATTAB-IO-AREA-X.
               10  STATTAB-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(32).
       FD FKREC2
               BLOCK CONTAINS 1600
               RECORD CONTAINS 160.
       01  FKREC2-IO-AREA.
           05  FKREC2-IO-AREA-X            PICTURE X(160).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABF-MAX   VALUE 300            PICTURE 9(4) USAGE BINARY.
       77  TABMM-MAX   VALUE 300           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  TABF-TABLE.
               10  TABF-ENTRY
                                           OCCURS 300 TIMES
                                           INDEXED BY TABF-I
                                                      TABF-S
                                                      TABMM-I
                                                      TABMM-S.
                   15  TABF                PICTURE X(3).
                   15  TABMM               PICTURE X(57).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TABMAIL-STATUS              PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  INNFILE-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  STATTAB-STATUS              PICTURE 99 VALUE 0.
           10  FKREC2-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TABMAIL-EOF-OFF         VALUE '0'.
               88  TABMAIL-EOF             VALUE '1'.
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
               88  INNFILE-EOF-OFF         VALUE '0'.
               88  INNFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-READ-OFF        VALUE '0'.
               88  INNFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-PROCESS-OFF     VALUE '0'.
               88  INNFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  INNFILE-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  INNFILE-LEVEL-02.
               10  INNFILE-02-L2.
                   15  INNFILE-02-L2-FIRMA PICTURE X(3).
               10  INNFILE-02-L1.
                   15  INNFILE-02-L1-FANUM PICTURE X(6).
           05  INNFILE-DATA-FIELDS.
               10  RESKNR                  PICTURE X(6).
               10  RNR2                    PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  FANUM                   PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN                    PICTURE X(30).
           05  KUNDEMX-DATA-FIELDS.
               10  KMAIL                   PICTURE X(70).
               10  FPRTYP                  PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
           05  STATTAB-DATA-FIELDS.
               10  VALUTA-IO.
                   15  VALUTA              PICTURE S9(4)V9(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KXKEY1                  PICTURE X(9).
               10  KXKEY2                  PICTURE X(10).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(7).
               10  FNRTYP                  PICTURE X(5).
               10  SKEY                    PICTURE X(8).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-44YY9                PICTURE Z.ZZZ,9999.
               10  XO-70YN9                PICTURE ZZZZZZ9.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
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
 
           IF  INNFILE-PROCESS
               SET INNFILE-PROCESS-OFF     TO TRUE
               SET INNFILE-READ            TO TRUE
           END-IF
 
           IF  INNFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFILE-GET
               SET INNFILE-READ-OFF        TO TRUE
               IF  NOT INNFILE-EOF
                   SET INNFILE-PROCESS     TO TRUE
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
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-IDSET
           END-IF
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-CHK-LEVEL
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
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SUBTRACT ANTALL             FROM ANTALL
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-55)
               SET NOT-I-25                TO TRUE
               IF  KONFNR > '000'
                   SET I-25                TO TRUE
               END-IF
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
      **************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-10                TO TRUE
               SET TABF-S                  TO TABF-I
               PERFORM WITH TEST AFTER
                       VARYING TABF-I FROM 1 BY 1
                         UNTIL TABF-I >= TABF-MAX
                            OR I-10
                   IF  FIRMA = TABF (TABF-I)
                       SET I-10            TO TRUE
                       SET TABF-S          TO TABF-I
                   END-IF
               END-PERFORM
               SET TABF-I                  TO TABF-S
               IF  I-10
               AND TABF-I NOT > TABMM-MAX
                   SET TABMM-I             TO TABF-I
               END-IF
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '828'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-45                TO TRUE
           END-IF
           IF  (I-L1 AND I-40)
               SET NOT-I-41                TO TRUE
      *  TESTE PÅ RNR PÅ IPC-FOMA
           END-IF
           IF  (I-L1 AND I-40)
               SET NOT-I-41                TO TRUE
               IF  RNR2 = '15'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-40 AND I-41)
               SET I-45                    TO TRUE
      *  UNDERAVDELINGER
           END-IF
           IF  (I-L1)
               SET NOT-I-70                TO TRUE
               IF  FIRMA = '555'
                   SET I-70                TO TRUE
               END-IF
               SET NOT-I-71                TO TRUE
               IF  FIRMA = '704'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  FIRMA = '722'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  FIRMA = '725'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  FIRMA = '739'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  FIRMA = '754'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-72                TO TRUE
               IF  FIRMA = '968'
                   SET I-72                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '821'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-73)
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '366'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-73)
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '571'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-73)
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '141'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-73)
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '442'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-73)
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '641'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-73)
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '819'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-74                TO TRUE
               IF  FIRMA = '638'
                   SET I-74                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-74)
               SET NOT-I-74                TO TRUE
               IF  FIRMA = '652'
                   SET I-74                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-74)
               SET NOT-I-74                TO TRUE
               IF  FIRMA = '659'
                   SET I-74                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-74)
               SET NOT-I-74                TO TRUE
               IF  FIRMA = '673'
                   SET I-74                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-75                TO TRUE
               IF  FIRMA = '349'
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '676'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '694'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '695'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '696'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '697'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '698'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '699'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '742'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-64                TO TRUE
               IF  FIRMA = '628'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FIRMA = '109'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FIRMA = '163'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FIRMA = '256'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FIRMA = '259'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FIRMA = '264'
                   SET I-64                TO TRUE
               END-IF
      *  L1      FIRMA     COMP "941"                    77
           END-IF
           IF  (I-L1)
               SET NOT-I-78                TO TRUE
               IF  FIRMA = '971'
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-79                TO TRUE
               IF  FIRMA = '994'
                   SET I-79                TO TRUE
               END-IF
               SET NOT-I-80                TO TRUE
               IF  FIRMA = '478'
                   SET I-80                TO TRUE
               END-IF
               SET NOT-I-81                TO TRUE
               IF  FIRMA = '908'
                   SET I-81                TO TRUE
               END-IF
      *  L1      FIRMA     COMP "660"                    82
           END-IF
           IF  (I-L1)
               SET NOT-I-83                TO TRUE
               IF  FIRMA = '787'
                   SET I-83                TO TRUE
               END-IF
               SET NOT-I-88                TO TRUE
               IF  FIRMA = '110'
                   SET I-88                TO TRUE
               END-IF
               SET NOT-I-85                TO TRUE
               IF  FIRMA = '461'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-85)
               SET NOT-I-85                TO TRUE
               IF  FIRMA = '466'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '684'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '226'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '100'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '143'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '156'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '196'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '207'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '209'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '212'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '326'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '392'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '419'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '435'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '443'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '474'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '494'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '526'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '561'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '590'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '594'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '627'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '632'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '646'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '651'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '300'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '679'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '700'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '894'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '934'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '521'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '953'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '613'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FIRMA = '878'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '702'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '713'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '778'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '727'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '744'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '745'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '750'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '810'
                   SET I-35                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-60                TO TRUE
               MOVE FIRMA                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND NOT-I-55 AND I-25)
               MOVE KONFNR                 TO KXKEY1 (7:3)
           END-IF
           IF  (I-L1 AND I-34)
               MOVE '529'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-35)
               MOVE '370'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-70)
               MOVE '855'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-71)
               MOVE '940'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-72)
               MOVE '965'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-73)
               MOVE '709'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-74)
               MOVE '658'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-75)
               MOVE '391'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-76)
               MOVE '693'                  TO KXKEY1 (1:3)
      *  L1 77             MOVEL"938"     KXKEY1
           END-IF
           IF  (I-L1 AND I-78)
               MOVE '977'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-79)
               MOVE '992'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-80)
               MOVE '901'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-81)
               MOVE '904'                  TO KXKEY1 (1:3)
      *  L1 82             MOVEL"749"     KXKEY1
           END-IF
           IF  (I-L1 AND I-83)
               MOVE '764'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-85)
               MOVE '459'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-88)
               MOVE '756'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1 AND I-64)
               MOVE '628'                  TO KXKEY1 (1:3)
           END-IF
           IF  (I-L1)
               MOVE RESKNR                 TO KXKEY1 (4:6)
               MOVE KXKEY1                 TO KXKEY2 (1:9)
               MOVE '1'                    TO KXKEY2 (10:1)
               MOVE KXKEY2                 TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-63                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-63            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-63)
               SET NOT-I-60                TO TRUE
               IF  FPRTYP = 'M'
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-63 AND I-60)
               MOVE '6'                    TO KXKEY2 (10:1)
               MOVE KXKEY2                 TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-62                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-62            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L1)
               MOVE KXKEY1                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-91                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-91            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-45 AND I-60)
               PERFORM IPCRUT-S
           END-IF
           IF  (I-L1 AND I-60)
               ADD 1                       TO ANTALL
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'FAK17'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK690  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      ******************************************************
      *    SUBRUTINE FOR IPC FOR Å HENTE VALUTA            *
      ******************************************************
 
       IPCRUT-S SECTION.
       IPCRUT-S-P.
           MOVE FIRMA                      TO FNRTYP (1:3)
           MOVE '06'                       TO FNRTYP (4:2)
           MOVE FNRTYP                     TO SKEY (1:5)
           MOVE 'CVS'                      TO SKEY (6:3)
           MOVE SKEY                       TO STATTAB-KEY1
           READ STATTAB RECORD KEY IS STATTAB-KEY1
           INVALID KEY
               SET I-95                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-95                TO TRUE
               PERFORM STATTAB-FLDSET
               PERFORM STATTAB-IDSET
           END-READ
      *
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
 
       INNFILE-GET SECTION.
       INNFILE-GET-P.
           IF  INNFILE-EOF-OFF
               READ INNFILE
               AT END
                   SET INNFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFILE-FLDSET SECTION.
       INNFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '9' )
               MOVE INNFILE-IO-AREA (3:6)  TO RESKNR (1:6)
               MOVE INNFILE-IO-AREA (3:2)  TO RNR2 (1:2)
               MOVE INNFILE-IO-AREA (113:3) TO FIRMA (1:3)
               MOVE INNFILE-IO-AREA (116:6) TO FANUM (1:6)
           END-EVALUATE.
 
       INNFILE-IDSET SECTION.
       INNFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '9' )
               SET I-02                    TO TRUE
           WHEN  OTHER
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       INNFILE-CHK-LEVEL SECTION.
       INNFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '9' )
               MOVE LOW-VALUES             TO INNFILE-LEVEL-02
               MOVE INNFILE-IO-AREA (113:3) TO INNFILE-02-L2-FIRMA
               MOVE INNFILE-IO-AREA (116:6) TO INNFILE-02-L1-FANUM
               IF  INNFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFILE-02-L2         TO THE-PRIOR-L2
               MOVE  INNFILE-02-L1         TO THE-PRIOR-L1
               SET INNFILE-LEVEL-INIT      TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-06                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (11:70) TO KMAIL (1:70)
               MOVE KUNDEMX-IO-AREA (43:1) TO FPRTYP (1:1)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-05                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-09                        TO TRUE.
 
       STATTAB-FLDSET SECTION.
       STATTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATTAB-IO-AREA (17:8) TO VALUTA-IO
               INSPECT VALUTA-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       STATTAB-IDSET SECTION.
       STATTAB-IDSET-P.
           SET I-07                        TO TRUE.
 
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
 
       TABMAIL-LOAD SECTION.
       TABMAIL-LOAD-P.
           OPEN INPUT TABMAIL
           SET TABF-I                      TO 1
           PERFORM UNTIL TABMAIL-EOF
               READ TABMAIL
               AT END
                   SET TABMAIL-EOF         TO TRUE
               NOT AT END
                   MOVE TABMAIL-IO-AREA (1:60) TO TABF-ENTRY (TABF-I)
                   SET TABF-I              UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABMAIL.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L1 AND I-60)
               MOVE SPACES TO FKREC2-IO-AREA
               INITIALIZE FKREC2-IO-AREA
               MOVE FIRMA                  TO FKREC2-IO-AREA (1:3)
               MOVE RESKNR                 TO FKREC2-IO-AREA (4:6)
               MOVE FANUM                  TO FKREC2-IO-AREA (10:6)
               MOVE 'FAKTURAMAIL@AUTOD'    TO FKREC2-IO-AREA (16:17)
               MOVE 'ATA.NO              ' TO FKREC2-IO-AREA (33:20)
               MOVE '                    ' TO FKREC2-IO-AREA (53:20)
               MOVE '             '        TO FKREC2-IO-AREA (73:13)
               IF  (NOT-I-62)
                   MOVE KMAIL              TO FKREC2-IO-AREA (16:70)
               END-IF
               MOVE 'RETUR@AUTODATA.NO'    TO FKREC2-IO-AREA (86:17)
               MOVE '                    ' TO FKREC2-IO-AREA (103:20)
               MOVE '                    ' TO FKREC2-IO-AREA (123:20)
               IF  (I-10 AND NOT-I-45)
                   MOVE TABMM (TABMM-I)    TO FKREC2-IO-AREA (86:57)
               END-IF
               IF  (I-45)
                   MOVE 'INFO@CENTRALVAC.SE  ' TO FKREC2-IO-AREA
                                                               (86:20)
               END-IF
               MOVE '00000000'             TO FKREC2-IO-AREA (143:8)
               IF  (I-45 AND NOT-I-95)
                   MOVE VALUTA-IO          TO FKREC2-IO-AREA (143:8)
               END-IF
               MOVE FPRTYP                 TO FKREC2-IO-AREA (151:1)
               WRITE FKREC2-IO-AREA
           END-IF
           IF  (I-L1 AND I-60 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RESKNR                 TO LISTE-IO-AREA (1:6)
               IF  (NOT-I-91)
                   MOVE NAVN               TO LISTE-IO-AREA (8:30)
                   INITIALIZE NAVN
               END-IF
               IF  (I-91)
                   MOVE 'KUNDE UKJENT'     TO LISTE-IO-AREA (26:12)
               END-IF
               MOVE FANUM                  TO LISTE-IO-AREA (41:6)
               IF  (NOT-I-62)
                   MOVE KMAIL              TO LISTE-IO-AREA (51:70)
               END-IF
               IF  (I-45 AND NOT-I-95)
                   MOVE VALUTA             TO XO-44YY9
                   MOVE XO-44YY9           TO LISTE-IO-AREA (123:10)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
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
               MOVE 'KUNDE'                TO LISTE-IO-AREA (1:5)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (11:9)
               MOVE 'FAKTURANR'            TO LISTE-IO-AREA (40:9)
               MOVE 'MAIL.ADRESSE'         TO LISTE-IO-AREA (51:12)
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
           IF  (I-OF AND NOT-I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
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
               MOVE 'KUNDE'                TO LISTE-IO-AREA (1:5)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (11:9)
               MOVE 'FAKTURANR'            TO LISTE-IO-AREA (40:9)
               MOVE 'MAIL.ADRESSE'         TO LISTE-IO-AREA (51:12)
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
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ANTALL FAKTNR DEN' TO LISTE-IO-AREA (1:24)
               MOVE 'NE FAKTUAOMGANGEN       ' TO LISTE-IO-AREA (25:24)
               MOVE ANTALL                 TO XO-70YN9
               MOVE XO-70YN9               TO LISTE-IO-AREA (54:7)
               INITIALIZE ANTALL
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           PERFORM TABMAIL-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET INNFILE-LEVEL-INIT          TO TRUE
           INITIALIZE INNFILE-DATA-FIELDS
           SET INNFILE-EOF-OFF             TO TRUE
           SET INNFILE-PROCESS             TO TRUE
           OPEN INPUT INNFILE
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE STATTAB-DATA-FIELDS
           OPEN INPUT STATTAB
           OPEN OUTPUT FKREC2
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABF-I                      TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE INNFILE
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE FIRMAF
           CLOSE STATTAB
           CLOSE FKREC2
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
