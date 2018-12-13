       IDENTIFICATION DIVISION.
       PROGRAM-ID. RGN010R.
      **********************************************  Z-WIN-RPG2   ****
      * ENDRING AV VGR TIL KONTONR. HVIS SALG OG VGR                  *
      * FINNES I TABELLEN.                                            *
      * AVGIFSKODE 4 (FRITT SALG) OMDØPES TIL KODE 0.                 *
      * ENDR 14.04.97: UTVIDET TABELL TIL 600 ELEMENTER. TABELLSPREKK.*
      * ENDR 02.04.98: UTVIDET TABELL TIL 900 ELEMENTER. TABELLSPREKK.*
      * ENDR 20.12.00: UTVIDET TABELL TIL1200 ELEMENTER. TABELLSPREKK.*
      * ENDR 02.01.05: UTVIDET TABELL TIL2000 ELEMENTER. TABELLSPREKK.*
      * ENDR 07.11.05: LAGT INN ENDRING AV AK=4->AK=0 NÅR VALGFRI KTO *
      *                FOR AVG.FRITT SALG ER VALGT I RG02.            *
      *                ENDRER IKKE HVIS FLERE KONTI FOR AVG.SALG ER   *
      *                VALGT OG PERIODE > 2005/11.                    *
      *      21.07.09  SKAL VEL VÆRE:                                 *
      *                ENDRER IKKE HVIS VALGFRI KONTO FOR AVG.FRITT   *
      *                SALG ER VALGT OG PERIODE < 2005/12.            *
      * ENDR 26.10.11: UTVIDET TABELL TIL3000 ELEMENTER. TABELLSPREKK.*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RGN010.rpg
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
           SELECT VGRTAB
               ASSIGN TO UT-S-VGRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VGRTAB-STATUS.
           SELECT REGFILE
               ASSIGN TO UT-S-REGFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGFILE-STATUS.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VGRTAB
               BLOCK CONTAINS 1536
               RECORD CONTAINS 16.
       01  VGRTAB-IO-AREA.
           05  VGRTAB-IO-AREA-X            PICTURE X(16).
       FD REGFILE
               BLOCK CONTAINS 9360
               RECORD CONTAINS 120.
       01  REGFILE-IO-AREA.
           05  REGFILE-IO-AREA-X           PICTURE X(120).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABVGR-MAX   VALUE 4000         PICTURE 9(4) USAGE BINARY.
       77  TABKTO-MAX   VALUE 4000         PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABVGR-TABLE.
               10  TABVGR-ENTRY
                                           OCCURS 4000 TIMES
                                           INDEXED BY TABVGR-I
                                                      TABVGR-S
                                                      TABKTO-I
                                                      TABKTO-S.
                   15  TABVGR              PICTURE X(8).
                   15  TABKTO              PICTURE X(8).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VGRTAB-STATUS               PICTURE 99 VALUE 0.
           10  REGFILE-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VGRTAB-EOF-OFF          VALUE '0'.
               88  VGRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFILE-EOF-OFF         VALUE '0'.
               88  REGFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFILE-READ-OFF        VALUE '0'.
               88  REGFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGFILE-PROCESS-OFF     VALUE '0'.
               88  REGFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  REGFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  REGFILE-LEVEL-INIT      VALUE '1'.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  REGFILE-LEVEL-01.
               10  REGFILE-01-L1.
                   15  REGFILE-01-L1-FNR   PICTURE X(3).
           05  REGFILE-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  BILART                  PICTURE X(1).
               10  REGPER                  PICTURE X(6).
               10  KTOKL                   PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  KTO2S                   PICTURE X(2).
               10  KTO4S                   PICTURE X(4).
               10  AK                      PICTURE X(1).
           05  SYSPARM-DATA-FIELDS.
               10  VFKAVF                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  SYSKEY                  PICTURE X(10).
               10  FNRVGR                  PICTURE X(8).
               10  ANTLOK-IO.
                   15  ANTLOK              PICTURE S9(5).
               10  NYKTO                   PICTURE X(8).
               10  NYAK                    PICTURE X(1).
               10  ANTKTO-IO.
                   15  ANTKTO              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  REGFILE-PROCESS
               SET REGFILE-PROCESS-OFF     TO TRUE
               SET REGFILE-READ            TO TRUE
           END-IF
 
           IF  REGFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM REGFILE-GET
               SET REGFILE-READ-OFF        TO TRUE
               IF  NOT REGFILE-EOF
                   SET REGFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REGFILE-PROCESS
               PERFORM REGFILE-IDSET
           END-IF
 
           IF  REGFILE-PROCESS
               PERFORM REGFILE-CHK-LEVEL
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
 
           IF  REGFILE-PROCESS
               PERFORM REGFILE-FLDOFF
               PERFORM REGFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  REGFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-34                TO TRUE
               MOVE FNR                    TO SYSKEY (1:3)
               MOVE 'REGA011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-31                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-31            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-31)
               SET NOT-I-34                TO TRUE
               IF  VFKAVF = 'J'
                   SET I-34                TO TRUE
               END-IF
      *  L1                MOVE "VFKAVF  "BUGFL2  8        LEDETXT DEBUG
      *  L1      BUGFL2    DEBUGBUGFILO   VFKAVF           VIS FELT/IND
           END-IF
           SET NOT-I-20                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  BILART = '0'
               SET I-12                    TO TRUE
           END-IF
           IF  (NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  BILART = '1'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  BILART = '2'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  BILART = '3'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-12)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-08)
               OR  (NOT-I-09)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  KTOKL = '5'
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-15)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  AK = '0'
               SET I-25                    TO TRUE
           END-IF
           IF  (I-25)
               MOVE '6'                    TO NYAK
      *
           END-IF
           MOVE FNR                        TO FNRVGR (1:3)
           MOVE VGR                        TO FNRVGR (4:5)
           SET NOT-I-20                    TO TRUE
           SET TABVGR-S                    TO TABVGR-I
           PERFORM WITH TEST AFTER
                   VARYING TABVGR-I FROM 1 BY 1
                     UNTIL TABVGR-I >= TABVGR-MAX
                        OR I-20
               IF  FNRVGR = TABVGR (TABVGR-I)
                   SET I-20                TO TRUE
                   SET TABVGR-S            TO TABVGR-I
               END-IF
           END-PERFORM
           SET TABVGR-I                    TO TABVGR-S
           IF  I-20
           AND TABVGR-I NOT > TABKTO-MAX
               SET TABKTO-I                TO TABVGR-I
           END-IF
           ADD 1                           TO ANTLOK
           IF  (NOT-I-20)
               GO TO SLUTT-T
           END-IF
           MOVE TABKTO(TABKTO-I)           TO NYKTO
           MOVE AK                         TO NYAK
           SET NOT-I-11                    TO TRUE
           IF  REGPER < '200512'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  AK = '4'
               SET I-10                    TO TRUE
           END-IF
           IF  (I-10 AND NOT-I-34)
               MOVE '0'                    TO NYAK
           END-IF
           IF  (I-10 AND I-34 AND I-11)
               MOVE '0'                    TO NYAK
           END-IF
           ADD 1                           TO ANTKTO.
 
       SLUTT-T.
           CONTINUE.
 
       REGFILE-GET SECTION.
       REGFILE-GET-P.
           IF  REGFILE-EOF-OFF
               READ REGFILE
               AT END
                   SET REGFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REGFILE-FLDOFF SECTION.
       REGFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       REGFILE-FLDSET SECTION.
       REGFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE REGFILE-IO-AREA (3:3)  TO FNR (1:3)
               MOVE REGFILE-IO-AREA (6:1)  TO BILART (1:1)
               MOVE REGFILE-IO-AREA (41:6) TO REGPER (1:6)
               MOVE REGFILE-IO-AREA (47:1) TO KTOKL (1:1)
               MOVE REGFILE-IO-AREA (48:5) TO VGR (1:5)
               MOVE REGFILE-IO-AREA (53:2) TO KTO2S (1:2)
               IF  KTO2S = SPACES
                   SET I-09                TO TRUE
               END-IF
               MOVE REGFILE-IO-AREA (51:4) TO KTO4S (1:4)
               IF  KTO4S = SPACES
                   SET I-08                TO TRUE
               END-IF
               MOVE REGFILE-IO-AREA (56:1) TO AK (1:1)
           END-EVALUATE.
 
       REGFILE-IDSET SECTION.
       REGFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       REGFILE-CHK-LEVEL SECTION.
       REGFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO REGFILE-LEVEL-01
               MOVE REGFILE-IO-AREA (3:3)  TO REGFILE-01-L1-FNR
               IF  REGFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  REGFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  REGFILE-01-L1         TO THE-PRIOR-L1
               SET REGFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (159:1) TO VFKAVF (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
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
               MOVE 7                      TO LISTE-AFTER-SKIP
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
 
       VGRTAB-LOAD SECTION.
       VGRTAB-LOAD-P.
           OPEN INPUT VGRTAB
           SET TABVGR-I                    TO 1
           PERFORM UNTIL VGRTAB-EOF
               READ VGRTAB
               AT END
                   SET VGRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE VGRTAB-IO-AREA (1:16) TO TABVGR-ENTRY
                                                            (TABVGR-I)
                   SET TABVGR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE VGRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-20)
               MOVE NYKTO                  TO REGFILE-IO-AREA (47:8)
               MOVE NYAK                   TO REGFILE-IO-AREA (56:1)
               REWRITE REGFILE-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-20 AND I-25)
               MOVE NYAK                   TO REGFILE-IO-AREA (56:1)
               REWRITE REGFILE-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-01 AND I-20 AND I-U8)
           AND (I-34)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FRA VGRP: '           TO LISTE-IO-AREA (1:10)
               MOVE VGR                    TO LISTE-IO-AREA (11:5)
               MOVE ' TIL KTO: '           TO LISTE-IO-AREA (16:10)
               MOVE NYKTO                  TO LISTE-IO-AREA (26:8)
               MOVE '  FRA AK: '           TO LISTE-IO-AREA (34:10)
               MOVE AK                     TO LISTE-IO-AREA (44:1)
               MOVE ' TIL AK: '            TO LISTE-IO-AREA (45:9)
               MOVE NYAK                   TO LISTE-IO-AREA (54:1)
               MOVE ' VFKAVF: '            TO LISTE-IO-AREA (55:9)
               MOVE VFKAVF                 TO LISTE-IO-AREA (64:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTLOK                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE 'ANT. TABELLOPPSLAG.'  TO LISTE-IO-AREA (12:19)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTKTO                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE 'ANT. KONTOENDRINGER.' TO LISTE-IO-AREA (12:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND I-U2)
           AND (I-02 AND I-U1 AND I-U2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (10:1)
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
           PERFORM VGRTAB-LOAD
           SET REGFILE-LEVEL-INIT          TO TRUE
           INITIALIZE REGFILE-DATA-FIELDS
           SET REGFILE-EOF-OFF             TO TRUE
           SET REGFILE-PROCESS             TO TRUE
           OPEN I-O REGFILE
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABVGR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGFILE
           CLOSE SYSPARM
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
