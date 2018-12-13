       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA694R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: STA694                                               *
      * 29/7-96   LAGT INN UTPLUKK PÅ RESK.GRP = 99 = SHELL           *
      * 05/2-01   LAGT INN UTPLUKK PÅ ALLE KATEGORIER.                *
      * 21/8-12   LAGT INN ""alt"" i firmautplukk, tar m/alle firma     *
      *                                                               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA694.rpg
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
           SELECT KUNTAB
               ASSIGN TO UT-S-KUNTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KUNTAB-STATUS.
           SELECT VRGTAB
               ASSIGN TO UT-S-VRGTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VRGTAB-STATUS.
           SELECT MNDTAB
               ASSIGN TO UT-S-MNDTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MNDTAB-STATUS.
           SELECT EDBTAB
               ASSIGN TO UT-S-EDBTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDBTAB-STATUS.
           SELECT KHNTAB
               ASSIGN TO UT-S-KHNTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KHNTAB-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT SALGFIL
               ASSIGN TO UT-S-SALGFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SALGFIL-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KUNTAB
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  KUNTAB-IO-AREA.
           05  KUNTAB-IO-AREA-X            PICTURE X(60).
       FD VRGTAB
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  VRGTAB-IO-AREA.
           05  VRGTAB-IO-AREA-X            PICTURE X(60).
       FD MNDTAB
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  MNDTAB-IO-AREA.
           05  MNDTAB-IO-AREA-X            PICTURE X(60).
       FD EDBTAB
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  EDBTAB-IO-AREA.
           05  EDBTAB-IO-AREA-X            PICTURE X(60).
       FD KHNTAB
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  KHNTAB-IO-AREA.
           05  KHNTAB-IO-AREA-X            PICTURE X(60).
       FD PARAM
               BLOCK CONTAINS 1200
               RECORD CONTAINS 1200.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(1200).
       FD SALGFIL
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  SALGFIL-IO-AREA.
           05  SALGFIL-IO-AREA-X           PICTURE X(82).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      *ISTE   O   F 132 132     OF     PRINTERSYSLST
       FD OUTFILE
               BLOCK CONTAINS 4070
               RECORD CONTAINS 110.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(110).
       WORKING-STORAGE SECTION.
       77  TABKUN-MAX   VALUE 150          PICTURE 9(4) USAGE BINARY.
       77  TABVRG-MAX   VALUE 300          PICTURE 9(4) USAGE BINARY.
       77  TABMND-MAX   VALUE 12           PICTURE 9(4) USAGE BINARY.
       77  TABEDB-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       77  TABKHN-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABKUN-TABLE.
               10  TABKUN-ENTRY
                                           OCCURS 150 TIMES
                                           INDEXED BY TABKUN-I
                                                      TABKUN-S.
                   15  TABKUN              PICTURE X(6).
           05  TABVRG-TABLE.
               10  TABVRG-ENTRY
                                           OCCURS 300 TIMES
                                           INDEXED BY TABVRG-I
                                                      TABVRG-S.
                   15  TABVRG              PICTURE X(5).
           05  TABMND-TABLE.
               10  TABMND-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY TABMND-I
                                                      TABMND-S.
                   15  TABMND              PICTURE X(2).
           05  TABEDB-TABLE.
               10  TABEDB-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY TABEDB-I
                                                      TABEDB-S.
                   15  TABEDB              PICTURE X(7).
           05  TABKHN-TABLE.
               10  TABKHN-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY TABKHN-I
                                                      TABKHN-S.
                   15  TABKHN              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KUNTAB-STATUS               PICTURE 99 VALUE 0.
           10  VRGTAB-STATUS               PICTURE 99 VALUE 0.
           10  MNDTAB-STATUS               PICTURE 99 VALUE 0.
           10  EDBTAB-STATUS               PICTURE 99 VALUE 0.
           10  KHNTAB-STATUS               PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  SALGFIL-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNTAB-EOF-OFF          VALUE '0'.
               88  KUNTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VRGTAB-EOF-OFF          VALUE '0'.
               88  VRGTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDTAB-EOF-OFF          VALUE '0'.
               88  MNDTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBTAB-EOF-OFF          VALUE '0'.
               88  EDBTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KHNTAB-EOF-OFF          VALUE '0'.
               88  KHNTAB-EOF              VALUE '1'.
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
               88  SALGFIL-EOF-OFF         VALUE '0'.
               88  SALGFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SALGFIL-READ-OFF        VALUE '0'.
               88  SALGFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SALGFIL-PROCESS-OFF     VALUE '0'.
               88  SALGFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SALGFIL-LEVEL-INIT-OFF  VALUE '0'.
               88  SALGFIL-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PARAM-DATA-FIELDS.
               10  PFIRMA                  PICTURE X(3).
               10  TAB1                    PICTURE X(1).
               10  TAB2                    PICTURE X(1).
               10  TAB3                    PICTURE X(1).
               10  TAB4                    PICTURE X(1).
               10  PTAB                    PICTURE X(1).
               10  TAB5                    PICTURE X(1).
           05  SALGFIL-LEVEL-01.
               10  SALGFIL-01-L2.
                   15  SALGFIL-01-L2-FIRMA PICTURE X(3).
               10  SALGFIL-01-L1.
                   15  SALGFIL-01-L1-EDBNR PICTURE X(7).
           05  SALGFIL-DATA-FIELDS.
               10  REC                     PICTURE X(82).
               10  EDBNR                   PICTURE X(7).
               10  MNDER                   PICTURE X(2).
               10  RESKNR                  PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  HDIST                   PICTURE X(3).
               10  VRG                     PICTURE X(5).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
           05  KUNDEMA-DATA-FIELDS.
               10  KUKAT-IO.
                   15  KUKAT               PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  KUHND                   PICTURE X(3).
               10  RESGRP                  PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  KKEY                    PICTURE X(9).
               10  FKKEY                   PICTURE X(9).
               10  KAT2-IO.
                   15  KAT2                PICTURE S9(3).
               10  KAT                     PICTURE X(3).
               10  VKEY                    PICTURE X(10).
               10  FVKEY                   PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-30D                  PICTURE S9(3).
               10  XO-30U                  PICTURE 9(3).
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-03                    TO TRUE
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
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  SALGFIL-PROCESS
               SET SALGFIL-PROCESS-OFF     TO TRUE
               SET SALGFIL-READ            TO TRUE
           END-IF
 
           IF  SALGFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM SALGFIL-GET
               SET SALGFIL-READ-OFF        TO TRUE
               IF  NOT SALGFIL-EOF
                   SET SALGFIL-PROCESS     TO TRUE
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
 
           IF  SALGFIL-PROCESS
               PERFORM SALGFIL-IDSET
           END-IF
 
           IF  SALGFIL-PROCESS
               PERFORM SALGFIL-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  SALGFIL-PROCESS
               PERFORM SALGFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SALGFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-05)
               SET NOT-I-10                TO TRUE
               IF  TAB1 = 'J'
                   SET I-10                TO TRUE
               END-IF
               SET NOT-I-11                TO TRUE
               IF  TAB2 = 'J'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-21                TO TRUE
               IF  TAB3 = 'J'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  TAB4 = 'J'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  PTAB = 'M'
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-13                TO TRUE
               IF  PTAB = 'U'
                   SET I-13                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  TAB5 = 'K'
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  TAB5 = 'H'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-94                TO TRUE
               IF  TAB5 = 'S'
                   SET I-94                TO TRUE
               END-IF
               SET NOT-I-19                TO TRUE
               IF  TAB5 = 'A'
                   SET I-19                TO TRUE
               END-IF
               SET NOT-I-18                TO TRUE
               IF  TAB5 = 'B'
                   SET I-18                TO TRUE
               END-IF
               SET NOT-I-95                TO TRUE
               IF  TAB5 = 'C'
                   SET I-95                TO TRUE
               END-IF
               SET NOT-I-96                TO TRUE
               IF  PFIRMA = 'ALT'
                   SET I-96                TO TRUE
               END-IF
               GO TO END-X-T
      **************************************************************************
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  (NOT-I-96)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = PFIRMA
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-15 AND NOT-I-96)
               GO TO END-X-T
           END-IF
           IF  (I-10)
               SET NOT-I-16                TO TRUE
               SET TABKUN-S                TO TABKUN-I
               PERFORM WITH TEST AFTER
                       VARYING TABKUN-I FROM 1 BY 1
                         UNTIL TABKUN-I >= TABKUN-MAX
                            OR I-16
                   IF  RESKNR = TABKUN (TABKUN-I)
                       SET I-16            TO TRUE
                       SET TABKUN-S        TO TABKUN-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-10 AND NOT-I-16)
               GO TO END-X-T
      *
           END-IF
           IF  (I-21)
               SET NOT-I-25                TO TRUE
               SET TABMND-S                TO TABMND-I
               PERFORM WITH TEST AFTER
                       VARYING TABMND-I FROM 1 BY 1
                         UNTIL TABMND-I >= TABMND-MAX
                            OR I-25
                   IF  MNDER = TABMND (TABMND-I)
                       SET I-25            TO TRUE
                       SET TABMND-S        TO TABMND-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-21 AND NOT-I-25)
               GO TO END-X-T
      *
           END-IF
           IF  (I-22)
               SET NOT-I-26                TO TRUE
               SET TABEDB-S                TO TABEDB-I
               PERFORM WITH TEST AFTER
                       VARYING TABEDB-I FROM 1 BY 1
                         UNTIL TABEDB-I >= TABEDB-MAX
                            OR I-26
                   IF  EDBNR = TABEDB (TABEDB-I)
                       SET I-26            TO TRUE
                       SET TABEDB-S        TO TABEDB-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-22 AND NOT-I-26)
               GO TO END-X-T
           END-IF
           IF  (I-11)
               SET NOT-I-17                TO TRUE
               SET TABVRG-S                TO TABVRG-I
               PERFORM WITH TEST AFTER
                       VARYING TABVRG-I FROM 1 BY 1
                         UNTIL TABVRG-I >= TABVRG-MAX
                            OR I-17
                   IF  VRG = TABVRG (TABVRG-I)
                       SET I-17            TO TRUE
                       SET TABVRG-S        TO TABVRG-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-11 AND NOT-I-17 AND I-12)
               GO TO END-X-T
           END-IF
           IF  (I-11 AND I-17 AND I-13)
               GO TO END-X-T
           END-IF
           IF  (NOT-I-19 AND NOT-I-23 AND NOT-I-24)
               AND (NOT-I-94 AND NOT-I-95)
               GO TO UTRUT-T
           END-IF
           MOVE FIRMA                      TO KKEY (1:3)
           MOVE RESKNR                     TO KKEY (4:6)
           SET NOT-I-32                    TO TRUE
           IF  KKEY = FKKEY
               SET I-32                    TO TRUE
           END-IF
           MOVE KKEY                       TO FKKEY
           IF  (NOT-I-32)
               MOVE KKEY                   TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-28                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-28            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-28)
               GO TO END-X-T
           END-IF
           IF  (I-18)
               GO TO UTRUT-T
           END-IF
           IF  (I-19)
               GO TO UTRUT-T
           END-IF
           IF  (I-95)
               GO TO UTRUT-T
           END-IF
           IF  (I-24 AND NOT-I-28)
               SET NOT-I-27                TO TRUE
               SET TABKHN-S                TO TABKHN-I
               PERFORM WITH TEST AFTER
                       VARYING TABKHN-I FROM 1 BY 1
                         UNTIL TABKHN-I >= TABKHN-MAX
                            OR I-27
                   IF  KUHND = TABKHN (TABKHN-I)
                       SET I-27            TO TRUE
                       SET TABKHN-S        TO TABKHN-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-24 AND NOT-I-28 AND I-U1)
               SET NOT-I-27                TO TRUE
               SET TABKHN-S                TO TABKHN-I
               PERFORM WITH TEST AFTER
                       VARYING TABKHN-I FROM 1 BY 1
                         UNTIL TABKHN-I >= TABKHN-MAX
                            OR I-27
                   IF  HDIST = TABKHN (TABKHN-I)
                       SET I-27            TO TRUE
                       SET TABKHN-S        TO TABKHN-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-24 AND NOT-I-27)
               GO TO END-X-T
           END-IF
           IF  (I-23)
               ADD KUKAT TO ZERO       GIVING KAT2
               MOVE KAT2                   TO KAT
               SET NOT-I-27                TO TRUE
               SET TABKHN-S                TO TABKHN-I
               PERFORM WITH TEST AFTER
                       VARYING TABKHN-I FROM 1 BY 1
                         UNTIL TABKHN-I >= TABKHN-MAX
                            OR I-27
                   IF  KAT = TABKHN (TABKHN-I)
                       SET I-27            TO TRUE
                       SET TABKHN-S        TO TABKHN-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-23 AND NOT-I-27)
               GO TO END-X-T
      *****************************************************************
      * UTPUKK AV SHELL = RESK.GRP. 99                                *
      *****************************************************************
           END-IF
           IF  (I-94)
               SET NOT-I-27                TO TRUE
               IF  RESGRP = '99'
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-94 AND NOT-I-27)
               GO TO END-X-T
      *****************************************************************
           END-IF
           .
 
       UTRUT-T.
           SET I-20                        TO TRUE
           MOVE FIRMA                      TO VKEY (1:3)
           MOVE EDBNR                      TO VKEY (4:7)
           SET NOT-I-33                    TO TRUE
           IF  VKEY = FVKEY
               SET I-33                    TO TRUE
           END-IF
           MOVE VKEY                       TO FVKEY
           IF  (NOT-I-33)
               MOVE VKEY                   TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF.
 
       END-X-T.
           CONTINUE.
 
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
           WHEN ANY
               MOVE PARAM-IO-AREA (7:3)    TO PFIRMA (1:3)
               MOVE PARAM-IO-AREA (16:1)   TO TAB1 (1:1)
               MOVE PARAM-IO-AREA (23:1)   TO TAB2 (1:1)
               MOVE PARAM-IO-AREA (30:1)   TO TAB3 (1:1)
               MOVE PARAM-IO-AREA (37:1)   TO TAB4 (1:1)
               MOVE PARAM-IO-AREA (47:1)   TO PTAB (1:1)
               MOVE PARAM-IO-AREA (54:1)   TO TAB5 (1:1)
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-05                        TO TRUE.
 
       SALGFIL-GET SECTION.
       SALGFIL-GET-P.
           IF  SALGFIL-EOF-OFF
               READ SALGFIL
               AT END
                   SET SALGFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SALGFIL-FLDSET SECTION.
       SALGFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SALGFIL-IO-AREA (1:82) TO REC (1:82)
               MOVE SALGFIL-IO-AREA (16:7) TO EDBNR (1:7)
               MOVE SALGFIL-IO-AREA (42:2) TO MNDER (1:2)
               MOVE SALGFIL-IO-AREA (45:6) TO RESKNR (1:6)
               MOVE SALGFIL-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE SALGFIL-IO-AREA (54:3) TO HDIST (1:3)
               MOVE SALGFIL-IO-AREA (60:5) TO VRG (1:5)
           END-EVALUATE.
 
       SALGFIL-IDSET SECTION.
       SALGFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       SALGFIL-CHK-LEVEL SECTION.
       SALGFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SALGFIL-LEVEL-01
               MOVE SALGFIL-IO-AREA (51:3) TO SALGFIL-01-L2-FIRMA
               MOVE SALGFIL-IO-AREA (16:7) TO SALGFIL-01-L1-EDBNR
               IF  SALGFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SALGFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SALGFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SALGFIL-01-L2         TO THE-PRIOR-L2
               MOVE  SALGFIL-01-L1         TO THE-PRIOR-L1
               SET SALGFIL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (162:2) TO KUKAT-IO
               MOVE KUNDEMA-IO-AREA (185:3) TO KUHND (1:3)
               MOVE KUNDEMA-IO-AREA (125:2) TO RESGRP (1:2)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       KUNTAB-LOAD SECTION.
       KUNTAB-LOAD-P.
           OPEN INPUT KUNTAB
           SET TABKUN-I                    TO 1
           PERFORM UNTIL KUNTAB-EOF
               READ KUNTAB
               AT END
                   SET KUNTAB-EOF          TO TRUE
               NOT AT END
                   MOVE KUNTAB-IO-AREA (1:6) TO TABKUN-ENTRY (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (7:6) TO TABKUN-ENTRY (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (13:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (19:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (25:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (31:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (37:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (43:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (49:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
                   MOVE KUNTAB-IO-AREA (55:6) TO TABKUN-ENTRY
                                                            (TABKUN-I)
                   SET TABKUN-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KUNTAB.
 
       VRGTAB-LOAD SECTION.
       VRGTAB-LOAD-P.
           OPEN INPUT VRGTAB
           SET TABVRG-I                    TO 1
           PERFORM UNTIL VRGTAB-EOF
               READ VRGTAB
               AT END
                   SET VRGTAB-EOF          TO TRUE
               NOT AT END
                   MOVE VRGTAB-IO-AREA (1:5) TO TABVRG-ENTRY (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (6:5) TO TABVRG-ENTRY (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (11:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (16:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (21:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (26:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (31:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (36:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (41:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (46:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (51:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
                   MOVE VRGTAB-IO-AREA (56:5) TO TABVRG-ENTRY
                                                            (TABVRG-I)
                   SET TABVRG-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE VRGTAB.
 
       MNDTAB-LOAD SECTION.
       MNDTAB-LOAD-P.
           OPEN INPUT MNDTAB
           SET TABMND-I                    TO 1
           PERFORM UNTIL MNDTAB-EOF
               READ MNDTAB
               AT END
                   SET MNDTAB-EOF          TO TRUE
               NOT AT END
                   MOVE MNDTAB-IO-AREA (1:2) TO TABMND-ENTRY (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (3:2) TO TABMND-ENTRY (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (5:2) TO TABMND-ENTRY (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (7:2) TO TABMND-ENTRY (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (9:2) TO TABMND-ENTRY (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (11:2) TO TABMND-ENTRY
                                                            (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (13:2) TO TABMND-ENTRY
                                                            (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (15:2) TO TABMND-ENTRY
                                                            (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (17:2) TO TABMND-ENTRY
                                                            (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (19:2) TO TABMND-ENTRY
                                                            (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (21:2) TO TABMND-ENTRY
                                                            (TABMND-I)
                   SET TABMND-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (23:2) TO TABMND-ENTRY
                                                            (TABMND-I)
                   SET TABMND-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE MNDTAB.
 
       EDBTAB-LOAD SECTION.
       EDBTAB-LOAD-P.
           OPEN INPUT EDBTAB
           SET TABEDB-I                    TO 1
           PERFORM UNTIL EDBTAB-EOF
               READ EDBTAB
               AT END
                   SET EDBTAB-EOF          TO TRUE
               NOT AT END
                   MOVE EDBTAB-IO-AREA (1:7) TO TABEDB-ENTRY (TABEDB-I)
                   SET TABEDB-I            UP BY 1
                   MOVE EDBTAB-IO-AREA (8:7) TO TABEDB-ENTRY (TABEDB-I)
                   SET TABEDB-I            UP BY 1
                   MOVE EDBTAB-IO-AREA (15:7) TO TABEDB-ENTRY
                                                            (TABEDB-I)
                   SET TABEDB-I            UP BY 1
                   MOVE EDBTAB-IO-AREA (22:7) TO TABEDB-ENTRY
                                                            (TABEDB-I)
                   SET TABEDB-I            UP BY 1
                   MOVE EDBTAB-IO-AREA (29:7) TO TABEDB-ENTRY
                                                            (TABEDB-I)
                   SET TABEDB-I            UP BY 1
                   MOVE EDBTAB-IO-AREA (36:7) TO TABEDB-ENTRY
                                                            (TABEDB-I)
                   SET TABEDB-I            UP BY 1
                   MOVE EDBTAB-IO-AREA (43:7) TO TABEDB-ENTRY
                                                            (TABEDB-I)
                   SET TABEDB-I            UP BY 1
                   MOVE EDBTAB-IO-AREA (50:7) TO TABEDB-ENTRY
                                                            (TABEDB-I)
                   SET TABEDB-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE EDBTAB.
 
       KHNTAB-LOAD SECTION.
       KHNTAB-LOAD-P.
           OPEN INPUT KHNTAB
           SET TABKHN-I                    TO 1
           PERFORM UNTIL KHNTAB-EOF
               READ KHNTAB
               AT END
                   SET KHNTAB-EOF          TO TRUE
               NOT AT END
                   MOVE KHNTAB-IO-AREA (1:3) TO TABKHN-ENTRY (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (4:3) TO TABKHN-ENTRY (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (7:3) TO TABKHN-ENTRY (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (10:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (13:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (16:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (19:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (22:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (25:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (28:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (31:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (34:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (37:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (40:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (43:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (46:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (49:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (52:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (55:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
                   MOVE KHNTAB-IO-AREA (58:3) TO TABKHN-ENTRY
                                                            (TABKHN-I)
                   SET TABKHN-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KHNTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-20)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE REC                    TO OUTFILE-IO-AREA (1:82)
               IF  (I-19)
                   MOVE KUHND              TO OUTFILE-IO-AREA (54:3)
               END-IF
               IF  (NOT-I-55)
                   MOVE ALFA               TO OUTFILE-IO-AREA (83:3)
               END-IF
               IF  (NOT-I-55)
                   MOVE ARTNR              TO OUTFILE-IO-AREA (86:20)
               END-IF
               IF  (I-23 AND I-27)
                   MOVE KUKAT              TO XO-30U
                   MOVE XO-30U (1:3)       TO OUTFILE-IO-AREA (108:3)
               END-IF
               IF  (I-24 AND I-27)
                   MOVE KUHND              TO OUTFILE-IO-AREA (108:3)
               END-IF
               IF  (I-U1 AND I-24 AND I-27)
                   MOVE HDIST              TO OUTFILE-IO-AREA (108:3)
               END-IF
               IF  (I-U2)
                   MOVE HDIST              TO OUTFILE-IO-AREA (108:3)
      *                U2      HDIST     56
               END-IF
               IF  (I-95)
                   IF KUKAT < 0
                     MOVE KUKAT            TO XO-30D
                     MOVE XO-30D (1:3)     TO OUTFILE-IO-AREA (108:3)
                   ELSE
                     MOVE KUKAT            TO XO-30U
                     MOVE XO-30U (1:3)     TO OUTFILE-IO-AREA (108:3)
                   END-IF
               END-IF
               IF  (I-94 AND I-27)
                   MOVE RESGRP             TO OUTFILE-IO-AREA (109:2)
      *ISTE   H  101   1P
      *                                  24 "------------------------"
      *                                  48 "------------------------"
      *                                  72 "------------------------"
      *                                  96 "------------------------"
      *                                 120 "------------------------"
      *                                 132 "------------"
      *       D  1     01 20
      *                        FIRMA      3
      *                        RESKNR    11
      *                        KUKAT X   15
      *                        KAT       20
               END-IF
               WRITE OUTFILE-IO-AREA
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
           PERFORM KUNTAB-LOAD
           PERFORM VRGTAB-LOAD
           PERFORM MNDTAB-LOAD
           PERFORM EDBTAB-LOAD
           PERFORM KHNTAB-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET SALGFIL-LEVEL-INIT          TO TRUE
           INITIALIZE SALGFIL-DATA-FIELDS
           SET SALGFIL-EOF-OFF             TO TRUE
           SET SALGFIL-PROCESS             TO TRUE
           OPEN INPUT SALGFIL
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT OUTFILE.
           SET TABKUN-I                    TO 1
           SET TABVRG-I                    TO 1
           SET TABMND-I                    TO 1
           SET TABEDB-I                    TO 1
           SET TABKHN-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE SALGFIL
           CLOSE VAREMAS
           CLOSE KUNDEMA
           CLOSE OUTFILE.
 
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
