       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB011R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM  NYB011                                                      *
      *   UTLISTING AV VARETILGANGSLISTE MED MANKO                             *
      *   RECORDS FRA VARETILGANGSFILE                                         *
      **************************************************************************
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB011.rpg
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
           SELECT TILFILE
               ASSIGN TO UT-S-TILFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILFILE-STATUS.
           SELECT NYEBEST
               ASSIGN TO NYEBEST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS NYEBEST-STATUS
               RECORD KEY IS NYEBEST-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
           SELECT OUTFIL2
               ASSIGN TO UT-S-OUTFIL2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL2-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TILFILE
               BLOCK CONTAINS 4050
               RECORD CONTAINS 150.
       01  TILFILE-IO-AREA.
           05  TILFILE-IO-AREA-X           PICTURE X(150).
       FD NYEBEST
               RECORD CONTAINS 150.
       01  NYEBEST-IO-AREA.
           05  NYEBEST-IO-AREA-X.
               10  NYEBEST-KEY1            PICTURE X(16).
               10  FILLER                  PICTURE X(134).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD OUTFIL
               BLOCK CONTAINS 9420
               RECORD CONTAINS 60.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(60).
       FD OUTFIL2
               BLOCK CONTAINS 6000
               RECORD CONTAINS 30.
       01  OUTFIL2-IO-AREA.
           05  OUTFIL2-IO-AREA-X           PICTURE X(30).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARV-MAX   VALUE 150             PICTURE 9(4) USAGE BINARY.
       77  ARB-MAX   VALUE 150             PICTURE 9(4) USAGE BINARY.
       77  ARU-MAX   VALUE 150             PICTURE 9(4) USAGE BINARY.
       77  ARC-MAX   VALUE 150             PICTURE 9(4) USAGE BINARY.
       77  ARW-MAX   VALUE 150             PICTURE 9(4) USAGE BINARY.
       77  ARD-MAX   VALUE 150             PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARV-TABLE.
               10  ARV-ENTRY
                                           OCCURS 150 TIMES
                                           INDEXED BY ARV-I
                                                      ARV-S
                                                      ARB-I
                                                      ARB-S.
                   15  ARV                 PICTURE X(5).
                   15  ARB                 PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  ARU-TABLE.
               10  ARU-ENTRY
                                           OCCURS 150 TIMES
                                           INDEXED BY ARU-I
                                                      ARU-S
                                                      ARC-I
                                                      ARC-S.
                   15  ARU                 PICTURE X(5).
                   15  ARC                 PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARW-TABLE.
               10  ARW-ENTRY
                                           OCCURS 150 TIMES
                                           INDEXED BY ARW-I
                                                      ARW-S
                                                      ARD-I
                                                      ARD-S.
                   15  ARW                 PICTURE X(5).
                   15  ARD                 PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TILFILE-STATUS              PICTURE 99 VALUE 0.
           10  NYEBEST-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
           10  OUTFIL2-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TILFILE-EOF-OFF         VALUE '0'.
               88  TILFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILFILE-READ-OFF        VALUE '0'.
               88  TILFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILFILE-PROCESS-OFF     VALUE '0'.
               88  TILFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  TILFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  TILFILE-LEVEL-INIT      VALUE '1'.
           05  NYEBEST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  TILFILE-LEVEL-01.
               10  TILFILE-01-L4.
                   15  TILFILE-01-L4-FIRMA PICTURE X(3).
               10  TILFILE-01-L3.
                   15  TILFILE-01-L3-AVD   PICTURE X(1).
               10  TILFILE-01-L2.
                   15  TILFILE-01-L2-TILGNR PICTURE X(5).
               10  TILFILE-01-L1.
                   15  TILFILE-01-L1-BESNR PICTURE S9(5).
           05  TILFILE-DATA-FIELDS.
               10  KEY-X                   PICTURE X(16).
               10  FIRMA                   PICTURE X(3).
               10  BESNR-IO.
                   15  BESNR               PICTURE S9(5).
               10  POSNR-IO.
                   15  POSNR               PICTURE S9(4).
               10  AVD                     PICTURE X(1).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BANT-IO.
                   15  BANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LANT-IO.
                   15  LANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PSEDD                   PICTURE X(6).
               10  IPRIS-IO.
                   15  IPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TEKST                   PICTURE X(1).
               10  TILGNR                  PICTURE X(5).
               10  LEV-IO.
                   15  LEV                 PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  LK                      PICTURE X(2).
           05  NYEBEST-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  VAREMAS-DATA-FIELDS.
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRIST-IO.
                   15  PRIST               PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAB-IO.
                   15  RAB                 PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  VGRP                    PICTURE X(5).
               10  LOCAT                   PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(5).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  KEY1                    PICTURE X(10).
               10  Y-IO.
                   15  Y                   PICTURE S9(3).
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(6).
               10  EDB1-IO.
                   15  EDB1                PICTURE S9(7).
               10  SKAF                    PICTURE X(1).
               10  NYBEH-IO.
                   15  NYBEH               PICTURE S9(7).
               10  REST-IO.
                   15  REST                PICTURE S9(6)V9(2).
               10  NYBANT-IO.
                   15  NYBANT              PICTURE S9(6)V9(2).
               10  NYLANT-IO.
                   15  NYLANT              PICTURE S9(6)V9(2).
               10  SUMPOS-IO.
                   15  SUMPOS              PICTURE S9(8)V9(2).
               10  SVSPOS-IO.
                   15  SVSPOS              PICTURE S9(8)V9(2).
               10  SUMTIL-IO.
                   15  SUMTIL              PICTURE S9(8)V9(2).
               10  SUMBES-IO.
                   15  SUMBES              PICTURE S9(8)V9(2).
               10  SUMFAK-IO.
                   15  SUMFAK              PICTURE S9(8)V9(2).
               10  SUMSVS-IO.
                   15  SUMSVS              PICTURE S9(8)V9(2).
               10  TILSVS-IO.
                   15  TILSVS              PICTURE S9(8)V9(2).
               10  TILBES-IO.
                   15  TILBES              PICTURE S9(8)V9(2).
               10  TILFAK-IO.
                   15  TILFAK              PICTURE S9(8)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(3).
           05  EDITTING-FIELDS.
               10  XO-62P-EF.
                 15  XO-62P                PICTURE S9(6)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-82P-EF.
                 15  XO-82P                PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-70YNZ                PICTURE ZZZZZZZ.
               10  XO-72YNZ                PICTURE ZZZZZZZ,ZZ.
               10  XO-62YNZR               PICTURE ZZZZZZ,ZZ-.
               10  XO-82YNZ                PICTURE ZZZZZZZZ,ZZ.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
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
           IF  TILFILE-PROCESS
               SET TILFILE-PROCESS-OFF     TO TRUE
               SET TILFILE-READ            TO TRUE
           END-IF
 
           IF  TILFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM TILFILE-GET
               SET TILFILE-READ-OFF        TO TRUE
               IF  NOT TILFILE-EOF
                   SET TILFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  TILFILE-PROCESS
               PERFORM TILFILE-IDSET
           END-IF
 
           IF  TILFILE-PROCESS
               PERFORM TILFILE-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  TILFILE-PROCESS
               PERFORM TILFILE-FLDOFF
               PERFORM TILFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  TILFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L4)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L4)
               MOVE 0                      TO SUMBES
               MOVE 0                      TO TILBES
               MOVE 0                      TO SUMSVS
               MOVE 0                      TO SUMFAK
               MOVE 0                      TO TILFAK
               MOVE 0                      TO TILSVS
               MOVE FIRMA                  TO KEY1 (1:3)
           END-IF
           IF  (I-L2)
               MOVE 0                      TO Y
               PERFORM VARYING ARV-I FROM 1 BY 1
                         UNTIL ARV-I > ARV-MAX
                   MOVE '     '            TO ARV (ARV-I)
               END-PERFORM
               PERFORM VARYING ARB-I FROM 1 BY 1
                         UNTIL ARB-I > ARB-MAX
                   MOVE 0                  TO ARB (ARB-I)
               END-PERFORM
               SET ARB-I                   TO 1
               PERFORM VARYING ARC-I FROM 1 BY 1
                         UNTIL ARC-I > ARC-MAX
                   MOVE 0                  TO ARC (ARC-I)
               END-PERFORM
               SET ARC-I                   TO 1
               PERFORM VARYING ARD-I FROM 1 BY 1
                         UNTIL ARD-I > ARD-MAX
                   MOVE 0                  TO ARD (ARD-I)
               END-PERFORM
               SET ARD-I                   TO 1
           END-IF
           ADD LEV TO ZERO             GIVING LEVNR
           SET NOT-I-15                    TO TRUE
           IF  LANT = 0
               SET I-15                    TO TRUE
           END-IF
           SET I-72                        TO TRUE
           IF  (I-15)
               GO TO SLUTT-T
      *
           END-IF
           ADD EDBNR TO ZERO           GIVING EDB1
           MOVE EDB1 (1:1)                 TO SKAF
           SET NOT-I-09                    TO TRUE
           IF  SKAF = '9'
               SET I-09                    TO TRUE
           END-IF
      *
           SET NOT-I-72                    TO TRUE
           IF  TEKST = 'T'
               SET I-72                    TO TRUE
           END-IF
           IF  (NOT-I-72)
               SET NOT-I-72                TO TRUE
               IF  TEKST = 'S'
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-72)
               GO TO SLUTT-T
      *
           END-IF
           MOVE EDB1                       TO KEY1 (4:7)
           MOVE KEY1                       TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-71                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-71                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (NOT-I-71)
               SUBTRACT ANTUT FROM ANTIN GIVING NYBEH
           END-IF
           MOVE KEY-X                      TO NYEBEST-KEY1
           READ NYEBEST RECORD KEY IS NYEBEST-KEY1
           INVALID KEY
               SET I-72                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-72                TO TRUE
               PERFORM NYEBEST-IDSET
           END-READ
           SUBTRACT LANT FROM BANT     GIVING REST
           SET NOT-I-73                    TO TRUE
           IF  REST NOT > 0
               SET I-73                    TO TRUE
           END-IF
           IF  (I-73)
               MOVE 0                      TO NYBANT
           END-IF
           IF  (NOT-I-73)
               ADD REST TO ZERO        GIVING NYBANT
           END-IF
           MOVE 0                          TO NYLANT
      *
           SET NOT-I-60                    TO TRUE
           IF  IPRIS = 0
               SET I-60                    TO TRUE
           END-IF
           MULTIPLY IPRIS BY LANT      GIVING SUMPOS
           IF  (I-60)
               MULTIPLY SELVK BY LANT  GIVING SUMPOS
           END-IF
           MULTIPLY SELVK BY LANT      GIVING SVSPOS
           MULTIPLY PRIST BY LANT      GIVING SUMTIL
           ADD SUMPOS                      TO SUMBES
           ADD SUMTIL                      TO SUMBES
           ADD SUMPOS                      TO SUMFAK
           ADD SUMTIL                      TO SUMFAK
           ADD SVSPOS                      TO SUMSVS
           ADD SUMTIL                      TO SUMSVS
           ADD SVSPOS                      TO TILSVS
           ADD SUMTIL                      TO TILSVS
           ADD SUMTIL                      TO TILBES
           ADD SUMTIL                      TO TILFAK
      ******************************************************
      *       RUTINE FOR ARRAY TOTALSUMMERING PR. VGR.     *
      ******************************************************
           MOVE 1                          TO X
           SET NOT-I-40                    TO TRUE
           SET ARV-S                       TO 1
           PERFORM WITH TEST AFTER
                   VARYING ARV-I FROM X BY 1
                     UNTIL ARV-I >= ARV-MAX
                        OR I-40
               IF  VGRP = ARV (ARV-I)
                   SET I-40                TO TRUE
                   SET ARV-S               TO ARV-I
               END-IF
           END-PERFORM
           SET X                           TO ARV-S
           IF  (NOT-I-40)
               ADD 1                       TO Y
               SET NOT-I-41                TO TRUE
               IF  Y = 50
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-41)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-40)
               ADD Y TO ZERO           GIVING X
               MOVE 1                      TO MOVEA-SA1
               COMPUTE MOVEA-SA2 = 5 * ( X - 1 ) + 1
               MOVE 5                      TO MOVEA-SIZE1
               COMPUTE MOVEA-SIZE2 = ARV-MAX * 5 - MOVEA-SA2 + 1
               IF  MOVEA-SIZE1 > MOVEA-SIZE2
                   MOVE MOVEA-SIZE2        TO MOVEA-SIZE1
               END-IF
               MOVE VGRP
                        TO ARV-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           END-IF
           ADD SUMPOS                      TO ARB (X)
           ADD SUMTIL                      TO ARB (X)
           ADD SUMTIL                      TO ARC (X)
           ADD SVSPOS                      TO ARD (X)
           ADD SUMTIL                      TO ARD (X).
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           SET NOT-I-35                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'BES04'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'NYB011  '                 TO LPROG
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
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-15)
               GO TO VGREND-T
           END-IF
           IF  (I-L1 AND NOT-I-35)
               SET I-46                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-46                TO TRUE
           END-IF
           IF  (I-L1)
               MOVE 0                      TO SUMBES
               MOVE 0                      TO TILBES
               MOVE 0                      TO SUMSVS
           END-IF
           IF  (I-L2 AND NOT-I-35)
               SET I-47                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-47                TO TRUE
           END-IF
           IF  (I-L2)
               SET I-48                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-48                TO TRUE
               MOVE 0                      TO SUMFAK
               MOVE 0                      TO TILFAK
               MOVE 0                      TO TILSVS
               SET NOT-I-42                TO TRUE
               IF  Y = 1
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-42)
               GO TO VGREND-T
           END-IF
           IF  (I-L2)
               MOVE 1                      TO X
           END-IF.
 
       VGLOOP-T.
           IF  (I-L2 AND NOT-I-35)
               SET I-45                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-45                TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-43                TO TRUE
               IF  X < Y
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-43)
               ADD 1                       TO X
               GO TO VGLOOP-T
           END-IF.
 
       VGREND-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       TILFILE-GET SECTION.
       TILFILE-GET-P.
           IF  TILFILE-EOF-OFF
               READ TILFILE
               AT END
                   SET TILFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TILFILE-FLDOFF SECTION.
       TILFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       TILFILE-FLDSET SECTION.
       TILFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE TILFILE-IO-AREA (1:16) TO KEY-X (1:16)
               MOVE TILFILE-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE TILFILE-IO-AREA (5:5)  TO BESNR-IO
               INSPECT BESNR-IO REPLACING ALL ' ' BY '0'
               MOVE TILFILE-IO-AREA (11:4) TO POSNR-IO
               INSPECT POSNR-IO REPLACING ALL ' ' BY '0'
               MOVE TILFILE-IO-AREA (17:1) TO AVD (1:1)
               MOVE TILFILE-IO-AREA (22:3) TO ALFA (1:3)
               MOVE TILFILE-IO-AREA (25:20) TO ARTNR (1:20)
               MOVE TILFILE-IO-AREA (75:4) TO EDBNR-IO
               MOVE TILFILE-IO-AREA (79:5) TO PRIS-IO
               IF  PRIS = ZERO
                   SET I-09                TO TRUE
               END-IF
               MOVE TILFILE-IO-AREA (89:5) TO BANT-IO
               MOVE TILFILE-IO-AREA (99:5) TO LANT-IO
               MOVE TILFILE-IO-AREA (104:6) TO PSEDD (1:6)
               MOVE TILFILE-IO-AREA (143:5) TO IPRIS-IO
               MOVE TILFILE-IO-AREA (124:1) TO TEKST (1:1)
               MOVE TILFILE-IO-AREA (135:5) TO TILGNR (1:5)
               MOVE TILFILE-IO-AREA (131:4) TO LEV-IO
               MOVE TILFILE-IO-AREA (140:2) TO LK (1:2)
           END-EVALUATE.
 
       TILFILE-IDSET SECTION.
       TILFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       TILFILE-CHK-LEVEL SECTION.
       TILFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO TILFILE-LEVEL-01
               MOVE TILFILE-IO-AREA (2:3)  TO TILFILE-01-L4-FIRMA
               MOVE TILFILE-IO-AREA (17:1) TO TILFILE-01-L3-AVD
               MOVE TILFILE-IO-AREA (135:5) TO TILFILE-01-L2-TILGNR
               MOVE TILFILE-IO-AREA (5:5)  TO TILFILE-01-L1-BESNR
               IF  TILFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  TILFILE-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  TILFILE-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  TILFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  TILFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  TILFILE-01-L4         TO THE-PRIOR-L4
               MOVE  TILFILE-01-L3         TO THE-PRIOR-L3
               MOVE  TILFILE-01-L2         TO THE-PRIOR-L2
               MOVE  TILFILE-01-L1         TO THE-PRIOR-L1
               SET TILFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       NYEBEST-IDSET SECTION.
       NYEBEST-IDSET-P.
           SET I-03                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (97:5) TO ANTIN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO ANTUT-IO
               MOVE VAREMAS-IO-AREA (161:4) TO PRIST-IO
               MOVE VAREMAS-IO-AREA (153:2) TO RAB-IO
               MOVE VAREMAS-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (118:5) TO VGRP (1:5)
               MOVE VAREMAS-IO-AREA (140:6) TO LOCAT (1:6)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-04                        TO TRUE.
 
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
           IF  (I-01 AND NOT-I-72 AND I-U1)
               MOVE NYBANT                 TO XO-62P
               MOVE XO-62P-EF              TO NYEBEST-IO-AREA (89:5)
               MOVE NYLANT                 TO XO-62P
               MOVE XO-62P-EF              TO NYEBEST-IO-AREA (99:5)
               REWRITE NYEBEST-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = NYEBEST'
               END-REWRITE
           END-IF
           IF  (I-01)
               MOVE SPACES TO OUTFIL2-IO-AREA
               INITIALIZE OUTFIL2-IO-AREA
               MOVE FIRMA                  TO OUTFIL2-IO-AREA (1:3)
               MOVE VGRP                   TO OUTFIL2-IO-AREA (4:5)
               MOVE LK                     TO OUTFIL2-IO-AREA (9:2)
               MOVE SUMPOS                 TO XO-82P
               MOVE XO-82P-EF              TO OUTFIL2-IO-AREA (11:6)
               MOVE UYEAR                  TO OUTFIL2-IO-AREA (17:2)
               MOVE UMONTH                 TO OUTFIL2-IO-AREA (19:2)
               MOVE UDAY                   TO OUTFIL2-IO-AREA (21:2)
               MOVE AVD                    TO OUTFIL2-IO-AREA (23:1)
               WRITE OUTFIL2-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BESNR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (1:5)
               MOVE POSNR                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (7:4)
               MOVE LK                     TO LISTE-IO-AREA (12:2)
               MOVE ALFA                   TO LISTE-IO-AREA (15:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (19:20)
               IF  (I-09)
                   MOVE 'SKAFFEVARE'       TO LISTE-IO-AREA (40:10)
               END-IF
               IF  (NOT-I-09)
                   MOVE NYBEH              TO XO-70YNZ
                   MOVE XO-70YNZ           TO LISTE-IO-AREA (43:7)
               END-IF
               MOVE BANT                   TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (50:10)
               MOVE LANT                   TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (60:10)
               MOVE REST                   TO XO-62YNZR
               MOVE XO-62YNZR              TO LISTE-IO-AREA (70:10)
               MOVE SELVK                  TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (81:10)
               INITIALIZE SELVK
               MOVE IPRIS                  TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (92:10)
               INITIALIZE IPRIS
               MOVE SUMPOS                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (108:11)
               INITIALIZE SUMPOS
               MOVE LOCAT                  TO LISTE-IO-AREA (120:6)
               MOVE VGRP                   TO LISTE-IO-AREA (128:5)
      *       T 11     L1N35NL2
      *                                  97 "TOTALT PR BESTILLINGSNR."
      *                        BESNR Z  105
      *                        SUMBES4B 118
      *                                 120 "*"
      *       T 11     L2N35NL2
      *                                  97 "TOTALT PR TILGANGSNUMMER"
      *                        TILGNR   105
      *                        SUMFAK4B 118
      *                                 121 "**"
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-46)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT PR BESTILLINGSNR.' TO LISTE-IO-AREA (1:24)
               MOVE BESNR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (27:5)
               MOVE 'VERDI INNM. SELVKOST' TO LISTE-IO-AREA (34:20)
               MOVE SUMBES                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (55:11)
               INITIALIZE SUMBES
               MOVE 'VERDI REGISTER SVS  ' TO LISTE-IO-AREA (69:20)
               MOVE SUMSVS                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (90:11)
               INITIALIZE SUMSVS
               MOVE ' - I PRISTILLEGG'     TO LISTE-IO-AREA (104:16)
               MOVE TILBES                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (122:11)
               INITIALIZE TILBES
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-47)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT PR TILGANGSNUMMER' TO LISTE-IO-AREA (2:24)
               MOVE TILGNR                 TO LISTE-IO-AREA (27:5)
               MOVE 'VERDI INNM. SELVKOST' TO LISTE-IO-AREA (34:20)
               MOVE SUMFAK                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (55:11)
               MOVE 'VERDI REGISTER SVS  ' TO LISTE-IO-AREA (69:20)
               MOVE TILSVS                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (90:11)
               MOVE ' - I PRISTILLEGG'     TO LISTE-IO-AREA (104:16)
               MOVE TILFAK                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (122:11)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-45)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL FOR VAREGRUPPE' TO LISTE-IO-AREA (6:20)
               MOVE ARV (X)                TO LISTE-IO-AREA (27:5)
               MOVE 'VERDI INNMELDT SVS  ' TO LISTE-IO-AREA (34:20)
               MOVE ARB (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (52:14)
               MOVE 'VERDI REGISTER SVS  ' TO LISTE-IO-AREA (69:20)
               MOVE ARD (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (87:14)
               MOVE '** I PRISTILLEGG'     TO LISTE-IO-AREA (104:16)
               MOVE ARC (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-48)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '2'                    TO OUTFIL-IO-AREA (1:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (2:3)
               MOVE TILGNR                 TO OUTFIL-IO-AREA (5:5)
               MOVE SUMFAK-IO              TO OUTFIL-IO-AREA (10:10)
               INITIALIZE SUMFAK-IO
               MOVE UDATE                  TO OUTFIL-IO-AREA (20:6)
               MOVE TILSVS-IO              TO OUTFIL-IO-AREA (26:10)
               INITIALIZE TILSVS-IO
               MOVE LEVNR-IO               TO OUTFIL-IO-AREA (55:6)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'V A R E T I L G A N G S' TO LISTE-IO-AREA (38:23)
               MOVE 'L I S T E'            TO LISTE-IO-AREA (62:9)
               MOVE 'DATO:'                TO LISTE-IO-AREA (75:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (81:8)
               MOVE 'AVD:'                 TO LISTE-IO-AREA (90:4)
               MOVE AVD                    TO LISTE-IO-AREA (95:1)
               MOVE 'LK:'                  TO LISTE-IO-AREA (97:3)
               MOVE LK                     TO LISTE-IO-AREA (101:2)
               MOVE 'ORNR:'                TO LISTE-IO-AREA (104:5)
               MOVE PSEDD                  TO LISTE-IO-AREA (110:6)
               MOVE 'TILG.NR:'             TO LISTE-IO-AREA (119:8)
               MOVE TILGNR                 TO LISTE-IO-AREA (128:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 'LEV.NR :'             TO LISTE-IO-AREA (31:8)
               MOVE LEVNR-IO               TO LISTE-IO-AREA (43:6)
               MOVE '  **********************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEST. POS.'           TO LISTE-IO-AREA (1:10)
               MOVE 'A R T I K K'          TO LISTE-IO-AREA (16:11)
               MOVE 'E L N A V N   NY LAGER-' TO LISTE-IO-AREA (28:23)
               MOVE 'BEST    LEVERT'       TO LISTE-IO-AREA (54:14)
               MOVE 'REST   REGIST.  INNM.' TO LISTE-IO-AREA (77:21)
               MOVE 'SUM'                  TO LISTE-IO-AREA (110:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.   NR.  LK.'       TO LISTE-IO-AREA (1:14)
               MOVE 'ALFA     BEN'         TO LISTE-IO-AREA (16:12)
               MOVE 'EVNELSE       BEHOLDNING' TO LISTE-IO-AREA (28:24)
               MOVE 'ANTALL  ANTALL'       TO LISTE-IO-AREA (54:14)
               MOVE '  OVERSK. SVSPRIS'    TO LISTE-IO-AREA (74:17)
               MOVE 'SVSPRIS'              TO LISTE-IO-AREA (93:7)
               MOVE 'SELVKOST   LOC     VGR  ' TO LISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2 AND NOT-I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'V A R E T I L G A N G S' TO LISTE-IO-AREA (38:23)
               MOVE 'L I S T E'            TO LISTE-IO-AREA (62:9)
               MOVE 'DATO:'                TO LISTE-IO-AREA (75:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (81:8)
               MOVE 'AVD:'                 TO LISTE-IO-AREA (90:4)
               MOVE AVD                    TO LISTE-IO-AREA (95:1)
               MOVE 'LK:'                  TO LISTE-IO-AREA (97:3)
               MOVE LK                     TO LISTE-IO-AREA (101:2)
               MOVE 'ORNR:'                TO LISTE-IO-AREA (104:5)
               MOVE PSEDD                  TO LISTE-IO-AREA (110:6)
               MOVE 'TILG.NR:'             TO LISTE-IO-AREA (119:8)
               MOVE TILGNR                 TO LISTE-IO-AREA (128:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 'LEV.NR :'             TO LISTE-IO-AREA (31:8)
               MOVE LEVNR-IO               TO LISTE-IO-AREA (43:6)
               MOVE '  **********************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEST. POS.'           TO LISTE-IO-AREA (1:10)
               MOVE 'A R T I K K'          TO LISTE-IO-AREA (16:11)
               MOVE 'E L N A V N   NY LAGER-' TO LISTE-IO-AREA (28:23)
               MOVE 'BEST    LEVERT'       TO LISTE-IO-AREA (54:14)
               MOVE 'REST   REGIST.  INNM.' TO LISTE-IO-AREA (77:21)
               MOVE 'SUM'                  TO LISTE-IO-AREA (110:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.   NR.  LK.'       TO LISTE-IO-AREA (1:14)
               MOVE 'ALFA     BEN'         TO LISTE-IO-AREA (16:12)
               MOVE 'EVNELSE       BEHOLDNING' TO LISTE-IO-AREA (28:24)
               MOVE 'ANTALL  ANTALL'       TO LISTE-IO-AREA (54:14)
               MOVE '  OVERSK. SVSPRIS'    TO LISTE-IO-AREA (74:17)
               MOVE 'SVSPRIS'              TO LISTE-IO-AREA (93:7)
               MOVE 'SELVKOST   LOC     VGR  ' TO LISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           SET TILFILE-LEVEL-INIT          TO TRUE
           INITIALIZE TILFILE-DATA-FIELDS
           SET TILFILE-EOF-OFF             TO TRUE
           SET TILFILE-PROCESS             TO TRUE
           OPEN INPUT TILFILE
           OPEN I-O NYEBEST
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTFIL
           OPEN OUTPUT OUTFIL2
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARV-I FROM 1 BY 1
                     UNTIL ARV-I > ARV-MAX
               INITIALIZE ARV (ARV-I)
               INITIALIZE ARB (ARV-I)
           END-PERFORM
           SET ARV-I                       TO 1
           PERFORM VARYING ARU-I FROM 1 BY 1
                     UNTIL ARU-I > ARU-MAX
               INITIALIZE ARU (ARU-I)
               INITIALIZE ARC (ARU-I)
           END-PERFORM
           SET ARU-I                       TO 1
           PERFORM VARYING ARW-I FROM 1 BY 1
                     UNTIL ARW-I > ARW-MAX
               INITIALIZE ARW (ARW-I)
               INITIALIZE ARD (ARW-I)
           END-PERFORM
           SET ARW-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILFILE
           CLOSE NYEBEST
           CLOSE VAREMAS
           CLOSE OUTFIL
           CLOSE OUTFIL2
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
