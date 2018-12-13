       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB001R.
      **********************************************  Z-WIN-RPG2   ****
      *  LISTER UT BESTILLINGER OG HVILKE TILGANGER SOM ER REG. *
      **********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB001.rpg
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
           SELECT NYEBEST
               ASSIGN TO NYEBEST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS NYEBEST-STATUS
               RECORD KEY IS NYEBEST-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT BEST
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BEST-STATUS.
           SELECT TILGANG
               ASSIGN TO UT-S-TILGANG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILGANG-STATUS.
           SELECT INKFIL
               ASSIGN TO UT-S-INKFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INKFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
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
       FD BEST
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  BEST-IO-PRINT.
           05  BEST-IO-AREA-CONTROL        PICTURE X VALUE ' '.
        02 BEST-IO-AREA.
           05  BEST-IO-AREA-X              PICTURE X(132).
       FD TILGANG
               BLOCK CONTAINS 4050
               RECORD CONTAINS 150.
       01  TILGANG-IO-AREA.
           05  TILGANG-IO-AREA-X           PICTURE X(150).
       FD INKFIL
               BLOCK CONTAINS 4050
               RECORD CONTAINS 75.
       01  INKFIL-IO-AREA.
           05  INKFIL-IO-AREA-X            PICTURE X(75).
       WORKING-STORAGE SECTION.
       77  ARV-MAX   VALUE 500             PICTURE 9(4) USAGE BINARY.
       77  ARB-MAX   VALUE 500             PICTURE 9(4) USAGE BINARY.
       77  ARU-MAX   VALUE 500             PICTURE 9(4) USAGE BINARY.
       77  ARC-MAX   VALUE 500             PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARV-TABLE.
               10  ARV-ENTRY
                                           OCCURS 500 TIMES
                                           INDEXED BY ARV-I
                                                      ARV-S
                                                      ARB-I
                                                      ARB-S.
                   15  ARV                 PICTURE X(5).
                   15  ARB                 PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARU-TABLE.
               10  ARU-ENTRY
                                           OCCURS 500 TIMES
                                           INDEXED BY ARU-I
                                                      ARU-S
                                                      ARC-I
                                                      ARC-S.
                   15  ARU                 PICTURE X(5).
                   15  ARC                 PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  NYEBEST-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  BEST-STATUS                 PICTURE 99 VALUE 0.
           10  TILGANG-STATUS              PICTURE 99 VALUE 0.
           10  INKFIL-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  NYEBEST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-EOF-OFF         VALUE '0'.
               88  NYEBEST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-READ-OFF        VALUE '0'.
               88  NYEBEST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEBEST-PROCESS-OFF     VALUE '0'.
               88  NYEBEST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  NYEBEST-LEVEL-INIT-OFF  VALUE '0'.
               88  NYEBEST-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  BEST-DATA-FIELDS.
               10  BEST-AFTER-SPACE        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BEST-AFTER-SKIP         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BEST-BEFORE-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BEST-BEFORE-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BEST-MAX-LINES          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BEST-LINE-COUNT         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BEST-CLR-IO             PICTURE X VALUE 'Y'.
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
           05  NYEBEST-LEVEL-01.
               10  NYEBEST-01-L4.
                   15  NYEBEST-01-L4-FIRMA PICTURE X(3).
               10  NYEBEST-01-L2.
                   15  NYEBEST-01-L2-BESNR PICTURE S9(5).
           05  NYEBEST-DATA-FIELDS.
               10  RECA                    PICTURE X(150).
               10  REC                     PICTURE X(150).
               10  RA                      PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  BESNR-IO.
                   15  BESNR               PICTURE S9(5).
               10  POS-X-IO.
                   15  POS-X               PICTURE S9(4).
               10  STATUS-X                PICTURE X(1).
               10  NAVN                    PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  PNR                     PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  VADR1                   PICTURE X(30).
               10  VADR2                   PICTURE X(30).
               10  VADR3                   PICTURE X(30).
               10  VADR4                   PICTURE X(30).
               10  PSEDD                   PICTURE X(6).
               10  SIGN-X                  PICTURE X(2).
               10  BS1                     PICTURE X(1).
               10  BS2                     PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  LEVUKE                  PICTURE X(2).
               10  LEVA-ELGR               PICTURE X(2).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VTYP                    PICTURE X(1).
               10  BANT-IO.
                   15  BANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LANT-IO.
                   15  LANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TEKST                   PICTURE X(1).
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BSTAR                   PICTURE X(1).
               10  TSTAR                   PICTURE X(1).
               10  RECART                  PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  VGRP                    PICTURE X(5).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  U-IO.
                   15  U                   PICTURE S9(3).
               10  GTOTAF-IO.
                   15  GTOTAF              PICTURE S9(8)V9(2).
               10  Y-IO.
                   15  Y                   PICTURE S9(3).
               10  SUMBES-IO.
                   15  SUMBES              PICTURE S9(7)V9(2).
               10  ANTLIN-IO.
                   15  ANTLIN              PICTURE S9(2).
               10  GTOTAL-IO.
                   15  GTOTAL              PICTURE S9(8)V9(2).
               10  NAVN1                   PICTURE X(30).
               10  ADR1                    PICTURE X(30).
               10  PNR1                    PICTURE X(4).
               10  PSTED1                  PICTURE X(16).
               10  SIGN1                   PICTURE X(2).
               10  BSA                     PICTURE X(1).
               10  BSB                     PICTURE X(1).
               10  ORNR                    PICTURE X(6).
               10  VARE1                   PICTURE X(30).
               10  VARE2                   PICTURE X(30).
               10  VARE3                   PICTURE X(30).
               10  VARE4                   PICTURE X(30).
               10  LEVRNR-IO.
                   15  LEVRNR              PICTURE S9(6).
               10  LEVNR-N-IO.
                   15  LEVNR-N             PICTURE S9(7).
               10  LUKE                    PICTURE X(2).
               10  LA-ELGR                 PICTURE X(2).
               10  AVD1                    PICTURE X(1).
               10  PRIS1-IO.
                   15  PRIS1               PICTURE S9(5)V9(2).
               10  PRIS-N-IO.
                   15  PRIS-N              PICTURE S9(7)V9(2).
               10  EDBNR1-IO.
                   15  EDBNR1              PICTURE S9(7).
               10  VARKEY                  PICTURE X(10).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  SKAFF                   PICTURE X(1).
               10  X-IO.
                   15  X                   PICTURE S9(3).
               10  V-IO.
                   15  V                   PICTURE S9(3).
               10  DTOTAL-IO.
                   15  DTOTAL              PICTURE S9(7)V9(2).
               10  SUMPOS-IO.
                   15  SUMPOS              PICTURE S9(7)V9(2).
               10  SUMFAK-IO.
                   15  SUMFAK              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-72YYZ                PICTURE Z.ZZZ.ZZZ,ZZ.
               10  XO-52YYZ                PICTURE ZZ.ZZZ,ZZ.
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  NYEBEST-PROCESS
               SET NYEBEST-PROCESS-OFF     TO TRUE
               SET NYEBEST-READ            TO TRUE
           END-IF
 
           IF  NYEBEST-READ
           AND RECORD-SELECTED-OFF
               PERFORM NYEBEST-GET
               SET NYEBEST-READ-OFF        TO TRUE
               IF  NOT NYEBEST-EOF
                   SET NYEBEST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-IDSET
           END-IF
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-CHK-LEVEL
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
 
           IF  NYEBEST-PROCESS
               PERFORM NYEBEST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  NYEBEST-PROCESS
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
               SET NOT-I-56                TO TRUE
               SET NOT-I-55                TO TRUE
               SET NOT-I-91                TO TRUE
               IF  FIRMA = '915'
                   SET I-91                TO TRUE
               END-IF
               MOVE 0                      TO U
               PERFORM VARYING ARU-I FROM 1 BY 1
                         UNTIL ARU-I > ARU-MAX
                   MOVE '     '            TO ARU (ARU-I)
               END-PERFORM
               PERFORM VARYING ARC-I FROM 1 BY 1
                         UNTIL ARC-I > ARC-MAX
                   MOVE 0                  TO ARC (ARC-I)
               END-PERFORM
               SET ARC-I                   TO 1
               MOVE 0                      TO GTOTAF
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-70                TO TRUE
               IF  STATUS-X = 'A'
                   SET I-70                TO TRUE
               END-IF
               SET NOT-I-71                TO TRUE
               IF  STATUS-X = 'B'
                   SET I-71                TO TRUE
               END-IF
               SET NOT-I-72                TO TRUE
               IF  STATUS-X = 'C'
                   SET I-72                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  STATUS-X = 'D'
                   SET I-73                TO TRUE
               END-IF
               SET NOT-I-74                TO TRUE
               IF  STATUS-X = 'E'
                   SET I-74                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  STATUS-X = 'F'
                   SET I-75                TO TRUE
               END-IF
      *
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
               MOVE 0                      TO SUMBES
               MOVE 0                      TO ANTLIN
               MOVE 0                      TO GTOTAL
               SET NOT-I-65                TO TRUE
               SET NOT-I-66                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-28                TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-02                    TO TRUE
           IF  RA = '9'
               SET I-02                    TO TRUE
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
      ************************************************
      * HER SKAL DET PLUKKES UT TIL KVITTERINGS-     *
      * LISTER FOR BESTILLINGER OG TILGANGER.        *
      ************************************************
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  RECART = '1'
               SET I-30                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  RECART = '2'
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           SET NOT-I-32                    TO TRUE
           IF  RECART > '3'
               SET I-33                    TO TRUE
           END-IF
           IF  RECART = '3'
               SET I-32                    TO TRUE
           END-IF
      **
           IF  (I-70)
               OR  (I-71)
               GO TO SLUTT-T
           END-IF
           IF  (I-32)
               GO TO SLUTT-T
      **
           END-IF
           IF  (I-30)
               MOVE NAVN                   TO NAVN1
               MOVE ADR                    TO ADR1
               MOVE PNR                    TO PNR1
               MOVE PSTED                  TO PSTED1 (2:15)
               MOVE SIGN-X                 TO SIGN1
               MOVE BS1                    TO BSA
               MOVE BS2                    TO BSB
               MOVE PSEDD                  TO ORNR
               GO TO SLUTT-T
      **
           END-IF
           IF  (I-31)
               MOVE VADR1                  TO VARE1
               MOVE VADR2                  TO VARE2
               MOVE VADR3                  TO VARE3
               MOVE VADR4                  TO VARE4
               GO TO SLUTT-T
      **
           END-IF
           IF  (I-33)
               MOVE LEVNR                  TO LEVNR-N
               MOVE LEVNR-N-IO (2:6)       TO LEVRNR-IO
               MOVE LEVUKE                 TO LUKE
               MOVE LEVA-ELGR              TO LA-ELGR
               MOVE AVD                    TO AVD1
               MOVE PRIS                   TO PRIS-N
               MOVE PRIS-N-IO (3:7)        TO PRIS1-IO
               SET NOT-I-31                TO TRUE
               IF  TEKST = 'T'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  TEKST = 'S'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-31)
               GO TO SLUTT-T
           END-IF
           IF  (I-33)
               SET NOT-I-24                TO TRUE
               IF  PRIS = 0
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-21                TO TRUE
               IF  BSTAR = '*'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  TSTAR = '*'
                   SET I-22                TO TRUE
               END-IF
      ***
           END-IF
           IF  (I-33)
               ADD EDBNR TO ZERO       GIVING EDBNR1
           END-IF
           IF  (I-33 AND I-21)
               MOVE FIRMA                  TO VARKEY (1:3)
               MOVE EDBNR                  TO EDBNR-N
               MOVE EDBNR-N-IO             TO VARKEY (4:7)
               MOVE VARKEY                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-52                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-52            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
      **
           END-IF
           IF  (I-33 AND I-21)
               MOVE EDBNR1 (1:1)           TO SKAFF
      **
           END-IF
           IF  (I-33 AND I-21)
               SET NOT-I-51                TO TRUE
               IF  SKAFF = '9'
                   SET I-51                TO TRUE
               END-IF
      **
           END-IF
           IF  (I-33 AND I-21)
               PERFORM BESRUT-S
           END-IF
           IF  (I-33 AND I-21)
               SET I-23                    TO TRUE
           END-IF.
 
       SLUTT-T.
      **
           CONTINUE.
 
       BESRUT-S SECTION.
       BESRUT-S-P.
           IF  (NOT-I-56)
               PERFORM RBSRUT-S
      *
           END-IF
           SET I-56                        TO TRUE
           IF  (NOT-I-65)
               SET I-60                    TO TRUE
               SET I-65                    TO TRUE
           END-IF
           ADD 1                           TO ANTLIN
           SET NOT-I-62                    TO TRUE
           IF  ANTLIN = 31
               SET I-62                    TO TRUE
           END-IF
           IF  (I-62)
               MOVE 0                      TO ANTLIN
               SET I-60                    TO TRUE
           END-IF
           IF  (I-60)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           SET NOT-I-60                    TO TRUE
           SET NOT-I-62                    TO TRUE
      *
           SET NOT-I-50                    TO TRUE
           IF  VTYP = '0'
               SET I-50                    TO TRUE
           END-IF
           IF  (NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  VTYP = ' '
                   SET I-50                TO TRUE
               END-IF
      *
           END-IF
           IF  (NOT-I-52)
               MULTIPLY SELVK BY BANT  GIVING DTOTAL
           END-IF
           ADD DTOTAL                      TO GTOTAL
           ADD DTOTAL                      TO GTOTAF
      *
           MULTIPLY SELVK BY LANT      GIVING SUMPOS
           ADD SUMPOS                      TO SUMBES
           ADD SUMPOS                      TO SUMFAK
      ******************************************************
      *    SUBRUTINE FOR ARRAY TOTALSUMMERING PR. VGR.     *
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
               IF  Y = 199
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-41)
               GO TO END-X-T
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
           ADD DTOTAL                      TO ARB (X)
      ******************************************************
      *    SUBRUTINE FOR ARRAY TOTALSUMMERING PR. VGR. TOT *
      ******************************************************
           MOVE 1                          TO V
           SET NOT-I-40                    TO TRUE
           SET ARU-S                       TO 1
           PERFORM WITH TEST AFTER
                   VARYING ARU-I FROM V BY 1
                     UNTIL ARU-I >= ARU-MAX
                        OR I-40
               IF  VGRP = ARU (ARU-I)
                   SET I-40                TO TRUE
                   SET ARU-S               TO ARU-I
               END-IF
           END-PERFORM
           SET V                           TO ARU-S
           IF  (NOT-I-40)
               ADD 1                       TO U
               SET NOT-I-41                TO TRUE
               IF  U = 199
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND I-41)
               GO TO END-X-T
           END-IF
           IF  (NOT-I-40)
               ADD U TO ZERO           GIVING V
               MOVE 1                      TO MOVEA-SA1
               COMPUTE MOVEA-SA2 = 5 * ( V - 1 ) + 1
               MOVE 5                      TO MOVEA-SIZE1
               COMPUTE MOVEA-SIZE2 = ARU-MAX * 5 - MOVEA-SA2 + 1
               IF  MOVEA-SIZE1 > MOVEA-SIZE2
                   MOVE MOVEA-SIZE2        TO MOVEA-SIZE1
               END-IF
               MOVE VGRP
                        TO ARU-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           END-IF
           ADD DTOTAL                      TO ARC (V).
 
       END-X-T.
           CONTINUE.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'BES02'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'NYB001  '                 TO LPROG
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
           IF  (I-L0 AND NOT-I-23)
               GO TO BUNN-T
           END-IF
           IF  (I-L0 AND I-70)
               GO TO BUNN-T
           END-IF
           IF  (I-L2)
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
           IF  (I-L2)
               SET I-45                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-45                TO TRUE
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
           IF  (I-L2)
               SET I-46                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-46                TO TRUE
           END-IF.
 
       BUNN-T.
           IF  (I-L0 AND I-70)
               GO TO BUNF-T
           END-IF
           IF  (I-L4)
               SET NOT-I-42                TO TRUE
               IF  U = 1
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND I-42)
               GO TO VGRENF-T
           END-IF
           IF  (I-L4)
               MOVE 1                      TO V
           END-IF.
 
       VGLOOF-T.
           IF  (I-L4)
               SET I-47                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-47                TO TRUE
               SET NOT-I-43                TO TRUE
               IF  V < U
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND I-43)
               ADD 1                       TO V
               GO TO VGLOOF-T
           END-IF.
 
       VGRENF-T.
           IF  (I-L4)
               SET I-48                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-48                TO TRUE
           END-IF.
 
       BUNF-T.
      * ** ** ** ** SUBRUTINER  ** ** ** ** ** ** ** ** ** ** ** **
           CONTINUE.
 
       NYEBEST-GET SECTION.
       NYEBEST-GET-P.
           IF  NYEBEST-EOF-OFF
               READ NYEBEST
               AT END
                   SET NYEBEST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYEBEST-FLDSET SECTION.
       NYEBEST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEBEST-IO-AREA (1:150) TO RECA (1:150)
               MOVE NYEBEST-IO-AREA (1:150) TO REC (1:150)
               MOVE NYEBEST-IO-AREA (5:1)  TO RA (1:1)
               MOVE NYEBEST-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE NYEBEST-IO-AREA (5:5)  TO BESNR-IO
               INSPECT BESNR-IO REPLACING ALL ' ' BY '0'
               MOVE NYEBEST-IO-AREA (11:4) TO POS-X-IO
               INSPECT POS-X-IO REPLACING ALL ' ' BY '0'
               MOVE NYEBEST-IO-AREA (17:1) TO STATUS-X (1:1)
               MOVE NYEBEST-IO-AREA (18:30) TO NAVN (1:30)
               MOVE NYEBEST-IO-AREA (48:30) TO ADR (1:30)
               MOVE NYEBEST-IO-AREA (78:4) TO PNR (1:4)
               MOVE NYEBEST-IO-AREA (82:15) TO PSTED (1:15)
               MOVE NYEBEST-IO-AREA (17:30) TO VADR1 (1:30)
               MOVE NYEBEST-IO-AREA (47:30) TO VADR2 (1:30)
               MOVE NYEBEST-IO-AREA (77:30) TO VADR3 (1:30)
               MOVE NYEBEST-IO-AREA (107:30) TO VADR4 (1:30)
               MOVE NYEBEST-IO-AREA (107:6) TO PSEDD (1:6)
               MOVE NYEBEST-IO-AREA (97:2) TO SIGN-X (1:2)
               MOVE NYEBEST-IO-AREA (97:1) TO BS1 (1:1)
               MOVE NYEBEST-IO-AREA (98:1) TO BS2 (1:1)
               MOVE NYEBEST-IO-AREA (17:1) TO AVD (1:1)
               MOVE NYEBEST-IO-AREA (20:2) TO LEVUKE (1:2)
               MOVE NYEBEST-IO-AREA (18:2) TO LEVA-ELGR (1:2)
               MOVE NYEBEST-IO-AREA (22:3) TO ALFA (1:3)
               MOVE NYEBEST-IO-AREA (25:20) TO ARTNR (1:20)
               MOVE NYEBEST-IO-AREA (75:4) TO EDBNR-IO
               MOVE NYEBEST-IO-AREA (79:5) TO PRIS-IO
               MOVE NYEBEST-IO-AREA (84:1) TO VTYP (1:1)
               MOVE NYEBEST-IO-AREA (89:5) TO BANT-IO
               MOVE NYEBEST-IO-AREA (99:5) TO LANT-IO
               MOVE NYEBEST-IO-AREA (124:1) TO TEKST (1:1)
               MOVE NYEBEST-IO-AREA (131:4) TO LEVNR-IO
               MOVE NYEBEST-IO-AREA (148:1) TO BSTAR (1:1)
               MOVE NYEBEST-IO-AREA (149:1) TO TSTAR (1:1)
               MOVE NYEBEST-IO-AREA (150:1) TO RECART (1:1)
           END-EVALUATE.
 
       NYEBEST-IDSET SECTION.
       NYEBEST-IDSET-P.
           SET I-01                        TO TRUE.
 
       NYEBEST-CHK-LEVEL SECTION.
       NYEBEST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO NYEBEST-LEVEL-01
               MOVE NYEBEST-IO-AREA (2:3)  TO NYEBEST-01-L4-FIRMA
               MOVE NYEBEST-IO-AREA (5:5)  TO NYEBEST-01-L2-BESNR
               IF  NYEBEST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYEBEST-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  NYEBEST-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  NYEBEST-01-L4         TO THE-PRIOR-L4
               MOVE  NYEBEST-01-L2         TO THE-PRIOR-L2
               SET NYEBEST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (97:5) TO ANTIN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO ANTUT-IO
               MOVE VAREMAS-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (118:5) TO VGRP (1:5)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       BEST-PRINT-LINE SECTION.
       BEST-PRINT-LINE-P.
           IF  BEST-BEFORE-SKIP > 0
               PERFORM BEST-SKIP-BEFORE
           END-IF
           IF  BEST-BEFORE-SPACE > 0
               PERFORM BEST-SPACE-BEFORE
               IF  BEST-AFTER-SKIP > 0
                   PERFORM BEST-SKIP-AFTER
               END-IF
               IF  BEST-AFTER-SPACE > 0
                   PERFORM BEST-SPACE-AFTER
               END-IF
           ELSE
               IF  BEST-AFTER-SKIP > 0
                   PERFORM BEST-SKIP-AFTER
               END-IF
               PERFORM BEST-SPACE-AFTER
           END-IF
           IF  BEST-LINE-COUNT NOT < BEST-MAX-LINES
               MOVE 7                      TO BEST-AFTER-SKIP
           END-IF.
 
       BEST-SKIP-BEFORE SECTION.
       BEST-SKIP-BEFORE-P.
           WRITE BEST-IO-PRINT          AFTER ADVANCING PAGE
           MOVE 1                          TO BEST-LINE-COUNT
           MOVE 0                          TO BEST-BEFORE-SKIP
           INITIALIZE BEST-IO-AREA.
 
       BEST-SPACE-BEFORE SECTION.
       BEST-SPACE-BEFORE-P.
           WRITE BEST-IO-PRINT          AFTER BEST-BEFORE-SPACE LINES
           ADD BEST-BEFORE-SPACE           TO BEST-LINE-COUNT
           MOVE SPACES TO BEST-IO-AREA
           INITIALIZE BEST-IO-AREA
           MOVE 0                          TO BEST-BEFORE-SPACE.
 
       BEST-SKIP-AFTER SECTION.
       BEST-SKIP-AFTER-P.
           WRITE BEST-IO-PRINT         BEFORE ADVANCING PAGE
           MOVE 1                          TO BEST-LINE-COUNT
           MOVE 0                          TO BEST-AFTER-SKIP
           INITIALIZE BEST-IO-AREA.
 
       BEST-SPACE-AFTER SECTION.
       BEST-SPACE-AFTER-P.
           WRITE BEST-IO-PRINT         BEFORE BEST-AFTER-SPACE LINES
           ADD BEST-AFTER-SPACE            TO BEST-LINE-COUNT
           INITIALIZE BEST-IO-AREA
           MOVE 0                          TO BEST-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-21 AND NOT-I-86)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE POS-X                  TO XO-40YNZ
               MOVE XO-40YNZ               TO BEST-IO-AREA (4:4)
               MOVE ALFA                   TO BEST-IO-AREA (11:3)
               MOVE ARTNR                  TO BEST-IO-AREA (17:20)
               IF  (I-51)
                   MOVE '(SKAFFEVARE)'     TO BEST-IO-AREA (39:12)
               END-IF
               MOVE BANT                   TO XO-72YYZ
               MOVE XO-72YYZ               TO BEST-IO-AREA (50:12)
               IF  (NOT-I-24)
                   MOVE PRIS1              TO XO-52YYZ
                   MOVE XO-52YYZ           TO BEST-IO-AREA (70:9)
               END-IF
               IF  (I-24)
                   MOVE SELVK              TO XO-72YYZ
                   MOVE XO-72YYZ           TO BEST-IO-AREA (67:12)
               END-IF
               MOVE VGRP                   TO BEST-IO-AREA (82:5)
               MOVE DTOTAL                 TO XO-72YYZ
               MOVE XO-72YYZ               TO BEST-IO-AREA (89:12)
               MOVE '*'                    TO BEST-IO-AREA (102:1)
               IF  (NOT-I-24 AND I-50)
                   MOVE 'NORSK PRIS PÅFØRT' TO BEST-IO-AREA (104:17)
               END-IF
               IF  (NOT-I-24 AND NOT-I-50)
                   MOVE 'UTENL PRIS PÅFØRT' TO BEST-IO-AREA (104:17)
               END-IF
               IF  (I-91 AND NOT-I-50)
                   MOVE 'LEVR. PRIS PÅFØRT' TO BEST-IO-AREA (104:17)
               END-IF
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
           END-IF
           IF  (I-01 AND I-21)
               MOVE SPACES TO INKFIL-IO-AREA
               INITIALIZE INKFIL-IO-AREA
               MOVE 'I'                    TO INKFIL-IO-AREA (1:1)
               MOVE FIRMA                  TO INKFIL-IO-AREA (2:3)
               MOVE BESNR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO INKFIL-IO-AREA (5:5)
               MOVE POS-X-IO               TO INKFIL-IO-AREA (10:4)
               MOVE VGRP                   TO INKFIL-IO-AREA (14:5)
               MOVE ALFA                   TO INKFIL-IO-AREA (19:3)
               MOVE ARTNR                  TO INKFIL-IO-AREA (23:20)
               MOVE EDBNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO INKFIL-IO-AREA (43:7)
               MOVE PRIS                   TO XO-72P
               MOVE XO-72P-EF              TO INKFIL-IO-AREA (56:5)
               MOVE DTOTAL                 TO XO-72P
               MOVE XO-72P-EF              TO INKFIL-IO-AREA (62:5)
               INITIALIZE DTOTAL
               MOVE SIGN1                  TO INKFIL-IO-AREA (67:2)
               WRITE INKFIL-IO-AREA
           END-IF
           IF  (I-01 AND I-22)
               MOVE SPACES TO TILGANG-IO-AREA
               INITIALIZE TILGANG-IO-AREA
               MOVE REC                    TO TILGANG-IO-AREA (1:150)
               MOVE ORNR                   TO TILGANG-IO-AREA (104:6)
               WRITE TILGANG-IO-AREA
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-60 AND NOT-I-86)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE FINAVN                 TO BEST-IO-AREA (2:30)
               MOVE LOPNVN                 TO BEST-IO-AREA (38:35)
               MOVE 'DATO'                 TO BEST-IO-AREA (80:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO BEST-IO-AREA (86:8)
               MOVE 01                     TO BEST-BEFORE-SKIP
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE '------------------------' TO BEST-IO-AREA (1:24)
               MOVE '------------------------' TO BEST-IO-AREA (25:24)
               MOVE '------------------------' TO BEST-IO-AREA (49:24)
               MOVE '------------------------' TO BEST-IO-AREA (73:24)
               MOVE '------------------------' TO BEST-IO-AREA (97:24)
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE 'LEVR.NR.  NAVN/ADRESSE' TO BEST-IO-AREA (3:22)
               MOVE 'BESR.NR.     BESTILLER' TO BEST-IO-AREA (56:22)
               MOVE 'AVD     LEV.TID'      TO BEST-IO-AREA (82:15)
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE 'POS     ALFA/ARTIKKEL' TO BEST-IO-AREA (4:21)
               MOVE 'ANTALL         PRIS'  TO BEST-IO-AREA (56:19)
               MOVE 'VGR'                  TO BEST-IO-AREA (82:3)
               MOVE 'TOTAL          STATUS' TO BEST-IO-AREA (91:21)
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE '------------------------' TO BEST-IO-AREA (1:24)
               MOVE '------------------------' TO BEST-IO-AREA (25:24)
               MOVE '------------------------' TO BEST-IO-AREA (49:24)
               MOVE '------------------------' TO BEST-IO-AREA (73:24)
               MOVE '------------------------' TO BEST-IO-AREA (97:24)
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE LEVRNR-IO              TO BEST-IO-AREA (3:6)
               MOVE NAVN1                  TO BEST-IO-AREA (11:30)
               MOVE BESNR-IO               TO BEST-IO-AREA (57:5)
               MOVE BSA                    TO BEST-IO-AREA (71:1)
               MOVE '.'                    TO BEST-IO-AREA (72:1)
               MOVE BSB                    TO BEST-IO-AREA (73:1)
               MOVE '.'                    TO BEST-IO-AREA (74:1)
               MOVE AVD1                   TO BEST-IO-AREA (83:1)
               MOVE 'UKE'                  TO BEST-IO-AREA (87:3)
               MOVE LUKE                   TO BEST-IO-AREA (92:2)
               MOVE LA-ELGR                TO BEST-IO-AREA (95:2)
               MOVE '.'                    TO BEST-IO-AREA (94:1)
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               IF  (I-70)
                   MOVE 'REGIST. IKKE FULLFØRT' TO BEST-IO-AREA
                                                              (105:21)
               END-IF
               IF  (I-71)
                   MOVE 'GODKJENT             ' TO BEST-IO-AREA
                                                              (105:21)
               END-IF
               IF  (I-72)
                   MOVE 'SENDT TIL PRINT      ' TO BEST-IO-AREA
                                                              (105:21)
               END-IF
               IF  (I-73)
                   MOVE 'UTGÅRMELDT           ' TO BEST-IO-AREA
                                                              (105:21)
               END-IF
               IF  (I-74)
                   MOVE 'DELVIS LEVERT        ' TO BEST-IO-AREA
                                                              (105:21)
               END-IF
               IF  (I-75)
                   MOVE 'FULLT LEVERT         ' TO BEST-IO-AREA
                                                              (105:21)
               END-IF
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE ADR1                   TO BEST-IO-AREA (11:30)
               MOVE VARE1                  TO BEST-IO-AREA (51:30)
               MOVE 1                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE PNR1                   TO BEST-IO-AREA (11:4)
               MOVE PSTED1                 TO BEST-IO-AREA (15:16)
               MOVE VARE2                  TO BEST-IO-AREA (51:30)
               MOVE 2                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
           END-IF
           IF  (I-45 AND NOT-I-86 AND I-35)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE 'TOTAL FOR VAREGRUPPE' TO BEST-IO-AREA (54:20)
               MOVE ARV (X)                TO BEST-IO-AREA (82:5)
               MOVE ARB (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO BEST-IO-AREA (87:14)
               MOVE '**'                   TO BEST-IO-AREA (102:2)
               MOVE 1                      TO BEST-BEFORE-SPACE
               PERFORM BEST-PRINT-LINE
           END-IF
           IF  (I-46 AND NOT-I-86 AND I-35)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE 'TOTAL BESTILLINGSNR.' TO BEST-IO-AREA (54:20)
               MOVE BESNR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO BEST-IO-AREA (75:5)
               MOVE GTOTAL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO BEST-IO-AREA (87:14)
               INITIALIZE GTOTAL
               MOVE '*** TIL SELVKOSTPRIS' TO BEST-IO-AREA (102:20)
               MOVE 2                      TO BEST-BEFORE-SPACE
               PERFORM BEST-PRINT-LINE
           END-IF
           IF  (I-47 AND NOT-I-86 AND I-35)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE 'TOTAL FOR FIRMA VRG ' TO BEST-IO-AREA (54:20)
               MOVE ARU (V)                TO BEST-IO-AREA (82:5)
               MOVE ARC (V)                TO XO-82YY9R
               MOVE XO-82YY9R              TO BEST-IO-AREA (87:14)
               MOVE '**'                   TO BEST-IO-AREA (102:2)
               MOVE 2                      TO BEST-BEFORE-SPACE
               MOVE 2                      TO BEST-AFTER-SPACE
               PERFORM BEST-PRINT-LINE
           END-IF
           IF  (I-48 AND NOT-I-86 AND I-35)
               MOVE SPACES TO BEST-IO-AREA
               INITIALIZE BEST-IO-AREA
               MOVE 'TOTAL FIRMA         ' TO BEST-IO-AREA (54:20)
               MOVE GTOTAF                 TO XO-82YY9R
               MOVE XO-82YY9R              TO BEST-IO-AREA (87:14)
               INITIALIZE GTOTAF
               MOVE '*** TIL SELVKOSTPRIS' TO BEST-IO-AREA (102:20)
               MOVE 2                      TO BEST-BEFORE-SPACE
               PERFORM BEST-PRINT-LINE
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
           SET NYEBEST-LEVEL-INIT          TO TRUE
           INITIALIZE NYEBEST-DATA-FIELDS
           SET NYEBEST-EOF-OFF             TO TRUE
           SET NYEBEST-PROCESS             TO TRUE
           OPEN INPUT NYEBEST
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT BEST
           INITIALIZE BEST-IO-AREA
           INITIALIZE BEST-DATA-FIELDS
           MOVE 57                         TO BEST-MAX-LINES
           OPEN OUTPUT TILGANG
           OPEN OUTPUT INKFIL.
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
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE NYEBEST
           CLOSE VAREMAS
           IF BEST-IO-AREA NOT = SPACES
             WRITE BEST-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO BEST-IO-AREA
           END-IF
           CLOSE BEST
           CLOSE TILGANG
           CLOSE INKFIL.
 
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
