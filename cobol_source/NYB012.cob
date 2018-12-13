       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB012R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM  NYB012
      *   UTLISTING AV VARETILGANGSLISTE MED MANKO                             *
      *   RECORDS FRA VARETILGANGSFILE                                         *
      *   HENTING AV RABMAST FJERNET 11.09.95 SS                               *
      *   LAGT INN,LINJE 142-950 (ANT*SISTE INNPRIS=SUM) LISTE VISER           *
      *   SISTE INNPRIS FOR HAFNOR 950                                         *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB012.rpg
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
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
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
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD OUTFIL
               BLOCK CONTAINS 9420
               RECORD CONTAINS 60.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(60).
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
       77  ARA-MAX   VALUE 9               PICTURE 9(4) USAGE BINARY.
       77  ARP-MAX   VALUE 9               PICTURE 9(4) USAGE BINARY.
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
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 9 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S
                                                      ARP-I
                                                      ARP-S.
                   15  ARA                 PICTURE X(7).
                   15  ARP                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TILFILE-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
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
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  TILFILE-LEVEL-01.
               10  TILFILE-01-L5.
                   15  TILFILE-01-L5-FIRMA PICTURE X(3).
               10  TILFILE-01-L4.
                   15  TILFILE-01-L4-AVD   PICTURE X(1).
               10  TILFILE-01-L3.
                   15  TILFILE-01-L3-TILGNR PICTURE X(5).
               10  TILFILE-01-L2.
                   15  TILFILE-01-L2-BESNR PICTURE S9(5).
           05  TILFILE-DATA-FIELDS.
               10  KEY-X                   PICTURE X(16).
               10  FIRMA                   PICTURE X(3).
               10  BESNR-IO.
                   15  BESNR               PICTURE S9(5).
               10  POSNR-IO.
                   15  POSNR               PICTURE S9(4).
               10  AVD                     PICTURE X(1).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(15).
               10  ARTHEL                  PICTURE X(20).
               10  BBETEG                  PICTURE X(30).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BPRIS-IO.
                   15  BPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BRAB1-IO.
                   15  BRAB1               PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  BRAB2-IO.
                   15  BRAB2               PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  BANT-IO.
                   15  BANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LANT-IO.
                   15  LANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PSEDD                   PICTURE X(6).
               10  TEKST                   PICTURE X(1).
               10  TILGNR                  PICTURE X(5).
               10  LK                      PICTURE X(2).
               10  LEV-IO.
                   15  LEV                 PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  IPRIS-IO.
                   15  IPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  IPRISE-IO.
                   15  IPRISE              PICTURE S9(5)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  IPRIST                  PICTURE X(5).
           05  VAREMAS-DATA-FIELDS.
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  LRAB-IO.
                   15  LRAB                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  LEVP-IO.
                   15  LEVP                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVPE-IO.
                   15  LEVPE               PICTURE S9(5)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  VGR                     PICTURE X(5).
               10  TIL-IO.
                   15  TIL                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TEK                     PICTURE X(1).
               10  VAL                     PICTURE X(1).
           05  KUNDEMA-DATA-FIELDS.
               10  LEVNVN                  PICTURE X(30).
               10  GRAB1                   PICTURE X(1).
               10  GRAB2                   PICTURE X(1).
               10  GRAB3                   PICTURE X(1).
               10  GRAB4                   PICTURE X(1).
               10  GRAB5                   PICTURE X(1).
               10  GRAB6                   PICTURE X(1).
               10  GRAB7                   PICTURE X(1).
               10  GRAB8                   PICTURE X(1).
               10  GRAB9                   PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  KFNR                    PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(1).
               10  THE-PRIOR-L3            PICTURE X(5).
               10  THE-PRIOR-L2            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  KEY1                    PICTURE X(10).
               10  LEVBES-IO.
                   15  LEVBES              PICTURE S9(8)V9(2).
               10  LEVBE2-IO.
                   15  LEVBE2              PICTURE S9(8)V9(2).
               10  LEVTIL-IO.
                   15  LEVTIL              PICTURE S9(8)V9(2).
               10  LEVTI2-IO.
                   15  LEVTI2              PICTURE S9(8)V9(2).
               10  LEVSUM-IO.
                   15  LEVSUM              PICTURE S9(8)V9(2).
               10  LEVSU2-IO.
                   15  LEVSU2              PICTURE S9(8)V9(2).
               10  LEVNTO-IO.
                   15  LEVNTO              PICTURE S9(8)V9(2).
               10  LEVNT2-IO.
                   15  LEVNT2              PICTURE S9(8)V9(2).
               10  Y-IO.
                   15  Y                   PICTURE S9(3).
               10  A-IO.
                   15  A                   PICTURE S9(3).
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(6).
               10  KNRKEY                  PICTURE X(9).
               10  LEVR1-IO.
                   15  LEVR1               PICTURE S9(2)V9(1).
               10  LEVR2-IO.
                   15  LEVR2               PICTURE S9(2)V9(1).
               10  EDB1-IO.
                   15  EDB1                PICTURE S9(7).
               10  SKAF                    PICTURE X(1).
               10  NYBEH-IO.
                   15  NYBEH               PICTURE S9(7).
               10  REST-IO.
                   15  REST                PICTURE S9(6)V9(2).
               10  LEVPR-IO.
                   15  LEVPR               PICTURE S9(5)V9(4).
               10  LEVPR2-IO.
                   15  LEVPR2              PICTURE S9(5)V9(4).
               10  PTIL-IO.
                   15  PTIL                PICTURE S9(7)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(3).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(9)V9(2).
               10  VERDI-IO.
                   15  VERDI               PICTURE S9(8)V9(2).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(9)V9(2).
               10  VERDI2-IO.
                   15  VERDI2              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-70YNZ                PICTURE ZZZZZZZ.
               10  XO-72YNZ                PICTURE ZZZZZZZ,ZZ.
               10  XO-62YNZR               PICTURE ZZZZZZ,ZZ-.
               10  XO-54YNZ                PICTURE ZZZZZ,ZZZZ.
               10  XO-21YNZ                PICTURE ZZ,Z.
               10  XO-82YNZ                PICTURE ZZZZZZZZ,ZZ.
               10  XO-72YYZ                PICTURE Z.ZZZ.ZZZ,ZZ.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
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
           IF  (I-L5)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L5)
               MOVE FIRMA                  TO KEY1 (1:3)
               MOVE 0                      TO LEVBES
           END-IF
           IF  (I-L2)
               MOVE 0                      TO LEVBE2
           END-IF
           IF  (I-L5)
               MOVE 0                      TO LEVTIL
           END-IF
           IF  (I-L2)
               MOVE 0                      TO LEVTI2
           END-IF
           IF  (I-L5)
               MOVE 0                      TO LEVSUM
           END-IF
           IF  (I-L2)
               MOVE 0                      TO LEVSU2
           END-IF
           IF  (I-L5)
               MOVE 0                      TO LEVNTO
           END-IF
           IF  (I-L2)
               MOVE 0                      TO LEVNT2
      *
           END-IF
           IF  (I-L3)
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
               MOVE 0                      TO A
               PERFORM VARYING ARA-I FROM 1 BY 1
                         UNTIL ARA-I > ARA-MAX
                   MOVE '       '          TO ARA (ARA-I)
               END-PERFORM
               PERFORM VARYING ARP-I FROM 1 BY 1
                         UNTIL ARP-I > ARP-MAX
                   MOVE 0                  TO ARP (ARP-I)
               END-PERFORM
               SET ARP-I                   TO 1
      *
           END-IF
           IF  (I-L3)
               ADD LEV TO ZERO         GIVING LEVNR
               MOVE LEVNR                  TO KNRKEY (4:6)
               SET NOT-I-90                TO TRUE
               IF  FIRMA = '999'
                   SET I-90                TO TRUE
               END-IF
               SET NOT-I-96                TO TRUE
               IF  FIRMA = '918'
                   SET I-96                TO TRUE
               END-IF
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-48                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-48            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-48)
               SET NOT-I-87                TO TRUE
               IF  FIRMA NOT = KFNR
                   SET I-87                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-87 AND NOT-I-48)
               MOVE KFNR                   TO KNRKEY (1:3)
           END-IF
           IF  (I-L3 AND NOT-I-87 AND NOT-I-48)
               MOVE FIRMA                  TO KNRKEY (1:3)
           END-IF
           IF  (I-L3)
               MOVE KNRKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-75                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-75            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      *
           END-IF
           SET NOT-I-61                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-63                    TO TRUE
           SET NOT-I-64                    TO TRUE
           SET NOT-I-65                    TO TRUE
           SET NOT-I-66                    TO TRUE
           SET NOT-I-67                    TO TRUE
           SET NOT-I-68                    TO TRUE
           SET NOT-I-69                    TO TRUE
      *
           MOVE 0                          TO LEVR1
           MOVE 0                          TO LEVR2
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
               SET NOT-I-78                TO TRUE
               IF  VAL = 'L'
                   SET I-78                TO TRUE
               END-IF
           END-IF
           SUBTRACT LANT FROM BANT     GIVING REST
      *
           IF  (I-78)
               ADD LEVPE TO ZERO       GIVING LEVPR
           END-IF
           IF  (NOT-I-78)
               ADD LEVP TO ZERO        GIVING LEVPR
           END-IF.
 
       R1-T.
           IF  (I-90 AND NOT-I-94 AND NOT-I-95)
               ADD IPRIS TO ZERO       GIVING LEVPR2
      *  90N94N95
      *N 78                Z-ADDIPRISE    LEVPR
           END-IF
           IF  (I-90 AND NOT-I-92)
               ADD BRAB1 TO ZERO       GIVING LEVR1
           END-IF
           IF  (I-90 AND NOT-I-93)
               ADD BRAB2 TO ZERO       GIVING LEVR2
           END-IF
           IF  (I-90)
               SET NOT-I-97                TO TRUE
               IF  LEVR1 = 0
                   SET I-97                TO TRUE
               END-IF
      *  90N92             SETOF                     12
           END-IF
           IF  (I-90 AND I-97)
               ADD LRAB TO ZERO        GIVING LEVR1
           END-IF
           IF  (I-90)
               MOVE 0                      TO LEVR2
           END-IF
           IF  (I-96)
               ADD LRAB TO ZERO        GIVING LEVR1
               MOVE 0                      TO LEVR2
           END-IF
           PERFORM RABRUT-S
           IF  (I-90)
               PERFORM RABRU2-S
           END-IF
           ADD VERDI                       TO LEVSUM
           IF  (I-90)
               ADD VERDI2                  TO LEVSU2
           END-IF
           DIVIDE LEVSUM BY LANT       GIVING LEVNTO
           IF  (I-90)
               DIVIDE LEVSU2 BY LANT   GIVING LEVNT2
           END-IF
           ADD VERDI                       TO LEVTIL
           IF  (I-90)
               ADD VERDI2                  TO LEVTI2
           END-IF
           ADD VERDI                       TO LEVBES
           IF  (I-90)
               ADD VERDI2                  TO LEVBE2
      *******************************************************
           END-IF
           MULTIPLY TIL BY LANT        GIVING PTIL
           SET NOT-I-70                    TO TRUE
           IF  TEK > ' '
               SET I-70                    TO TRUE
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  TEK = 'P'
               SET I-61                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  TEK = 'N'
               SET I-62                    TO TRUE
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  TEK = 'Y'
               SET I-63                    TO TRUE
           END-IF
           SET NOT-I-64                    TO TRUE
           IF  TEK = 'F'
               SET I-64                    TO TRUE
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  TEK = 'E'
               SET I-65                    TO TRUE
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  TEK = 'K'
               SET I-66                    TO TRUE
           END-IF
           SET NOT-I-67                    TO TRUE
           IF  TEK = 'M'
               SET I-67                    TO TRUE
           END-IF
           SET NOT-I-68                    TO TRUE
           IF  TEK = 'S'
               SET I-68                    TO TRUE
           END-IF
           SET NOT-I-69                    TO TRUE
           IF  TEK = 'I'
               SET I-69                    TO TRUE
           END-IF
           IF  (I-61)
               MOVE 'PANT   '              TO ARA (1)
           END-IF
           IF  (I-62)
               MOVE 'NRK AV.'              TO ARA (2)
           END-IF
           IF  (I-63)
               MOVE 'PAR.AV.'              TO ARA (3)
           END-IF
           IF  (I-64)
               MOVE 'FRAKT  '              TO ARA (4)
           END-IF
           IF  (I-65)
               MOVE 'FRAKT/M'              TO ARA (5)
           END-IF
           IF  (I-66)
               MOVE 'KASSETT'              TO ARA (6)
           END-IF
           IF  (I-67)
               MOVE 'MILJØ  '              TO ARA (7)
           END-IF
           IF  (I-68)
               MOVE 'STAMME '              TO ARA (8)
           END-IF
           IF  (I-69)
               MOVE 'PIGG   '              TO ARA (9)
           END-IF
           IF  (I-61)
               ADD PTIL                    TO ARP (1)
           END-IF
           IF  (I-62)
               ADD PTIL                    TO ARP (2)
           END-IF
           IF  (I-63)
               ADD PTIL                    TO ARP (3)
           END-IF
           IF  (I-64)
               ADD PTIL                    TO ARP (4)
           END-IF
           IF  (I-65)
               ADD PTIL                    TO ARP (5)
           END-IF
           IF  (I-66)
               ADD PTIL                    TO ARP (6)
           END-IF
           IF  (I-67)
               ADD PTIL                    TO ARP (7)
           END-IF
           IF  (I-68)
               ADD PTIL                    TO ARP (8)
           END-IF
           IF  (I-69)
               ADD PTIL                    TO ARP (9)
      ******************************************************
      *       RUTINE FOR ARRAY TOTALSUMMERING PR. VGR.     *
      ******************************************************
           END-IF
           MOVE 1                          TO X
           SET NOT-I-40                    TO TRUE
           SET ARV-S                       TO 1
           PERFORM WITH TEST AFTER
                   VARYING ARV-I FROM X BY 1
                     UNTIL ARV-I >= ARV-MAX
                        OR I-40
               IF  VGR = ARV (ARV-I)
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
               MOVE VGR
                        TO ARV-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           END-IF
           ADD VERDI                       TO ARB (X).
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           SET NOT-I-35                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'BES08'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'NYB012  '                 TO LPROG
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
      *****************************************************
 
       RABRUT-S SECTION.
       RABRUT-S-P.
           MOVE 0                          TO SUM-X
           MULTIPLY LEVPR BY LANT      GIVING VERDI ROUNDED
           MULTIPLY LEVR1 BY VERDI     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM VERDI
      ****
           MULTIPLY LEVR2 BY VERDI     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM VERDI
      ****
      ***12N73   VERDI     MULT LEVR3     SUM
      ***12N73   SUM       DIV  100       SUM       H
      ***12N73   VERDI     SUB  SUM       VERDI
           .
      ******************************************************
      *****************************************************
 
       RABRU2-S SECTION.
       RABRU2-S-P.
           MOVE 0                          TO SUM2
           MULTIPLY LEVPR2 BY LANT     GIVING VERDI2 ROUNDED
           MULTIPLY LEVR1 BY VERDI2    GIVING SUM2
           DIVIDE SUM2 BY 100          GIVING SUM2 ROUNDED
           SUBTRACT SUM2                   FROM VERDI2
      ****
           MULTIPLY LEVR2 BY VERDI2    GIVING SUM2
           DIVIDE SUM2 BY 100          GIVING SUM2 ROUNDED
           SUBTRACT SUM2                   FROM VERDI2
      ****
      ***12N73   VERDI2    MULT LEVR3     SUM2
      ***12N73   SUM2      DIV  100       SUM2      H
      ***12N73   VERDI2    SUB  SUM2      VERDI2
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-15)
               GO TO VGREND-T
           END-IF
           IF  (I-L2 AND NOT-I-35)
               SET I-46                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-46                TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-35)
               SET I-47                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-47                TO TRUE
           END-IF
           IF  (I-L3)
               MOVE 1                      TO A
           END-IF.
 
       TELOOP-T.
           IF  (I-L3)
               SET NOT-I-54                TO TRUE
               IF  ARP (A) = 0
                   SET I-54                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-35 AND NOT-I-54)
               SET I-55                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-55                TO TRUE
           END-IF
           IF  (I-L3)
               ADD 1                       TO A
               SET NOT-I-53                TO TRUE
               IF  A NOT < 10
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-53)
               GO TO ENDTEK-T
           END-IF
           IF  (I-L3)
               GO TO TELOOP-T
           END-IF.
 
       ENDTEK-T.
           IF  (I-L3)
               SET NOT-I-42                TO TRUE
               IF  Y = 1
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-42)
               GO TO VGREND-T
           END-IF
           IF  (I-L3)
               MOVE 1                      TO X
           END-IF.
 
       VGLOOP-T.
           IF  (I-L3 AND NOT-I-35)
               SET I-45                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-45                TO TRUE
           END-IF
           IF  (I-L3)
               SET NOT-I-43                TO TRUE
               IF  X < Y
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-43)
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
               SET NOT-I-91                TO TRUE
               SET NOT-I-92                TO TRUE
               SET NOT-I-93                TO TRUE
               SET NOT-I-94                TO TRUE
               SET NOT-I-94                TO TRUE
               SET NOT-I-95                TO TRUE
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
               MOVE TILFILE-IO-AREA (25:15) TO ARTNR (1:15)
               MOVE TILFILE-IO-AREA (25:20) TO ARTHEL (1:20)
               MOVE TILFILE-IO-AREA (45:30) TO BBETEG (1:30)
               MOVE TILFILE-IO-AREA (75:4) TO EDBNR-IO
               MOVE TILFILE-IO-AREA (79:5) TO BPRIS-IO
               IF  BPRIS = ZERO
                   SET I-91                TO TRUE
               END-IF
               MOVE TILFILE-IO-AREA (85:2) TO BRAB1-IO
               IF  BRAB1 = ZERO
                   SET I-92                TO TRUE
               END-IF
               MOVE TILFILE-IO-AREA (87:2) TO BRAB2-IO
               IF  BRAB2 = ZERO
                   SET I-93                TO TRUE
               END-IF
               MOVE TILFILE-IO-AREA (89:5) TO BANT-IO
               MOVE TILFILE-IO-AREA (99:5) TO LANT-IO
               MOVE TILFILE-IO-AREA (104:6) TO PSEDD (1:6)
               MOVE TILFILE-IO-AREA (124:1) TO TEKST (1:1)
               MOVE TILFILE-IO-AREA (135:5) TO TILGNR (1:5)
               MOVE TILFILE-IO-AREA (140:2) TO LK (1:2)
               MOVE TILFILE-IO-AREA (131:4) TO LEV-IO
               MOVE TILFILE-IO-AREA (143:5) TO IPRIS-IO
               IF  IPRIS = ZERO
                   SET I-94                TO TRUE
               END-IF
               MOVE TILFILE-IO-AREA (143:5) TO IPRISE-IO
               IF  IPRISE = ZERO
                   SET I-94                TO TRUE
               END-IF
               MOVE TILFILE-IO-AREA (143:5) TO IPRIST (1:5)
               IF  IPRIST = SPACES
                   SET I-95                TO TRUE
               END-IF
           END-EVALUATE.
 
       TILFILE-IDSET SECTION.
       TILFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       TILFILE-CHK-LEVEL SECTION.
       TILFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO TILFILE-LEVEL-01
               MOVE TILFILE-IO-AREA (2:3)  TO TILFILE-01-L5-FIRMA
               MOVE TILFILE-IO-AREA (17:1) TO TILFILE-01-L4-AVD
               MOVE TILFILE-IO-AREA (135:5) TO TILFILE-01-L3-TILGNR
               MOVE TILFILE-IO-AREA (5:5)  TO TILFILE-01-L2-BESNR
               IF  TILFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  TILFILE-01-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  TILFILE-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  TILFILE-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  TILFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  TILFILE-01-L5         TO THE-PRIOR-L5
               MOVE  TILFILE-01-L4         TO THE-PRIOR-L4
               MOVE  TILFILE-01-L3         TO THE-PRIOR-L3
               MOVE  TILFILE-01-L2         TO THE-PRIOR-L2
               SET TILFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (97:5) TO ANTIN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO ANTUT-IO
               MOVE VAREMAS-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (153:2) TO LRAB-IO
               MOVE VAREMAS-IO-AREA (165:5) TO LEVP-IO
               MOVE VAREMAS-IO-AREA (165:5) TO LEVPE-IO
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (161:4) TO TIL-IO
               MOVE VAREMAS-IO-AREA (107:1) TO TEK (1:1)
               MOVE VAREMAS-IO-AREA (170:1) TO VAL (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-04                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO LEVNVN (1:30)
               MOVE KUNDEMA-IO-AREA (190:1) TO GRAB1 (1:1)
               MOVE KUNDEMA-IO-AREA (191:1) TO GRAB2 (1:1)
               MOVE KUNDEMA-IO-AREA (192:1) TO GRAB3 (1:1)
               MOVE KUNDEMA-IO-AREA (193:1) TO GRAB4 (1:1)
               MOVE KUNDEMA-IO-AREA (194:1) TO GRAB5 (1:1)
               MOVE KUNDEMA-IO-AREA (195:1) TO GRAB6 (1:1)
               MOVE KUNDEMA-IO-AREA (196:1) TO GRAB7 (1:1)
               MOVE KUNDEMA-IO-AREA (197:1) TO GRAB8 (1:1)
               MOVE KUNDEMA-IO-AREA (198:1) TO GRAB9 (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-05                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KFNR (1:3)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-06                        TO TRUE.
 
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
           IF  (I-01 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BESNR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (1:5)
               MOVE POSNR                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (7:4)
               MOVE ALFA                   TO LISTE-IO-AREA (12:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (16:15)
               IF  (NOT-I-90 AND I-09)
                   MOVE 'SKAFFEVARE'       TO LISTE-IO-AREA (29:10)
               END-IF
               IF  (NOT-I-09)
                   MOVE NYBEH              TO XO-70YNZ
                   MOVE XO-70YNZ           TO LISTE-IO-AREA (32:7)
               END-IF
               MOVE BANT                   TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (39:10)
               MOVE LANT                   TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (49:10)
               MOVE REST                   TO XO-62YNZR
               MOVE XO-62YNZR              TO LISTE-IO-AREA (59:10)
               MOVE SELVK                  TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (69:10)
               INITIALIZE SELVK
               MOVE LEVPR                  TO XO-54YNZ
               MOVE XO-54YNZ               TO LISTE-IO-AREA (79:10)
               INITIALIZE LEVPR
      *                90N94N95IPRIS 4B  88
               MOVE LEVR1                  TO XO-21YNZ
               MOVE XO-21YNZ               TO LISTE-IO-AREA (90:4)
               INITIALIZE LEVR1
               MOVE LEVR2                  TO XO-21YNZ
               MOVE XO-21YNZ               TO LISTE-IO-AREA (95:4)
               INITIALIZE LEVR2
      *                     N73LEVR3 4B 103
               MOVE LEVNTO                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (103:11)
               INITIALIZE LEVNTO
               MOVE LEVSUM                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (115:11)
               INITIALIZE LEVSUM
               MOVE VAL                    TO LISTE-IO-AREA (126:1)
               MOVE VGR                    TO LISTE-IO-AREA (128:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86 AND I-90)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BESNR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (1:5)
               MOVE POSNR                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (7:4)
               MOVE ALFA                   TO LISTE-IO-AREA (12:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (16:15)
               IF  (NOT-I-09)
                   MOVE NYBEH              TO XO-70YNZ
                   MOVE XO-70YNZ           TO LISTE-IO-AREA (32:7)
               END-IF
               MOVE BANT                   TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (39:10)
               MOVE LANT                   TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (49:10)
               MOVE REST                   TO XO-62YNZR
               MOVE XO-62YNZR              TO LISTE-IO-AREA (59:10)
               MOVE SELVK                  TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (69:10)
               INITIALIZE SELVK
      *                        LEVPR 4B  88
               IF  (I-90 AND NOT-I-94 AND NOT-I-95)
                   MOVE IPRIS              TO XO-72YNZ
                   MOVE XO-72YNZ           TO LISTE-IO-AREA (79:10)
                   INITIALIZE IPRIS
               END-IF
               MOVE LEVR1                  TO XO-21YNZ
               MOVE XO-21YNZ               TO LISTE-IO-AREA (90:4)
               INITIALIZE LEVR1
               MOVE LEVR2                  TO XO-21YNZ
               MOVE XO-21YNZ               TO LISTE-IO-AREA (95:4)
               INITIALIZE LEVR2
      *                     N73LEVR3 4B 103
               MOVE LEVNT2                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (103:11)
               INITIALIZE LEVNT2
               MOVE LEVSU2                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (115:11)
               INITIALIZE LEVSU2
               MOVE VAL                    TO LISTE-IO-AREA (126:1)
               MOVE VGR                    TO LISTE-IO-AREA (128:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (5:1)
               MOVE ARTHEL                 TO LISTE-IO-AREA (1:20)
               IF  (I-90)
                   MOVE BBETEG             TO LISTE-IO-AREA (36:30)
               END-IF
               IF  (I-96)
                   MOVE 'SUM DIVERSE TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-61)
                   MOVE 'SUM PANT    TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-62)
                   MOVE 'SUM NRK AV. TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-63)
                   MOVE 'SUM PAR.AV. TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-64)
                   MOVE 'SUM FRAKT   TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-65)
                   MOVE 'SUM FRAKT/M TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-66)
                   MOVE 'SUM KASETT  TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-67)
                   MOVE 'SUM MILJØ   TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-68)
                   MOVE 'SUM STAMME  TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96 AND I-69)
                   MOVE 'SUM PIGG    TILLEGG' TO LISTE-IO-AREA (47:19)
               END-IF
               IF  (I-96)
                   MOVE PTIL               TO XO-72YYZ
                   MOVE XO-72YYZ           TO LISTE-IO-AREA (67:12)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-46 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT PR BESTILLINGSNR.' TO LISTE-IO-AREA (74:24)
               MOVE BESNR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (101:5)
               MOVE LEVBES                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (115:11)
               INITIALIZE LEVBES
               MOVE '*'                    TO LISTE-IO-AREA (130:1)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-47 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT PR TILGANGSNUMMER' TO LISTE-IO-AREA (74:24)
               MOVE TILGNR                 TO LISTE-IO-AREA (101:5)
               MOVE LEVTIL                 TO XO-82YNZ
               MOVE XO-82YNZ               TO LISTE-IO-AREA (115:11)
               MOVE '**'                   TO LISTE-IO-AREA (129:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-55 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL FOR PRISTILLEGG' TO LISTE-IO-AREA (77:21)
               MOVE ARA (A)                TO LISTE-IO-AREA (100:7)
               MOVE ARP (A)                TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (114:13)
               MOVE '***'                  TO LISTE-IO-AREA (128:3)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-45 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL FOR VAREGRUPPE' TO LISTE-IO-AREA (54:20)
               MOVE ARV (X)                TO LISTE-IO-AREA (82:5)
               MOVE ARB (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (112:14)
               MOVE '**'                   TO LISTE-IO-AREA (129:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-47)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '1'                    TO OUTFIL-IO-AREA (1:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (2:3)
               MOVE TILGNR                 TO OUTFIL-IO-AREA (5:5)
               MOVE LEVTIL-IO              TO OUTFIL-IO-AREA (10:10)
               INITIALIZE LEVTIL-IO
               MOVE UDATE                  TO OUTFIL-IO-AREA (20:6)
               MOVE LEVNR-IO               TO OUTFIL-IO-AREA (55:6)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '**'                   TO LISTE-IO-AREA (34:2)
               MOVE 'V A R E T I L G A N G S' TO LISTE-IO-AREA (37:23)
               MOVE 'L I S T E **'         TO LISTE-IO-AREA (61:12)
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
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LEVERANDØRNR.'        TO LISTE-IO-AREA (1:13)
               MOVE LEVNR-IO               TO LISTE-IO-AREA (15:6)
               MOVE LEVNVN                 TO LISTE-IO-AREA (26:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEST. POS.'           TO LISTE-IO-AREA (1:10)
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (12:18)
               MOVE 'LAGER   BEST.     LEVERT' TO LISTE-IO-AREA (32:24)
               MOVE 'REST/     SELV-'      TO LISTE-IO-AREA (60:15)
               MOVE 'L E V E R A N D Ø R  ' TO LISTE-IO-AREA (86:21)
               MOVE 'D A T A '             TO LISTE-IO-AREA (107:8)
               MOVE 'VGR'                  TO LISTE-IO-AREA (128:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' NR.  NR.'            TO LISTE-IO-AREA (1:9)
               IF  (I-90)
                   MOVE 'BETEGNELSE    '   TO LISTE-IO-AREA (16:14)
               END-IF
               MOVE 'BEHOLD. ANTALL    ANTALL' TO LISTE-IO-AREA (32:24)
               MOVE 'OVERSK.   KOSTPRIS'   TO LISTE-IO-AREA (60:18)
               MOVE 'PRIS   RAB1 RAB2     ' TO LISTE-IO-AREA (83:21)
               MOVE 'NETTOPRIS   SUM'      TO LISTE-IO-AREA (105:15)
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
           IF  (I-OF AND NOT-I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '**'                   TO LISTE-IO-AREA (34:2)
               MOVE 'V A R E T I L G A N G S' TO LISTE-IO-AREA (37:23)
               MOVE 'L I S T E **'         TO LISTE-IO-AREA (61:12)
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
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LEVERANDØRNR.'        TO LISTE-IO-AREA (1:13)
               MOVE LEVNR-IO               TO LISTE-IO-AREA (15:6)
               MOVE LEVNVN                 TO LISTE-IO-AREA (26:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEST. POS.'           TO LISTE-IO-AREA (1:10)
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (12:18)
               MOVE 'LAGER   BEST.     LEVERT' TO LISTE-IO-AREA (32:24)
               MOVE 'REST/     SELV-'      TO LISTE-IO-AREA (60:15)
               MOVE 'L E V E R A N D Ø R  ' TO LISTE-IO-AREA (86:21)
               MOVE 'D A T A '             TO LISTE-IO-AREA (107:8)
               MOVE 'VGR'                  TO LISTE-IO-AREA (128:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' NR.  NR.'            TO LISTE-IO-AREA (1:9)
               IF  (I-90)
                   MOVE 'BETEGNELSE    '   TO LISTE-IO-AREA (16:14)
               END-IF
               MOVE 'BEHOLD. ANTALL    ANTALL' TO LISTE-IO-AREA (32:24)
               MOVE 'OVERSK.   KOSTPRIS'   TO LISTE-IO-AREA (60:18)
               MOVE 'PRIS   RAB1 RAB2     ' TO LISTE-IO-AREA (83:21)
               MOVE 'NETTOPRIS   SUM'      TO LISTE-IO-AREA (105:15)
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
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT OUTFIL
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
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
               INITIALIZE ARP (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILFILE
           CLOSE VAREMAS
           CLOSE KUNDEMA
           CLOSE FIRMAF
           CLOSE OUTFIL
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
