       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG822R.
      **********************************************  Z-WIN-RPG2      *
      *  MERGE RECORDS SOM SKAL KOPIERES MOT FIRMA-EDBNR-TABELL FOR Å     *
      *  HENTE EDBNUMMER HVIS DE VAR MED PÅ KOPIERING VED FORRIGE         *
      *  KJØRING. OM IKKE MERGING GJØRES OPPSLAG MOT OPPSLAGSMASTER       *
      *  FOR Å SJEKKE OM DEN LIGGER INNE FRA FØR. OM DETTE HELLER IKKE    *
      *  SLÅR TIL VIL DET VÆRE EN NY ARTIKKEL ,OG HENTES EDBNR FRA FIRMA  *
      *  DET LEGGES UT SORTBEGREP I KOLONNE 93 PÅ KOPIUT, DETTE FOR AT    *
      *  NYE KOPIER SKAL KOMME FØRST DERSOM FLERE PÅ SAMME KEY.           *
      *  NY RUTINE FOR Å FINNE LEDIGE EDBNR INNIMELLOM                    *
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG822.rpg
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
           SELECT EDBTAB
               ASSIGN TO UT-S-EDBTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDBTAB-STATUS.
           SELECT KOPI
               ASSIGN TO UT-S-KOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPI-STATUS.
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS OPPSMAS-STATUS
               RECORD KEY IS OPPSMAS-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KOPIUT
               ASSIGN TO UT-S-KOPIUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPIUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD EDBTAB
               BLOCK CONTAINS 9000
               RECORD CONTAINS 20.
       01  EDBTAB-IO-AREA.
           05  EDBTAB-IO-AREA-X            PICTURE X(20).
       FD KOPI
               BLOCK CONTAINS 9180
               RECORD CONTAINS 170.
       01  KOPI-IO-AREA.
           05  KOPI-IO-AREA-X              PICTURE X(170).
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X.
               10  OPPSMAS-KEY1            PICTURE X(21).
               10  FILLER                  PICTURE X(9).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KOPIUT
               BLOCK CONTAINS 8400
               RECORD CONTAINS 140.
       01  KOPIUT-IO-AREA.
           05  KOPIUT-IO-AREA-X            PICTURE X(140).
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
           10  EDBTAB-STATUS               PICTURE 99 VALUE 0.
           10  KOPI-STATUS                 PICTURE 99 VALUE 0.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KOPIUT-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBTAB-EOF-OFF          VALUE '0'.
               88  EDBTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBTAB-READ-OFF         VALUE '0'.
               88  EDBTAB-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  EDBTAB-PROCESS-OFF      VALUE '0'.
               88  EDBTAB-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-EOF-OFF            VALUE '0'.
               88  KOPI-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-READ-OFF           VALUE '0'.
               88  KOPI-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-PROCESS-OFF        VALUE '0'.
               88  KOPI-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOPI-LEVEL-INIT-OFF     VALUE '0'.
               88  KOPI-LEVEL-INIT         VALUE '1'.
           05  OPPSMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  EDBTAB-DATA-FIELDS.
               10  TFNR                    PICTURE X(3).
               10  FFNR                    PICTURE X(3).
               10  EDB                     PICTURE X(7).
               10  TEDB                    PICTURE X(7).
           05  EDBTAB-MP                   PICTURE X(13).
           05  EDBTAB-MC                   PICTURE X(13).
           05  EDBTAB-M-05             REDEFINES EDBTAB-MC.
               10  EDBTAB-M-05-M3.
                   15  EDBTAB-M-05-M3-TFNR-G.
                       20  EDBTAB-M-05-M3-TFNR PICTURE X(3).
               10  EDBTAB-M-05-M2.
                   15  EDBTAB-M-05-M2-FFNR-G.
                       20  EDBTAB-M-05-M2-FFNR PICTURE X(3).
               10  EDBTAB-M-05-M1.
                   15  EDBTAB-M-05-M1-EDB-G.
                       20  EDBTAB-M-05-M1-EDB PICTURE X(7).
           05  KOPI-LEVEL-02.
               10  KOPI-02-L1.
                   15  KOPI-02-L1-FIRMA    PICTURE X(3).
           05  KOPI-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  BETEGN                  PICTURE X(30).
               10  IPRIS-IO.
                   15  IPRIS               PICTURE S9(7)V9(2).
               10  UPRIS-IO.
                   15  UPRIS               PICTURE S9(7)V9(2).
               10  VGR                     PICTURE X(5).
               10  PT                      PICTURE X(1).
               10  MERKNA                  PICTURE X(1).
               10  FAB                     PICTURE X(1).
               10  REC2                    PICTURE X(91).
               10  ARTKOM                  PICTURE X(14).
               10  EDATO-IO.
                   15  EDATO               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VALGMU                  PICTURE X(17).
               10  VALG01                  PICTURE X(1).
               10  VALG02                  PICTURE X(1).
               10  PRISTI-IO.
                   15  PRISTI              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRISTT                  PICTURE X(1).
               10  FFRN                    PICTURE X(3).
               10  LEVPRI-IO.
                   15  LEVPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVRAB-IO.
                   15  LEVRAB              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
           05  KOPI-MP                     PICTURE X(13).
           05  KOPI-MC                     PICTURE X(13).
           05  KOPI-M-02               REDEFINES KOPI-MC.
               10  KOPI-M-02-M3.
                   15  KOPI-M-02-M3-FIRMA-G.
                       20  KOPI-M-02-M3-FIRMA PICTURE X(3).
               10  KOPI-M-02-M2.
                   15  KOPI-M-02-M2-FFRN-G.
                       20  KOPI-M-02-M2-FFRN PICTURE X(3).
               10  KOPI-M-02-M1.
                   15  KOPI-M-02-M1-EDBNR-G.
                       20  KOPI-M-02-M1-EDBNR PICTURE X(7).
           05  OPPSMAS-DATA-FIELDS.
               10  RA                      PICTURE X(1).
               10  EDBNR                   PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  FIRMAF-DATA-FIELDS.
               10  SISTE-IO.
                   15  SISTE               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMP-REMAINDER              PICTURE S9(9)V9(9).
           05  TEMPORARY-FIELDS.
               10  NYNR-IO.
                   15  NYNR                PICTURE S9(7).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(8).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(8).
               10  KEY6                    PICTURE X(6).
               10  KEY7                    PICTURE X(7).
               10  KEY-X                   PICTURE X(21).
               10  NR1-IO.
                   15  NR1                 PICTURE S9(1).
               10  NR2-IO.
                   15  NR2                 PICTURE S9(1).
               10  NR3-IO.
                   15  NR3                 PICTURE S9(1).
               10  NR4-IO.
                   15  NR4                 PICTURE S9(1).
               10  NR5-IO.
                   15  NR5                 PICTURE S9(1).
               10  NR6-IO.
                   15  NR6                 PICTURE S9(1).
               10  NR7-IO.
                   15  NR7                 PICTURE S9(1).
               10  R1-IO.
                   15  R1                  PICTURE S9(2).
               10  R2-IO.
                   15  R2                  PICTURE S9(2).
               10  R3-IO.
                   15  R3                  PICTURE S9(2).
               10  R4-IO.
                   15  R4                  PICTURE S9(2).
               10  R5-IO.
                   15  R5                  PICTURE S9(2).
               10  R6-IO.
                   15  R6                  PICTURE S9(2).
               10  R7-IO.
                   15  R7                  PICTURE S9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(3).
               10  MODUL-IO.
                   15  MODUL               PICTURE S9(6).
               10  REST-IO.
                   15  REST                PICTURE S9(3).
               10  KEYNY                   PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-05                    TO TRUE
           SET NOT-I-02                    TO TRUE
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
           IF  EDBTAB-PROCESS
               SET EDBTAB-PROCESS-OFF      TO TRUE
               SET EDBTAB-READ             TO TRUE
           END-IF
 
           IF  EDBTAB-READ
               PERFORM EDBTAB-GET
               SET EDBTAB-READ-OFF         TO TRUE
               IF  NOT EDBTAB-EOF
                   PERFORM EDBTAB-MATCH-SET
               END-IF
           END-IF
 
           IF  KOPI-PROCESS
               SET KOPI-PROCESS-OFF        TO TRUE
               SET KOPI-READ               TO TRUE
           END-IF
 
           IF  KOPI-READ
               PERFORM KOPI-GET
               SET KOPI-READ-OFF           TO TRUE
               IF  NOT KOPI-EOF
                   PERFORM KOPI-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM KOPI-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  EDBTAB-PROCESS
               PERFORM EDBTAB-IDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-IDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  EDBTAB-PROCESS
               PERFORM EDBTAB-FLDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KOPI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           SET NOT-I-71                    TO TRUE
           SET NOT-I-43                    TO TRUE
           IF  (I-L1)
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-97                TO TRUE
               MOVE 0                      TO PAGE0
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-10)
               ADD SISTE TO ZERO       GIVING NYNR
      ********************************************
      *  SJEKKER FIRMANR, FOR OM GÅTT RUNDT      *
      ********************************************
           END-IF
           IF  (I-L1 AND NOT-I-10)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '658'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '614'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '996'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '626'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '948'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '951'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '699'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '958'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '992'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '911'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '956'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '910'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '901'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '981'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '938'
                   SET I-55                TO TRUE
               END-IF
      *  L1N10N55FIRMA     COMP "627"                    55
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '391'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '940'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '985'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '722'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '725'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '972'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '739'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '709'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '680'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '682'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '995'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '685'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '967'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '630'
                   SET I-55                TO TRUE
               END-IF
      *  L1N10N55FIRMA     COMP "720"                    55
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '163'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '834'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '909'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '968'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '965'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '218'
                   SET I-55                TO TRUE
               END-IF
      *  L1N10N55FIRMA     COMP "410"                    55
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '424'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '426'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '732'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '754'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '585'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '429'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '628'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '199'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '674'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '969'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '929'
                   SET I-55                TO TRUE
               END-IF
      *  L1N10N55FIRMA     COMP "656"                    55
      *  L1N10N55FIRMA     COMP "939"                    55
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '977'
                   SET I-55                TO TRUE
               END-IF
      *  L1N10N55FIRMA     COMP "665"                    55
      *  L1N10N55FIRMA     COMP "395"                    55
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '612'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '738'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '900'
                   SET I-55                TO TRUE
               END-IF
      *  L1N10N55FIRMA     COMP "845"                    55
      *  L1N10N55FIRMA     COMP "964"                    55
           END-IF
           IF  (I-L1 AND NOT-I-10 AND NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  FIRMA = '740'
                   SET I-55                TO TRUE
               END-IF
      *  L1N10N55FIRMA     COMP "540"                    55
      *  L1N10N55FIRMA     COMP "259"                    55
      ********************************************
      *  SJEKKER FIRMANR, FOR SJEKK EDBNR        *
      ********************************************
           END-IF
           IF  (I-L1 AND NOT-I-10)
               SET NOT-I-12                TO TRUE
               IF  FIRMA = '658'
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-13                TO TRUE
               IF  FIRMA = '614'
                   SET I-13                TO TRUE
               END-IF
               SET NOT-I-14                TO TRUE
               IF  FIRMA = '996'
                   SET I-14                TO TRUE
               END-IF
               SET NOT-I-16                TO TRUE
               IF  FIRMA = '626'
                   SET I-16                TO TRUE
               END-IF
               SET NOT-I-17                TO TRUE
               IF  FIRMA = '948'
                   SET I-17                TO TRUE
               END-IF
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '951'
                   SET I-18                TO TRUE
               END-IF
               SET NOT-I-19                TO TRUE
               IF  FIRMA = '699'
                   SET I-19                TO TRUE
               END-IF
               SET NOT-I-20                TO TRUE
               IF  FIRMA = '958'
                   SET I-20                TO TRUE
               END-IF
               SET NOT-I-21                TO TRUE
               IF  FIRMA = '992'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  FIRMA = '911'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  FIRMA = '956'
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  FIRMA = '910'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  FIRMA = '901'
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-28                TO TRUE
               IF  FIRMA = '981'
                   SET I-28                TO TRUE
               END-IF
               SET NOT-I-29                TO TRUE
               IF  FIRMA = '938'
                   SET I-29                TO TRUE
               END-IF
      *  L1N10   FIRMA     COMP "627"                    29
           END-IF
           IF  (I-L1 AND NOT-I-10)
               SET NOT-I-30                TO TRUE
               IF  FIRMA = '391'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '940'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '985'
                   SET I-32                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               IF  FIRMA = '722'
                   SET I-33                TO TRUE
               END-IF
      *  L1N10   FIRMA     COMP "755"                    34
           END-IF
           IF  (I-L1 AND NOT-I-10)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '725'
                   SET I-35                TO TRUE
               END-IF
               SET NOT-I-36                TO TRUE
               IF  FIRMA = '972'
                   SET I-36                TO TRUE
               END-IF
               SET NOT-I-37                TO TRUE
               IF  FIRMA = '739'
                   SET I-37                TO TRUE
               END-IF
               SET NOT-I-38                TO TRUE
               IF  FIRMA = '709'
                   SET I-38                TO TRUE
               END-IF
               SET NOT-I-39                TO TRUE
               IF  FIRMA = '680'
                   SET I-39                TO TRUE
               END-IF
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '682'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  FIRMA = '995'
                   SET I-42                TO TRUE
               END-IF
               SET NOT-I-44                TO TRUE
               IF  FIRMA = '685'
                   SET I-44                TO TRUE
               END-IF
               SET NOT-I-45                TO TRUE
               IF  FIRMA = '967'
                   SET I-45                TO TRUE
               END-IF
               SET NOT-I-46                TO TRUE
               IF  FIRMA = '630'
                   SET I-46                TO TRUE
               END-IF
      *  L1N10   FIRMA     COMP "720"                    47
           END-IF
           IF  (I-L1 AND NOT-I-10)
               SET NOT-I-47                TO TRUE
               IF  FIRMA = '163'
                   SET I-47                TO TRUE
               END-IF
               SET NOT-I-48                TO TRUE
               IF  FIRMA = '834'
                   SET I-48                TO TRUE
               END-IF
               SET NOT-I-49                TO TRUE
               IF  FIRMA = '909'
                   SET I-49                TO TRUE
               END-IF
               SET NOT-I-51                TO TRUE
               IF  FIRMA = '968'
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  FIRMA = '965'
                   SET I-52                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  FIRMA = '977'
                   SET I-53                TO TRUE
               END-IF
               SET NOT-I-54                TO TRUE
               IF  FIRMA = '218'
                   SET I-54                TO TRUE
               END-IF
               SET NOT-I-56                TO TRUE
               IF  FIRMA = '395'
                   SET I-56                TO TRUE
               END-IF
               SET NOT-I-57                TO TRUE
               IF  FIRMA = '410'
                   SET I-57                TO TRUE
               END-IF
               SET NOT-I-58                TO TRUE
               IF  FIRMA = '424'
                   SET I-58                TO TRUE
               END-IF
               SET NOT-I-59                TO TRUE
               IF  FIRMA = '426'
                   SET I-59                TO TRUE
               END-IF
               SET NOT-I-60                TO TRUE
               IF  FIRMA = '612'
                   SET I-60                TO TRUE
               END-IF
               SET NOT-I-61                TO TRUE
               IF  FIRMA = '732'
                   SET I-61                TO TRUE
               END-IF
               SET NOT-I-62                TO TRUE
               IF  FIRMA = '754'
                   SET I-62                TO TRUE
               END-IF
               SET NOT-I-63                TO TRUE
               IF  FIRMA = '738'
                   SET I-63                TO TRUE
               END-IF
               SET NOT-I-64                TO TRUE
               IF  FIRMA = '585'
                   SET I-64                TO TRUE
               END-IF
               SET NOT-I-65                TO TRUE
               IF  FIRMA = '429'
                   SET I-65                TO TRUE
               END-IF
               SET NOT-I-66                TO TRUE
               IF  FIRMA = '628'
                   SET I-66                TO TRUE
               END-IF
               SET NOT-I-67                TO TRUE
               IF  FIRMA = '199'
                   SET I-67                TO TRUE
               END-IF
               SET NOT-I-68                TO TRUE
               IF  FIRMA = '674'
                   SET I-68                TO TRUE
               END-IF
               SET NOT-I-69                TO TRUE
               IF  FIRMA = '969'
                   SET I-69                TO TRUE
               END-IF
               SET NOT-I-72                TO TRUE
               IF  FIRMA = '900'
                   SET I-72                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  FIRMA = '939'
                   SET I-73                TO TRUE
               END-IF
               SET NOT-I-74                TO TRUE
               IF  FIRMA = '502'
                   SET I-74                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  FIRMA = '929'
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '845'
                   SET I-76                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  FIRMA = '656'
                   SET I-77                TO TRUE
               END-IF
               SET NOT-I-78                TO TRUE
               IF  FIRMA = '964'
                   SET I-78                TO TRUE
               END-IF
      *  L1N10   FIRMA     COMP "388"                    79
           END-IF
           IF  (I-L1 AND NOT-I-10)
               SET NOT-I-80                TO TRUE
               IF  FIRMA = '740'
                   SET I-80                TO TRUE
               END-IF
               SET NOT-I-81                TO TRUE
               IF  FIRMA = '540'
                   SET I-81                TO TRUE
               END-IF
               SET NOT-I-82                TO TRUE
               IF  FIRMA = '665'
                   SET I-82                TO TRUE
               END-IF
               SET NOT-I-83                TO TRUE
               IF  FIRMA = '259'
                   SET I-83                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-12)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8618372
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-13)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7202539
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-14)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 5295572
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-16)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-17)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8289743
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-18)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6972276
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-19)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 3693945
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-20)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8572135
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-21)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-22)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6678637
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-23)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8047839
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-24)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-27)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6000002
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-28)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6662560
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-29)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 4812328
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-30)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-31)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6470696
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-32)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-33)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6311148
                   SET I-11                TO TRUE
               END-IF
      * N10 34   NYNR      COMP 6256783              11
           END-IF
           IF  (NOT-I-10 AND I-35)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6218067
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-36)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-37)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6232078
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-38)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 5522005
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-39)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-40)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7292708
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-42)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 5541840
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-44)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6809235
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-45)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-46)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7927290
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-47)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 4724518
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-48)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-49)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7089406
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-51)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8338949
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-52)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7331134
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-53)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 5761891
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-54)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7261772
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-56)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 5968038
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-57)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8939012
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-58)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6512801
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-59)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6006469
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-60)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6541186
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-61)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6916767
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-62)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7270208
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-63)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 2511282
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-64)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6616747
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-65)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6495567
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-66)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6885748
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-67)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-68)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6789021
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-69)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6815340
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-72)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 6845622
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-73)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 1000004
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-74)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 7746695
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-75)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-76)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 1000004
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-77)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 1016989
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-78)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 1000004
                   SET I-11                TO TRUE
               END-IF
      * N10 79   NYNR      COMP 5667100              11
           END-IF
           IF  (NOT-I-10 AND I-80)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 8999900
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-81)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 3782778
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-82)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 1000004
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND I-83)
               SET NOT-I-11                TO TRUE
               IF  NYNR > 1004068
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-11)
               SET I-43                    TO TRUE
           END-IF
           IF  (I-43)
               GO TO SLUTT-T
           END-IF
           IF  (I-25)
               SET I-26                    TO TRUE
      *
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  VALG01 = 'J'
               SET I-41                    TO TRUE
           END-IF
           IF  (I-MR)
               ADD 1                       TO ANTMR
           END-IF
           IF  (NOT-I-MR)
               ADD 1                       TO ANTNMR
           END-IF
           IF  (I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-05)
               GO TO SLUTT-T
      ********************************************
      *  RUTINE FOR Å OVERFØRE NYE VARER.        *
      ********************************************
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  MERKNA = '1'
               SET I-43                    TO TRUE
           END-IF
           IF  (I-43)
               GO TO SLUTT-T
           END-IF
           MOVE FIRMA                      TO KEY6 (1:3)
           MOVE ALFA                       TO KEY6 (4:3)
           MOVE KEY6                       TO KEY7 (1:6)
           MOVE ' '                        TO KEY7 (7:1)
           MOVE KEY7                       TO KEY-X (1:7)
           MOVE ARTKOM                     TO KEY-X (8:14)
           MOVE KEY-X                      TO OPPSMAS-KEY1
           READ OPPSMAS RECORD KEY IS OPPSMAS-KEY1
           INVALID KEY
               SET I-70                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-70                TO TRUE
               PERFORM OPPSMAS-FLDSET
               PERFORM OPPSMAS-IDSET
           END-READ
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  RA = 'A'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET I-71                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  VALG02 = 'J'
               SET I-43                    TO TRUE
           END-IF
           IF  (I-43 AND NOT-I-70)
               SET NOT-I-43                TO TRUE
           END-IF
           IF  (NOT-I-70)
               GO TO SLUTT-T
           END-IF
           IF  (I-43)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-25)
               PERFORM RBSRUT-S
           END-IF
           IF  (NOT-I-55)
               PERFORM DANNE-S
           END-IF
           IF  (I-55)
               PERFORM MELLOM-S
           END-IF
           SET I-25                        TO TRUE
           SET I-50                        TO TRUE
           SET NOT-I-99                    TO TRUE
           IF  PT = 'N'
               SET I-99                    TO TRUE
           END-IF.
 
       SLUTT-T.
      ********************************************
      *  RUTINE FOR DANNING AV NYTT EDBNR        *
      ********************************************
           CONTINUE.
 
       DANNE-S SECTION.
       DANNE-S-P.
           ADD 1                           TO NYNR.
 
       START-X-T.
           DIVIDE NYNR BY 1            GIVING NR1
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 10           GIVING NR2
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 100          GIVING NR3
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 1000         GIVING NR4
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 10000        GIVING NR5
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 100000       GIVING NR6
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 1000000      GIVING NR7
                                    REMAINDER TEMP-REMAINDER
           MULTIPLY 1 BY NR1           GIVING R1
           MULTIPLY 2 BY NR2           GIVING R2
           MULTIPLY 3 BY NR3           GIVING R3
           MULTIPLY 4 BY NR4           GIVING R4
           MULTIPLY 5 BY NR5           GIVING R5
           MULTIPLY 6 BY NR6           GIVING R6
           MULTIPLY 7 BY NR7           GIVING R7
           ADD R1 TO ZERO              GIVING SUM-X
           ADD R2                          TO SUM-X
           ADD R3                          TO SUM-X
           ADD R4                          TO SUM-X
           ADD R5                          TO SUM-X
           ADD R6                          TO SUM-X
           ADD R7                          TO SUM-X
           DIVIDE SUM-X BY 11          GIVING MODUL
                                    REMAINDER TEMP-REMAINDER
           MOVE TEMP-REMAINDER             TO REST
           SET NOT-I-15                    TO TRUE
           IF  REST = 0
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-15)
               ADD 1                       TO NYNR
               GO TO START-X-T
           END-IF.
      *
      ********************************************
      *  RUTINE FOR Å FINNE LEDIGE EDBNR INNIMELLOM
      ********************************************
 
       MELLOM-S SECTION.
       MELLOM-S-P.
           IF  (NOT-I-97)
               MOVE 0000050                TO NYNR
           END-IF
           SET I-97                        TO TRUE
           ADD 1                           TO NYNR.
 
       START1-T.
           DIVIDE NYNR BY 1            GIVING NR1
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 10           GIVING NR2
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 100          GIVING NR3
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 1000         GIVING NR4
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 10000        GIVING NR5
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 100000       GIVING NR6
                                    REMAINDER TEMP-REMAINDER
           DIVIDE NYNR BY 1000000      GIVING NR7
                                    REMAINDER TEMP-REMAINDER
           MULTIPLY 1 BY NR1           GIVING R1
           MULTIPLY 2 BY NR2           GIVING R2
           MULTIPLY 3 BY NR3           GIVING R3
           MULTIPLY 4 BY NR4           GIVING R4
           MULTIPLY 5 BY NR5           GIVING R5
           MULTIPLY 6 BY NR6           GIVING R6
           MULTIPLY 7 BY NR7           GIVING R7
           ADD R1 TO ZERO              GIVING SUM-X
           ADD R2                          TO SUM-X
           ADD R3                          TO SUM-X
           ADD R4                          TO SUM-X
           ADD R5                          TO SUM-X
           ADD R6                          TO SUM-X
           ADD R7                          TO SUM-X
           DIVIDE SUM-X BY 11          GIVING MODUL
                                    REMAINDER TEMP-REMAINDER
           MOVE TEMP-REMAINDER             TO REST
           SET NOT-I-15                    TO TRUE
           IF  REST = 0
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-15)
               ADD 1                       TO NYNR
               GO TO START1-T
      * FINNER NYE EDBNR INNIMELLOM
           END-IF
           MOVE FIRMA                      TO KEYNY (1:3)
           MOVE NYNR                       TO KEYNY (4:7)
           MOVE KEYNY                      TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-13                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-13                TO TRUE
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (NOT-I-13)
               ADD 1                       TO NYNR
               GO TO START1-T
           END-IF.
      *
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE '  '                       TO BBEST
           MOVE 'VAR27'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'VRG822  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       EDBTAB-GET SECTION.
       EDBTAB-GET-P.
           IF  EDBTAB-EOF-OFF
               READ EDBTAB
               AT END
                   SET EDBTAB-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       EDBTAB-FLDSET SECTION.
       EDBTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE EDBTAB-IO-AREA (1:3)   TO TFNR (1:3)
               MOVE EDBTAB-IO-AREA (4:3)   TO FFNR (1:3)
               MOVE EDBTAB-IO-AREA (7:7)   TO EDB (1:7)
               MOVE EDBTAB-IO-AREA (14:7)  TO TEDB (1:7)
           END-EVALUATE.
 
       EDBTAB-IDSET SECTION.
       EDBTAB-IDSET-P.
           SET I-05                        TO TRUE.
 
       EDBTAB-MATCH-SET SECTION.
       EDBTAB-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE EDBTAB-IO-AREA (1:3)   TO EDBTAB-M-05-M3-TFNR
               MOVE EDBTAB-IO-AREA (4:3)   TO EDBTAB-M-05-M2-FFNR
               MOVE EDBTAB-IO-AREA (7:7)   TO EDBTAB-M-05-M1-EDB
           END-EVALUATE.
 
       KOPI-GET SECTION.
       KOPI-GET-P.
           IF  KOPI-EOF-OFF
               READ KOPI
               AT END
                   SET KOPI-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KOPI-FLDSET SECTION.
       KOPI-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KOPI-IO-AREA (1:1) = '7'
            AND   KOPI-IO-AREA (2:1) = '1' )
               MOVE KOPI-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE KOPI-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE KOPI-IO-AREA (13:3)    TO ALFA (1:3)
               MOVE KOPI-IO-AREA (16:20)   TO ARTNR (1:20)
               MOVE KOPI-IO-AREA (36:30)   TO BETEGN (1:30)
               MOVE KOPI-IO-AREA (66:9)    TO IPRIS-IO
               INSPECT IPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE KOPI-IO-AREA (75:9)    TO UPRIS-IO
               INSPECT UPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE KOPI-IO-AREA (84:5)    TO VGR (1:5)
               MOVE KOPI-IO-AREA (89:1)    TO PT (1:1)
               MOVE KOPI-IO-AREA (90:1)    TO MERKNA (1:1)
               MOVE KOPI-IO-AREA (91:1)    TO FAB (1:1)
               MOVE KOPI-IO-AREA (1:91)    TO REC2 (1:91)
               MOVE KOPI-IO-AREA (99:14)   TO ARTKOM (1:14)
               MOVE KOPI-IO-AREA (119:4)   TO EDATO-IO
               MOVE KOPI-IO-AREA (123:17)  TO VALGMU (1:17)
               MOVE KOPI-IO-AREA (123:1)   TO VALG01 (1:1)
               MOVE KOPI-IO-AREA (124:1)   TO VALG02 (1:1)
               MOVE KOPI-IO-AREA (141:4)   TO PRISTI-IO
               MOVE KOPI-IO-AREA (145:1)   TO PRISTT (1:1)
               MOVE KOPI-IO-AREA (146:3)   TO FFRN (1:3)
               MOVE KOPI-IO-AREA (149:5)   TO LEVPRI-IO
               MOVE KOPI-IO-AREA (154:2)   TO LEVRAB-IO
           END-EVALUATE.
 
       KOPI-IDCHK SECTION.
       KOPI-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KOPI-IO-AREA (1:1) = '7'
            AND   KOPI-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KOPI-IDSET SECTION.
       KOPI-IDSET-P.
           EVALUATE TRUE
           WHEN ( KOPI-IO-AREA (1:1) = '7'
            AND   KOPI-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       KOPI-CHK-LEVEL SECTION.
       KOPI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KOPI-IO-AREA (1:1) = '7'
            AND   KOPI-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO KOPI-LEVEL-02
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-02-L1-FIRMA
               IF  KOPI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOPI-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOPI-02-L1            TO THE-PRIOR-L1
               SET KOPI-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       KOPI-MATCH-SET SECTION.
       KOPI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( KOPI-IO-AREA (1:1) = '7'
            AND   KOPI-IO-AREA (2:1) = '1' )
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-M-02-M3-FIRMA
               MOVE KOPI-IO-AREA (146:3)   TO KOPI-M-02-M2-FFRN
               MOVE KOPI-IO-AREA (6:7)     TO KOPI-M-02-M1-EDBNR
           END-EVALUATE.
 
       OPPSMAS-FLDSET SECTION.
       OPPSMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (1:1)  TO RA (1:1)
               MOVE OPPSMAS-IO-AREA (23:7) TO EDBNR (1:7)
           END-EVALUATE.
 
       OPPSMAS-IDSET SECTION.
       OPPSMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (124:4) TO SISTE-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  EDBTAB-EOF
               MOVE HIGH-VALUES            TO EDBTAB-MC
                                              EDBTAB-MP
           END-IF
           IF  KOPI-EOF
               MOVE HIGH-VALUES            TO KOPI-MC
                                              KOPI-MP
           END-IF
           IF  EDBTAB-MC < EDBTAB-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KOPI-MC < KOPI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  EDBTAB-MC < KOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET EDBTAB-PROCESS      TO TRUE
                   MOVE EDBTAB-MC          TO EDBTAB-MP
                   IF  EDBTAB-MC = KOPI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KOPI-MC < EDBTAB-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KOPI-PROCESS        TO TRUE
                   MOVE KOPI-MC            TO KOPI-MP
                   IF  KOPI-MC = EDBTAB-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  EDBTAB-MC = KOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET EDBTAB-PROCESS      TO TRUE
                   MOVE EDBTAB-MC          TO EDBTAB-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR AND NOT-I-41)
               MOVE SPACES TO KOPIUT-IO-AREA
               INITIALIZE KOPIUT-IO-AREA
               MOVE REC2                   TO KOPIUT-IO-AREA (1:91)
               MOVE TEDB                   TO KOPIUT-IO-AREA (6:7)
               MOVE '2'                    TO KOPIUT-IO-AREA (93:1)
               MOVE EDATO                  TO XO-70P
               MOVE XO-70P-EF              TO KOPIUT-IO-AREA (94:4)
               MOVE PRISTT                 TO KOPIUT-IO-AREA (98:1)
               MOVE PRISTI                 TO XO-52P
               MOVE XO-52P-EF              TO KOPIUT-IO-AREA (99:4)
               MOVE VALGMU                 TO KOPIUT-IO-AREA (103:17)
               MOVE LEVPRI                 TO XO-72P
               MOVE XO-72P-EF              TO KOPIUT-IO-AREA (121:5)
               MOVE LEVRAB                 TO XO-21P
               MOVE XO-21P-EF              TO KOPIUT-IO-AREA (126:2)
               WRITE KOPIUT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR AND I-71)
           AND (NOT-I-43)
               MOVE SPACES TO KOPIUT-IO-AREA
               INITIALIZE KOPIUT-IO-AREA
               MOVE REC2                   TO KOPIUT-IO-AREA (1:91)
               MOVE EDBNR                  TO KOPIUT-IO-AREA (6:7)
               MOVE '2'                    TO KOPIUT-IO-AREA (93:1)
               MOVE EDATO                  TO XO-70P
               MOVE XO-70P-EF              TO KOPIUT-IO-AREA (94:4)
               MOVE PRISTT                 TO KOPIUT-IO-AREA (98:1)
               MOVE PRISTI                 TO XO-52P
               MOVE XO-52P-EF              TO KOPIUT-IO-AREA (99:4)
               MOVE VALGMU                 TO KOPIUT-IO-AREA (103:17)
               MOVE LEVPRI                 TO XO-72P
               MOVE XO-72P-EF              TO KOPIUT-IO-AREA (121:5)
               MOVE LEVRAB                 TO XO-21P
               MOVE XO-21P-EF              TO KOPIUT-IO-AREA (126:2)
               WRITE KOPIUT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR AND NOT-I-43)
           AND (NOT-I-71)
               MOVE SPACES TO KOPIUT-IO-AREA
               INITIALIZE KOPIUT-IO-AREA
               MOVE REC2                   TO KOPIUT-IO-AREA (1:91)
               MOVE NYNR-IO                TO KOPIUT-IO-AREA (6:7)
               MOVE '1'                    TO KOPIUT-IO-AREA (93:1)
               MOVE EDATO                  TO XO-70P
               MOVE XO-70P-EF              TO KOPIUT-IO-AREA (94:4)
               MOVE PRISTT                 TO KOPIUT-IO-AREA (98:1)
               MOVE PRISTI                 TO XO-52P
               MOVE XO-52P-EF              TO KOPIUT-IO-AREA (99:4)
               MOVE VALGMU                 TO KOPIUT-IO-AREA (103:17)
               MOVE LEVPRI                 TO XO-72P
               MOVE XO-72P-EF              TO KOPIUT-IO-AREA (121:5)
               MOVE LEVRAB                 TO XO-21P
               MOVE XO-21P-EF              TO KOPIUT-IO-AREA (126:2)
               WRITE KOPIUT-IO-AREA
           END-IF
           IF  (I-02 AND I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE NYNR-IO                TO LISTE-IO-AREA (2:7)
               MOVE VGR                    TO LISTE-IO-AREA (11:5)
               MOVE ALFA                   TO LISTE-IO-AREA (18:3)
               MOVE FAB                    TO LISTE-IO-AREA (23:1)
               MOVE ARTNR                  TO LISTE-IO-AREA (26:20)
               MOVE BETEGN                 TO LISTE-IO-AREA (48:30)
               MOVE UPRIS                  TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (79:12)
               MOVE IPRIS                  TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (92:12)
               IF  (I-99)
                   MOVE '**  N E T T O - P R I S ' TO LISTE-IO-AREA
                                                              (106:24)
               END-IF
               IF  (I-99)
                   MOVE ' **'              TO LISTE-IO-AREA (130:3)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-02 AND I-50 AND NOT-I-26)
           AND (NOT-I-86)
      *                        FINAVN    30
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (28:3)
               MOVE LOPNVN                 TO LISTE-IO-AREA (42:35)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (93:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (119:4)
               IF  (I-L1)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (123:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'EDB-NR.  VGR    ALF  F' TO LISTE-IO-AREA (2:22)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (26:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (48:14)
               MOVE 'SALGSPRIS'            TO LISTE-IO-AREA (82:9)
               MOVE 'SELVKOST'             TO LISTE-IO-AREA (96:8)
               MOVE 'MERKNAD '             TO LISTE-IO-AREA (113:8)
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
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
      *                        FINAVN    30
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (28:3)
               MOVE LOPNVN                 TO LISTE-IO-AREA (42:35)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (93:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (119:4)
               IF  (I-L1)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (123:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'EDB-NR.  VGR    ALF  F' TO LISTE-IO-AREA (2:22)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (26:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (48:14)
               MOVE 'SALGSPRIS'            TO LISTE-IO-AREA (82:9)
               MOVE 'SELVKOST'             TO LISTE-IO-AREA (96:8)
               MOVE 'MERKNAD '             TO LISTE-IO-AREA (113:8)
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
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-55)
               MOVE NYNR                   TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (124:4)
               REWRITE FIRMAF-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = FIRMAF'
               END-REWRITE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTMR-IO               TO LISTE-IO-AREA (3:8)
               MOVE ANTNMR-IO              TO LISTE-IO-AREA (13:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-11)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE NYNR-IO                TO LISTE-IO-AREA (2:7)
               MOVE '** IKKE FLERE NYE ARTIKL' TO LISTE-IO-AREA (12:24)
               MOVE 'ER KOPIERT FEIL M/EDBNR.' TO LISTE-IO-AREA (36:24)
               MOVE ' KONTAKT AUTODATA    ** ' TO LISTE-IO-AREA (60:24)
               MOVE FIRMA                  TO LISTE-IO-AREA (85:3)
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
           INITIALIZE EDBTAB-DATA-FIELDS
           SET EDBTAB-EOF-OFF              TO TRUE
           SET EDBTAB-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO EDBTAB-MC
                                              EDBTAB-MP
           OPEN INPUT EDBTAB
           SET KOPI-LEVEL-INIT             TO TRUE
           INITIALIZE KOPI-DATA-FIELDS
           SET KOPI-EOF-OFF                TO TRUE
           SET KOPI-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO KOPI-MC
                                              KOPI-MP
           OPEN INPUT KOPI
           INITIALIZE OPPSMAS-DATA-FIELDS
           OPEN INPUT OPPSMAS
           OPEN INPUT VAREMAS
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN I-O FIRMAF
           OPEN OUTPUT KOPIUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE EDBTAB
           CLOSE KOPI
           CLOSE OPPSMAS
           CLOSE VAREMAS
           CLOSE FIRMAF
           CLOSE KOPIUT
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
