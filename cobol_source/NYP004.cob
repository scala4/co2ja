       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYP004R.
      **********************************************  Z-WIN-RPG2   ****
      *  OPPDATERING AV VAREARKIVET NYE PRISER              **
      *  OG NY DATO + KVITTERINGSLISTE ART.SOM ER ENDRET    **
      ********************************************************
      *                                          XX2000XXOKXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYP004.rpg
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
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT PRISMAS
               ASSIGN TO PRISMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS PRISMAS-STATUS
               RECORD KEY IS PRISMAS-KEY1.
           SELECT PRINTF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNPUT
               BLOCK CONTAINS 9440
               RECORD CONTAINS 80.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(80).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD PRISMAS
               RECORD CONTAINS 80.
       01  PRISMAS-IO-AREA.
           05  PRISMAS-IO-AREA-X.
               10  PRISMAS-KEY1            PICTURE X(13).
               10  FILLER                  PICTURE X(67).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD PRINTF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRINTF-IO-PRINT.
           05  PRINTF-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 PRINTF-IO-AREA.
           05  PRINTF-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  PRISMAS-STATUS              PICTURE 99 VALUE 0.
           10  PRINTF-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNPUT-LEVEL-INIT-OFF   VALUE '0'.
               88  INNPUT-LEVEL-INIT       VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRISMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRINTF-DATA-FIELDS.
               10  PRINTF-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-CLR-IO           PICTURE X VALUE 'Y'.
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
           05  DATOER-XX REDEFINES LDATA-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(242).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(234).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
               10  FILLER                  PICTURE X(177).
      *DSDS: DATA STRUCTURE FIELDS
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  INNPUT-LEVEL-01.
               10  INNPUT-01-L3.
                   15  INNPUT-01-L3-FIRMA  PICTURE X(3).
               10  INNPUT-01-L1.
                   15  INNPUT-01-L1-REFNR  PICTURE X(5).
           05  INNPUT-DATA-FIELDS.
               10  REC                     PICTURE X(80).
               10  RA                      PICTURE X(1).
               10  PKEY                    PICTURE X(13).
               10  FIRMA                   PICTURE X(3).
               10  REFNR                   PICTURE X(5).
               10  SEQ                     PICTURE X(4).
               10  EDBNR                   PICTURE X(7).
               10  NYSVS-IO.
                   15  NYSVS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  UTSALG-IO.
                   15  UTSALG              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NYLEVP-IO.
                   15  NYLEVP              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  DATO-IO.
                   15  DATO                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  LEVDAT-IO.
                   15  LEVDAT              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  OPPLEV                  PICTURE X(1).
      *
               10  REFPER                  PICTURE X(2).
               10  OPPDAT                  PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VSELVK-IO.
                   15  VSELVK              PICTURE S9(7)V9(2).
               10  VPRIS-IO.
                   15  VPRIS               PICTURE S9(7)V9(2).
               10  VDATO-IO.
                   15  VDATO               PICTURE S9(4).
               10  PT                      PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  AVD                     PICTURE X(1).
           05  PRISMAS-DATA-FIELDS.
      **** SNU UDATE TIL FORMAT ≈≈/MM/DD.  *****************
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  TEST11-IO.
                   15  TEST11              PICTURE S9(4).
               10  TEST12-IO.
                   15  TEST12              PICTURE S9(6).
               10  TEST08-IO.
                   15  TEST08              PICTURE S9(8).
               10  OPPD1                   PICTURE X(1).
               10  REFP1                   PICTURE X(2).
               10  OPPL1                   PICTURE X(1).
               10  DATO-N-IO.
                   15  DATO-N              PICTURE S9(7).
               10  TEST2-IO.
                   15  TEST2               PICTURE S9(8).
               10  LEVDAT-N-IO.
                   15  LEVDAT-N            PICTURE S9(7).
               10  TEST4-IO.
                   15  TEST4               PICTURE S9(8).
               10  KEY4                    PICTURE X(4).
               10  KEY9                    PICTURE X(9).
               10  KEY13                   PICTURE X(13).
               10  FREFNR                  PICTURE X(5).
               10  FFNR                    PICTURE X(3).
               10  KEY-X                   PICTURE X(10).
               10  NSVS-IO.
                   15  NSVS                PICTURE S9(7)V9(2).
               10  NUTP-IO.
                   15  NUTP                PICTURE S9(7)V9(2).
               10  NLEVP-IO.
                   15  NLEVP               PICTURE S9(7)V9(2).
               10  EDATO-IO.
                   15  EDATO               PICTURE S9(6).
               10  LDATO-IO.
                   15  LDATO               PICTURE S9(6).
               10  DATOO-IO.
                   15  DATOO               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72YYZ                PICTURE Z.ZZZ.ZZZ,ZZ.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   SET INNPUT-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNPUT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-50)
               MOVE UYEAR                  TO TEST11-IO (3:2)
               MOVE UMONTH                 TO TEST11 (1:2)
               MOVE TEST11                 TO TEST12-IO (3:4)
               MOVE UDAY                   TO TEST12 (1:2)
               MOVE 'A'                    TO DATOK
               MOVE TEST12                 TO DATO6
               CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           END-IF
           IF  (NOT-I-50)
               MOVE AMD8                   TO TEST08-IO
           END-IF
           SET I-50                        TO TRUE
      ******************************************************
           IF  (I-L1)
               SET NOT-I-42                TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           SET NOT-I-60                    TO TRUE
           SET NOT-I-61                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-24                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-26                    TO TRUE
           SET NOT-I-41                    TO TRUE
           IF  RA = 'B'
               SET I-41                    TO TRUE
           END-IF
           IF  (I-41)
               SET I-42                    TO TRUE
               MOVE OPPDAT                 TO OPPD1
               MOVE REFPER                 TO REFP1
               MOVE OPPLEV                 TO OPPL1
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-42)
               GO TO SLUTT-T
      **** SNU ENDRINGSDATO TIL FORMAT ≈≈/MM/DD.  **********
           END-IF
           MOVE 'A'                        TO DATOK
           MOVE DATO                       TO DATO-N
           MOVE DATO-N-IO (2:6)            TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO TEST2-IO
      *
      **** SNU LEVRANDÿRDATO TIL FORMAT ≈≈/MM/DD.  **********
           MOVE 'A'                        TO DATOK
           MOVE LEVDAT                     TO LEVDAT-N
           MOVE LEVDAT-N-IO (2:6)          TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO TEST4-IO
      ******************************************************
           SET NOT-I-94                    TO TRUE
           IF  OPPL1 = 'J'
               SET I-94                    TO TRUE
           END-IF
           IF  (NOT-I-94)
               MOVE 0                      TO NYLEVP
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  OPPD1 = '5'
               SET I-45                    TO TRUE
           END-IF
           IF  (NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  OPPD1 = '1'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-45)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           IF  TEST2 > TEST08
               SET I-21                    TO TRUE
           END-IF
           IF  TEST2 < TEST08
               SET I-22                    TO TRUE
           END-IF
           IF  TEST2 = TEST08
               SET I-23                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO LEVRUT-T
           END-IF
           IF  (I-22)
               OR  (I-23)
               SET NOT-I-46                TO TRUE
               IF  OPPD1 = '3'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (I-46)
               GO TO LEVRUT-T
           END-IF
           SET I-60                        TO TRUE
           SET I-61                        TO TRUE
      *
           .
 
       LEVRUT-T.
           SET NOT-I-24                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-26                    TO TRUE
           IF  TEST4 > TEST08
               SET I-24                    TO TRUE
           END-IF
           IF  TEST4 < TEST08
               SET I-25                    TO TRUE
           END-IF
           IF  TEST4 = TEST08
               SET I-26                    TO TRUE
           END-IF
           IF  (I-24 AND I-60)
               GO TO END-X-T
           END-IF
           IF  (I-24 AND NOT-I-60)
               GO TO SLUTT-T
           END-IF
           IF  (I-25)
               OR  (I-26)
               SET NOT-I-47                TO TRUE
               IF  OPPD1 = '4'
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (I-47 AND NOT-I-60)
               GO TO SLUTT-T
           END-IF
           IF  (I-47 AND I-60)
               GO TO END-X-T
           END-IF
           IF  (I-25)
               OR  (I-26)
               SET I-60                    TO TRUE
               SET I-62                    TO TRUE
           END-IF.
 
       END-X-T.
      ******************************************************
           SET NOT-I-12                    TO TRUE
           IF  REFNR NOT = FREFNR
               SET I-12                    TO TRUE
           END-IF
           IF  (I-12)
               MOVE 'B'                    TO KEY4 (1:1)
               MOVE FFNR                   TO KEY4 (2:3)
               MOVE KEY4                   TO KEY9 (1:4)
               MOVE FREFNR                 TO KEY9 (5:5)
               MOVE KEY9                   TO KEY13 (1:9)
               MOVE '0000'                 TO KEY13 (10:4)
               MOVE KEY13                  TO PRISMAS-KEY1
               READ PRISMAS RECORD KEY IS PRISMAS-KEY1
               INVALID KEY
                   SET I-14                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-14            TO TRUE
                   PERFORM PRISMAS-IDSET
               END-READ
           END-IF
           IF  (I-12 AND NOT-I-14)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-12)
               MOVE REFNR                  TO FREFNR
               SET NOT-I-63                TO TRUE
               SET NOT-I-64                TO TRUE
           END-IF
           SET NOT-I-90                    TO TRUE
           IF  FIRMA NOT = FFNR
               SET I-90                    TO TRUE
           END-IF
           IF  (I-90 AND I-60)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-90)
               MOVE FIRMA                  TO FFNR
           END-IF
           MOVE FIRMA                      TO KEY-X (1:3)
           MOVE EDBNR                      TO KEY-X (4:7)
           SET NOT-I-13                    TO TRUE
           MOVE KEY-X                      TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (I-11)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  NYSVS = 0
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  UTSALG = 0
               SET I-16                    TO TRUE
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  NYLEVP = 0
               SET I-17                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  NYSVS > VPRIS
               SET I-13                    TO TRUE
           END-IF
           IF  (I-61)
               ADD NYSVS TO ZERO       GIVING NSVS
               ADD UTSALG TO ZERO      GIVING NUTP
           END-IF
           IF  (I-62)
               ADD NYLEVP TO ZERO      GIVING NLEVP
           END-IF
           ADD DATO TO ZERO            GIVING EDATO
           ADD LEVDAT TO ZERO          GIVING LDATO
           ADD UDATE TO ZERO           GIVING DATOO.
 
       SLUTT-T.
           IF  (I-22)
               OR  (I-23)
               SET I-63                    TO TRUE
           END-IF
           IF  (I-25)
               OR  (I-26)
               SET I-64                    TO TRUE
           END-IF.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'NYP04'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'NYP004  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-U2)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 2
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (I-U3)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 3
                   SET I-86                TO TRUE
               END-IF
           END-IF.
      ******************************************************
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           MOVE 'B'                        TO KEY4 (1:1)
           MOVE FFNR                       TO KEY4 (2:3)
           MOVE KEY4                       TO KEY9 (1:4)
           MOVE FREFNR                     TO KEY9 (5:5)
           MOVE KEY9                       TO KEY13 (1:9)
           MOVE '0000'                     TO KEY13 (10:4)
           MOVE KEY13                      TO PRISMAS-KEY1
           READ PRISMAS RECORD KEY IS PRISMAS-KEY1
           INVALID KEY
               SET I-14                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-14                TO TRUE
               PERFORM PRISMAS-IDSET
           END-READ
           IF  (NOT-I-14)
               PERFORM EXCEPTION-OUTPUT
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (1:80)  TO REC (1:80)
               MOVE INNPUT-IO-AREA (1:1)   TO RA (1:1)
               MOVE INNPUT-IO-AREA (1:13)  TO PKEY (1:13)
               MOVE INNPUT-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE INNPUT-IO-AREA (5:5)   TO REFNR (1:5)
               MOVE INNPUT-IO-AREA (10:4)  TO SEQ (1:4)
               MOVE INNPUT-IO-AREA (14:7)  TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (26:5)  TO NYSVS-IO
               MOVE INNPUT-IO-AREA (36:5)  TO UTSALG-IO
               MOVE INNPUT-IO-AREA (46:5)  TO NYLEVP-IO
               MOVE INNPUT-IO-AREA (51:4)  TO DATO-IO
               MOVE INNPUT-IO-AREA (55:4)  TO LEVDAT-IO
               MOVE INNPUT-IO-AREA (35:1)  TO OPPLEV (1:1)
               MOVE INNPUT-IO-AREA (78:2)  TO REFPER (1:2)
               MOVE INNPUT-IO-AREA (80:1)  TO OPPDAT (1:1)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNPUT-CHK-LEVEL SECTION.
       INNPUT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNPUT-LEVEL-01
               MOVE INNPUT-IO-AREA (2:3)   TO INNPUT-01-L3-FIRMA
               MOVE INNPUT-IO-AREA (5:5)   TO INNPUT-01-L1-REFNR
               IF  INNPUT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNPUT-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNPUT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNPUT-01-L3          TO THE-PRIOR-L3
               MOVE  INNPUT-01-L1          TO THE-PRIOR-L1
               SET INNPUT-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (66:9) TO VSELVK-IO
               INSPECT VSELVK-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (75:9) TO VPRIS-IO
               INSPECT VPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (84:4) TO VDATO-IO
               INSPECT VDATO-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (95:1) TO PT (1:1)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (118:1) TO AVD (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       PRISMAS-IDSET SECTION.
       PRISMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       PRINTF-PRINT-LINE SECTION.
       PRINTF-PRINT-LINE-P.
           IF  PRINTF-BEFORE-SKIP > 0
               PERFORM PRINTF-SKIP-BEFORE
           END-IF
           IF  PRINTF-BEFORE-SPACE > 0
               PERFORM PRINTF-SPACE-BEFORE
               IF  PRINTF-AFTER-SKIP > 0
                   PERFORM PRINTF-SKIP-AFTER
               END-IF
               IF  PRINTF-AFTER-SPACE > 0
                   PERFORM PRINTF-SPACE-AFTER
               END-IF
           ELSE
               IF  PRINTF-AFTER-SKIP > 0
                   PERFORM PRINTF-SKIP-AFTER
               END-IF
               PERFORM PRINTF-SPACE-AFTER
           END-IF
           IF  PRINTF-LINE-COUNT NOT < PRINTF-MAX-LINES
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
           END-IF.
 
       PRINTF-SKIP-BEFORE SECTION.
       PRINTF-SKIP-BEFORE-P.
           WRITE PRINTF-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO PRINTF-LINE-COUNT
           MOVE 0                          TO PRINTF-BEFORE-SKIP
           INITIALIZE PRINTF-IO-AREA.
 
       PRINTF-SPACE-BEFORE SECTION.
       PRINTF-SPACE-BEFORE-P.
           WRITE PRINTF-IO-PRINT        AFTER PRINTF-BEFORE-SPACE LINES
           ADD PRINTF-BEFORE-SPACE         TO PRINTF-LINE-COUNT
           MOVE SPACES TO PRINTF-IO-AREA
           INITIALIZE PRINTF-IO-AREA
           MOVE 0                          TO PRINTF-BEFORE-SPACE.
 
       PRINTF-SKIP-AFTER SECTION.
       PRINTF-SKIP-AFTER-P.
           WRITE PRINTF-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO PRINTF-LINE-COUNT
           MOVE 0                          TO PRINTF-AFTER-SKIP
           INITIALIZE PRINTF-IO-AREA.
 
       PRINTF-SPACE-AFTER SECTION.
       PRINTF-SPACE-AFTER-P.
           WRITE PRINTF-IO-PRINT       BEFORE PRINTF-AFTER-SPACE LINES
           ADD PRINTF-AFTER-SPACE          TO PRINTF-LINE-COUNT
           INITIALIZE PRINTF-IO-AREA
           MOVE 0                          TO PRINTF-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-U7 AND I-60)
           AND (NOT-I-11)
               IF  (I-61 AND NOT-I-15)
                   MOVE NSVS-IO            TO VAREMAS-IO-AREA (66:9)
               END-IF
               IF  (I-61 AND NOT-I-16)
                   MOVE NUTP-IO            TO VAREMAS-IO-AREA (75:9)
               END-IF
               IF  (I-62 AND NOT-I-17)
                   MOVE NLEVP              TO XO-72P
                   MOVE XO-72P-EF          TO VAREMAS-IO-AREA (165:5)
               END-IF
               IF  (I-61)
                   MOVE DATOO              TO XO-70P
                   MOVE XO-70P-EF          TO VAREMAS-IO-AREA (84:4)
               END-IF
               REWRITE VAREMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAREMAS'
               END-REWRITE
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-86)
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               IF  (I-12)
                   MOVE REFNR              TO PRINTF-IO-AREA (1:5)
                   INITIALIZE REFNR
               END-IF
               MOVE VGR                    TO PRINTF-IO-AREA (10:5)
               INITIALIZE VGR
               MOVE ALFA                   TO PRINTF-IO-AREA (19:3)
               INITIALIZE ALFA
               MOVE ARTNR                  TO PRINTF-IO-AREA (24:20)
               INITIALIZE ARTNR
               IF  (I-61 AND NOT-I-15)
                   MOVE NSVS               TO XO-72YYZ
                   MOVE XO-72YYZ           TO PRINTF-IO-AREA (46:12)
                   INITIALIZE NSVS
               END-IF
               IF  (I-61 AND NOT-I-16)
                   MOVE NUTP               TO XO-72YYZ
                   MOVE XO-72YYZ           TO PRINTF-IO-AREA (60:12)
                   INITIALIZE NUTP
               END-IF
               IF  (I-62 AND NOT-I-17)
                   MOVE NLEVP              TO XO-72YYZ
                   MOVE XO-72YYZ           TO PRINTF-IO-AREA (74:12)
                   INITIALIZE NLEVP
               END-IF
               MOVE PT                     TO PRINTF-IO-AREA (87:1)
               INITIALIZE PT
               IF  (I-U6)
                   MOVE 'ER OPPDAT.'       TO PRINTF-IO-AREA (90:10)
               END-IF
               IF  (NOT-I-U6)
                   MOVE 'ER TESTET.'       TO PRINTF-IO-AREA (90:10)
               END-IF
               IF  (I-13)
                   MOVE 'OBS SVS'          TO PRINTF-IO-AREA (126:7)
               END-IF
               IF  (I-11)
                   MOVE 'IKKE MAKKER I VAREARKIV' TO PRINTF-IO-AREA
                                                              (110:23)
               END-IF
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-U6 AND NOT-I-14)
               IF  (I-63)
                   MOVE '3'                TO PRISMAS-IO-AREA (80:1)
               END-IF
               IF  (I-64)
                   MOVE '4'                TO PRISMAS-IO-AREA (80:1)
               END-IF
               IF  (I-63 AND I-64)
                   MOVE '5'                TO PRISMAS-IO-AREA (80:1)
               END-IF
               REWRITE PRISMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = PRISMAS'
               END-REWRITE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-12 AND NOT-I-86)
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE FINAVN                 TO PRINTF-IO-AREA (1:30)
               MOVE 'AVD.NR='              TO PRINTF-IO-AREA (34:7)
               MOVE AVD                    TO PRINTF-IO-AREA (41:1)
               MOVE '** ART. SOM ER ENDRE' TO PRINTF-IO-AREA (45:20)
               MOVE 'T M/NYE PRISER PR.'   TO PRINTF-IO-AREA (65:18)
               MOVE 'DATO ='               TO PRINTF-IO-AREA (90:6)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINTF-IO-AREA (97:8)
               MOVE 'REFERANSE-PERSON ='   TO PRINTF-IO-AREA (109:18)
               MOVE REFP1                  TO PRINTF-IO-AREA (128:2)
               MOVE 01                     TO PRINTF-BEFORE-SKIP
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'NY'                   TO PRINTF-IO-AREA (46:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (62:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (74:2)
               MOVE 'P'                    TO PRINTF-IO-AREA (87:1)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'REFNR    VGR      ALFA' TO PRINTF-IO-AREA (1:22)
               MOVE 'ARIKKELNR '           TO PRINTF-IO-AREA (24:10)
               MOVE 'SELVKOSTPRIS'         TO PRINTF-IO-AREA (46:12)
               MOVE 'UTSALGPRIS'           TO PRINTF-IO-AREA (62:10)
               MOVE 'LEV.DÿR PRIS'         TO PRINTF-IO-AREA (74:12)
               MOVE 'T'                    TO PRINTF-IO-AREA (87:1)
               MOVE 'DATO ENDRES'          TO PRINTF-IO-AREA (89:11)
               MOVE 'MERKNADER'            TO PRINTF-IO-AREA (105:9)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE '------------------------' TO PRINTF-IO-AREA (1:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (97:24)
               MOVE '--------------'       TO PRINTF-IO-AREA (119:14)
               MOVE 2                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV AND NOT-I-12 AND NOT-I-86)
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE FINAVN                 TO PRINTF-IO-AREA (1:30)
               MOVE 'AVD.NR='              TO PRINTF-IO-AREA (34:7)
               MOVE AVD                    TO PRINTF-IO-AREA (41:1)
               MOVE '** ART. SOM ER ENDRE' TO PRINTF-IO-AREA (45:20)
               MOVE 'T M/NYE PRISER PR.'   TO PRINTF-IO-AREA (65:18)
               MOVE 'DATO ='               TO PRINTF-IO-AREA (90:6)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINTF-IO-AREA (97:8)
               MOVE 'REFERANSE-PERSON ='   TO PRINTF-IO-AREA (109:18)
               MOVE REFP1                  TO PRINTF-IO-AREA (128:2)
               MOVE 01                     TO PRINTF-BEFORE-SKIP
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'NY'                   TO PRINTF-IO-AREA (46:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (62:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (74:2)
               MOVE 'P'                    TO PRINTF-IO-AREA (87:1)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'REFNR    VGR      ALFA' TO PRINTF-IO-AREA (1:22)
               MOVE 'ARIKKELNR '           TO PRINTF-IO-AREA (24:10)
               MOVE 'SELVKOSTPRIS'         TO PRINTF-IO-AREA (46:12)
               MOVE 'UTSALGPRIS'           TO PRINTF-IO-AREA (62:10)
               MOVE 'LEV.DÿR PRIS'         TO PRINTF-IO-AREA (74:12)
               MOVE 'T'                    TO PRINTF-IO-AREA (87:1)
               MOVE 'DATO ENDRES'          TO PRINTF-IO-AREA (89:11)
               MOVE 'MERKNADER'            TO PRINTF-IO-AREA (105:9)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE '------------------------' TO PRINTF-IO-AREA (1:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (97:24)
               MOVE '--------------'       TO PRINTF-IO-AREA (119:14)
               MOVE 2                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
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
           SET INNPUT-LEVEL-INIT           TO TRUE
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           OPEN INPUT INNPUT
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN I-O VAREMAS
           OPEN I-O PRISMAS
           OPEN OUTPUT PRINTF
           INITIALIZE PRINTF-IO-AREA
           INITIALIZE PRINTF-DATA-FIELDS
           MOVE 57                         TO PRINTF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNPUT
           CLOSE VAREMAS
           CLOSE PRISMAS
           IF PRINTF-IO-AREA NOT = SPACES
             WRITE PRINTF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINTF-IO-AREA
           END-IF
           CLOSE PRINTF.
 
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
