       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD507R.
      **********************************************  Z-WIN-RPG2   ****
      ***** Program har excel i RWEB *********** XX2000XXOKXXEL *******
      *  AVIKSLISTE FRA REGISTRERING AV ORDRE, SALG OG KREDIT    *
      ************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD507.rpg
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
           SELECT INF
               ASSIGN TO UT-S-INF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF-STATUS.
           SELECT GMLFILE
               ASSIGN TO UT-S-GMLFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLFILE-STATUS.
           SELECT FIRMAK
               ASSIGN TO FIRMAK
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAK-STATUS
               RECORD KEY IS FIRMAK-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
           SELECT NYFILE
               ASSIGN TO UT-S-NYFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  INF-IO-AREA.
           05  INF-IO-AREA-X               PICTURE X(200).
       FD GMLFILE
               BLOCK CONTAINS 188
               RECORD CONTAINS 94.
       01  GMLFILE-IO-AREA.
           05  GMLFILE-IO-AREA-X           PICTURE X(94).
       FD FIRMAK
               RECORD CONTAINS 1000.
       01  FIRMAK-IO-AREA.
           05  FIRMAK-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAK-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD OUTF
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(30).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD NYFILE
               BLOCK CONTAINS 188
               RECORD CONTAINS 94.
       01  NYFILE-IO-AREA.
           05  NYFILE-IO-AREA-X            PICTURE X(94).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INF-STATUS                  PICTURE 99 VALUE 0.
           10  GMLFILE-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAK-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
           10  NYFILE-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-EOF-OFF             VALUE '0'.
               88  INF-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-READ-OFF            VALUE '0'.
               88  INF-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-PROCESS-OFF         VALUE '0'.
               88  INF-PROCESS             VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INF-LEVEL-INIT-OFF      VALUE '0'.
               88  INF-LEVEL-INIT          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-EOF-OFF         VALUE '0'.
               88  GMLFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-READ-OFF        VALUE '0'.
               88  GMLFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-PROCESS-OFF     VALUE '0'.
               88  GMLFILE-PROCESS         VALUE '1'.
           05  FIRMAK-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  INF-LEVEL-01.
               10  INF-01-L4.
                   15  INF-01-L4-FNR       PICTURE X(3).
               10  INF-01-L3.
                   15  INF-01-L3-AVD       PICTURE X(1).
               10  INF-01-L2.
                   15  INF-01-L2-ORDMO2    PICTURE X(2).
               10  INF-01-L1.
                   15  INF-01-L1-ONR       PICTURE X(6).
           05  INF-LEVEL-02.
               10  INF-02-L4.
                   15  INF-02-L4-FNR       PICTURE X(3).
               10  INF-02-L3.
                   15  INF-02-L3-AVD       PICTURE X(1).
               10  INF-02-L2.
                   15  INF-02-L2-ORDMOX    PICTURE X(2).
           05  INF-DATA-FIELDS.
               10  REC01                   PICTURE X(65).
               10  FNR                     PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  OM                      PICTURE X(2).
               10  VGR                     PICTURE X(5).
               10  ONR                     PICTURE X(6).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  BETEGN                  PICTURE X(20).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(5)V9(2).
               10  ORDREP-IO.
                   15  ORDREP              PICTURE S9(7)V9(2).
               10  REGP-IO.
                   15  REGP                PICTURE S9(7)V9(2).
               10  OR1-IO.
                   15  OR1                 PICTURE S9(2)V9(1).
               10  OR2-IO.
                   15  OR2                 PICTURE S9(2)V9(1).
               10  OR3-IO.
                   15  OR3                 PICTURE S9(2)V9(1).
               10  RR1-IO.
                   15  RR1                 PICTURE S9(2)V9(1).
               10  RR2-IO.
                   15  RR2                 PICTURE S9(2)V9(1).
               10  RR3-IO.
                   15  RR3                 PICTURE S9(2)V9(1).
               10  SKAFF                   PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  NETFAK-IO.
                   15  NETFAK              PICTURE S9(1)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  SELPRI-IO.
                   15  SELPRI              PICTURE S9(7)V9(2).
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  ORDMO2                  PICTURE X(2).
               10  RETLEV                  PICTURE X(1).
               10  ANAVN                   PICTURE X(20).
               10  ANTO-IO.
                   15  ANTO                PICTURE S9(5).
               10  ANTVL-IO.
                   15  ANTVL               PICTURE S9(5).
               10  ANTVR-IO.
                   15  ANTVR               PICTURE S9(5).
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(7)V9(2).
               10  TORSUM-IO.
                   15  TORSUM              PICTURE S9(7)V9(2).
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(6).
               10  ANTK-IO.
                   15  ANTK                PICTURE S9(5).
               10  ANTVLK-IO.
                   15  ANTVLK              PICTURE S9(5).
               10  TOKSUM-IO.
                   15  TOKSUM              PICTURE S9(7)V9(2).
               10  REGO-IO.
                   15  REGO                PICTURE S9(7)V9(2).
               10  REGK-IO.
                   15  REGK                PICTURE S9(7)V9(2).
               10  OPPKR-IO.
                   15  OPPKR               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KONT-IO.
                   15  KONT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORDMOX                  PICTURE X(2).
           05  GMLFILE-DATA-FIELDS.
               10  REC04                   PICTURE X(94).
           05  FIRMAK-DATA-FIELDS.
               10  PRSYST                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  PA-ELGSLAG-IO.
                   15  PA-ELGSLAG          PICTURE S9(1)V9(2).
               10  SUM11-IO.
                   15  SUM11               PICTURE S9(9)V9(2).
               10  SUM9-IO.
                   15  SUM9                PICTURE S9(7)V9(2).
               10  NTOPRI-IO.
                   15  NTOPRI              PICTURE S9(7)V9(2).
               10  FORTJ-IO.
                   15  FORTJ               PICTURE S9(7)V9(2).
               10  FORTJ2-IO.
                   15  FORTJ2              PICTURE S9(9)V9(2).
               10  BRFPRO-IO.
                   15  BRFPRO              PICTURE S9(3)V9(1).
               10  GJOSUM-IO.
                   15  GJOSUM              PICTURE S9(6)V9(2).
               10  GJVL-IO.
                   15  GJVL                PICTURE S9(2).
               10  SUMPR1-IO.
                   15  SUMPR1              PICTURE S9(5).
               10  ANTRPR-IO.
                   15  ANTRPR              PICTURE S9(3)V9(2).
               10  TORSPR-IO.
                   15  TORSPR              PICTURE S9(9)V9(2).
               10  BELRPR-IO.
                   15  BELRPR              PICTURE S9(3)V9(2).
               10  AVIKO-IO.
                   15  AVIKO               PICTURE S9(7)V9(2).
               10  AVIKK-IO.
                   15  AVIKK               PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-21YNZ                PICTURE ZZ,Z.
               10  XO-12YNZ                PICTURE Z,ZZ.
               10  XO-31YNZR               PICTURE ZZZ,Z-.
               10  XO-14YNZ                PICTURE Z,ZZZZ.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-32YY9                PICTURE ZZZ,99.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-62YY9                PICTURE ZZZ.ZZZ,99.
               10  XO-20YY9                PICTURE Z9.
               10  XO-72YYZ                PICTURE Z.ZZZ.ZZZ,ZZ.
               10  XO-72YYZR               PICTURE Z.ZZZ.ZZZ,ZZ-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INF-PROCESS
               SET INF-PROCESS-OFF         TO TRUE
               SET INF-READ                TO TRUE
           END-IF
 
           IF  INF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INF-GET
               SET INF-READ-OFF            TO TRUE
               IF  NOT INF-EOF
                   PERFORM INF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET INF-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  GMLFILE-PROCESS
               SET GMLFILE-PROCESS-OFF     TO TRUE
               SET GMLFILE-READ            TO TRUE
           END-IF
 
           IF  GMLFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM GMLFILE-GET
               SET GMLFILE-READ-OFF        TO TRUE
               IF  NOT GMLFILE-EOF
                   SET GMLFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-IDSET
           END-IF
 
           IF  GMLFILE-PROCESS
               PERFORM GMLFILE-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-CHK-LEVEL
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
 
           IF  INF-PROCESS
               PERFORM INF-FLDOFF
               PERFORM INF-FLDSET
           END-IF
 
           IF  GMLFILE-PROCESS
               PERFORM GMLFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               MOVE FNR                    TO FIRMAK-KEY1
               READ FIRMAK RECORD KEY IS FIRMAK-KEY1
               INVALID KEY
                   SET I-85                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-85            TO TRUE
                   PERFORM FIRMAK-FLDSET
                   PERFORM FIRMAK-IDSET
               END-READ
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4 AND NOT-I-85)
               SET NOT-I-20                TO TRUE
               IF  PRSYST = 'N'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-98                TO TRUE
               IF  SKAFF = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  SKAFF = 'R'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-97                TO TRUE
               IF  SKAFF = 'N'
                   SET I-97                TO TRUE
               END-IF
               SET NOT-I-91                TO TRUE
               IF  RETLEV = 'R'
                   SET I-91                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-01 AND NOT-I-95)
               DIVIDE ORDREP BY REGP   GIVING PA-ELGSLAG ROUNDED
      ******************************************************
      * BEREGNING AV BRUTTOFORTJ. PROSENT.  (BRF.PROS.)    *
      ******************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-81                TO TRUE
               IF  ORDREP = 0,00
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-81)
               GO TO BRFEND-T
           END-IF
           IF  (I-01)
               SET NOT-I-82                TO TRUE
               IF  SELPRI = 0,00
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-82)
               GO TO BRFEND-T
      *
           END-IF
           IF  (I-01)
               MULTIPLY OR1 BY ORDREP  GIVING SUM11
               DIVIDE SUM11 BY 100     GIVING SUM9
               SUBTRACT SUM9 FROM ORDREP GIVING NTOPRI
               MULTIPLY OR2 BY NTOPRI  GIVING SUM11
               DIVIDE SUM11 BY 100     GIVING SUM9
               SUBTRACT SUM9               FROM NTOPRI
               MULTIPLY OR3 BY NTOPRI  GIVING SUM11
               DIVIDE SUM11 BY 100     GIVING SUM9
               SUBTRACT SUM9               FROM NTOPRI
      *
           END-IF
           IF  (I-01)
               SUBTRACT SELPRI FROM NTOPRI GIVING FORTJ
               MULTIPLY 100 BY FORTJ   GIVING FORTJ2
               DIVIDE FORTJ2 BY NTOPRI GIVING BRFPRO ROUNDED
           END-IF.
 
       BRFEND-T.
      *****************************************************************
      * RUTINE FOR OG UNDETRYKKE UTSKRIFT AV AVIK.                    *
      *****************************************************************
           IF  (I-01)
               SET NOT-I-50                TO TRUE
      *
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  FNR = '923'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51)
               SET NOT-I-52                TO TRUE
               IF  VGR = '42000'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51 AND NOT-I-52)
               SET NOT-I-52                TO TRUE
               IF  VGR = '99999'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51 AND I-52)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND I-50)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  FNR = '950'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51)
               SET NOT-I-52                TO TRUE
               IF  VGR = '99999'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51 AND I-52)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND I-50)
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR AVDELING SUMMERING.                               *
      *****************************************************************
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET NOT-I-11                TO TRUE
               IF  TORSUM > 0
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-16                TO TRUE
               IF  ANTO > 0
                   SET I-16                TO TRUE
               END-IF
               SET NOT-I-17                TO TRUE
               IF  ANTVL > 0
                   SET I-17                TO TRUE
               END-IF
               SET NOT-I-18                TO TRUE
               IF  TOTSUM > 0
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-16)
               DIVIDE TOTSUM BY ANTO   GIVING GJOSUM ROUNDED
               DIVIDE ANTVL BY ANTO    GIVING GJVL ROUNDED
           END-IF
           IF  (I-02)
               MULTIPLY 100 BY ANTVR   GIVING SUMPR1
           END-IF
           IF  (I-02 AND I-17)
               DIVIDE SUMPR1 BY ANTVL  GIVING ANTRPR ROUNDED
           END-IF
           IF  (I-02)
               MULTIPLY 100 BY TORSUM  GIVING TORSPR
           END-IF
           IF  (I-02 AND I-18)
               DIVIDE TORSPR BY TOTSUM GIVING BELRPR ROUNDED
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'ORD05'                    TO LONR
           MOVE FNR                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD507  '                 TO LPROG
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
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-86)
               GO TO L1END-T
           END-IF
           IF  (I-L3)
               SUBTRACT REGO FROM TOTSUM GIVING AVIKO
               SUBTRACT REGK FROM TOKSUM GIVING AVIKK
           END-IF.
 
       L1END-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       INF-GET SECTION.
       INF-GET-P.
           IF  INF-EOF-OFF
               READ INF
               AT END
                   SET INF-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF-FLDOFF SECTION.
       INF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (111:1) = 'D' )
               SET NOT-I-95                TO TRUE
               SET NOT-I-29                TO TRUE
           END-EVALUATE.
 
       INF-FLDSET SECTION.
       INF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (111:1) = 'D' )
               MOVE INF-IO-AREA (1:65)     TO REC01 (1:65)
               MOVE INF-IO-AREA (1:3)      TO FNR (1:3)
               MOVE INF-IO-AREA (4:6)      TO RESKNR (1:6)
               MOVE INF-IO-AREA (10:2)     TO OM (1:2)
               MOVE INF-IO-AREA (12:5)     TO VGR (1:5)
               MOVE INF-IO-AREA (17:6)     TO ONR (1:6)
               MOVE INF-IO-AREA (23:3)     TO ALFA (1:3)
               MOVE INF-IO-AREA (26:20)    TO ARTNR (1:20)
               MOVE INF-IO-AREA (46:20)    TO BETEGN (1:20)
               MOVE INF-IO-AREA (66:7)     TO ANTALL-IO
               INSPECT ANTALL-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (73:9)     TO ORDREP-IO
               INSPECT ORDREP-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (82:9)     TO REGP-IO
               INSPECT REGP-IO REPLACING ALL ' ' BY '0'
               IF  REGP = ZERO
                   SET I-95                TO TRUE
               END-IF
               MOVE INF-IO-AREA (91:3)     TO OR1-IO
               INSPECT OR1-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (94:3)     TO OR2-IO
               INSPECT OR2-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (97:3)     TO OR3-IO
               INSPECT OR3-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (100:3)    TO RR1-IO
               INSPECT RR1-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (103:3)    TO RR2-IO
               INSPECT RR2-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (106:3)    TO RR3-IO
               INSPECT RR3-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (109:1)    TO SKAFF (1:1)
               MOVE INF-IO-AREA (110:1)    TO AVD (1:1)
               MOVE INF-IO-AREA (112:3)    TO NETFAK-IO
               IF  NETFAK = ZERO
                   SET I-29                TO TRUE
               END-IF
               MOVE INF-IO-AREA (115:9)    TO SELPRI-IO
               INSPECT SELPRI-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (124:30)   TO KNAVN1 (1:30)
               MOVE INF-IO-AREA (154:30)   TO KNAVN2 (1:30)
               MOVE INF-IO-AREA (184:2)    TO ORDMO2 (1:2)
               MOVE INF-IO-AREA (186:1)    TO RETLEV (1:1)
           WHEN ( INF-IO-AREA (111:1) = 'T' )
               MOVE INF-IO-AREA (1:3)      TO FNR (1:3)
               MOVE INF-IO-AREA (4:20)     TO ANAVN (1:20)
               MOVE INF-IO-AREA (110:1)    TO AVD (1:1)
               MOVE INF-IO-AREA (25:5)     TO ANTO-IO
               INSPECT ANTO-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (30:5)     TO ANTVL-IO
               INSPECT ANTVL-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (35:5)     TO ANTVR-IO
               INSPECT ANTVR-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (40:9)     TO TOTSUM-IO
               INSPECT TOTSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (49:9)     TO TORSUM-IO
               INSPECT TORSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (58:6)     TO ODATO-IO
               INSPECT ODATO-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (64:5)     TO ANTK-IO
               INSPECT ANTK-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (69:5)     TO ANTVLK-IO
               INSPECT ANTVLK-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (74:9)     TO TOKSUM-IO
               INSPECT TOKSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (83:9)     TO REGO-IO
               INSPECT REGO-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (92:9)     TO REGK-IO
               INSPECT REGK-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (121:5)    TO OPPKR-IO
               MOVE INF-IO-AREA (126:5)    TO KONT-IO
               MOVE INF-IO-AREA (184:2)    TO ORDMOX (1:2)
           END-EVALUATE.
 
       INF-IDCHK SECTION.
       INF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (111:1) = 'D' )
             OR ( INF-IO-AREA (111:1) = 'T' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INF-IDSET SECTION.
       INF-IDSET-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (111:1) = 'D' )
               SET I-01                    TO TRUE
           WHEN ( INF-IO-AREA (111:1) = 'T' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       INF-CHK-LEVEL SECTION.
       INF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (111:1) = 'D' )
               MOVE LOW-VALUES             TO INF-LEVEL-01
               MOVE INF-IO-AREA (1:3)      TO INF-01-L4-FNR
               MOVE INF-IO-AREA (110:1)    TO INF-01-L3-AVD
               MOVE INF-IO-AREA (184:2)    TO INF-01-L2-ORDMO2
               MOVE INF-IO-AREA (17:6)     TO INF-01-L1-ONR
               IF  INF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INF-01-L4             TO THE-PRIOR-L4
               MOVE  INF-01-L3             TO THE-PRIOR-L3
               MOVE  INF-01-L2             TO THE-PRIOR-L2
               MOVE  INF-01-L1             TO THE-PRIOR-L1
               SET INF-LEVEL-INIT          TO TRUE
           WHEN ( INF-IO-AREA (111:1) = 'T' )
               MOVE LOW-VALUES             TO INF-LEVEL-02
               MOVE INF-IO-AREA (1:3)      TO INF-02-L4-FNR
               MOVE INF-IO-AREA (110:1)    TO INF-02-L3-AVD
               MOVE INF-IO-AREA (184:2)    TO INF-02-L2-ORDMOX
               IF  INF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INF-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INF-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  INF-02-L4             TO THE-PRIOR-L4
               MOVE  INF-02-L3             TO THE-PRIOR-L3
               MOVE  INF-02-L2             TO THE-PRIOR-L2
               SET INF-LEVEL-INIT          TO TRUE
           END-EVALUATE.
 
       GMLFILE-GET SECTION.
       GMLFILE-GET-P.
           IF  GMLFILE-EOF-OFF
               READ GMLFILE
               AT END
                   SET GMLFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLFILE-FLDSET SECTION.
       GMLFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLFILE-IO-AREA (1:94) TO REC04 (1:94)
           END-EVALUATE.
 
       GMLFILE-IDSET SECTION.
       GMLFILE-IDSET-P.
           SET I-04                        TO TRUE.
 
       FIRMAK-FLDSET SECTION.
       FIRMAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAK-IO-AREA (783:1) TO PRSYST (1:1)
           END-EVALUATE.
 
       FIRMAK-IDSET SECTION.
       FIRMAK-IDSET-P.
           SET I-03                        TO TRUE.
 
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
           IF  (I-01 AND NOT-I-86 AND NOT-I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ONR                    TO LISTE-IO-AREA (1:6)
               MOVE RESKNR                 TO LISTE-IO-AREA (8:6)
               MOVE OM                     TO LISTE-IO-AREA (15:2)
               MOVE VGR                    TO LISTE-IO-AREA (18:5)
               MOVE ALFA                   TO LISTE-IO-AREA (24:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (28:20)
               MOVE BETEGN                 TO LISTE-IO-AREA (49:20)
               MOVE SKAFF                  TO LISTE-IO-AREA (70:1)
               MOVE ANTALL                 TO XO-52YN9
               MOVE XO-52YN9               TO LISTE-IO-AREA (72:8)
               MOVE SELPRI                 TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (81:10)
               MOVE ORDREP                 TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (93:10)
               MOVE OR1                    TO XO-21YNZ
               MOVE XO-21YNZ               TO LISTE-IO-AREA (104:4)
               MOVE OR2                    TO XO-21YNZ
               MOVE XO-21YNZ               TO LISTE-IO-AREA (109:4)
               MOVE OR3                    TO XO-21YNZ
               MOVE XO-21YNZ               TO LISTE-IO-AREA (114:4)
               IF  (NOT-I-95 AND NOT-I-29)
                   MOVE 'ORD-PÅSL'         TO LISTE-IO-AREA (118:8)
               END-IF
               IF  (NOT-I-95 AND NOT-I-29)
                   MOVE PA-ELGSLAG         TO XO-12YNZ
                   MOVE XO-12YNZ           TO LISTE-IO-AREA (129:4)
                   INITIALIZE PA-ELGSLAG
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KNAVN1                 TO LISTE-IO-AREA (15:30)
               MOVE KNAVN2                 TO LISTE-IO-AREA (46:30)
               MOVE BRFPRO                 TO XO-31YNZR
               MOVE XO-31YNZR              TO LISTE-IO-AREA (85:6)
               INITIALIZE BRFPRO
               MOVE REGP                   TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (93:10)
               IF  (I-29)
                   MOVE RR1                TO XO-21YNZ
                   MOVE XO-21YNZ           TO LISTE-IO-AREA (104:4)
               END-IF
               IF  (I-29)
                   MOVE RR2                TO XO-21YNZ
                   MOVE XO-21YNZ           TO LISTE-IO-AREA (109:4)
               END-IF
               IF  (I-29)
                   MOVE RR3                TO XO-21YNZ
                   MOVE XO-21YNZ           TO LISTE-IO-AREA (114:4)
               END-IF
               IF  (NOT-I-29)
                   MOVE 'REG-PÅSL'         TO LISTE-IO-AREA (118:8)
               END-IF
               IF  (NOT-I-29)
                   MOVE NETFAK             TO XO-14YNZ
                   MOVE XO-14YNZ           TO LISTE-IO-AREA (127:6)
               END-IF
               IF  (I-91)
                   MOVE 'RETUR LEV. *** '  TO LISTE-IO-AREA (118:15)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-97 AND NOT-I-20)
           AND (NOT-I-86 AND NOT-I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  OVENFORSTÅENDE ARTI' TO LISTE-IO-AREA (25:24)
               MOVE 'KKEL HAR NETTOPRIS  *** ' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE '71'                   TO OUTF-IO-AREA (1:2)
               MOVE AVD                    TO OUTF-IO-AREA (3:1)
               MOVE ODATO-IO               TO OUTF-IO-AREA (4:6)
               MOVE ANTVL                  TO XO-50P
               MOVE XO-50P-EF              TO OUTF-IO-AREA (10:3)
               MOVE ANTVR                  TO XO-50P
               MOVE XO-50P-EF              TO OUTF-IO-AREA (13:3)
               MOVE TOTSUM                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (16:5)
               MOVE TORSUM                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (21:5)
               MOVE FNR                    TO OUTF-IO-AREA (28:3)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               MOVE REC04                  TO NYFILE-IO-AREA (1:94)
               WRITE NYFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'ORDRESTATISTIKK OG'   TO LISTE-IO-AREA (34:18)
               MOVE 'AVIKSLISTE'           TO LISTE-IO-AREA (53:10)
               MOVE 'PR.'                  TO LISTE-IO-AREA (77:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE ANAVN                  TO LISTE-IO-AREA (84:20)
               MOVE ORDMO2                 TO LISTE-IO-AREA (105:2)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (109:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (113:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDNR  RESKNR OM VGRP' TO LISTE-IO-AREA (1:21)
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (24:18)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (49:14)
               MOVE 'T   ANTALL  SELVKPRIS' TO LISTE-IO-AREA (70:21)
               MOVE 'ORDR-PRIS  ORDR-RABATTER' TO LISTE-IO-AREA (93:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BRF.PROS.'            TO LISTE-IO-AREA (82:9)
               MOVE 'REG-PRIS   REG-RABATTER' TO LISTE-IO-AREA (93:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
           OR  (I-L1 AND NOT-I-86 AND I-01)
           AND (NOT-I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               EVALUATE TRUE
               WHEN (I-L2 AND NOT-I-86)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-OF AND NOT-I-L2 AND NOT-I-86)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-L1 AND NOT-I-86 AND I-01)
               AND  (NOT-I-L2)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'ORDRESTATISTIKK OG'   TO LISTE-IO-AREA (34:18)
               MOVE 'AVIKSLISTE'           TO LISTE-IO-AREA (53:10)
               MOVE 'PR.'                  TO LISTE-IO-AREA (77:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE ANAVN                  TO LISTE-IO-AREA (84:20)
               MOVE ORDMO2                 TO LISTE-IO-AREA (105:2)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (109:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (113:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDNR  RESKNR OM VGRP' TO LISTE-IO-AREA (1:21)
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (24:18)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (49:14)
               MOVE 'T   ANTALL  SELVKPRIS' TO LISTE-IO-AREA (70:21)
               MOVE 'ORDR-PRIS  ORDR-RABATTER' TO LISTE-IO-AREA (93:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BRF.PROS.'            TO LISTE-IO-AREA (82:9)
               MOVE 'REG-PRIS   REG-RABATTER' TO LISTE-IO-AREA (93:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-OF AND NOT-I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               EVALUATE TRUE
               WHEN (I-L2 AND NOT-I-86)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-OF AND NOT-I-L2 AND NOT-I-86)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-L1 AND NOT-I-86 AND I-01)
               AND  (NOT-I-L2)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'G R A N D T O T A L E R' TO LISTE-IO-AREA (10:23)
               MOVE 'F O R   O R D R E R'  TO LISTE-IO-AREA (36:19)
               MOVE 'R E G I S T R E R T'  TO LISTE-IO-AREA (58:19)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (80:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (10:24)
               MOVE '------------------------' TO LISTE-IO-AREA (34:24)
               MOVE '------------------------' TO LISTE-IO-AREA (58:24)
               MOVE '-----'                TO LISTE-IO-AREA (82:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL ORDRE'         TO LISTE-IO-AREA (10:12)
               MOVE ANTO                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (37:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL VARELINJER'    TO LISTE-IO-AREA (10:17)
               MOVE ANTVL                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (37:6)
               IF  (I-11)
                   MOVE 'ANTALL VARELINJER' TO LISTE-IO-AREA (46:17)
               END-IF
               IF  (I-11)
                   MOVE 'MED RESTORDRE'    TO LISTE-IO-AREA (64:13)
               END-IF
               IF  (I-11)
                   MOVE ANTVR              TO XO-50YY9
                   MOVE XO-50YY9           TO LISTE-IO-AREA (79:6)
               END-IF
               IF  (I-11)
                   MOVE ANTRPR             TO XO-32YY9
                   MOVE XO-32YY9           TO LISTE-IO-AREA (88:6)
                   INITIALIZE ANTRPR
               END-IF
               IF  (I-11)
                   MOVE 'PROSENT'          TO LISTE-IO-AREA (95:7)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ORDRESUM IN'   TO LISTE-IO-AREA (10:18)
               MOVE 'KL. RESTORDRE'        TO LISTE-IO-AREA (28:13)
               MOVE TOTSUM                 TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (47:12)
               IF  (I-11)
                   MOVE 'HERAV RESTORDRESUM' TO LISTE-IO-AREA (62:18)
               END-IF
               IF  (I-11)
                   MOVE TORSUM             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (86:12)
               END-IF
               IF  (I-11)
                   MOVE BELRPR             TO XO-32YY9
                   MOVE XO-32YY9           TO LISTE-IO-AREA (101:6)
                   INITIALIZE BELRPR
               END-IF
               IF  (I-11)
                   MOVE 'PROSENT'          TO LISTE-IO-AREA (108:7)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GJENNOMSNITTELIG'     TO LISTE-IO-AREA (10:16)
               MOVE 'ORDREBELØP PR. ORDRE' TO LISTE-IO-AREA (27:20)
               MOVE GJOSUM                 TO XO-62YY9
               MOVE XO-62YY9               TO LISTE-IO-AREA (49:10)
               INITIALIZE GJOSUM
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GJENNOMSNITTELIG'     TO LISTE-IO-AREA (10:16)
               MOVE 'VARELINJER PR. ORDRE' TO LISTE-IO-AREA (27:20)
               MOVE GJVL                   TO XO-20YY9
               MOVE XO-20YY9               TO LISTE-IO-AREA (57:2)
               INITIALIZE GJVL
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ORDREBELØP MED OR' TO LISTE-IO-AREA (10:24)
               MOVE 'DRE PRISER OG RABATTER' TO LISTE-IO-AREA (34:22)
               MOVE TOTSUM                 TO XO-72YYZ
               MOVE XO-72YYZ               TO LISTE-IO-AREA (71:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ORDREBELØP MED RE' TO LISTE-IO-AREA (10:24)
               MOVE 'GISTER PRISER OG RABATTE' TO LISTE-IO-AREA (34:24)
               MOVE 'R'                    TO LISTE-IO-AREA (58:1)
               MOVE REGO                   TO XO-72YYZ
               MOVE XO-72YYZ               TO LISTE-IO-AREA (71:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVIK I KRONER OG ØRE' TO LISTE-IO-AREA (10:20)
               MOVE AVIKO                  TO XO-72YYZR
               MOVE XO-72YYZR              TO LISTE-IO-AREA (71:13)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL KREDIT-ORDRE'  TO LISTE-IO-AREA (11:19)
               MOVE ANTK                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (45:6)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL VARELINJER'    TO LISTE-IO-AREA (10:17)
               MOVE 'I KREDIT'             TO LISTE-IO-AREA (28:8)
               MOVE ANTVLK                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (45:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT KREDITBELØP MED O' TO LISTE-IO-AREA (10:24)
               MOVE 'RDRE PRISER OG RABATTER' TO LISTE-IO-AREA (34:23)
               MOVE TOKSUM                 TO XO-72YYZ
               MOVE XO-72YYZ               TO LISTE-IO-AREA (71:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT KREDITBELØP MED R' TO LISTE-IO-AREA (10:24)
               MOVE 'EGISTER PRISER OG RABATT' TO LISTE-IO-AREA (34:24)
               MOVE 'ER'                   TO LISTE-IO-AREA (58:2)
               MOVE REGK                   TO XO-72YYZ
               MOVE XO-72YYZ               TO LISTE-IO-AREA (71:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVIK I KRONER OG ØRE' TO LISTE-IO-AREA (10:20)
               MOVE AVIKK                  TO XO-72YYZR
               MOVE XO-72YYZR              TO LISTE-IO-AREA (71:13)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DAGENS OPPKRAVSALG   ' TO LISTE-IO-AREA (10:21)
               MOVE OPPKR                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (31:13)
               INITIALIZE OPPKR
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DAGENS KONTANTSALG   ' TO LISTE-IO-AREA (10:21)
               MOVE KONT                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (31:13)
               INITIALIZE KONT
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
           MOVE 2                          TO LR-CHECK
           SET INF-LEVEL-INIT              TO TRUE
           INITIALIZE INF-DATA-FIELDS
           SET INF-EOF-OFF                 TO TRUE
           SET INF-PROCESS                 TO TRUE
           OPEN INPUT INF
           INITIALIZE GMLFILE-DATA-FIELDS
           SET GMLFILE-EOF-OFF             TO TRUE
           SET GMLFILE-PROCESS             TO TRUE
           OPEN INPUT GMLFILE
           INITIALIZE FIRMAK-DATA-FIELDS
           OPEN INPUT FIRMAK
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTF
           OPEN OUTPUT NYFILE.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INF
           CLOSE GMLFILE
           CLOSE FIRMAK
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTF
           CLOSE NYFILE.
 
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
