       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD600R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring excel på Report Web *****************
      * PROGRAM ORD600 ,UTPLUKK TIL DAGENS SALG MED BRF     *
      * MINDRE ENN XX PROSENT.                              *
      * LESER SEQ ORDREFILE OG DANNER ORDRESUM RECORDS.     *
      *                                                     *
      * PROGR.  STEIN SANDVOLD                              *
      * DATO    15.03.95                                    *
      * 02.04.96 IKKE LAGEROVERF. KUNDENR=59XXXX            *
      * 17.06.13 Lagt inn skaf.   EN                        *
      * 25.06.13 Lagt inn PRITTYP EN                        *
      *******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD600.rpg
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
           SELECT ORDSEQ
               ASSIGN TO UT-S-ORDSEQ
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSEQ-STATUS.
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
       FD ORDSEQ
               BLOCK CONTAINS 4100
               RECORD CONTAINS 164.
       01  ORDSEQ-IO-AREA.
           05  ORDSEQ-IO-AREA-X            PICTURE X(164).
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
           10  ORDSEQ-STATUS               PICTURE 99 VALUE 0.
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
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSEQ-EOF-OFF          VALUE '0'.
               88  ORDSEQ-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSEQ-READ-OFF         VALUE '0'.
               88  ORDSEQ-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSEQ-PROCESS-OFF      VALUE '0'.
               88  ORDSEQ-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDSEQ-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDSEQ-LEVEL-INIT       VALUE '1'.
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
               10  PPRO-IO.
                   15  PPRO                PICTURE S9(3)V9(2).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ORDSEQ-LEVEL-01.
               10  ORDSEQ-01-L2.
                   15  ORDSEQ-01-L2-FIRMA  PICTURE X(3).
               10  ORDSEQ-01-L1.
                   15  ORDSEQ-01-L1-ORDNR  PICTURE X(6).
           05  ORDSEQ-LEVEL-02.
               10  ORDSEQ-02-L2.
                   15  ORDSEQ-02-L2-FIRMA  PICTURE X(3).
               10  ORDSEQ-02-L1.
                   15  ORDSEQ-02-L1-ORDNR  PICTURE X(6).
           05  ORDSEQ-LEVEL-03.
               10  ORDSEQ-03-L2.
                   15  ORDSEQ-03-L2-FIRMA  PICTURE X(3).
               10  ORDSEQ-03-L1.
                   15  ORDSEQ-03-L1-ORDNR  PICTURE X(6).
           05  ORDSEQ-LEVEL-04.
               10  ORDSEQ-04-L2.
                   15  ORDSEQ-04-L2-FIRMA  PICTURE X(3).
               10  ORDSEQ-04-L1.
                   15  ORDSEQ-04-L1-ORDNR  PICTURE X(6).
           05  ORDSEQ-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  KUND2F                  PICTURE X(2).
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KTSIFF                  PICTURE X(1).
               10  DIRREG                  PICTURE X(1).
               10  FRITT                   PICTURE X(1).
               10  LAGER                   PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  SKAF                    PICTURE X(1).
               10  BETM                    PICTURE X(2).
               10  GEBYR                   PICTURE X(1).
               10  FRAKT                   PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
               10  REST                    PICTURE X(1).
               10  PRIKOD                  PICTURE X(1).
               10  STAM                    PICTURE X(1).
               10  KIS                     PICTURE X(1).
               10  KOMUTA                  PICTURE X(1).
               10  PAKKER                  PICTURE X(2).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDMA-ELGR              PICTURE X(4).
               10  ORDMND                  PICTURE X(2).
               10  ORDDAG                  PICTURE X(2).
               10  ORDAAR                  PICTURE X(2).
               10  ORDMOT                  PICTURE X(2).
               10  TERMID                  PICTURE X(4).
               10  SELGKP                  PICTURE X(1).
               10  FERDIM                  PICTURE X(1).
               10  FAKTNR                  PICTURE X(2).
               10  REGKL-IO.
                   15  REGKL               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RUTID                   PICTURE X(1).
               10  OPDATO-IO.
                   15  OPDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ANTPRT-IO.
                   15  ANTPRT              PICTURE S9(2).
               10  STATUS-X                PICTURE X(1).
               10  FAKREF                  PICTURE X(6).
               10  AVNAVN                  PICTURE X(11).
               10  OKKNR                   PICTURE X(6).
               10  REKVNR                  PICTURE X(15).
               10  FORSM                   PICTURE X(15).
               10  HND                     PICTURE X(3).
               10  KADR                    PICTURE X(30).
               10  POSTNR                  PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  VAADR1                  PICTURE X(30).
               10  VAADR2                  PICTURE X(30).
               10  VAADR3                  PICTURE X(30).
               10  VAADR4                  PICTURE X(20).
               10  LAGLOC                  PICTURE X(6).
               10  POSNR-IO.
                   15  POSNR               PICTURE S9(3).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTRES-IO.
                   15  ANTRES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NOREST                  PICTURE X(1).
               10  ALF                     PICTURE X(3).
               10  TEKST                   PICTURE X(50).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VGR-IO.
                   15  VGR                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ORPRIS-IO.
                   15  ORPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB1-IO.
                   15  ORRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB2-IO.
                   15  ORRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB3-IO.
                   15  ORRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGPRIS-IO.
                   15  RGPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB1-IO.
                   15  RGRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB2-IO.
                   15  RGRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB3-IO.
                   15  RGRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VEIPRI-IO.
                   15  VEIPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KOSPRI-IO.
                   15  KOSPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KODATO-IO.
                   15  KODATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KOSIGN                  PICTURE X(2).
               10  KOSTAT                  PICTURE X(1).
               10  PRITYP                  PICTURE X(1).
               10  NET1                    PICTURE X(1).
               10  NETFAK                  PICTURE X(3).
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(7).
               10  SIGN-X                  PICTURE X(2).
               10  BK2                     PICTURE X(1).
               10  HJSUM-IO.
                   15  HJSUM               PICTURE S9(9)V9(2).
               10  BRF-IO.
                   15  BRF                 PICTURE S9(3)V9(2).
               10  ANTVL-IO.
                   15  ANTVL               PICTURE S9(9).
               10  ANTVLR-IO.
                   15  ANTVLR              PICTURE S9(9).
               10  EDBNR3-IO.
                   15  EDBNR3              PICTURE S9(3).
               10  EDBNR2-IO.
                   15  EDBNR2              PICTURE S9(2).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(4).
               10  NETSTK-IO.
                   15  NETSTK              PICTURE S9(7)V9(2).
               10  AVANSE-IO.
                   15  AVANSE              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-32YN9R               PICTURE ZZZ,99-.
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
           SET NOT-I-85                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
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
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  ORDSEQ-PROCESS
               SET ORDSEQ-PROCESS-OFF      TO TRUE
               SET ORDSEQ-READ             TO TRUE
           END-IF
 
           IF  ORDSEQ-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDSEQ-GET
               SET ORDSEQ-READ-OFF         TO TRUE
               IF  NOT ORDSEQ-EOF
                   PERFORM ORDSEQ-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORDSEQ-PROCESS      TO TRUE
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
 
           IF  ORDSEQ-PROCESS
               PERFORM ORDSEQ-IDSET
           END-IF
 
           IF  ORDSEQ-PROCESS
               PERFORM ORDSEQ-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
           PERFORM DETAIL-OVERFLOW
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  ORDSEQ-PROCESS
               PERFORM ORDSEQ-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDSEQ-PROCESS
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
           IF  (I-L2)
               PERFORM RBSRUT-S
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               IF  SELGKP = '*'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-50)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  BK = 'I'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  BK = 'J'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-51)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-52                TO TRUE
               IF  KUND2F = '59'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-52)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-60                TO TRUE
               IF  FIRMA = '970'
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '100780'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101131'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101145'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101146'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101149'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101150'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101154'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101155'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101156'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101157'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101158'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101159'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101161'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101162'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101164'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101166'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101167'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101186'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101757'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '101905'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '105247'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '105248'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '105960'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '111640'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KUNDNR = '111920'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-60 AND I-62)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  DIRREG = 'J'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  RUTID = 'K'
                   SET I-23                TO TRUE
               END-IF
               MOVE 1                      TO ANTORD
               MOVE ORDMOT                 TO SIGN-X
               MOVE BK                     TO BK2
           END-IF
           IF  (NOT-I-04)
               GO TO SLUTT-T
           END-IF
           PERFORM INRUT4-S
           PERFORM OSURUT-S
           SET NOT-I-78                    TO TRUE
           IF  NETSTK = 0
               SET I-78                    TO TRUE
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  ANTLEV = 0
               SET I-88                    TO TRUE
           END-IF
           IF  (I-88)
               GO TO SLUTT-T
           END-IF
           MULTIPLY 100 BY AVANSE      GIVING HJSUM
           IF  (NOT-I-78)
               DIVIDE HJSUM BY NETSTK  GIVING BRF ROUNDED
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  BRF < PPRO
               SET I-80                    TO TRUE
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *  SUBRUTINE FOR SETTING AV FELLES INDIKATORER PR. VARELINJE.   *
      *****************************************************************
           CONTINUE.
 
       INRUT4-S SECTION.
       INRUT4-S-P.
           SET NOT-I-24                    TO TRUE
           IF  EDBNR = 0000000
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  PRITYP = 'N'
               SET I-25                    TO TRUE
           END-IF
           IF  (NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  PRITYP = 'T'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  PRITIL > 0,00
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  ANTLEV > 0,00
               SET I-82                    TO TRUE
           END-IF
           IF  (NOT-I-24)
               ADD 1                       TO ANTVL
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  ANTBES > ANTLEV
               SET I-12                    TO TRUE
           END-IF
           IF  (NOT-I-24 AND I-12)
               ADD 1                       TO ANTVLR
           END-IF.
      *****************************************************************
      *                SUBRUTINE FOR ORDRESUMMERING.                  *
      *****************************************************************
 
       OSURUT-S SECTION.
       OSURUT-S-P.
           IF  (I-24)
               GO TO ENDOSU-T
      ****** SUMMERING AV ORDRELINJE TIL ORDRE PRIS/RABATT.
      *  SNU BELØP DERSOM KREDIT-GEBYR ELLER ANNEN RETUR.
           END-IF
           DIVIDE EDBNR BY 10000       GIVING EDBNR3
           DIVIDE EDBNR BY 100000      GIVING EDBNR2
           SET NOT-I-98                    TO TRUE
           IF  EDBNR2 = 94
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  EDBNR3 = 995
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-82)
               MULTIPLY ANTLEV BY ORPRIS GIVING NETTO ROUNDED
           END-IF
           IF  (NOT-I-82)
               ADD ORPRIS TO ZERO      GIVING NETTO
           END-IF
           MULTIPLY ORRAB1 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM NETTO ROUNDED
           MULTIPLY ORRAB2 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM NETTO ROUNDED
           MULTIPLY ORRAB3 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM NETTO ROUNDED
           IF  (I-98)
               MULTIPLY -1 BY NETTO    GIVING NETTO
           END-IF
           IF  (I-82)
               DIVIDE NETTO BY ANTLEV  GIVING NETSTK ROUNDED
           END-IF
           IF  (NOT-I-82)
               ADD NETTO TO ZERO       GIVING NETSTK
           END-IF
           SUBTRACT KOSPRI FROM NETSTK GIVING AVANSE.
 
       ENDOSU-T.
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
           MOVE 'ORD37'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD600  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      *****************************************************************
 
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
               MOVE PARAM-IO-AREA (5:5)    TO PPRO-IO
               INSPECT PPRO-IO REPLACING ALL ' ' BY '0'
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
               SET I-85                    TO TRUE
           END-EVALUATE.
 
       ORDSEQ-GET SECTION.
       ORDSEQ-GET-P.
           IF  ORDSEQ-EOF-OFF
               READ ORDSEQ
               AT END
                   SET ORDSEQ-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDSEQ-FLDSET SECTION.
       ORDSEQ-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
               MOVE ORDSEQ-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDSEQ-IO-AREA (21:6)  TO KUNDNR (1:6)
               MOVE ORDSEQ-IO-AREA (21:2)  TO KUND2F (1:2)
               MOVE ORDSEQ-IO-AREA (27:30) TO KNAVN1 (1:30)
               MOVE ORDSEQ-IO-AREA (57:30) TO KNAVN2 (1:30)
               MOVE ORDSEQ-IO-AREA (87:1)  TO KTSIFF (1:1)
               MOVE ORDSEQ-IO-AREA (88:1)  TO DIRREG (1:1)
               MOVE ORDSEQ-IO-AREA (89:1)  TO FRITT (1:1)
               MOVE ORDSEQ-IO-AREA (90:2)  TO LAGER (1:2)
               MOVE ORDSEQ-IO-AREA (92:1)  TO BK (1:1)
               MOVE ORDSEQ-IO-AREA (93:1)  TO SKAF (1:1)
               MOVE ORDSEQ-IO-AREA (94:2)  TO BETM (1:2)
               MOVE ORDSEQ-IO-AREA (96:1)  TO GEBYR (1:1)
               MOVE ORDSEQ-IO-AREA (97:1)  TO FRAKT (1:1)
               MOVE ORDSEQ-IO-AREA (98:1)  TO AVD (1:1)
               MOVE ORDSEQ-IO-AREA (99:1)  TO KRETYP (1:1)
               MOVE ORDSEQ-IO-AREA (100:1) TO REST (1:1)
               MOVE ORDSEQ-IO-AREA (101:1) TO PRIKOD (1:1)
               MOVE ORDSEQ-IO-AREA (102:1) TO STAM (1:1)
               MOVE ORDSEQ-IO-AREA (103:1) TO KIS (1:1)
               MOVE ORDSEQ-IO-AREA (105:1) TO KOMUTA (1:1)
               MOVE ORDSEQ-IO-AREA (108:2) TO PAKKER (1:2)
               MOVE ORDSEQ-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDSEQ-IO-AREA (138:4) TO ORDMA-ELGR (1:4)
               MOVE ORDSEQ-IO-AREA (138:2) TO ORDMND (1:2)
               MOVE ORDSEQ-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDSEQ-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDSEQ-IO-AREA (142:2) TO ORDMOT (1:2)
               MOVE ORDSEQ-IO-AREA (144:4) TO TERMID (1:4)
               MOVE ORDSEQ-IO-AREA (148:1) TO SELGKP (1:1)
               MOVE ORDSEQ-IO-AREA (149:1) TO FERDIM (1:1)
               MOVE ORDSEQ-IO-AREA (150:2) TO FAKTNR (1:2)
               MOVE ORDSEQ-IO-AREA (153:4) TO REGKL-IO
               MOVE ORDSEQ-IO-AREA (157:1) TO RUTID (1:1)
               MOVE ORDSEQ-IO-AREA (158:4) TO OPDATO-IO
               MOVE ORDSEQ-IO-AREA (162:2) TO ANTPRT-IO
               INSPECT ANTPRT-IO REPLACING ALL ' ' BY '0'
               MOVE ORDSEQ-IO-AREA (164:1) TO STATUS-X (1:1)
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
               MOVE ORDSEQ-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDSEQ-IO-AREA (44:6)  TO FAKREF (1:6)
               MOVE ORDSEQ-IO-AREA (50:11) TO AVNAVN (1:11)
               MOVE ORDSEQ-IO-AREA (61:6)  TO OKKNR (1:6)
               MOVE ORDSEQ-IO-AREA (67:15) TO REKVNR (1:15)
               MOVE ORDSEQ-IO-AREA (82:15) TO FORSM (1:15)
               MOVE ORDSEQ-IO-AREA (97:3)  TO HND (1:3)
               MOVE ORDSEQ-IO-AREA (101:30) TO KADR (1:30)
               MOVE ORDSEQ-IO-AREA (131:4) TO POSTNR (1:4)
               MOVE ORDSEQ-IO-AREA (135:15) TO PSTED (1:15)
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
               MOVE ORDSEQ-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDSEQ-IO-AREA (21:30) TO VAADR1 (1:30)
               MOVE ORDSEQ-IO-AREA (51:30) TO VAADR2 (1:30)
               MOVE ORDSEQ-IO-AREA (81:30) TO VAADR3 (1:30)
               MOVE ORDSEQ-IO-AREA (111:20) TO VAADR4 (1:20)
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDSEQ-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDSEQ-IO-AREA (11:6)  TO LAGLOC (1:6)
               MOVE ORDSEQ-IO-AREA (17:3)  TO POSNR-IO
               INSPECT POSNR-IO REPLACING ALL ' ' BY '0'
               MOVE ORDSEQ-IO-AREA (21:4)  TO ANTBES-IO
               MOVE ORDSEQ-IO-AREA (25:4)  TO ANTRES-IO
               MOVE ORDSEQ-IO-AREA (29:4)  TO ANTLEV-IO
               MOVE ORDSEQ-IO-AREA (33:1)  TO NOREST (1:1)
               MOVE ORDSEQ-IO-AREA (34:3)  TO ALF (1:3)
               MOVE ORDSEQ-IO-AREA (37:50) TO TEKST (1:50)
               MOVE ORDSEQ-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDSEQ-IO-AREA (57:30) TO VARBET (1:30)
               MOVE ORDSEQ-IO-AREA (87:4)  TO EDBNR-IO
               MOVE ORDSEQ-IO-AREA (91:3)  TO VGR-IO
               MOVE ORDSEQ-IO-AREA (94:5)  TO ORPRIS-IO
               MOVE ORDSEQ-IO-AREA (99:2)  TO ORRAB1-IO
               MOVE ORDSEQ-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDSEQ-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDSEQ-IO-AREA (105:5) TO RGPRIS-IO
               MOVE ORDSEQ-IO-AREA (110:2) TO RGRAB1-IO
               MOVE ORDSEQ-IO-AREA (112:2) TO RGRAB2-IO
               MOVE ORDSEQ-IO-AREA (114:2) TO RGRAB3-IO
               MOVE ORDSEQ-IO-AREA (116:5) TO VEIPRI-IO
               MOVE ORDSEQ-IO-AREA (121:5) TO KOSPRI-IO
               MOVE ORDSEQ-IO-AREA (126:4) TO PRITIL-IO
               MOVE ORDSEQ-IO-AREA (130:4) TO KODATO-IO
               MOVE ORDSEQ-IO-AREA (134:2) TO KOSIGN (1:2)
               MOVE ORDSEQ-IO-AREA (136:1) TO KOSTAT (1:1)
               MOVE ORDSEQ-IO-AREA (137:1) TO PRITYP (1:1)
               MOVE ORDSEQ-IO-AREA (158:1) TO NET1 (1:1)
               MOVE ORDSEQ-IO-AREA (158:3) TO NETFAK (1:3)
           END-EVALUATE.
 
       ORDSEQ-IDCHK SECTION.
       ORDSEQ-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
             OR ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
             OR ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
             OR ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDSEQ-IDSET SECTION.
       ORDSEQ-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDSEQ-CHK-LEVEL SECTION.
       ORDSEQ-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDSEQ-LEVEL-01
               MOVE ORDSEQ-IO-AREA (2:3)   TO ORDSEQ-01-L2-FIRMA
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDSEQ-01-L1-ORDNR
               IF  ORDSEQ-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSEQ-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDSEQ-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSEQ-01-L2          TO THE-PRIOR-L2
               MOVE  ORDSEQ-01-L1          TO THE-PRIOR-L1
               SET ORDSEQ-LEVEL-INIT       TO TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
               MOVE LOW-VALUES             TO ORDSEQ-LEVEL-02
               MOVE ORDSEQ-IO-AREA (2:3)   TO ORDSEQ-02-L2-FIRMA
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDSEQ-02-L1-ORDNR
               IF  ORDSEQ-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSEQ-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDSEQ-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSEQ-02-L2          TO THE-PRIOR-L2
               MOVE  ORDSEQ-02-L1          TO THE-PRIOR-L1
               SET ORDSEQ-LEVEL-INIT       TO TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
               MOVE LOW-VALUES             TO ORDSEQ-LEVEL-03
               MOVE ORDSEQ-IO-AREA (2:3)   TO ORDSEQ-03-L2-FIRMA
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDSEQ-03-L1-ORDNR
               IF  ORDSEQ-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSEQ-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDSEQ-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSEQ-03-L2          TO THE-PRIOR-L2
               MOVE  ORDSEQ-03-L1          TO THE-PRIOR-L1
               SET ORDSEQ-LEVEL-INIT       TO TRUE
           WHEN ( ORDSEQ-IO-AREA (1:1) = 'O'
            AND   ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDSEQ-LEVEL-04
               MOVE ORDSEQ-IO-AREA (2:3)   TO ORDSEQ-04-L2-FIRMA
               MOVE ORDSEQ-IO-AREA (5:6)   TO ORDSEQ-04-L1-ORDNR
               IF  ORDSEQ-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSEQ-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDSEQ-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSEQ-04-L2          TO THE-PRIOR-L2
               MOVE  ORDSEQ-04-L1          TO THE-PRIOR-L1
               SET ORDSEQ-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
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
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDRNR'               TO LISTE-IO-AREA (1:6)
               MOVE 'O.DATO'               TO LISTE-IO-AREA (10:6)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (17:5)
               MOVE 'ALF'                  TO LISTE-IO-AREA (24:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (28:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (51:14)
               MOVE 'BESTILLT'             TO LISTE-IO-AREA (82:8)
               MOVE 'NETTOPRIS'            TO LISTE-IO-AREA (92:9)
               MOVE 'KOSTPRIS'             TO LISTE-IO-AREA (104:8)
               MOVE 'BRF.'                 TO LISTE-IO-AREA (117:4)
               MOVE 'BK SIGN'              TO LISTE-IO-AREA (122:7)
               MOVE 'S P'                  TO LISTE-IO-AREA (130:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'K T'                  TO LISTE-IO-AREA (130:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-04 AND I-50 AND I-80)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDNR                  TO LISTE-IO-AREA (1:6)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (8:8)
               MOVE KUNDNR                 TO LISTE-IO-AREA (17:6)
               MOVE ALF                    TO LISTE-IO-AREA (24:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (28:20)
               MOVE VARBET                 TO LISTE-IO-AREA (49:30)
               MOVE ANTBES                 TO XO-52YN9
               MOVE XO-52YN9               TO LISTE-IO-AREA (82:8)
               INITIALIZE ANTBES
               MOVE NETSTK                 TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (91:10)
               INITIALIZE NETSTK
               MOVE KOSPRI                 TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (102:10)
               INITIALIZE KOSPRI
               MOVE BRF                    TO XO-32YN9R
               MOVE XO-32YN9R              TO LISTE-IO-AREA (114:7)
               INITIALIZE BRF
               MOVE BK2                    TO LISTE-IO-AREA (123:1)
               INITIALIZE BK2
               MOVE SIGN-X                 TO LISTE-IO-AREA (126:2)
               INITIALIZE SIGN-X
               MOVE SKAF                   TO LISTE-IO-AREA (130:1)
               INITIALIZE SKAF
               MOVE PRITYP                 TO LISTE-IO-AREA (132:1)
               INITIALIZE PRITYP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86 AND NOT-I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDRNR'               TO LISTE-IO-AREA (1:6)
               MOVE 'O.DATO'               TO LISTE-IO-AREA (10:6)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (17:5)
               MOVE 'ALF'                  TO LISTE-IO-AREA (24:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (28:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (51:14)
               MOVE 'BESTILLT'             TO LISTE-IO-AREA (82:8)
               MOVE 'NETTOPRIS'            TO LISTE-IO-AREA (92:9)
               MOVE 'KOSTPRIS'             TO LISTE-IO-AREA (104:8)
               MOVE 'BRF.'                 TO LISTE-IO-AREA (117:4)
               MOVE 'BK SIGN'              TO LISTE-IO-AREA (122:7)
               MOVE 'S P'                  TO LISTE-IO-AREA (130:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'K T'                  TO LISTE-IO-AREA (130:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------------' TO LISTE-IO-AREA (1:20)
               MOVE '--------------------' TO LISTE-IO-AREA (21:20)
               MOVE '--------------------' TO LISTE-IO-AREA (41:20)
               MOVE '--------------------' TO LISTE-IO-AREA (61:20)
               MOVE '--------------------' TO LISTE-IO-AREA (81:20)
               MOVE '--------------------' TO LISTE-IO-AREA (101:20)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (6:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (54:35)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (92:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (123:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86 AND NOT-I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (6:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (54:35)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (92:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (123:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET ORDSEQ-LEVEL-INIT           TO TRUE
           INITIALIZE ORDSEQ-DATA-FIELDS
           SET ORDSEQ-EOF-OFF              TO TRUE
           SET ORDSEQ-PROCESS              TO TRUE
           OPEN INPUT ORDSEQ
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE ORDSEQ
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
