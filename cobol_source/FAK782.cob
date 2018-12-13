       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK782R.
      *   ******************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: FAK782                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMMERT: 08.08.2005                                      *
      *  RETTET.....: 08.12.2005                                      *
      *                                                               *
      *  TREKKER UT DATA FRA FAKTURA.SALGSDATA OG DANNER INPUT TIL   *
      *  EXCEL/DATABASE. FELTER MED ;-SEPARERTE FELTER.              *
      *  TAR OGSÅ MED FRAKTKOSTNADER FRA EDI.FRAKT.FILE              *
      *  ER FRAKT BELØP OG SVS I ORDREN LIKE, REDUSERES SVS MED 15%  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK782.rpg
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
           SELECT FAKSALG
               ASSIGN TO UT-S-FAKSALG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKSALG-STATUS.
           SELECT FRAKTF
               ASSIGN TO UT-S-FRAKTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRAKTF-STATUS.
           SELECT PAR
               ASSIGN TO UT-S-PAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PAR-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FSDATA
               ASSIGN TO UT-S-FSDATA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FSDATA-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKSALG
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  FAKSALG-IO-AREA.
           05  FAKSALG-IO-AREA-X           PICTURE X(160).
       FD FRAKTF
               BLOCK CONTAINS 120
               RECORD CONTAINS 40.
       01  FRAKTF-IO-AREA.
           05  FRAKTF-IO-AREA-X            PICTURE X(40).
       FD PAR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PAR-IO-AREA.
           05  PAR-IO-AREA-X               PICTURE X(80).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD FSDATA
               BLOCK CONTAINS 128
               RECORD CONTAINS 128.
       01  FSDATA-IO-AREA.
           05  FSDATA-IO-AREA-X            PICTURE X(128).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKSALG-STATUS              PICTURE 99 VALUE 0.
           10  FRAKTF-STATUS               PICTURE 99 VALUE 0.
           10  PAR-STATUS                  PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FSDATA-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-EOF-OFF         VALUE '0'.
               88  FAKSALG-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-READ-OFF        VALUE '0'.
               88  FAKSALG-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-PROCESS-OFF     VALUE '0'.
               88  FAKSALG-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKSALG-LEVEL-INIT-OFF  VALUE '0'.
               88  FAKSALG-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTF-EOF-OFF          VALUE '0'.
               88  FRAKTF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTF-READ-OFF         VALUE '0'.
               88  FRAKTF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTF-PROCESS-OFF      VALUE '0'.
               88  FRAKTF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FRAKTF-LEVEL-INIT-OFF   VALUE '0'.
               88  FRAKTF-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-EOF-OFF             VALUE '0'.
               88  PAR-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-READ-OFF            VALUE '0'.
               88  PAR-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-PROCESS-OFF         VALUE '0'.
               88  PAR-PROCESS             VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FAKSALG-LEVEL-01.
               10  FAKSALG-01-L3.
                   15  FAKSALG-01-L3-FIRMA PICTURE X(3).
               10  FAKSALG-01-L2.
                   15  FAKSALG-01-L2-KUNDNR PICTURE X(6).
               10  FAKSALG-01-L1.
                   15  FAKSALG-01-L1-ORDNR PICTURE X(6).
           05  FAKSALG-DATA-FIELDS.
               10  FAKMND                  PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  SEQNR                   PICTURE X(5).
               10  ORDDTO                  PICTURE X(8).
               10  ORDTID                  PICTURE X(4).
               10  ORDNR                   PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  BM                      PICTURE X(2).
               10  LK                      PICTURE X(2).
               10  AVD                     PICTURE X(1).
               10  FK                      PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  KOSTN                   PICTURE X(1).
               10  SERVO                   PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  HND                     PICTURE X(3).
               10  BK                      PICTURE X(1).
               10  EDBNR                   PICTURE X(7).
               10  ALFAK                   PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ART8F                   PICTURE X(8).
               10  ANTB-IO.
                   15  ANTB                PICTURE S9(5)V9(2).
               10  ANTL-IO.
                   15  ANTL                PICTURE S9(5)V9(2).
               10  ORDPRI-IO.
                   15  ORDPRI              PICTURE S9(7)V9(2).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  SVSSUM-IO.
                   15  SVSSUM              PICTURE S9(7)V9(2).
               10  FAKTNR                  PICTURE X(6).
               10  ORDTYP                  PICTURE X(1).
               10  FAKOMG                  PICTURE X(1).
               10  ORDMOT                  PICTURE X(2).
               10  PLUKKA                  PICTURE X(2).
           05  FAKSALG-MP                  PICTURE X(15).
           05  FAKSALG-MC                  PICTURE X(15).
           05  FAKSALG-M-01            REDEFINES FAKSALG-MC.
               10  FAKSALG-M-01-M3.
                   15  FAKSALG-M-01-M3-FIRMA-G.
                       20  FAKSALG-M-01-M3-FIRMA PICTURE X(3).
               10  FAKSALG-M-01-M2.
                   15  FAKSALG-M-01-M2-KUNDNR-G.
                       20  FAKSALG-M-01-M2-KUNDNR PICTURE X(6).
               10  FAKSALG-M-01-M1.
                   15  FAKSALG-M-01-M1-ORDNR-G.
                       20  FAKSALG-M-01-M1-ORDNR PICTURE X(6).
           05  FRAKTF-LEVEL-05.
               10  FRAKTF-05-L3.
                   15  FRAKTF-05-L3-FIRMA  PICTURE X(3).
               10  FRAKTF-05-L2.
                   15  FRAKTF-05-L2-KUNDNR PICTURE X(6).
               10  FRAKTF-05-L1.
                   15  FRAKTF-05-L1-ORDNR  PICTURE X(6).
           05  FRAKTF-DATA-FIELDS.
               10  FRAKTK-IO.
                   15  FRAKTK              PICTURE S9(5).
           05  FRAKTF-MP                   PICTURE X(15).
           05  FRAKTF-MC                   PICTURE X(15).
           05  FRAKTF-M-05             REDEFINES FRAKTF-MC.
               10  FRAKTF-M-05-M3.
                   15  FRAKTF-M-05-M3-FIRMA-G.
                       20  FRAKTF-M-05-M3-FIRMA PICTURE X(3).
               10  FRAKTF-M-05-M2.
                   15  FRAKTF-M-05-M2-KUNDNR-G.
                       20  FRAKTF-M-05-M2-KUNDNR PICTURE X(6).
               10  FRAKTF-M-05-M1.
                   15  FRAKTF-M-05-M1-ORDNR-G.
                       20  FRAKTF-M-05-M1-ORDNR PICTURE X(6).
           05  PAR-DATA-FIELDS.
               10  PERIOD                  PICTURE X(43).
               10  FRAMND                  PICTURE X(6).
               10  TILMND                  PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  POSTST                  PICTURE X(15).
               10  POSTNR                  PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FNRKNR                  PICTURE X(9).
               10  ANTOR3-IO.
                   15  ANTOR3              PICTURE S9(6).
               10  OSUML3-IO.
                   15  OSUML3              PICTURE S9(8)V9(2).
               10  FSUML3-IO.
                   15  FSUML3              PICTURE S9(8)V9(2).
               10  KSUML3-IO.
                   15  KSUML3              PICTURE S9(8)V9(2).
               10  OSUML2-IO.
                   15  OSUML2              PICTURE S9(8)V9(2).
               10  FSUML2-IO.
                   15  FSUML2              PICTURE S9(8)V9(2).
               10  KSUML2-IO.
                   15  KSUML2              PICTURE S9(8)V9(2).
               10  ANTOR2-IO.
                   15  ANTOR2              PICTURE S9(6).
               10  OSUML1-IO.
                   15  OSUML1              PICTURE S9(8)V9(2).
               10  FSUML1-IO.
                   15  FSUML1              PICTURE S9(8)V9(2).
               10  KSUML1-IO.
                   15  KSUML1              PICTURE S9(8)V9(2).
               10  FRAKF1-IO.
                   15  FRAKF1              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  EDIT-OSUML2             PICTURE Z9999999,99.
               10  EDIT-FSUML2             PICTURE Z9999999,99.
               10  EDIT-KSUML2             PICTURE Z9999999,99.
               10  EDIT-OSUML3             PICTURE ZZZZZZZZ,ZZ.
               10  EDIT-FSUML3             PICTURE ZZZZZZZZ,ZZ.
               10  EDIT-KSUML3             PICTURE ZZZZZZZZ,ZZ.
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKSALG-PROCESS
               SET FAKSALG-PROCESS-OFF     TO TRUE
               SET FAKSALG-READ            TO TRUE
           END-IF
 
           IF  FAKSALG-READ
               PERFORM FAKSALG-GET
               SET FAKSALG-READ-OFF        TO TRUE
               IF  NOT FAKSALG-EOF
                   PERFORM FAKSALG-MATCH-SET
               END-IF
           END-IF
 
           IF  FRAKTF-PROCESS
               SET FRAKTF-PROCESS-OFF      TO TRUE
               SET FRAKTF-READ             TO TRUE
           END-IF
 
           IF  FRAKTF-READ
               PERFORM FRAKTF-GET
               SET FRAKTF-READ-OFF         TO TRUE
               IF  NOT FRAKTF-EOF
                   PERFORM FRAKTF-MATCH-SET
               END-IF
           END-IF
 
           IF  PAR-PROCESS
               SET PAR-PROCESS-OFF         TO TRUE
               SET PAR-READ                TO TRUE
           END-IF
 
           IF  PAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM PAR-GET
               SET PAR-READ-OFF            TO TRUE
               IF  NOT PAR-EOF
                   PERFORM PAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PAR-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
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
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-IDSET
           END-IF
 
           IF  FRAKTF-PROCESS
               PERFORM FRAKTF-IDSET
           END-IF
 
           IF  PAR-PROCESS
               PERFORM PAR-IDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-CHK-LEVEL
           END-IF
 
           IF  FRAKTF-PROCESS
               PERFORM FRAKTF-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-FLDSET
           END-IF
 
           IF  FRAKTF-PROCESS
               PERFORM FRAKTF-FLDSET
           END-IF
 
           IF  PAR-PROCESS
               PERFORM PAR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKSALG-PROCESS
           OR  FRAKTF-PROCESS
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
               SET NOT-I-60                TO TRUE
               SET NOT-I-52                TO TRUE
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
               SET NOT-I-29                TO TRUE
               SET NOT-I-21                TO TRUE
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
      *****************************************************************
      * HENTE DATA FRA KUNDEMASTER.                                   *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-88                TO TRUE
               IF  FIRMA = '913'
                   SET I-88                TO TRUE
               END-IF
               MOVE FIRMA                  TO FNRKNR (1:3)
               MOVE KUNDNR                 TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-51                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-51            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      *****************************************************************
      * NULLSTILLING.                                                 *
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE 0                      TO ANTOR3
               MOVE 0,00                   TO OSUML3
               MOVE 0,00                   TO FSUML3
               MOVE 0,00                   TO KSUML3
           END-IF
           IF  (I-L2)
               MOVE 0,00                   TO OSUML2
               MOVE 0,00                   TO FSUML2
               MOVE 0,00                   TO KSUML2
               MOVE 0                      TO ANTOR2
           END-IF
           IF  (I-L1)
               MOVE 0,00                   TO OSUML1
               MOVE 0,00                   TO FSUML1
               MOVE 0,00                   TO KSUML1
               MOVE 0,00                   TO FRAKF1
           END-IF
           IF  (I-05 AND NOT-I-MR)
               GO TO SLUTT-T
      *****************************************************************
      * SAVE FRAKTKOSTNAD.                                            *
      *****************************************************************
           END-IF
           IF  (I-05 AND I-MR)
               MULTIPLY 0,85 BY FRAKTK GIVING FRAKF1
           END-IF
           IF  (I-05)
               GO TO SLUTT-T
      *****************************************************************
      * SELEKSJONSRUTINE.                                             *
      *****************************************************************
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  FAKMND < FRAMND
               SET I-11                    TO TRUE
           END-IF
           IF  (I-11)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  FAKMND > TILMND
               SET I-12                    TO TRUE
           END-IF
           IF  (I-12)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-88)
               SET NOT-I-21                TO TRUE
               IF  VGR = '95010'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-88)
               SET NOT-I-21                TO TRUE
               IF  VGR = '99030'
                   SET I-21                TO TRUE
               END-IF
      * N21      VGR       COMP "19960"                  21 FAKT.FRAKT
      * N21      VGR       COMP "19962"                  21 FAKT.FRAKT
      * N21      VGR       COMP "39991"                  21 FAKT.FRAKT
      * N21      VGR       COMP "49991"                  21 FAKT.FRAKT
      *****************************************************************
      * SUMMERINGSRUTINE.                                             *
      *    KREDITNOTA HAR NEGATIVT BELØP, MEN IKKE ANTALL.            *
      *****************************************************************
           END-IF
           IF  (I-21)
               ADD NTOSUM                  TO FSUML1
               ADD NTOSUM                  TO FSUML2
               SET NOT-I-29                TO TRUE
               IF  SVSSUM > 0,00
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-21 AND I-29)
               SET NOT-I-28                TO TRUE
               IF  SVSSUM = NTOSUM
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-21 AND I-29 AND I-28)
               MULTIPLY 0,85 BY SVSSUM GIVING SVSSUM ROUNDED
           END-IF
           IF  (I-21 AND I-29)
               ADD SVSSUM                  TO KSUML1
               ADD SVSSUM                  TO KSUML2
           END-IF
           ADD NTOSUM                      TO OSUML1
           ADD NTOSUM                      TO OSUML2
      *****************************************************************
      * SETT PÅ INDIKATORER.                                          *
           SET I-50                        TO TRUE
           SET I-52                        TO TRUE
      *****************************************************************
           .
 
       SLUTT-T.
      *****************************************************************
      * TOTTALRUTINE FOR Å SETTE BELØP OG ANTALL NEGATIV.             *
      *****************************************************************
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-50 AND NOT-I-29)
               ADD FRAKF1                  TO KSUML2
           END-IF
           IF  (I-L1 AND I-50)
               ADD 1                       TO ANTOR2
           END-IF
           IF  (I-L2 AND NOT-I-52)
               GO TO SLUTL2-T
           END-IF
           IF  (I-L2)
               SET NOT-I-31                TO TRUE
               IF  OSUML2 < 0,00
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               SET NOT-I-62                TO TRUE
               IF  FSUML2 < 0,00
                   SET I-32                TO TRUE
               END-IF
               IF  FSUML2 = 0,00
                   SET I-62                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               SET NOT-I-63                TO TRUE
               IF  KSUML2 < 0,00
                   SET I-33                TO TRUE
               END-IF
               IF  KSUML2 = 0,00
                   SET I-63                TO TRUE
               END-IF
      *****************************************************************
      * OM KUNDEN HAR NULL I FRAKT SKAL DEN IKKE LEGGES UT.           *
      * TOTALENE SUMMERES OG LEGGES UT TIL SLUTT.                     *
      *****************************************************************
           END-IF
           IF  (I-L2 AND NOT-I-62)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-L2 AND NOT-I-63)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-L2 AND NOT-I-60)
               ADD OSUML2                  TO OSUML3
               ADD FSUML2                  TO FSUML3
               ADD KSUML2                  TO KSUML3
               ADD ANTOR2                  TO ANTOR3
           END-IF
           IF  (I-L2 AND I-60 AND NOT-I-98)
               SET I-99                    TO TRUE
           END-IF
           IF  (I-L2 AND I-60)
               SET I-98                    TO TRUE
           END-IF.
 
       SLUTL2-T.
           CONTINUE.
 
       FAKSALG-GET SECTION.
       FAKSALG-GET-P.
           IF  FAKSALG-EOF-OFF
               READ FAKSALG
               AT END
                   SET FAKSALG-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKSALG-FLDSET SECTION.
       FAKSALG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKSALG-IO-AREA (4:6)  TO FAKMND (1:6)
               MOVE FAKSALG-IO-AREA (10:6) TO KUNDNR (1:6)
               MOVE FAKSALG-IO-AREA (16:5) TO SEQNR (1:5)
               MOVE FAKSALG-IO-AREA (21:8) TO ORDDTO (1:8)
               MOVE FAKSALG-IO-AREA (29:4) TO ORDTID (1:4)
               MOVE FAKSALG-IO-AREA (33:6) TO ORDNR (1:6)
               MOVE FAKSALG-IO-AREA (39:3) TO FIRMA (1:3)
               MOVE FAKSALG-IO-AREA (42:2) TO BM (1:2)
               MOVE FAKSALG-IO-AREA (44:2) TO LK (1:2)
               MOVE FAKSALG-IO-AREA (46:1) TO AVD (1:1)
               MOVE FAKSALG-IO-AREA (47:1) TO FK (1:1)
               MOVE FAKSALG-IO-AREA (48:1) TO KRTYPE (1:1)
               MOVE FAKSALG-IO-AREA (49:1) TO KOSTN (1:1)
               MOVE FAKSALG-IO-AREA (50:1) TO SERVO (1:1)
               MOVE FAKSALG-IO-AREA (51:5) TO VGR (1:5)
               MOVE FAKSALG-IO-AREA (56:3) TO HND (1:3)
               MOVE FAKSALG-IO-AREA (59:1) TO BK (1:1)
               MOVE FAKSALG-IO-AREA (60:7) TO EDBNR (1:7)
               MOVE FAKSALG-IO-AREA (67:3) TO ALFAK (1:3)
               MOVE FAKSALG-IO-AREA (70:20) TO ARTNR (1:20)
               MOVE FAKSALG-IO-AREA (70:8) TO ART8F (1:8)
               MOVE FAKSALG-IO-AREA (90:7) TO ANTB-IO
               INSPECT ANTB-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (97:7) TO ANTL-IO
               INSPECT ANTL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (104:9) TO ORDPRI-IO
               INSPECT ORDPRI-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (113:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (116:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (119:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (122:9) TO NTOSUM-IO
               INSPECT NTOSUM-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (131:9) TO SVSSUM-IO
               INSPECT SVSSUM-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (140:6) TO FAKTNR (1:6)
               MOVE FAKSALG-IO-AREA (146:1) TO ORDTYP (1:1)
               MOVE FAKSALG-IO-AREA (147:1) TO FAKOMG (1:1)
               MOVE FAKSALG-IO-AREA (148:2) TO ORDMOT (1:2)
               MOVE FAKSALG-IO-AREA (150:2) TO PLUKKA (1:2)
           END-EVALUATE.
 
       FAKSALG-IDSET SECTION.
       FAKSALG-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKSALG-CHK-LEVEL SECTION.
       FAKSALG-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKSALG-LEVEL-01
               MOVE FAKSALG-IO-AREA (39:3) TO FAKSALG-01-L3-FIRMA
               MOVE FAKSALG-IO-AREA (10:6) TO FAKSALG-01-L2-KUNDNR
               MOVE FAKSALG-IO-AREA (33:6) TO FAKSALG-01-L1-ORDNR
               IF  FAKSALG-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKSALG-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKSALG-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKSALG-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKSALG-01-L3         TO THE-PRIOR-L3
               MOVE  FAKSALG-01-L2         TO THE-PRIOR-L2
               MOVE  FAKSALG-01-L1         TO THE-PRIOR-L1
               SET FAKSALG-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FAKSALG-MATCH-SET SECTION.
       FAKSALG-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKSALG-IO-AREA (39:3) TO FAKSALG-M-01-M3-FIRMA
               MOVE FAKSALG-IO-AREA (10:6) TO FAKSALG-M-01-M2-KUNDNR
               MOVE FAKSALG-IO-AREA (33:6) TO FAKSALG-M-01-M1-ORDNR
           END-EVALUATE.
 
       FRAKTF-GET SECTION.
       FRAKTF-GET-P.
           IF  FRAKTF-EOF-OFF
               READ FRAKTF
               AT END
                   SET FRAKTF-EOF          TO TRUE
               END-READ
           END-IF.
 
       FRAKTF-FLDSET SECTION.
       FRAKTF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FRAKTF-IO-AREA (4:3)   TO FIRMA (1:3)
               MOVE FRAKTF-IO-AREA (12:6)  TO ORDNR (1:6)
               MOVE FRAKTF-IO-AREA (18:6)  TO KUNDNR (1:6)
               MOVE FRAKTF-IO-AREA (24:5)  TO FRAKTK-IO
               INSPECT FRAKTK-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FRAKTF-IDSET SECTION.
       FRAKTF-IDSET-P.
           SET I-05                        TO TRUE.
 
       FRAKTF-CHK-LEVEL SECTION.
       FRAKTF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FRAKTF-LEVEL-05
               MOVE FRAKTF-IO-AREA (4:3)   TO FRAKTF-05-L3-FIRMA
               MOVE FRAKTF-IO-AREA (18:6)  TO FRAKTF-05-L2-KUNDNR
               MOVE FRAKTF-IO-AREA (12:6)  TO FRAKTF-05-L1-ORDNR
               IF  FRAKTF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FRAKTF-05-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FRAKTF-05-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FRAKTF-05-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FRAKTF-05-L3          TO THE-PRIOR-L3
               MOVE  FRAKTF-05-L2          TO THE-PRIOR-L2
               MOVE  FRAKTF-05-L1          TO THE-PRIOR-L1
               SET FRAKTF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FRAKTF-MATCH-SET SECTION.
       FRAKTF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FRAKTF-IO-AREA (4:3)   TO FRAKTF-M-05-M3-FIRMA
               MOVE FRAKTF-IO-AREA (18:6)  TO FRAKTF-M-05-M2-KUNDNR
               MOVE FRAKTF-IO-AREA (12:6)  TO FRAKTF-M-05-M1-ORDNR
           END-EVALUATE.
 
       PAR-GET SECTION.
       PAR-GET-P.
           IF  PAR-EOF-OFF
               READ PAR
               AT END
                   SET PAR-EOF             TO TRUE
               END-READ
           END-IF.
 
       PAR-FLDSET SECTION.
       PAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               MOVE PAR-IO-AREA (7:43)     TO PERIOD (1:43)
               MOVE PAR-IO-AREA (22:6)     TO FRAMND (1:6)
               MOVE PAR-IO-AREA (44:6)     TO TILMND (1:6)
           END-EVALUATE.
 
       PAR-IDCHK SECTION.
       PAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PAR-IDSET SECTION.
       PAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO POSTST (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO POSTNR (1:4)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FAKSALG-EOF
               MOVE HIGH-VALUES            TO FAKSALG-MC
                                              FAKSALG-MP
           END-IF
           IF  FRAKTF-EOF
               MOVE HIGH-VALUES            TO FRAKTF-MC
                                              FRAKTF-MP
           END-IF
           IF  FAKSALG-MC < FAKSALG-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FRAKTF-MC < FRAKTF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FAKSALG-MC < FRAKTF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKSALG-PROCESS     TO TRUE
                   MOVE FAKSALG-MC         TO FAKSALG-MP
                   IF  FAKSALG-MC = FRAKTF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FRAKTF-MC < FAKSALG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FRAKTF-PROCESS      TO TRUE
                   MOVE FRAKTF-MC          TO FRAKTF-MP
                   IF  FRAKTF-MC = FAKSALG-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKSALG-MC = FRAKTF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKSALG-PROCESS     TO TRUE
                   MOVE FAKSALG-MC         TO FAKSALG-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND I-60 AND I-99)
               MOVE SPACES TO FSDATA-IO-AREA
               INITIALIZE FSDATA-IO-AREA
               MOVE 'OPPGAVE FAK76M'       TO FSDATA-IO-AREA (1:14)
               MOVE 'SALG OG FRAKT OPPGAVE.' TO FSDATA-IO-AREA (16:22)
               MOVE PERIOD                 TO FSDATA-IO-AREA (41:43)
               WRITE FSDATA-IO-AREA
               MOVE SPACES TO FSDATA-IO-AREA
               INITIALIZE FSDATA-IO-AREA
               MOVE 'KUNDE '               TO FSDATA-IO-AREA (1:6)
               MOVE ';'                    TO FSDATA-IO-AREA (7:1)
               MOVE 'ANTALL'               TO FSDATA-IO-AREA (8:6)
               MOVE ';'                    TO FSDATA-IO-AREA (14:1)
               MOVE 'KUNDE NAVN          ' TO FSDATA-IO-AREA (15:20)
               MOVE ';'                    TO FSDATA-IO-AREA (45:1)
               MOVE 'P.NR'                 TO FSDATA-IO-AREA (46:4)
               MOVE ';'                    TO FSDATA-IO-AREA (50:1)
               MOVE 'POSTSTED       '      TO FSDATA-IO-AREA (51:15)
               MOVE ';'                    TO FSDATA-IO-AREA (66:1)
               MOVE 'ORDRE TOTAL'          TO FSDATA-IO-AREA (67:11)
               MOVE ';'                    TO FSDATA-IO-AREA (78:1)
               MOVE 'FRAKT FAKT.'          TO FSDATA-IO-AREA (79:11)
               MOVE ';'                    TO FSDATA-IO-AREA (90:1)
               MOVE 'FRAKT KOST.'          TO FSDATA-IO-AREA (91:11)
               MOVE ';'                    TO FSDATA-IO-AREA (102:1)
               WRITE FSDATA-IO-AREA
           END-IF
           IF  (I-L2 AND I-60)
               MOVE SPACES TO FSDATA-IO-AREA
               INITIALIZE FSDATA-IO-AREA
               MOVE KUNDNR                 TO FSDATA-IO-AREA (1:6)
               MOVE ';'                    TO FSDATA-IO-AREA (7:1)
               MOVE ANTOR2-IO              TO FSDATA-IO-AREA (8:6)
               MOVE ';'                    TO FSDATA-IO-AREA (14:1)
               IF  (NOT-I-51)
                   MOVE KNAVN1             TO FSDATA-IO-AREA (15:30)
               END-IF
               IF  (I-51)
                   MOVE '** KUNDE ER UKJENT  ' TO FSDATA-IO-AREA
                                                               (15:20)
               END-IF
               MOVE ';'                    TO FSDATA-IO-AREA (45:1)
               IF  (NOT-I-51)
                   MOVE POSTNR             TO FSDATA-IO-AREA (46:4)
               END-IF
               MOVE ';'                    TO FSDATA-IO-AREA (50:1)
               IF  (NOT-I-51)
                   MOVE POSTST             TO FSDATA-IO-AREA (51:15)
               END-IF
               MOVE ';'                    TO FSDATA-IO-AREA (66:1)
               MOVE OSUML2                 TO EDIT-OSUML2
               MOVE EDIT-OSUML2            TO FSDATA-IO-AREA (67:11)
               IF  (I-31)
                   MOVE '-'                TO FSDATA-IO-AREA (67:1)
               END-IF
               IF  (NOT-I-31)
                   MOVE '0'                TO FSDATA-IO-AREA (67:1)
               END-IF
               MOVE ';'                    TO FSDATA-IO-AREA (78:1)
               MOVE FSUML2                 TO EDIT-FSUML2
               MOVE EDIT-FSUML2            TO FSDATA-IO-AREA (79:11)
               IF  (I-32)
                   MOVE '-'                TO FSDATA-IO-AREA (79:1)
               END-IF
               IF  (NOT-I-32)
                   MOVE '0'                TO FSDATA-IO-AREA (79:1)
               END-IF
               MOVE ';'                    TO FSDATA-IO-AREA (90:1)
               MOVE KSUML2                 TO EDIT-KSUML2
               MOVE EDIT-KSUML2            TO FSDATA-IO-AREA (91:11)
               IF  (I-33)
                   MOVE '-'                TO FSDATA-IO-AREA (91:1)
               END-IF
               IF  (NOT-I-33)
                   MOVE '0'                TO FSDATA-IO-AREA (91:1)
               END-IF
               MOVE ';'                    TO FSDATA-IO-AREA (102:1)
               WRITE FSDATA-IO-AREA
           END-IF
           IF  (I-L3)
               MOVE SPACES TO FSDATA-IO-AREA
               INITIALIZE FSDATA-IO-AREA
               MOVE '999999'               TO FSDATA-IO-AREA (1:6)
               MOVE ';'                    TO FSDATA-IO-AREA (7:1)
               MOVE ANTOR3-IO              TO FSDATA-IO-AREA (8:6)
               MOVE ';'                    TO FSDATA-IO-AREA (14:1)
               MOVE '** DIVERSE KUNDER UT' TO FSDATA-IO-AREA (15:20)
               MOVE 'EN FRAKT. '           TO FSDATA-IO-AREA (35:10)
               MOVE ';'                    TO FSDATA-IO-AREA (45:1)
      *                     N51POSTNR    49
               MOVE ';'                    TO FSDATA-IO-AREA (50:1)
      *                     N51POSTST    65
               MOVE ';'                    TO FSDATA-IO-AREA (66:1)
               MOVE OSUML3                 TO EDIT-OSUML3
               MOVE EDIT-OSUML3            TO FSDATA-IO-AREA (67:11)
               MOVE ';'                    TO FSDATA-IO-AREA (78:1)
               MOVE FSUML3                 TO EDIT-FSUML3
               MOVE EDIT-FSUML3            TO FSDATA-IO-AREA (79:11)
               MOVE ';'                    TO FSDATA-IO-AREA (90:1)
               MOVE KSUML3                 TO EDIT-KSUML3
               MOVE EDIT-KSUML3            TO FSDATA-IO-AREA (91:11)
               MOVE ';'                    TO FSDATA-IO-AREA (102:1)
               WRITE FSDATA-IO-AREA
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
           SET FAKSALG-LEVEL-INIT          TO TRUE
           INITIALIZE FAKSALG-DATA-FIELDS
           SET FAKSALG-EOF-OFF             TO TRUE
           SET FAKSALG-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FAKSALG-MC
                                              FAKSALG-MP
           OPEN INPUT FAKSALG
           SET FRAKTF-LEVEL-INIT           TO TRUE
           INITIALIZE FRAKTF-DATA-FIELDS
           SET FRAKTF-EOF-OFF              TO TRUE
           SET FRAKTF-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FRAKTF-MC
                                              FRAKTF-MP
           OPEN INPUT FRAKTF
           INITIALIZE PAR-DATA-FIELDS
           SET PAR-EOF-OFF                 TO TRUE
           SET PAR-PROCESS                 TO TRUE
           OPEN INPUT PAR
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT FSDATA.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKSALG
           CLOSE FRAKTF
           CLOSE PAR
           CLOSE KUNDEMA
           CLOSE FSDATA.
 
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
