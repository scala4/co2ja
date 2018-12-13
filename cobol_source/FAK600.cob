       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK600R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM FAK600 AV ESPEN LARSEN 11.06.2001                     *
      * DANNER NY FAKTURAFILE SOM BENYTTES TIL DATABASE OG            *
      * TIL OG DANNE NY FAKTURA.LIST.RECORD.                          *
      *      HENSYNTAR MOMS UTEN AVRUNDING OG EGET AVR.BELØP I MVA.REC*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK600.rpg
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
           SELECT ENRLIM
               ASSIGN TO ENRLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ENRLIM-STATUS.
           SELECT INFILE
               ASSIGN TO INFILE
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS INFILE-STATUS
               RECORD KEY IS INFILE-KEY1.
           SELECT FKREC
               ASSIGN TO UT-S-FKREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKREC-STATUS.
           SELECT FAREC
               ASSIGN TO UT-S-FAREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAREC-STATUS.
           SELECT FVREC
               ASSIGN TO UT-S-FVREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FVREC-STATUS.
           SELECT FKREC2
               ASSIGN TO UT-S-FKREC2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKREC2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ENRLIM
               RECORD CONTAINS 80.
       01  ENRLIM-IO-AREA.
           05  ENRLIM-IO-AREA-X.
               10  ENRLIM-KEY1.
                   15  ENRLIM-KEY1N        PICTURE S9(11).
               10  FILLER                  PICTURE X(69).
      * RMAF  IC  F    1000R 3A        KSDS
       FD INFILE
               RECORD CONTAINS 146.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X.
               10  INFILE-KEY1             PICTURE X(11).
               10  FILLER                  PICTURE X(135).
       FD FKREC
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FKREC-IO-AREA.
           05  FKREC-IO-AREA-X             PICTURE X(200).
       FD FAREC
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FAREC-IO-AREA.
           05  FAREC-IO-AREA-X             PICTURE X(150).
       FD FVREC
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FVREC-IO-AREA.
           05  FVREC-IO-AREA-X             PICTURE X(150).
       FD FKREC2
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  FKREC2-IO-AREA.
           05  FKREC2-IO-AREA-X            PICTURE X(40).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ENRLIM-STATUS               PICTURE 99 VALUE 0.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  FKREC-STATUS                PICTURE 99 VALUE 0.
           10  FAREC-STATUS                PICTURE 99 VALUE 0.
           10  FVREC-STATUS                PICTURE 99 VALUE 0.
           10  FKREC2-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ENRLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-EOF-OFF          VALUE '0'.
               88  ENRLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-READ-OFF         VALUE '0'.
               88  ENRLIM-READ             VALUE '1'.
           05  ENRLIM-LOW-KEY              PICTURE X(11).
           05  ENRLIM-HIGH-KEY             PICTURE X(11).
           05  INFILE-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
           05  INFILE-LEVEL-01.
               10  INFILE-01-L2.
                   15  INFILE-01-L2-FIRMNR PICTURE X(3).
               10  INFILE-01-L1.
                   15  INFILE-01-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-02.
               10  INFILE-02-L2.
                   15  INFILE-02-L2-FIRMNR PICTURE X(3).
               10  INFILE-02-L1.
                   15  INFILE-02-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-03.
               10  INFILE-03-L2.
                   15  INFILE-03-L2-FIRMNR PICTURE X(3).
               10  INFILE-03-L1.
                   15  INFILE-03-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-04.
               10  INFILE-04-L2.
                   15  INFILE-04-L2-FIRMNR PICTURE X(3).
               10  INFILE-04-L1.
                   15  INFILE-04-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-05.
               10  INFILE-05-L2.
                   15  INFILE-05-L2-FIRMNR PICTURE X(3).
               10  INFILE-05-L1.
                   15  INFILE-05-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-06.
               10  INFILE-06-L2.
                   15  INFILE-06-L2-FIRMNR PICTURE X(3).
               10  INFILE-06-L1.
                   15  INFILE-06-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-07.
               10  INFILE-07-L2.
                   15  INFILE-07-L2-FIRMNR PICTURE X(3).
               10  INFILE-07-L1.
                   15  INFILE-07-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-08.
               10  INFILE-08-L2.
                   15  INFILE-08-L2-FIRMNR PICTURE X(3).
               10  INFILE-08-L1.
                   15  INFILE-08-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-22.
               10  INFILE-22-L2.
                   15  INFILE-22-L2-FIRMNR PICTURE X(3).
               10  INFILE-22-L1.
                   15  INFILE-22-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-23.
               10  INFILE-23-L2.
                   15  INFILE-23-L2-FIRMNR PICTURE X(3).
               10  INFILE-23-L1.
                   15  INFILE-23-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-25.
               10  INFILE-25-L2.
                   15  INFILE-25-L2-FIRMNR PICTURE X(3).
               10  INFILE-25-L1.
                   15  INFILE-25-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-26.
               10  INFILE-26-L2.
                   15  INFILE-26-L2-FIRMNR PICTURE X(3).
               10  INFILE-26-L1.
                   15  INFILE-26-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-27.
               10  INFILE-27-L2.
                   15  INFILE-27-L2-FIRMNR PICTURE X(3).
               10  INFILE-27-L1.
                   15  INFILE-27-L1-BRNR   PICTURE X(6).
           05  INFILE-LEVEL-28.
               10  INFILE-28-L2.
                   15  INFILE-28-L2-FIRMNR PICTURE X(3).
               10  INFILE-28-L1.
                   15  INFILE-28-L1-BRNR   PICTURE X(6).
           05  INFILE-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  BRNR                    PICTURE X(6).
               10  PADR1                   PICTURE X(30).
               10  VADR1                   PICTURE X(30).
               10  BTMA-ELGTE              PICTURE X(2).
      *                                     134 134 FAKRNR
               10  FLSIDE                  PICTURE X(1).
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * HEADINGLINJE 2                                                *
      *****************************************************************
               10  PADR2                   PICTURE X(30).
               10  VADR2                   PICTURE X(30).
               10  FANR1                   PICTURE X(6).
               10  FANR2                   PICTURE X(6).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * HEADINGLINJE 3                                                *
      *****************************************************************
               10  PADR3                   PICTURE X(30).
               10  VADR3                   PICTURE X(30).
      *                                     113 121 FTAKNR
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * HEADINGLINJE 4                                                *
      *****************************************************************
               10  PPNR                    PICTURE X(4).
               10  PPSTED                  PICTURE X(15).
      *                                      24  53 PADR4
               10  VADR4                   PICTURE X(19).
      *                                     113 125 EANLOC
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * HEADINGLINJE 5                                                *
      *****************************************************************
               10  KUNDNR                  PICTURE X(6).
               10  ORDNR                   PICTURE X(6).
      *                                      29  29 GJFAK
               10  KUNRE1                  PICTURE X(15).
               10  FAKREF                  PICTURE X(6).
               10  BETM                    PICTURE X(24).
      *                                      95  98 KIDTKS
               10  FAKKID                  PICTURE X(7).
      *                                     109 116 FFDTO
               10  FFDAG                   PICTURE X(2).
               10  FFMND                   PICTURE X(2).
               10  FFAAR                   PICTURE X(2).
      *                                     119 126 FAKDTO
               10  FAKDAG                  PICTURE X(2).
               10  FAKMND                  PICTURE X(2).
               10  FAKAAR                  PICTURE X(2).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * VARELINJE MED ORDRENR. OG KUNDEREF.                           *
      *****************************************************************
               10  ORDNR2                  PICTURE X(6).
      *                                      56  70 KREF2
               10  ORDREF                  PICTURE X(20).
               10  KUNRE2                  PICTURE X(30).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * VARELINJE MED TEKST.                                          *
      *****************************************************************
               10  TEKST1                  PICTURE X(20).
               10  TEKST2                  PICTURE X(30).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * VARELINJE MED VAREDATA.                                       *
      *****************************************************************
      *                                      14  21 ANTALT
               10  ANTHEL-IO.
                   15  ANTHEL              PICTURE S9(5).
               10  ANTDES-IO.
                   15  ANTDES              PICTURE S9(2).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
      *                                      76  85 ENHPRI
               10  ENHPH-IO.
                   15  ENHPH               PICTURE S9(7).
               10  ENHPD-IO.
                   15  ENHPD               PICTURE S9(2).
      *                                     100 113 RAB
               10  RAB1H-IO.
                   15  RAB1H               PICTURE S9(2).
               10  RAB1D-IO.
                   15  RAB1D               PICTURE S9(1).
               10  RAB2H-IO.
                   15  RAB2H               PICTURE S9(2).
               10  RAB2D-IO.
                   15  RAB2D               PICTURE S9(1).
               10  RAB3H-IO.
                   15  RAB3H               PICTURE S9(2).
               10  RAB3D-IO.
                   15  RAB3D               PICTURE S9(1).
               10  NETTO                   PICTURE X(5).
      *                                     116 127 NTOBEL
               10  NTOBH1-IO.
                   15  NTOBH1              PICTURE S9(4).
               10  NTOBH2-IO.
                   15  NTOBH2              PICTURE S9(3).
               10  NTOBD-IO.
                   15  NTOBD               PICTURE S9(2).
               10  SIGN-X                  PICTURE X(1).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * TOTALLINJE M/ STREKER                                         *
      *****************************************************************
      *****************************************************************
      * TOTALLINJE M/ SUM EKS. MOMS.                                  *
      *****************************************************************
               10  BELXH1-IO.
                   15  BELXH1              PICTURE S9(5).
               10  BELXH2-IO.
                   15  BELXH2              PICTURE S9(3).
               10  BELXD-IO.
                   15  BELXD               PICTURE S9(2).
               10  BELXS                   PICTURE X(1).
      *****************************************************************
      * TOTALLINJE M/ MOMS                                            *
      *****************************************************************
               10  AVRBH1-IO.
                   15  AVRBH1              PICTURE S9(3).
               10  AVRBD-IO.
                   15  AVRBD               PICTURE S9(2).
               10  AVRBS                   PICTURE X(1).
               10  MOMSH1-IO.
                   15  MOMSH1              PICTURE S9(5).
               10  MOMSH2-IO.
                   15  MOMSH2              PICTURE S9(3).
               10  MOMSD-IO.
                   15  MOMSD               PICTURE S9(2).
               10  MOMSS                   PICTURE X(1).
      *****************************************************************
      * TOTALLINJE M/ STREKER                                         *
      *****************************************************************
      *****************************************************************
      * TOTALLINJE M/ FAKTURATOTAL.                                   *
      *****************************************************************
               10  FAKTOT                  PICTURE X(13).
               10  FAKSH1-IO.
                   15  FAKSH1              PICTURE S9(5).
               10  FAKSH2-IO.
                   15  FAKSH2              PICTURE S9(3).
               10  FAKSD-IO.
                   15  FAKSD               PICTURE S9(2).
               10  FAKSS                   PICTURE X(1).
      *****************************************************************
      * TOTALLINJE M/ KJØP HITTIL I ÅR.                               *
      *****************************************************************
      *                                      13  13 AVDELI
               10  KJAAR                   PICTURE X(48).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * TOTALLINJE M/ INFORMASJONSTEKST 1 OG 2.                       *
      *****************************************************************
               10  INFO1A                  PICTURE X(50).
               10  INFO1B                  PICTURE X(50).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      *****************************************************************
      * VAREGRUPPE TOTALER TIL OLJESELSKAPER.                         *
      *****************************************************************
               10  INFO2A                  PICTURE X(39).
               10  INFO2B                  PICTURE X(10).
      *                                     134 134 FAKRNR
      *                                     136 136 RECKOD
      *                                     138 143 RESKNR
      * RMAF  EE  20
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
               10  FAKTNR                  PICTURE X(6).
               10  ORDRE                   PICTURE X(6).
               10  NULL7-IO.
                   15  NULL7               PICTURE S9(7).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(9).
               10  NULL3-IO.
                   15  NULL3               PICTURE S9(3).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(3).
               10  NULL6-IO.
                   15  NULL6               PICTURE S9(6).
               10  VLINNR-IO.
                   15  VLINNR              PICTURE S9(6).
               10  BELXM-IO.
                   15  BELXM               PICTURE S9(9)V9(2).
               10  MOMS-IO.
                   15  MOMS                PICTURE S9(9)V9(2).
               10  FAKSUM-IO.
                   15  FAKSUM              PICTURE S9(9)V9(2).
               10  KUNSUM-IO.
                   15  KUNSUM              PICTURE S9(9)V9(2).
               10  KUNSU2-IO.
                   15  KUNSU2              PICTURE S9(9)V9(2).
               10  AVRORE-IO.
                   15  AVRORE              PICTURE S9(3)V9(2).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  A2-IO.
                   15  A2                  PICTURE S9(2)V9(2).
               10  EHPRIS-IO.
                   15  EHPRIS              PICTURE S9(7)V9(2).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  A1-IO.
                   15  A1                  PICTURE S9(1)V9(1).
               10  NTOPRI-IO.
                   15  NTOPRI              PICTURE S9(7)V9(2).
               10  VLINNX-IO.
                   15  VLINNX              PICTURE S9(6).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-24                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-26                    TO TRUE
           SET NOT-I-27                    TO TRUE
           SET NOT-I-27                    TO TRUE
           SET NOT-I-28                    TO TRUE
           SET NOT-I-28                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   PERFORM INFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET INFILE-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDOFF
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
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
               SET NOT-I-58                TO TRUE
               IF  FIRMNR = '581'
                   SET I-58                TO TRUE
               END-IF
      *  L2N58   FIRMNR    COMP "670"                    58 = IKKE KID
           END-IF
           IF  (I-L2 AND NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  FIRMNR = '653'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  FIRMNR = '576'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  FIRMNR = '557'
                   SET I-58                TO TRUE
               END-IF
      *****************************************************************
      * NULLSTILLING VED NYTT BRUDDNR (NY FAKTURA) L1                 *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-35                TO TRUE
               MOVE '   '                  TO FIRMA
               MOVE '      '               TO KUNDE
               MOVE '      '               TO FAKTNR
               MOVE '      '               TO ORDRE
               MOVE 0                      TO NULL7
               MOVE 0                      TO NULL9
               MOVE 0                      TO NULL3
               MOVE 1                      TO ANTFAK
               MOVE 0                      TO NULL6
               MOVE 0                      TO VLINNR
               MOVE 0,00                   TO BELXM
               MOVE 0,00                   TO MOMS
               MOVE 0,00                   TO FAKSUM
               MOVE 0,00                   TO KUNSUM
               MOVE 0,00                   TO KUNSU2
               MOVE 0,00                   TO AVRORE
      *****************************************************************
      * BEHANDLING AV HEADING LINJER.                                 *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE FIRMNR                 TO FIRMA
           END-IF
           IF  (I-01 AND NOT-I-31)
               SET I-35                    TO TRUE
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-02)
               SET NOT-I-51                TO TRUE
               IF  FLSIDE = 'F'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  FIRMA = '936'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-51)
               MOVE FANR1                  TO FAKTNR
           END-IF
           IF  (I-02 AND NOT-I-51)
               MOVE FANR2                  TO FAKTNR
           END-IF
           IF  (I-02)
               SET NOT-I-99                TO TRUE
               IF  FAKTNR > '899999'
                   SET I-99                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-32)
               SET I-35                    TO TRUE
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-03 AND NOT-I-33)
               SET I-35                    TO TRUE
           END-IF
           IF  (I-03)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-04 AND NOT-I-34)
               SET I-35                    TO TRUE
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-05 AND I-99)
               SET NOT-I-88                TO TRUE
               IF  FAKREF > '100000'
                   SET I-88                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND I-99 AND I-88)
               SUBTRACT 1                  FROM ANTFAK
           END-IF
           IF  (I-05)
               MOVE KUNDNR                 TO KUNDE
               MOVE ORDNR                  TO ORDRE
               ADD 1                       TO VLINNR
               GO TO SLUTT-T
      *****************************************************************
      * VARELINJE RUTINE.                                             *
      *****************************************************************
           END-IF
           IF  (I-21)
               GO TO TOTLIN-T
           END-IF
           IF  (I-22)
               GO TO TOTLIN-T
           END-IF
           IF  (I-23)
               GO TO TOTLIN-T
           END-IF
           IF  (I-24)
               GO TO TOTLIN-T
           END-IF
           IF  (I-25)
               GO TO TOTLIN-T
           END-IF
           IF  (I-26)
               GO TO TOTLIN-T
           END-IF
           IF  (I-27)
               GO TO TOTLIN-T
           END-IF
           IF  (I-28)
               GO TO TOTLIN-T
           END-IF
           ADD 1                           TO VLINNR
           IF  (I-06)
               MOVE ORDNR2                 TO ORDRE
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-07)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-08)
               ADD ANTHEL TO ZERO      GIVING ANTLEV
               ADD ANTDES TO ZERO      GIVING A2
               DIVIDE A2 BY 100        GIVING A2
               ADD A2                      TO ANTLEV
               ADD ENHPH TO ZERO       GIVING EHPRIS
               ADD ENHPD TO ZERO       GIVING A2
               DIVIDE A2 BY 100        GIVING A2
               ADD A2                      TO EHPRIS
               MOVE 0,0                    TO RAB1
               MOVE 0,0                    TO RAB2
               MOVE 0,0                    TO RAB3
      *
           END-IF
           IF  (I-08)
               SET NOT-I-55                TO TRUE
               IF  NETTO = 'NETTO'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-08 AND NOT-I-55)
               ADD RAB1H TO ZERO       GIVING RAB1
               ADD RAB1D TO ZERO       GIVING A1
               DIVIDE A1 BY 10         GIVING A1
               ADD A1                      TO RAB1
               ADD RAB2H TO ZERO       GIVING RAB2
               ADD RAB2D TO ZERO       GIVING A1
               DIVIDE A1 BY 10         GIVING A1
               ADD A1                      TO RAB2
               ADD RAB3H TO ZERO       GIVING RAB3
               ADD RAB3D TO ZERO       GIVING A1
               DIVIDE A1 BY 10         GIVING A1
               ADD A1                      TO RAB3
      *
           END-IF
           IF  (I-08)
               ADD NTOBH1 TO ZERO      GIVING NTOPRI
               MULTIPLY 1000 BY NTOPRI GIVING NTOPRI
               ADD NTOBH2                  TO NTOPRI
               ADD NTOBD TO ZERO       GIVING A2
               DIVIDE A2 BY 100        GIVING A2
               ADD A2                      TO NTOPRI
               SET NOT-I-56                TO TRUE
               IF  SIGN-X = '-'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-08 AND I-56)
               MULTIPLY -1 BY NTOPRI   GIVING NTOPRI
           END-IF
           IF  (I-08)
               GO TO SLUTT-T
      *****************************************************************
      * TOTAL LINJE RUTINE.                                           *
      * INDIKATOR 14 = FAKTURA UTEN MVA.                              *
      *****************************************************************
           END-IF
           .
 
       TOTLIN-T.
           IF  (I-21)
               GO TO SLUTT-T
           END-IF
           IF  (I-24)
               GO TO SLUTT-T
           END-IF
           IF  (I-22)
               ADD BELXH1 TO ZERO      GIVING BELXM
               MULTIPLY 1000 BY BELXM  GIVING BELXM
               ADD BELXH2                  TO BELXM
               ADD BELXD TO ZERO       GIVING A2
               DIVIDE A2 BY 100        GIVING A2
               ADD A2                      TO BELXM
               SET NOT-I-56                TO TRUE
               IF  BELXS = '-'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND I-56)
               MULTIPLY -1 BY BELXM    GIVING BELXM
           END-IF
           IF  (I-22)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-23)
               ADD MOMSH1 TO ZERO      GIVING MOMS
               MULTIPLY 1000 BY MOMS   GIVING MOMS
               ADD MOMSH2                  TO MOMS
               ADD MOMSD TO ZERO       GIVING A2
               DIVIDE A2 BY 100        GIVING A2
               ADD A2                      TO MOMS
               SET NOT-I-59                TO TRUE
               IF  MOMS = 0,00
                   SET I-59                TO TRUE
               END-IF
               SET NOT-I-56                TO TRUE
               IF  MOMSS = '-'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-23 AND I-56)
               MULTIPLY -1 BY MOMS     GIVING MOMS
           END-IF
           IF  (I-23)
               ADD AVRBH1 TO ZERO      GIVING AVRORE
               ADD AVRBD TO ZERO       GIVING A2
               DIVIDE A2 BY 100        GIVING A2
               ADD A2                      TO AVRORE
               SET NOT-I-56                TO TRUE
               IF  AVRBS = '-'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-23 AND I-56)
               MULTIPLY -1 BY AVRORE   GIVING AVRORE
           END-IF
           IF  (I-23)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-25 AND NOT-I-14)
               ADD FAKSH1 TO ZERO      GIVING FAKSUM
           END-IF
           IF  (I-25 AND I-14)
               ADD BELXH1 TO ZERO      GIVING FAKSUM
           END-IF
           IF  (I-25)
               MULTIPLY 1000 BY FAKSUM GIVING FAKSUM
           END-IF
           IF  (I-25 AND NOT-I-14)
               ADD FAKSH2                  TO FAKSUM
           END-IF
           IF  (I-25 AND I-14)
               ADD BELXH2                  TO FAKSUM
           END-IF
           IF  (I-25 AND NOT-I-14)
               ADD FAKSD TO ZERO       GIVING A2
           END-IF
           IF  (I-25 AND I-14)
               ADD BELXD TO ZERO       GIVING A2
           END-IF
           IF  (I-25)
               DIVIDE A2 BY 100        GIVING A2
               ADD A2                      TO FAKSUM
           END-IF
           IF  (I-25 AND NOT-I-14)
               SET NOT-I-56                TO TRUE
               IF  FAKSS = '-'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-25 AND I-14)
               SET NOT-I-56                TO TRUE
               IF  BELXS = '-'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-25 AND I-56)
               MULTIPLY -1 BY FAKSUM   GIVING FAKSUM
           END-IF
           IF  (I-25 AND NOT-I-99)
               ADD FAKSUM                  TO KUNSUM
               ADD FAKSUM                  TO KUNSU2
           END-IF
           IF  (I-25 AND I-99)
               SUBTRACT FAKSUM             FROM KUNSUM
           END-IF
           IF  (I-25 AND I-99 AND NOT-I-88)
               SUBTRACT FAKSUM             FROM KUNSU2
           END-IF
           IF  (I-25 AND I-14)
               MOVE 0,00                   TO BELXM
               MOVE 0,00                   TO MOMS
               MOVE 0,00                   TO AVRORE
           END-IF
           IF  (I-25)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-26 AND NOT-I-13)
               ADD 1                       TO VLINNR
           END-IF
           IF  (I-26)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-27)
               ADD 1                       TO VLINNR
               ADD VLINNR TO ZERO      GIVING VLINNX
           END-IF
           IF  (I-27 AND NOT-I-11)
               ADD 1                       TO VLINNR
           END-IF
           IF  (I-27)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-28)
               ADD 1                       TO VLINNR
               ADD VLINNR TO ZERO      GIVING VLINNX
           END-IF.
 
       SLUTT-T.
      ********************************************************
      * FAKTURA POSTADRESSE RECORD (11)                               *
      *****************************************************************
           CONTINUE.
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ENRLIM-READ-OFF
                    OR ENRLIM-EOF
                   IF  ENRLIM-READ
                       SET ENRLIM-READ-OFF TO TRUE
                       READ ENRLIM
                       AT END
                           SET ENRLIM-EOF  TO TRUE
                           SET INFILE-EOF  TO TRUE
                       NOT AT END
                           MOVE ENRLIM-IO-AREA (1:4) TO INFILE-KEY1
                       END-READ
                   END-IF
                   IF  ENRLIM-EOF-OFF
                   AND ENRLIM-READ-OFF
                       READ INFILE
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       INFILE-FLDOFF SECTION.
       INFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '1' )
               SET NOT-I-31                TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '2' )
               SET NOT-I-32                TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '3' )
               SET NOT-I-33                TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '4' )
               SET NOT-I-34                TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '5' )
               SET NOT-I-36                TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '5' )
               SET NOT-I-14                TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '6' )
               SET NOT-I-13                TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '7' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '8' )
               SET NOT-I-11                TO TRUE
           END-EVALUATE.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '1' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (24:30) TO PADR1 (1:30)
               MOVE INFILE-IO-AREA (64:30) TO VADR1 (1:30)
               IF  VADR1 = SPACES
                   SET I-31                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (126:2) TO BTMA-ELGTE (1:2)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '2' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (24:30) TO PADR2 (1:30)
               MOVE INFILE-IO-AREA (64:30) TO VADR2 (1:30)
               IF  VADR2 = SPACES
                   SET I-32                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (113:6) TO FANR1 (1:6)
               MOVE INFILE-IO-AREA (116:6) TO FANR2 (1:6)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '3' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (24:30) TO PADR3 (1:30)
               MOVE INFILE-IO-AREA (64:30) TO VADR3 (1:30)
               IF  VADR3 = SPACES
                   SET I-33                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '4' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (24:4)  TO PPNR (1:4)
               MOVE INFILE-IO-AREA (29:15) TO PPSTED (1:15)
               MOVE INFILE-IO-AREA (64:19) TO VADR4 (1:19)
               IF  VADR4 = SPACES
                   SET I-34                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '5' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (15:6)  TO KUNDNR (1:6)
               MOVE INFILE-IO-AREA (23:6)  TO ORDNR (1:6)
               MOVE INFILE-IO-AREA (31:15) TO KUNRE1 (1:15)
               IF  KUNRE1 = SPACES
                   SET I-36                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (62:6)  TO FAKREF (1:6)
               MOVE INFILE-IO-AREA (70:24) TO BETM (1:24)
               MOVE INFILE-IO-AREA (99:7)  TO FAKKID (1:7)
               MOVE INFILE-IO-AREA (109:2) TO FFDAG (1:2)
               MOVE INFILE-IO-AREA (112:2) TO FFMND (1:2)
               MOVE INFILE-IO-AREA (115:2) TO FFAAR (1:2)
               MOVE INFILE-IO-AREA (119:2) TO FAKDAG (1:2)
               MOVE INFILE-IO-AREA (122:2) TO FAKMND (1:2)
               MOVE INFILE-IO-AREA (125:2) TO FAKAAR (1:2)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '6' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (33:6)  TO ORDNR2 (1:6)
               MOVE INFILE-IO-AREA (24:20) TO ORDREF (1:20)
               MOVE INFILE-IO-AREA (45:30) TO KUNRE2 (1:30)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '7' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (25:20) TO TEKST1 (1:20)
               MOVE INFILE-IO-AREA (45:30) TO TEKST2 (1:30)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '8' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (14:5)  TO ANTHEL-IO
               INSPECT ANTHEL-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (20:2)  TO ANTDES-IO
               INSPECT ANTDES-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (24:20) TO ARTNR (1:20)
               MOVE INFILE-IO-AREA (45:30) TO VARBET (1:30)
               MOVE INFILE-IO-AREA (76:7)  TO ENHPH-IO
               INSPECT ENHPH-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (84:2)  TO ENHPD-IO
               INSPECT ENHPD-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (100:2) TO RAB1H-IO
               INSPECT RAB1H-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (103:1) TO RAB1D-IO
               INSPECT RAB1D-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (105:2) TO RAB2H-IO
               INSPECT RAB2H-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (108:1) TO RAB2D-IO
               INSPECT RAB2D-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (110:2) TO RAB3H-IO
               INSPECT RAB3H-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (113:1) TO RAB3D-IO
               INSPECT RAB3D-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (100:5) TO NETTO (1:5)
               MOVE INFILE-IO-AREA (116:4) TO NTOBH1-IO
               INSPECT NTOBH1-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (121:3) TO NTOBH2-IO
               INSPECT NTOBH2-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (125:2) TO NTOBD-IO
               INSPECT NTOBD-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (127:1) TO SIGN-X (1:1)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '2' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (115:5) TO BELXH1-IO
               INSPECT BELXH1-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (121:3) TO BELXH2-IO
               INSPECT BELXH2-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (125:2) TO BELXD-IO
               INSPECT BELXD-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (127:1) TO BELXS (1:1)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '3' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (68:3)  TO AVRBH1-IO
               INSPECT AVRBH1-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (72:2)  TO AVRBD-IO
               INSPECT AVRBD-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (74:1)  TO AVRBS (1:1)
               MOVE INFILE-IO-AREA (115:5) TO MOMSH1-IO
               INSPECT MOMSH1-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (121:3) TO MOMSH2-IO
               INSPECT MOMSH2-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (125:2) TO MOMSD-IO
               INSPECT MOMSD-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (127:1) TO MOMSS (1:1)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '5' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (115:13) TO FAKTOT (1:13)
               IF  FAKTOT = SPACES
                   SET I-14                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (115:5) TO FAKSH1-IO
               INSPECT FAKSH1-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (121:3) TO FAKSH2-IO
               INSPECT FAKSH2-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (125:2) TO FAKSD-IO
               INSPECT FAKSD-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (127:1) TO FAKSS (1:1)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '6' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (44:48) TO KJAAR (1:48)
               IF  KJAAR = SPACES
                   SET I-13                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '7' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '8' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (24:50) TO INFO1A (1:50)
               MOVE INFILE-IO-AREA (74:50) TO INFO1B (1:50)
               IF  INFO1B = SPACES
                   SET I-11                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           WHEN ( INFILE-IO-AREA (10:1) = '1' )
             OR ( INFILE-IO-AREA (10:1) = '2' )
               MOVE INFILE-IO-AREA (144:3) TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (128:6) TO BRNR (1:6)
               MOVE INFILE-IO-AREA (31:39) TO INFO2A (1:39)
               MOVE INFILE-IO-AREA (76:10) TO INFO2B (1:10)
               MOVE INFILE-IO-AREA (135:1) TO FLSIDE (1:1)
           END-EVALUATE.
 
       INFILE-IDCHK SECTION.
       INFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '1' )
             OR ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '2' )
             OR ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '3' )
             OR ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '4' )
             OR ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '5' )
             OR ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '6' )
             OR ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '7' )
             OR ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '8' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '1' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '2' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '3' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '4' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '5' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '6' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '7' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '8' )
             OR ( INFILE-IO-AREA (10:1) = '1' )
             OR ( INFILE-IO-AREA (10:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '4' )
               SET I-04                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '5' )
               SET I-05                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '6' )
               SET I-06                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '7' )
               SET I-07                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '8' )
               SET I-08                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '1' )
               SET I-21                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '2' )
               SET I-22                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '3' )
               SET I-23                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '4' )
               SET I-24                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '5' )
               SET I-25                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '6' )
               SET I-26                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '7' )
               SET I-27                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '8' )
               SET I-27                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '1' )
               SET I-28                    TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '2' )
               SET I-28                    TO TRUE
           END-EVALUATE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '1' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (144:3) TO INFILE-01-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-01-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L2          TO THE-PRIOR-L2
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '2' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-02
               MOVE INFILE-IO-AREA (144:3) TO INFILE-02-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-02-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-02-L2          TO THE-PRIOR-L2
               MOVE  INFILE-02-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '3' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-03
               MOVE INFILE-IO-AREA (144:3) TO INFILE-03-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-03-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-03-L2          TO THE-PRIOR-L2
               MOVE  INFILE-03-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '4' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-04
               MOVE INFILE-IO-AREA (144:3) TO INFILE-04-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-04-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-04-L2          TO THE-PRIOR-L2
               MOVE  INFILE-04-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '5' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-05
               MOVE INFILE-IO-AREA (144:3) TO INFILE-05-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-05-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-05-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-05-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-05-L2          TO THE-PRIOR-L2
               MOVE  INFILE-05-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '6' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-06
               MOVE INFILE-IO-AREA (144:3) TO INFILE-06-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-06-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-06-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-06-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-06-L2          TO THE-PRIOR-L2
               MOVE  INFILE-06-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '7' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-07
               MOVE INFILE-IO-AREA (144:3) TO INFILE-07-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-07-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-07-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-07-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-07-L2          TO THE-PRIOR-L2
               MOVE  INFILE-07-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '0'
            AND   INFILE-IO-AREA (11:1) = '8' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-08
               MOVE INFILE-IO-AREA (144:3) TO INFILE-08-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-08-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-08-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-08-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-08-L2          TO THE-PRIOR-L2
               MOVE  INFILE-08-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '1' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '2' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-22
               MOVE INFILE-IO-AREA (144:3) TO INFILE-22-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-22-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-22-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-22-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-22-L2          TO THE-PRIOR-L2
               MOVE  INFILE-22-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '3' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-23
               MOVE INFILE-IO-AREA (144:3) TO INFILE-23-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-23-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-23-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-23-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-23-L2          TO THE-PRIOR-L2
               MOVE  INFILE-23-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '4' )
               CONTINUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '5' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-25
               MOVE INFILE-IO-AREA (144:3) TO INFILE-25-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-25-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-25-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-25-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-25-L2          TO THE-PRIOR-L2
               MOVE  INFILE-25-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '6' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-26
               MOVE INFILE-IO-AREA (144:3) TO INFILE-26-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-26-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-26-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-26-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-26-L2          TO THE-PRIOR-L2
               MOVE  INFILE-26-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '7' )
             OR ( INFILE-IO-AREA (10:1) = '3'
            AND   INFILE-IO-AREA (11:1) = '8' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-27
               MOVE INFILE-IO-AREA (144:3) TO INFILE-27-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-27-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-27-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-27-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-27-L2          TO THE-PRIOR-L2
               MOVE  INFILE-27-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           WHEN ( INFILE-IO-AREA (10:1) = '1' )
             OR ( INFILE-IO-AREA (10:1) = '2' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-28
               MOVE INFILE-IO-AREA (144:3) TO INFILE-28-L2-FIRMNR
               MOVE INFILE-IO-AREA (128:6) TO INFILE-28-L1-BRNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-28-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-28-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-28-L2          TO THE-PRIOR-L2
               MOVE  INFILE-28-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-05)
               MOVE SPACES TO FAREC-IO-AREA
               INITIALIZE FAREC-IO-AREA
               MOVE FIRMA                  TO FAREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FAREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FAREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FAREC-IO-AREA (16:6)
               MOVE '11'                   TO FAREC-IO-AREA (22:2)
               MOVE PADR1                  TO FAREC-IO-AREA (24:30)
               MOVE PADR2                  TO FAREC-IO-AREA (54:30)
               MOVE PADR3                  TO FAREC-IO-AREA (84:30)
               MOVE PPNR                   TO FAREC-IO-AREA (114:4)
               MOVE PPSTED                 TO FAREC-IO-AREA (118:15)
               MOVE BTMA-ELGTE             TO FAREC-IO-AREA (148:2)
               IF  (I-99 AND I-88)
                   MOVE 'N'                TO FAREC-IO-AREA (150:1)
      *****************************************************************
      * FAKTURA VAREADRESSE RECORD (12)                               *
      *****************************************************************
               END-IF
               WRITE FAREC-IO-AREA
           END-IF
           IF  (I-05 AND I-35)
               MOVE SPACES TO FAREC-IO-AREA
               INITIALIZE FAREC-IO-AREA
               MOVE FIRMA                  TO FAREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FAREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FAREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FAREC-IO-AREA (16:6)
               MOVE '12'                   TO FAREC-IO-AREA (22:2)
               MOVE VADR1                  TO FAREC-IO-AREA (24:30)
               MOVE VADR2                  TO FAREC-IO-AREA (54:30)
               MOVE VADR3                  TO FAREC-IO-AREA (84:30)
               MOVE VADR4                  TO FAREC-IO-AREA (114:19)
               MOVE BTMA-ELGTE             TO FAREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE KUNDEREF.LINJE (23)                         *
      *    DETTE ER 1. ORDRE PÅ FAKTURAEN OG DENNES KUNDEREF.         *
      *****************************************************************
               WRITE FAREC-IO-AREA
           END-IF
           IF  (I-05)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FVREC-IO-AREA (16:6)
               MOVE '23'                   TO FVREC-IO-AREA (22:2)
               MOVE NULL7-IO               TO FVREC-IO-AREA (24:7)
      *                                  38 "ORDRENR."
               MOVE 'VÅR O.NR'             TO FVREC-IO-AREA (31:8)
               MOVE ORDNR                  TO FVREC-IO-AREA (40:6)
               IF  (NOT-I-36)
                   MOVE 'DERES REF.'       TO FVREC-IO-AREA (51:10)
               END-IF
               IF  (NOT-I-36)
                   MOVE KUNRE1             TO FVREC-IO-AREA (62:15)
               END-IF
               IF  (I-99 AND I-88)
                   MOVE 'FAKTURAREF.NR.'   TO FVREC-IO-AREA (51:14)
               END-IF
               IF  (I-99 AND I-88)
                   MOVE ' '                TO FVREC-IO-AREA (65:1)
               END-IF
               IF  (I-99 AND I-88)
                   MOVE FAKREF             TO FVREC-IO-AREA (66:6)
               END-IF
               IF  (I-99 AND I-88)
                   MOVE '     '            TO FVREC-IO-AREA (72:5)
               END-IF
               MOVE NULL9-IO               TO FVREC-IO-AREA (81:9)
               MOVE NULL3-IO               TO FVREC-IO-AREA (90:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (93:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (96:3)
               MOVE NULL9-IO               TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE KUNDEREF.LINJE (23)                         *
      *    DETTE ER ETTERFØLGENE ORDRE PÅ FAKTURAEN OG DERES KUNDEREF.*
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF
           IF  (I-06)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FVREC-IO-AREA (16:6)
               MOVE '23'                   TO FVREC-IO-AREA (22:2)
               MOVE NULL7-IO               TO FVREC-IO-AREA (24:7)
               MOVE ORDREF                 TO FVREC-IO-AREA (31:20)
               MOVE KUNRE2                 TO FVREC-IO-AREA (51:30)
               MOVE NULL9-IO               TO FVREC-IO-AREA (81:9)
               MOVE NULL3-IO               TO FVREC-IO-AREA (90:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (93:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (96:3)
               MOVE NULL9-IO               TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE TEKSTLINJE (22)                             *
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF
           IF  (I-07)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FVREC-IO-AREA (16:6)
               MOVE '22'                   TO FVREC-IO-AREA (22:2)
               MOVE NULL7-IO               TO FVREC-IO-AREA (24:7)
               MOVE TEKST1                 TO FVREC-IO-AREA (31:20)
               MOVE TEKST2                 TO FVREC-IO-AREA (51:30)
               MOVE NULL9-IO               TO FVREC-IO-AREA (81:9)
               MOVE NULL3-IO               TO FVREC-IO-AREA (90:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (93:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (96:3)
               MOVE NULL9-IO               TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE (21)                                        *
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF
           IF  (I-08)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FVREC-IO-AREA (16:6)
               MOVE '21'                   TO FVREC-IO-AREA (22:2)
               MOVE ANTLEV-IO              TO FVREC-IO-AREA (24:7)
               MOVE ARTNR                  TO FVREC-IO-AREA (31:20)
               MOVE VARBET                 TO FVREC-IO-AREA (51:30)
               MOVE EHPRIS-IO              TO FVREC-IO-AREA (81:9)
               MOVE RAB1-IO                TO FVREC-IO-AREA (90:3)
               MOVE RAB2-IO                TO FVREC-IO-AREA (93:3)
               MOVE RAB3-IO                TO FVREC-IO-AREA (96:3)
               IF  (NOT-I-55)
                   MOVE ' '                TO FVREC-IO-AREA (99:1)
               END-IF
               IF  (I-55)
                   MOVE 'N'                TO FVREC-IO-AREA (99:1)
               END-IF
               MOVE NTOPRI-IO              TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE TEKSTLINJE (22, KJØP HITTIL I ÅR.           *
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF
           IF  (I-26 AND NOT-I-13)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FVREC-IO-AREA (16:6)
               MOVE '22'                   TO FVREC-IO-AREA (22:2)
               MOVE NULL7-IO               TO FVREC-IO-AREA (24:7)
               MOVE KJAAR                  TO FVREC-IO-AREA (31:48)
               MOVE NULL9-IO               TO FVREC-IO-AREA (81:9)
               MOVE NULL3-IO               TO FVREC-IO-AREA (90:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (93:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (96:3)
               MOVE NULL9-IO               TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE TEKSTLINJE (27, INFORMASJONLINJE 1 A        *
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF
           IF  (I-27)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNX-IO              TO FVREC-IO-AREA (16:6)
               MOVE '22'                   TO FVREC-IO-AREA (22:2)
               MOVE NULL7-IO               TO FVREC-IO-AREA (24:7)
               MOVE INFO1A                 TO FVREC-IO-AREA (31:50)
               MOVE NULL9-IO               TO FVREC-IO-AREA (81:9)
               MOVE NULL3-IO               TO FVREC-IO-AREA (90:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (93:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (96:3)
               MOVE NULL9-IO               TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE TEKSTLINJE (27, INFORMASJONLINJE 1 B        *
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF
           IF  (I-27 AND NOT-I-11)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNR-IO              TO FVREC-IO-AREA (16:6)
               MOVE '22'                   TO FVREC-IO-AREA (22:2)
               MOVE NULL7-IO               TO FVREC-IO-AREA (24:7)
               MOVE INFO1B                 TO FVREC-IO-AREA (31:50)
               MOVE NULL9-IO               TO FVREC-IO-AREA (81:9)
               MOVE NULL3-IO               TO FVREC-IO-AREA (90:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (93:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (96:3)
               MOVE NULL9-IO               TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA VARELINJE TEKSTLINJE (28, VAREGRUPPETOT.              *
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF
           IF  (I-28)
               MOVE SPACES TO FVREC-IO-AREA
               INITIALIZE FVREC-IO-AREA
               MOVE FIRMA                  TO FVREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FVREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FVREC-IO-AREA (10:6)
               MOVE VLINNX-IO              TO FVREC-IO-AREA (16:6)
               MOVE '22'                   TO FVREC-IO-AREA (22:2)
               MOVE NULL7-IO               TO FVREC-IO-AREA (24:7)
               MOVE INFO2A                 TO FVREC-IO-AREA (31:39)
               MOVE INFO2B                 TO FVREC-IO-AREA (71:10)
               MOVE NULL9-IO               TO FVREC-IO-AREA (81:9)
               MOVE NULL3-IO               TO FVREC-IO-AREA (90:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (93:3)
               MOVE NULL3-IO               TO FVREC-IO-AREA (96:3)
               MOVE NULL9-IO               TO FVREC-IO-AREA (100:9)
               MOVE ORDRE                  TO FVREC-IO-AREA (109:6)
               MOVE BTMA-ELGTE             TO FVREC-IO-AREA (148:2)
      *****************************************************************
      * FAKTURA KONTROLLREC 1   ( EN PR. FAKTURA)                     *
      *****************************************************************
               WRITE FVREC-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO FKREC-IO-AREA
               INITIALIZE FKREC-IO-AREA
               MOVE FIRMA                  TO FKREC-IO-AREA (1:3)
               MOVE KUNDE                  TO FKREC-IO-AREA (4:6)
               MOVE FAKTNR                 TO FKREC-IO-AREA (10:6)
               MOVE NULL6-IO               TO FKREC-IO-AREA (16:6)
               MOVE '01'                   TO FKREC-IO-AREA (22:2)
               MOVE VLINNR-IO              TO FKREC-IO-AREA (24:6)
      *                        ANTFAKX   32
               MOVE FAKDAG                 TO FKREC-IO-AREA (33:2)
               MOVE FAKMND                 TO FKREC-IO-AREA (35:2)
               MOVE '20'                   TO FKREC-IO-AREA (37:2)
               MOVE FAKAAR                 TO FKREC-IO-AREA (39:2)
               MOVE FFDAG                  TO FKREC-IO-AREA (41:2)
               MOVE FFMND                  TO FKREC-IO-AREA (43:2)
               MOVE '20'                   TO FKREC-IO-AREA (45:2)
               MOVE FFAAR                  TO FKREC-IO-AREA (47:2)
               MOVE BETM                   TO FKREC-IO-AREA (49:24)
               IF  (NOT-I-58)
                   MOVE FAKKID             TO FKREC-IO-AREA (73:7)
      *                        GIRO      80 "X"
      *                        FILTYP    81 "X"
               END-IF
               MOVE BELXM-IO               TO FKREC-IO-AREA (82:11)
               MOVE MOMS-IO                TO FKREC-IO-AREA (93:11)
               MOVE AVRORE-IO              TO FKREC-IO-AREA (104:5)
               MOVE FAKSUM-IO              TO FKREC-IO-AREA (109:11)
               IF  (I-59)
                   MOVE 'F'                TO FKREC-IO-AREA (120:1)
               END-IF
               IF  (NOT-I-59)
                   MOVE 'P'                TO FKREC-IO-AREA (120:1)
               END-IF
               MOVE KUNSUM-IO              TO FKREC-IO-AREA (126:11)
               MOVE BRNR                   TO FKREC-IO-AREA (137:6)
               MOVE ANTFAK-IO              TO FKREC-IO-AREA (143:3)
               MOVE BTMA-ELGTE             TO FKREC-IO-AREA (148:2)
               IF  (I-99 AND I-88)
                   MOVE 'N'                TO FKREC-IO-AREA (150:1)
      *****************************************************************
      * FAKTURA KONTROLLREC 2   ( EN PR. FAKTURA)                     *
      *****************************************************************
               END-IF
               WRITE FKREC-IO-AREA
           END-IF
           IF  (I-L1)
               MOVE SPACES TO FKREC2-IO-AREA
               INITIALIZE FKREC2-IO-AREA
               MOVE FIRMA                  TO FKREC2-IO-AREA (1:3)
               MOVE KUNDE                  TO FKREC2-IO-AREA (4:6)
               MOVE BTMA-ELGTE             TO FKREC2-IO-AREA (10:2)
               MOVE ANTFAK-IO              TO FKREC2-IO-AREA (12:3)
               MOVE KUNSU2-IO              TO FKREC2-IO-AREA (15:11)
               WRITE FKREC2-IO-AREA
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
           SET ENRLIM-EOF-OFF              TO TRUE
           SET ENRLIM-READ                 TO TRUE
           OPEN INPUT ENRLIM
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           OPEN OUTPUT FKREC
           OPEN OUTPUT FAREC
           OPEN OUTPUT FVREC
           OPEN OUTPUT FKREC2.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ENRLIM
           CLOSE INFILE
           CLOSE FKREC
           CLOSE FAREC
           CLOSE FVREC
           CLOSE FKREC2.
 
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
