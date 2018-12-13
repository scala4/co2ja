       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS072R.
      **********************************************  Z-WIN-RPG2   ****
      * DANNE RECORD FRA FAK.VAREREC HJLPEFILE OG ORDRE.HIST.MASTER   *
      *       -------------------------------------------------       *
      *  DETTE ER MÅNEDLIGE VARE.STAT.RECORDS FRA VARESETT.           *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS072.rpg
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
           SELECT FAKVARH
               ASSIGN TO UT-S-FAKVARH
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKVARH-STATUS.
           SELECT ORDHIST
               ASSIGN TO ORDHIST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDHIST-STATUS
               RECORD KEY IS ORDHIST-KEY1.
           SELECT VARSTVS
               ASSIGN TO UT-S-VARSTVS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTVS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKVARH
               BLOCK CONTAINS 62
               RECORD CONTAINS 31.
       01  FAKVARH-IO-AREA.
           05  FAKVARH-IO-AREA-X           PICTURE X(31).
       FD ORDHIST
               RECORD CONTAINS 164.
       01  ORDHIST-IO-AREA.
           05  ORDHIST-IO-AREA-X.
               10  ORDHIST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD VARSTVS
               BLOCK CONTAINS 100
               RECORD CONTAINS 50.
       01  VARSTVS-IO-AREA.
           05  VARSTVS-IO-AREA-X           PICTURE X(50).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKVARH-STATUS              PICTURE 99 VALUE 0.
           10  ORDHIST-STATUS              PICTURE 99 VALUE 0.
           10  VARSTVS-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARH-EOF-OFF         VALUE '0'.
               88  FAKVARH-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARH-READ-OFF        VALUE '0'.
               88  FAKVARH-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARH-PROCESS-OFF     VALUE '0'.
               88  FAKVARH-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKVARH-LEVEL-INIT-OFF  VALUE '0'.
               88  FAKVARH-LEVEL-INIT      VALUE '1'.
           05  ORDHIST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDHIST-EOF-OFF         VALUE '0'.
               88  ORDHIST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDHIST-READ-OFF        VALUE '0'.
               88  ORDHIST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDHIST-PROCESS-OFF     VALUE '0'.
               88  ORDHIST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDHIST-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDHIST-LEVEL-INIT      VALUE '1'.
           05  FAKVARH-LEVEL-09.
               10  FAKVARH-09-L3.
                   15  FAKVARH-09-L3-RA    PICTURE X(1).
               10  FAKVARH-09-L2.
                   15  FAKVARH-09-L2-FIRMA PICTURE X(3).
               10  FAKVARH-09-L1.
                   15  FAKVARH-09-L1-ORDNR PICTURE X(6).
           05  FAKVARH-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  FAKAAR                  PICTURE X(2).
               10  FAKMND                  PICTURE X(2).
               10  RA                      PICTURE X(1).
           05  FAKVARH-MP                  PICTURE X(10).
           05  FAKVARH-MC                  PICTURE X(10).
           05  FAKVARH-M-09            REDEFINES FAKVARH-MC.
               10  FAKVARH-M-09-M3.
                   15  FAKVARH-M-09-M3-RA-G.
                       20  FAKVARH-M-09-M3-RA PICTURE X(1).
               10  FAKVARH-M-09-M2.
                   15  FAKVARH-M-09-M2-FIRMA-G.
                       20  FAKVARH-M-09-M2-FIRMA PICTURE X(3).
               10  FAKVARH-M-09-M1.
                   15  FAKVARH-M-09-M1-ORDNR-G.
                       20  FAKVARH-M-09-M1-ORDNR PICTURE X(6).
           05  ORDHIST-LEVEL-01.
               10  ORDHIST-01-L3.
                   15  ORDHIST-01-L3-RA    PICTURE X(1).
               10  ORDHIST-01-L2.
                   15  ORDHIST-01-L2-FIRMA PICTURE X(3).
               10  ORDHIST-01-L1.
                   15  ORDHIST-01-L1-ORDNR PICTURE X(6).
           05  ORDHIST-LEVEL-04.
               10  ORDHIST-04-L3.
                   15  ORDHIST-04-L3-RA    PICTURE X(1).
               10  ORDHIST-04-L2.
                   15  ORDHIST-04-L2-FIRMA PICTURE X(3).
               10  ORDHIST-04-L1.
                   15  ORDHIST-04-L1-ORDNR PICTURE X(6).
           05  ORDHIST-DATA-FIELDS.
      *                                      21  21 KNR1
               10  LKODE                   PICTURE X(2).
      *                                      92  92 BK
               10  KRETYP                  PICTURE X(1).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDDAG                  PICTURE X(2).
      *                                     138 139 ORDMND
               10  ORDAAR                  PICTURE X(2).
               10  RUTID                   PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  POSNR                   PICTURE X(3).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALF                     PICTURE X(3).
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
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VARSET                  PICTURE X(1).
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           05  ORDHIST-MP                  PICTURE X(10).
           05  ORDHIST-MC                  PICTURE X(10).
           05  ORDHIST-M-01            REDEFINES ORDHIST-MC.
               10  ORDHIST-M-01-M3.
                   15  ORDHIST-M-01-M3-RA-G.
                       20  ORDHIST-M-01-M3-RA PICTURE X(1).
               10  ORDHIST-M-01-M2.
                   15  ORDHIST-M-01-M2-FIRMA-G.
                       20  ORDHIST-M-01-M2-FIRMA PICTURE X(3).
               10  ORDHIST-M-01-M1.
                   15  ORDHIST-M-01-M1-ORDNR-G.
                       20  ORDHIST-M-01-M1-ORDNR PICTURE X(6).
           05  ORDHIST-M-04            REDEFINES ORDHIST-MC.
               10  ORDHIST-M-04-M3.
                   15  ORDHIST-M-04-M3-RA-G.
                       20  ORDHIST-M-04-M3-RA PICTURE X(1).
               10  ORDHIST-M-04-M2.
                   15  ORDHIST-M-04-M2-FIRMA-G.
                       20  ORDHIST-M-04-M2-FIRMA PICTURE X(3).
               10  ORDHIST-M-04-M1.
                   15  ORDHIST-M-04-M1-ORDNR-G.
                       20  ORDHIST-M-04-M1-ORDNR PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  LAGERK                  PICTURE X(2).
               10  ORDAT2                  PICTURE X(6).
               10  ORDAT3-IO.
                   15  ORDAT3              PICTURE S9(6).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(2).
               10  NETSUM-IO.
                   15  NETSUM              PICTURE S9(7)V9(2).
               10  NETANT-IO.
                   15  NETANT              PICTURE S9(7)V9(2).
               10  PRITUT-IO.
                   15  PRITUT              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-50D                  PICTURE S9(5).
               10  XO-50U                  PICTURE 9(5).
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-09                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKVARH-PROCESS
               SET FAKVARH-PROCESS-OFF     TO TRUE
               SET FAKVARH-READ            TO TRUE
           END-IF
 
           IF  FAKVARH-READ
               PERFORM FAKVARH-GET
               SET FAKVARH-READ-OFF        TO TRUE
               IF  NOT FAKVARH-EOF
                   PERFORM FAKVARH-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDHIST-PROCESS
               SET ORDHIST-PROCESS-OFF     TO TRUE
               SET ORDHIST-READ            TO TRUE
           END-IF
 
           IF  ORDHIST-READ
               PERFORM ORDHIST-GET
               SET ORDHIST-READ-OFF        TO TRUE
               IF  NOT ORDHIST-EOF
                   PERFORM ORDHIST-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM ORDHIST-MATCH-SET
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
 
           IF  FAKVARH-PROCESS
               PERFORM FAKVARH-IDSET
           END-IF
 
           IF  ORDHIST-PROCESS
               PERFORM ORDHIST-IDSET
           END-IF
 
           IF  FAKVARH-PROCESS
               PERFORM FAKVARH-CHK-LEVEL
           END-IF
 
           IF  ORDHIST-PROCESS
               PERFORM ORDHIST-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FAKVARH-PROCESS
               PERFORM FAKVARH-FLDSET
           END-IF
 
           IF  ORDHIST-PROCESS
               PERFORM ORDHIST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKVARH-PROCESS
           OR  ORDHIST-PROCESS
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
           SET NOT-I-51                    TO TRUE
           IF  (I-09)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND NOT-I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-52                TO TRUE
               IF  STATUS-X = 'C'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-52)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-53                TO TRUE
               IF  RA = 'O'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-53)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-04 AND NOT-I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-04 AND NOT-I-52)
               GO TO SLUTT-T
           END-IF
           IF  (I-04 AND NOT-I-53)
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               SET NOT-I-51                TO TRUE
               IF  VARSET = 'V'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  VARSET = 'W'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-51)
               GO TO SLUTT-T
      *****************************************************************
      *  01      FIRMA     COMP "633"                    29  GIDSKEN
           END-IF
           IF  (I-01)
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '915'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-31)
               SET NOT-I-31                TO TRUE
               IF  LKODE = '15'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '626'
                   SET I-32                TO TRUE
               END-IF
      *  01N32   FIRMA     COMP "938"                    32  C&B
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '627'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '732'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '975'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  LKODE = '13'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-33                TO TRUE
               IF  FIRMA = '956'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-33)
               SET NOT-I-34                TO TRUE
               IF  LKODE = '13'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-33 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  LKODE = '15'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               PERFORM INRUT1-S
           END-IF
           IF  (I-04)
               PERFORM INRUT4-S
           END-IF
           IF  (I-04)
               PERFORM OSURUT-S
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *  SUBRUTINE FOR SETTING AV FELLES INDIKATORER PR. ORDRE.       *
      *****************************************************************
           CONTINUE.
 
       INRUT1-S SECTION.
       INRUT1-S-P.
           IF  (I-33 AND I-34)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'VEN'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'GAB'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'TIL'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND I-35)
               SET NOT-I-34                TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  RUTID = 'K'
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               SET NOT-I-22                TO TRUE
               IF  KRETYP = '2'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           MOVE LKODE                      TO LAGERK
      ***** RUTINE FOR Å SNU ORDREDATO TIL ÅR MND DAG       ******
           MOVE ORDATO                     TO ORDAT2
           MOVE ORDAAR                     TO ORDAT2 (1:2)
           MOVE ORDDAG                     TO ORDAT2 (5:2)
           MOVE ORDAT2                     TO ORDAT3-IO.
      *****************************************************************
      *  SUBRUTINE FOR SETTING AV FELLES INDIKATORER PR. VARELINJE.   *
      *****************************************************************
 
       INRUT4-S SECTION.
       INRUT4-S-P.
           SET NOT-I-24                    TO TRUE
           IF  EDBNR = 0000000
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  ANTLEV > 0,00
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  PRITIL > 0,00
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  ANTBES > 0,00
               SET I-82                    TO TRUE
           END-IF.
      *****************************************************************
      *                SUBRUTINE FOR ORDRESUMMERING.                  *
      *****************************************************************
 
       OSURUT-S SECTION.
       OSURUT-S-P.
           IF  (I-24)
               GO TO ENDOSU-T
           END-IF
           IF  (I-23 AND NOT-I-22)
               GO TO ENDOSU-T
           END-IF
           IF  (NOT-I-28)
               GO TO ENDOSU-T
      ****** SUMMERING AV ORDRELINJE TIL ORDRE PRIS/RABATT.
      *  SNU BELØP DERSOM KREDIT-GEBYR ELLER ANNEN RETUR.
           END-IF
           IF  (I-82)
               MULTIPLY ANTLEV BY ORPRIS GIVING NETTO ROUNDED
           END-IF
           IF  (NOT-I-82)
               ADD ORPRIS TO ZERO      GIVING NETTO
           END-IF
           MULTIPLY ORRAB1 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY ORRAB2 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY ORRAB3 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           IF  (NOT-I-23)
               ADD NETTO TO ZERO       GIVING NETSUM
               ADD ANTLEV TO ZERO      GIVING NETANT
           END-IF
           IF  (I-23)
               MULTIPLY -1 BY NETTO    GIVING NETSUM ROUNDED
               MULTIPLY -1 BY ANTLEV   GIVING NETANT ROUNDED
      ****** LEGG TIL NETTO PRISTILLEGG (FRAKTTILLEGG/PANT)  *****
           END-IF
           IF  (I-59)
               MULTIPLY ANTLEV BY PRITIL GIVING NETTO ROUNDED
           END-IF
           IF  (I-59 AND NOT-I-23)
               ADD NETTO TO ZERO       GIVING PRITUT
           END-IF
           IF  (I-59 AND I-23)
               SUBTRACT NETTO FROM ZERO GIVING PRITUT
           END-IF
           IF  (I-59)
               ADD PRITUT                  TO NETSUM
           END-IF
           SET I-50                        TO TRUE.
 
       ENDOSU-T.
           CONTINUE.
 
       FAKVARH-GET SECTION.
       FAKVARH-GET-P.
           IF  FAKVARH-EOF-OFF
               READ FAKVARH
               AT END
                   SET FAKVARH-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKVARH-FLDSET SECTION.
       FAKVARH-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKVARH-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE FAKVARH-IO-AREA (4:6)  TO ORDNR (1:6)
               MOVE FAKVARH-IO-AREA (10:2) TO FAKAAR (1:2)
               MOVE FAKVARH-IO-AREA (12:2) TO FAKMND (1:2)
               MOVE FAKVARH-IO-AREA (31:1) TO RA (1:1)
           END-EVALUATE.
 
       FAKVARH-IDSET SECTION.
       FAKVARH-IDSET-P.
           SET I-09                        TO TRUE.
 
       FAKVARH-CHK-LEVEL SECTION.
       FAKVARH-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKVARH-LEVEL-09
               MOVE FAKVARH-IO-AREA (31:1) TO FAKVARH-09-L3-RA
               MOVE FAKVARH-IO-AREA (1:3)  TO FAKVARH-09-L2-FIRMA
               MOVE FAKVARH-IO-AREA (4:6)  TO FAKVARH-09-L1-ORDNR
               IF  FAKVARH-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKVARH-09-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKVARH-09-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKVARH-09-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKVARH-09-L3         TO THE-PRIOR-L3
               MOVE  FAKVARH-09-L2         TO THE-PRIOR-L2
               MOVE  FAKVARH-09-L1         TO THE-PRIOR-L1
               SET FAKVARH-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FAKVARH-MATCH-SET SECTION.
       FAKVARH-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKVARH-IO-AREA (31:1) TO FAKVARH-M-09-M3-RA
               MOVE FAKVARH-IO-AREA (1:3)  TO FAKVARH-M-09-M2-FIRMA
               MOVE FAKVARH-IO-AREA (4:6)  TO FAKVARH-M-09-M1-ORDNR
           END-EVALUATE.
 
       ORDHIST-GET SECTION.
       ORDHIST-GET-P.
           IF  ORDHIST-EOF-OFF
               READ ORDHIST
               AT END
                   SET ORDHIST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDHIST-FLDSET SECTION.
       ORDHIST-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) = '1' )
               MOVE ORDHIST-IO-AREA (1:1)  TO RA (1:1)
               MOVE ORDHIST-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDHIST-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDHIST-IO-AREA (90:2) TO LKODE (1:2)
               MOVE ORDHIST-IO-AREA (99:1) TO KRETYP (1:1)
               MOVE ORDHIST-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDHIST-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDHIST-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDHIST-IO-AREA (157:1) TO RUTID (1:1)
               MOVE ORDHIST-IO-AREA (164:1) TO STATUS-X (1:1)
           WHEN ( ORDHIST-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDHIST-IO-AREA (1:1)  TO RA (1:1)
               MOVE ORDHIST-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDHIST-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDHIST-IO-AREA (17:3) TO POSNR (1:3)
               MOVE ORDHIST-IO-AREA (21:4) TO ANTBES-IO
               MOVE ORDHIST-IO-AREA (29:4) TO ANTLEV-IO
               MOVE ORDHIST-IO-AREA (34:3) TO ALF (1:3)
               MOVE ORDHIST-IO-AREA (87:4) TO EDBNR-IO
               MOVE ORDHIST-IO-AREA (91:3) TO VGR-IO
               MOVE ORDHIST-IO-AREA (94:5) TO ORPRIS-IO
               MOVE ORDHIST-IO-AREA (99:2) TO ORRAB1-IO
               MOVE ORDHIST-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDHIST-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDHIST-IO-AREA (126:4) TO PRITIL-IO
               MOVE ORDHIST-IO-AREA (135:1) TO VARSET (1:1)
           END-EVALUATE.
 
       ORDHIST-IDCHK SECTION.
       ORDHIST-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) = '1' )
             OR ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) NOT = '1' )
             OR ( ORDHIST-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDHIST-IDSET SECTION.
       ORDHIST-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) NOT = '1' )
               SET I-02                    TO TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDHIST-CHK-LEVEL SECTION.
       ORDHIST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDHIST-LEVEL-01
               MOVE ORDHIST-IO-AREA (1:1)  TO ORDHIST-01-L3-RA
               MOVE ORDHIST-IO-AREA (2:3)  TO ORDHIST-01-L2-FIRMA
               MOVE ORDHIST-IO-AREA (5:6)  TO ORDHIST-01-L1-ORDNR
               IF  ORDHIST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDHIST-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  ORDHIST-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDHIST-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDHIST-01-L3         TO THE-PRIOR-L3
               MOVE  ORDHIST-01-L2         TO THE-PRIOR-L2
               MOVE  ORDHIST-01-L1         TO THE-PRIOR-L1
               SET ORDHIST-LEVEL-INIT      TO TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) NOT = '1' )
               CONTINUE
           WHEN ( ORDHIST-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDHIST-LEVEL-04
               MOVE ORDHIST-IO-AREA (1:1)  TO ORDHIST-04-L3-RA
               MOVE ORDHIST-IO-AREA (2:3)  TO ORDHIST-04-L2-FIRMA
               MOVE ORDHIST-IO-AREA (5:6)  TO ORDHIST-04-L1-ORDNR
               IF  ORDHIST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDHIST-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  ORDHIST-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDHIST-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDHIST-04-L3         TO THE-PRIOR-L3
               MOVE  ORDHIST-04-L2         TO THE-PRIOR-L2
               MOVE  ORDHIST-04-L1         TO THE-PRIOR-L1
               SET ORDHIST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       ORDHIST-MATCH-SET SECTION.
       ORDHIST-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) = '1' )
               MOVE ORDHIST-IO-AREA (1:1)  TO ORDHIST-M-01-M3-RA
               MOVE ORDHIST-IO-AREA (2:3)  TO ORDHIST-M-01-M2-FIRMA
               MOVE ORDHIST-IO-AREA (5:6)  TO ORDHIST-M-01-M1-ORDNR
           WHEN ( ORDHIST-IO-AREA (19:1) = ' '
            AND   ORDHIST-IO-AREA (20:1) NOT = '1' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( ORDHIST-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDHIST-IO-AREA (1:1)  TO ORDHIST-M-04-M3-RA
               MOVE ORDHIST-IO-AREA (2:3)  TO ORDHIST-M-04-M2-FIRMA
               MOVE ORDHIST-IO-AREA (5:6)  TO ORDHIST-M-04-M1-ORDNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FAKVARH-EOF
               MOVE HIGH-VALUES            TO FAKVARH-MC
                                              FAKVARH-MP
           END-IF
           IF  ORDHIST-EOF
               MOVE HIGH-VALUES            TO ORDHIST-MC
                                              ORDHIST-MP
           END-IF
           IF  FAKVARH-MC < FAKVARH-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDHIST-MC < ORDHIST-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FAKVARH-MC < ORDHIST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKVARH-PROCESS     TO TRUE
                   MOVE FAKVARH-MC         TO FAKVARH-MP
                   IF  FAKVARH-MC = ORDHIST-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDHIST-MC < FAKVARH-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDHIST-PROCESS     TO TRUE
                   MOVE ORDHIST-MC         TO ORDHIST-MP
                   IF  ORDHIST-MC = FAKVARH-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKVARH-MC = ORDHIST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKVARH-PROCESS     TO TRUE
                   MOVE FAKVARH-MC         TO FAKVARH-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-04 AND I-50 AND I-51)
               MOVE SPACES TO VARSTVS-IO-AREA
               INITIALIZE VARSTVS-IO-AREA
               MOVE 'E'                    TO VARSTVS-IO-AREA (1:1)
               MOVE FIRMA                  TO VARSTVS-IO-AREA (2:3)
      *                   U1 29           4 "608"
               MOVE EDBNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO VARSTVS-IO-AREA (5:7)
               MOVE '00'                   TO VARSTVS-IO-AREA (12:2)
               IF  (I-31)
                   MOVE LAGERK             TO VARSTVS-IO-AREA (12:2)
               END-IF
               IF  (I-32)
                   MOVE LAGERK             TO VARSTVS-IO-AREA (12:2)
               END-IF
               IF  (I-33 AND I-34)
                   MOVE LAGERK             TO VARSTVS-IO-AREA (12:2)
               END-IF
               MOVE ALF                    TO VARSTVS-IO-AREA (14:3)
               MOVE VGR                    TO XO-50U
               MOVE XO-50U (1:5)           TO VARSTVS-IO-AREA (17:5)
               MOVE ORDAT3                 TO XO-60P
               MOVE XO-60P-EF              TO VARSTVS-IO-AREA (22:4)
               MOVE FAKAAR                 TO VARSTVS-IO-AREA (26:2)
               MOVE FAKMND                 TO VARSTVS-IO-AREA (28:2)
               MOVE VARSET                 TO VARSTVS-IO-AREA (30:1)
               MOVE NETANT                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTVS-IO-AREA (31:5)
               MOVE NETSUM                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTVS-IO-AREA (36:5)
               MOVE ORDNR                  TO VARSTVS-IO-AREA (41:6)
               MOVE POSNR                  TO VARSTVS-IO-AREA (47:3)
               WRITE VARSTVS-IO-AREA
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
           SET FAKVARH-LEVEL-INIT          TO TRUE
           INITIALIZE FAKVARH-DATA-FIELDS
           SET FAKVARH-EOF-OFF             TO TRUE
           SET FAKVARH-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FAKVARH-MC
                                              FAKVARH-MP
           OPEN INPUT FAKVARH
           SET ORDHIST-LEVEL-INIT          TO TRUE
           INITIALIZE ORDHIST-DATA-FIELDS
           SET ORDHIST-EOF-OFF             TO TRUE
           SET ORDHIST-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO ORDHIST-MC
                                              ORDHIST-MP
           OPEN INPUT ORDHIST
           OPEN OUTPUT VARSTVS.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKVARH
           CLOSE ORDHIST
           CLOSE VARSTVS.
 
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
