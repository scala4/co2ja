       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD724R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM ORD724 AV ESPEN LARSEN 28/09-2005                     *
      * LESER SEQ ORDREFILE OG DANNER EN FILE PÅ ALLE FORHÅNDSORDRE.  *
      * DENNE SKAL OPPDATERES SOM ANT.REST I VARE.MASTER.             *
      * DETTE UTFØRES I JOB VAR.XVAR60A DAGELIG.                      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD724.rpg
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
           SELECT ORDSEQ
               ASSIGN TO UT-S-ORDSEQ
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSEQ-STATUS.
           SELECT ORDSELR
               ASSIGN TO UT-S-ORDSELR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSELR-STATUS.
           SELECT ORDSELV
               ASSIGN TO UT-S-ORDSELV
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSELV-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDSEQ
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDSEQ-IO-AREA.
           05  ORDSEQ-IO-AREA-X            PICTURE X(164).
       FD ORDSELR
               BLOCK CONTAINS 30
               RECORD CONTAINS 15.
       01  ORDSELR-IO-AREA.
           05  ORDSELR-IO-AREA-X           PICTURE X(15).
       FD ORDSELV
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  ORDSELV-IO-AREA.
           05  ORDSELV-IO-AREA-X           PICTURE X(30).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDSEQ-STATUS               PICTURE 99 VALUE 0.
           10  ORDSELR-STATUS              PICTURE 99 VALUE 0.
           10  ORDSELV-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
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
               10  POSNR                   PICTURE X(3).
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
               10  FEILPA                  PICTURE X(2).
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
               10  VARSET                  PICTURE X(1).
               10  KOSTAT                  PICTURE X(1).
               10  PRITYP                  PICTURE X(1).
               10  NET1                    PICTURE X(1).
               10  NETFAK                  PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  BESANT-IO.
                   15  BESANT              PICTURE S9(5)V9(2).
               10  ENRNUM-IO.
                   15  ENRNUM              PICTURE S9(7).
               10  ENR                     PICTURE X(7).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
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
           SET NOT-I-60                    TO TRUE
      *****************************************************************
      *  RUTINE FOR HVILKE ORDRE SOM SKAL SELEKTERES.                 *
      *****************************************************************
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               IF  BK = 'F'
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-51                TO TRUE
               IF  STATUS-X = 'F'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-50)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-51)
               GO TO SLUTT-T
      *****************************************************************
           END-IF
           IF  (NOT-I-04)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  EDBNR = 0000000
               SET I-61                    TO TRUE
           END-IF
           IF  (I-61)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  ALF = '***'
               SET I-61                    TO TRUE
           END-IF
           IF  (I-61)
               GO TO SLUTT-T
           END-IF
           ADD ANTBES TO ZERO          GIVING BESANT
           ADD EDBNR TO ZERO           GIVING ENRNUM
           MOVE ENRNUM                     TO ENR
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE ENR (7:1)                  TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO ENR (7:1)
           SET I-60                        TO TRUE
      *****************************************************************
      * TEST OM DET ER VARESETT.                                      *
      *****************************************************************
           SET NOT-I-52                    TO TRUE
           IF  VARSET = 'V'
               SET I-52                    TO TRUE
           END-IF
           IF  (NOT-I-52)
               SET NOT-I-52                TO TRUE
               IF  VARSET = 'W'
                   SET I-52                TO TRUE
               END-IF
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * VANLIGE VARELINJER.                                           *
      *****************************************************************
           CONTINUE.
 
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
               MOVE ORDSEQ-IO-AREA (17:3)  TO POSNR (1:3)
               MOVE ORDSEQ-IO-AREA (21:4)  TO ANTBES-IO
               MOVE ORDSEQ-IO-AREA (25:4)  TO ANTRES-IO
               MOVE ORDSEQ-IO-AREA (29:4)  TO ANTLEV-IO
               MOVE ORDSEQ-IO-AREA (33:1)  TO NOREST (1:1)
               MOVE ORDSEQ-IO-AREA (34:3)  TO ALF (1:3)
               MOVE ORDSEQ-IO-AREA (37:2)  TO FEILPA (1:2)
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
               MOVE ORDSEQ-IO-AREA (135:1) TO VARSET (1:1)
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-04 AND I-60)
               MOVE SPACES TO ORDSELR-IO-AREA
               INITIALIZE ORDSELR-IO-AREA
               MOVE FIRMA                  TO ORDSELR-IO-AREA (1:3)
               MOVE ENR                    TO ORDSELR-IO-AREA (4:7)
               MOVE BESANT                 TO XO-52P
               MOVE XO-52P-EF              TO ORDSELR-IO-AREA (11:4)
               MOVE AVD                    TO ORDSELR-IO-AREA (15:1)
      *****************************************************************
      * VARESETT.                                                     *
      *****************************************************************
               WRITE ORDSELR-IO-AREA
           END-IF
           IF  (I-04 AND I-60 AND I-52)
               MOVE SPACES TO ORDSELV-IO-AREA
               INITIALIZE ORDSELV-IO-AREA
               MOVE FIRMA                  TO ORDSELV-IO-AREA (1:3)
               MOVE ENR                    TO ORDSELV-IO-AREA (4:7)
               MOVE BESANT                 TO XO-52P
               MOVE XO-52P-EF              TO ORDSELV-IO-AREA (11:4)
               MOVE AVD                    TO ORDSELV-IO-AREA (15:1)
               MOVE VARSET                 TO ORDSELV-IO-AREA (16:1)
               MOVE ORDNR                  TO ORDSELV-IO-AREA (17:6)
               MOVE POSNR                  TO ORDSELV-IO-AREA (23:3)
               WRITE ORDSELV-IO-AREA
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
           SET ORDSEQ-LEVEL-INIT           TO TRUE
           INITIALIZE ORDSEQ-DATA-FIELDS
           SET ORDSEQ-EOF-OFF              TO TRUE
           SET ORDSEQ-PROCESS              TO TRUE
           OPEN INPUT ORDSEQ
           OPEN OUTPUT ORDSELR
           OPEN OUTPUT ORDSELV.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDSEQ
           CLOSE ORDSELR
           CLOSE ORDSELV.
 
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
