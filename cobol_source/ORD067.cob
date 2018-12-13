       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD067R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD067.rpg
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
           SELECT ORDLIM
               ASSIGN TO ORDLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDLIM-STATUS.
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT OUTPUN
               ASSIGN TO UT-S-OUTPUN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDLIM
               RECORD CONTAINS 80.
       01  ORDLIM-IO-AREA.
           05  ORDLIM-IO-AREA-X.
               10  ORDLIM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(60).
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD OUTPUN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTPUN-IO-AREA.
           05  OUTPUN-IO-AREA-X            PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDLIM-STATUS               PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTPUN-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ORDLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDLIM-EOF-OFF          VALUE '0'.
               88  ORDLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDLIM-READ-OFF         VALUE '0'.
               88  ORDLIM-READ             VALUE '1'.
           05  ORDLIM-LOW-KEY              PICTURE X(20).
           05  ORDLIM-HIGH-KEY             PICTURE X(20).
           05  ORDREM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-EOF-OFF          VALUE '0'.
               88  ORDREM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-READ-OFF         VALUE '0'.
               88  ORDREM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-PROCESS-OFF      VALUE '0'.
               88  ORDREM-PROCESS          VALUE '1'.
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
           05  ORDREM-DATA-FIELDS.
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
               10  OHREC1                  PICTURE X(164).
               10  FAKREF                  PICTURE X(6).
               10  AVNAVN                  PICTURE X(11).
               10  OKKNR                   PICTURE X(6).
               10  REKVNR                  PICTURE X(15).
               10  FORSM                   PICTURE X(15).
               10  HND                     PICTURE X(3).
               10  KADR                    PICTURE X(30).
               10  POSTNR                  PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  OHREC2                  PICTURE X(164).
               10  VAADR1                  PICTURE X(30).
               10  VAADR2                  PICTURE X(30).
               10  VAADR3                  PICTURE X(30).
               10  VAADR4                  PICTURE X(20).
               10  OHREC3                  PICTURE X(164).
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
               10  OVREC                   PICTURE X(164).
           05  TEMPORARY-FIELDS.
               10  ANTTOT-IO.
                   15  ANTTOT              PICTURE S9(6).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-52YY9R               PICTURE ZZ.ZZZ,99-.
               10  EDIT-ORPRIS             PICTURE Z999999,99.
               10  EDIT-ORRAB1             PICTURE Z9,9.
               10  EDIT-ORRAB2             PICTURE Z9,9.
               10  EDIT-ORRAB3             PICTURE Z9,9.
               10  XO-50D                  PICTURE S9(5).
               10  XO-50U                  PICTURE 9(5).
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREM-PROCESS
               SET ORDREM-PROCESS-OFF      TO TRUE
               SET ORDREM-READ             TO TRUE
           END-IF
 
           IF  ORDREM-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREM-GET
               SET ORDREM-READ-OFF         TO TRUE
               IF  NOT ORDREM-EOF
                   SET ORDREM-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-IDSET
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
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
      *          FIRMA     COMP "918"                LR  95
      * N95                GOTO SLUTT                       NEI
           IF  (I-U1)
               SET NOT-I-94                TO TRUE
               IF  KUNDNR = '999981'
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-U1)
               SET NOT-I-94                TO TRUE
               IF  KUNDNR = '999996'
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-94)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-04)
               GO TO SLUTT-T
      *          FIRMA     COMP "764"                LR  95
      * N95                GOTO SLUTT                       NEI
      *          EDBNR     COMP 9002510                  12 RIKTIG VARE
      *          ORDNR     COMP "859318"                 12 RIKTIG VARE
      * N12      ORDNR     COMP "857682"                 12 RIKTIG VARE
      * N12      ORDNR     COMP "857699"                 12 RIKTIG VARE
      * N12      ORDNR     COMP "857905"                 12 RIKTIG VARE
      * N12      ORDNR     COMP "857998"                 12 RIKTIG VARE
      *  12                SETON                     10
           END-IF
           SET I-10                        TO TRUE
           ADD 1                           TO ANTTOT
           IF  (I-10)
               ADD 1                       TO ANT
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       ORDREM-GET SECTION.
       ORDREM-GET-P.
           IF  ORDREM-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ORDLIM-READ-OFF
                    OR ORDLIM-EOF
                   IF  ORDLIM-READ
                       SET ORDLIM-READ-OFF TO TRUE
                       READ ORDLIM
                       AT END
                           SET ORDLIM-EOF  TO TRUE
                           SET ORDREM-EOF  TO TRUE
                           SUBTRACT 1    FROM LR-CHECK
                       NOT AT END
                           MOVE ORDLIM-IO-AREA (1:4) TO ORDREM-KEY1
                       END-READ
                   END-IF
                   IF  ORDLIM-EOF-OFF
                   AND ORDLIM-READ-OFF
                       READ ORDREM
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:6)  TO KUNDNR (1:6)
               MOVE ORDREM-IO-AREA (27:30) TO KNAVN1 (1:30)
               MOVE ORDREM-IO-AREA (57:30) TO KNAVN2 (1:30)
               MOVE ORDREM-IO-AREA (87:1)  TO KTSIFF (1:1)
               MOVE ORDREM-IO-AREA (88:1)  TO DIRREG (1:1)
               MOVE ORDREM-IO-AREA (89:1)  TO FRITT (1:1)
               MOVE ORDREM-IO-AREA (90:2)  TO LAGER (1:2)
               MOVE ORDREM-IO-AREA (92:1)  TO BK (1:1)
               MOVE ORDREM-IO-AREA (93:1)  TO SKAF (1:1)
               MOVE ORDREM-IO-AREA (94:2)  TO BETM (1:2)
               MOVE ORDREM-IO-AREA (96:1)  TO GEBYR (1:1)
               MOVE ORDREM-IO-AREA (97:1)  TO FRAKT (1:1)
               MOVE ORDREM-IO-AREA (98:1)  TO AVD (1:1)
               MOVE ORDREM-IO-AREA (99:1)  TO KRETYP (1:1)
               MOVE ORDREM-IO-AREA (100:1) TO REST (1:1)
               MOVE ORDREM-IO-AREA (101:1) TO PRIKOD (1:1)
               MOVE ORDREM-IO-AREA (102:1) TO STAM (1:1)
               MOVE ORDREM-IO-AREA (103:1) TO KIS (1:1)
               MOVE ORDREM-IO-AREA (105:1) TO KOMUTA (1:1)
               MOVE ORDREM-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (138:4) TO ORDMA-ELGR (1:4)
               MOVE ORDREM-IO-AREA (138:2) TO ORDMND (1:2)
               MOVE ORDREM-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDREM-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDREM-IO-AREA (142:2) TO ORDMOT (1:2)
               MOVE ORDREM-IO-AREA (144:4) TO TERMID (1:4)
               MOVE ORDREM-IO-AREA (148:1) TO SELGKP (1:1)
               MOVE ORDREM-IO-AREA (149:1) TO FERDIM (1:1)
               MOVE ORDREM-IO-AREA (150:2) TO FAKTNR (1:2)
               MOVE ORDREM-IO-AREA (153:4) TO REGKL-IO
               MOVE ORDREM-IO-AREA (157:1) TO RUTID (1:1)
               MOVE ORDREM-IO-AREA (158:4) TO OPDATO-IO
               MOVE ORDREM-IO-AREA (162:2) TO ANTPRT-IO
               INSPECT ANTPRT-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (164:1) TO STATUS-X (1:1)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC1 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (44:6)  TO FAKREF (1:6)
               MOVE ORDREM-IO-AREA (50:11) TO AVNAVN (1:11)
               MOVE ORDREM-IO-AREA (61:6)  TO OKKNR (1:6)
               MOVE ORDREM-IO-AREA (67:15) TO REKVNR (1:15)
               MOVE ORDREM-IO-AREA (82:15) TO FORSM (1:15)
               MOVE ORDREM-IO-AREA (97:3)  TO HND (1:3)
               MOVE ORDREM-IO-AREA (101:30) TO KADR (1:30)
               MOVE ORDREM-IO-AREA (131:4) TO POSTNR (1:4)
               MOVE ORDREM-IO-AREA (135:15) TO PSTED (1:15)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC2 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:30) TO VAADR1 (1:30)
               MOVE ORDREM-IO-AREA (51:30) TO VAADR2 (1:30)
               MOVE ORDREM-IO-AREA (81:30) TO VAADR3 (1:30)
               MOVE ORDREM-IO-AREA (111:20) TO VAADR4 (1:20)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC3 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (11:6)  TO LAGLOC (1:6)
               MOVE ORDREM-IO-AREA (17:3)  TO POSNR-IO
               INSPECT POSNR-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (21:4)  TO ANTBES-IO
               MOVE ORDREM-IO-AREA (25:4)  TO ANTRES-IO
               MOVE ORDREM-IO-AREA (29:4)  TO ANTLEV-IO
               MOVE ORDREM-IO-AREA (33:1)  TO NOREST (1:1)
               MOVE ORDREM-IO-AREA (34:3)  TO ALF (1:3)
               MOVE ORDREM-IO-AREA (37:50) TO TEKST (1:50)
               MOVE ORDREM-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDREM-IO-AREA (57:30) TO VARBET (1:30)
               MOVE ORDREM-IO-AREA (87:4)  TO EDBNR-IO
               MOVE ORDREM-IO-AREA (91:3)  TO VGR-IO
               MOVE ORDREM-IO-AREA (94:5)  TO ORPRIS-IO
               MOVE ORDREM-IO-AREA (99:2)  TO ORRAB1-IO
               MOVE ORDREM-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDREM-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDREM-IO-AREA (105:5) TO RGPRIS-IO
               MOVE ORDREM-IO-AREA (110:2) TO RGRAB1-IO
               MOVE ORDREM-IO-AREA (112:2) TO RGRAB2-IO
               MOVE ORDREM-IO-AREA (114:2) TO RGRAB3-IO
               MOVE ORDREM-IO-AREA (116:5) TO VEIPRI-IO
               MOVE ORDREM-IO-AREA (121:5) TO KOSPRI-IO
               MOVE ORDREM-IO-AREA (126:4) TO PRITIL-IO
               MOVE ORDREM-IO-AREA (130:4) TO KODATO-IO
               MOVE ORDREM-IO-AREA (134:2) TO KOSIGN (1:2)
               MOVE ORDREM-IO-AREA (136:1) TO KOSTAT (1:1)
               MOVE ORDREM-IO-AREA (137:1) TO PRITYP (1:1)
               MOVE ORDREM-IO-AREA (1:164) TO OVREC (1:164)
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           WHEN  OTHER
               SET I-07                    TO TRUE
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
           IF  (I-04 AND I-10)
      *                        FIRMA      3
      *                        EDBNR X   10
      *                        ANTBESX   17
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE ALF                    TO OUTPUN-IO-AREA (1:3)
               MOVE ';'                    TO OUTPUN-IO-AREA (4:1)
               MOVE ARTNR                  TO OUTPUN-IO-AREA (5:20)
               MOVE ';'                    TO OUTPUN-IO-AREA (25:1)
               MOVE VARBET                 TO OUTPUN-IO-AREA (26:30)
               MOVE ';'                    TO OUTPUN-IO-AREA (56:1)
               MOVE ANTLEV                 TO XO-52YY9R
               MOVE XO-52YY9R              TO OUTPUN-IO-AREA (61:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (71:1)
               MOVE ORPRIS                 TO EDIT-ORPRIS
               MOVE EDIT-ORPRIS            TO OUTPUN-IO-AREA (72:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (82:1)
               MOVE ORRAB1                 TO EDIT-ORRAB1
               MOVE EDIT-ORRAB1            TO OUTPUN-IO-AREA (83:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (87:1)
               MOVE ORRAB2                 TO EDIT-ORRAB2
               MOVE EDIT-ORRAB2            TO OUTPUN-IO-AREA (88:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (92:1)
               MOVE ORRAB3                 TO EDIT-ORRAB3
               MOVE EDIT-ORRAB3            TO OUTPUN-IO-AREA (93:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (97:1)
               MOVE ORDNR                  TO OUTPUN-IO-AREA (98:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (104:1)
               MOVE ORDDAG                 TO OUTPUN-IO-AREA (105:2)
               MOVE '.'                    TO OUTPUN-IO-AREA (107:1)
               MOVE ORDMND                 TO OUTPUN-IO-AREA (108:2)
               MOVE '.'                    TO OUTPUN-IO-AREA (110:1)
               MOVE ORDAAR                 TO OUTPUN-IO-AREA (111:2)
               MOVE ';'                    TO OUTPUN-IO-AREA (113:1)
               MOVE STATUS-X               TO OUTPUN-IO-AREA (114:1)
               MOVE ';'                    TO OUTPUN-IO-AREA (115:1)
               MOVE FIRMA                  TO OUTPUN-IO-AREA (116:3)
               MOVE ';'                    TO OUTPUN-IO-AREA (119:1)
               MOVE VGR                    TO XO-50U
               MOVE XO-50U (1:5)           TO OUTPUN-IO-AREA (120:5)
               MOVE ';'                    TO OUTPUN-IO-AREA (125:1)
               WRITE OUTPUN-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOT. ANT. POSTER'     TO LISTE-IO-AREA (5:16)
               MOVE ANTTOT                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL SELEKTERT'     TO LISTE-IO-AREA (5:16)
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           SET ORDLIM-EOF-OFF              TO TRUE
           SET ORDLIM-READ                 TO TRUE
           OPEN INPUT ORDLIM
           INITIALIZE ORDREM-DATA-FIELDS
           SET ORDREM-EOF-OFF              TO TRUE
           SET ORDREM-PROCESS              TO TRUE
           OPEN INPUT ORDREM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTPUN.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDLIM
           CLOSE ORDREM
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTPUN.
 
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
