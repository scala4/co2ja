       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD860R.
      **********************************************  Z-WIN-RPG2   ****
      *       O R D R E R U T I N E P R O G R A M   O R D 8 6 0       *
      *       -------------------------------------------------       *
      *  1. DANNE RECORD SOM SKAL DANNE SALG.TOTAL.FILE               *
      *      04/4-2002 AV ESPEN LARSEN.                               *
      *      OBS: UTGÅRMELDTE ORDRE ER MED I SUMMEN.                  *
      *           LAGEROVERFØRING ER IKKE MED I SUMMEN.               *
      *           ** DETTE ER DET SAMME SOM PÅ OPPGAVE ORD80. **      *
      * 19.11.02 UTGÅRMELDTE ORDRE OG VERKSTEDSUTTAK BLIR IKKE MED.   *
      *          DETTE VIL GI FEIL TOTALSUMMER.                       *
      * 03.01.03 UPSI 1 DANNER SALG.TOTAL.HIST.FILE VED Å TESTE PÅ.   *
      *          FJOREÅRET OG ÅRET FØR FJORÅRET.                      *
      * 12.10.10 LAGT INN SUM KUN ORDRE OG SUM KUN KREDIRTTORDRE      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD860.rpg
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
           SELECT UKETAB
               ASSIGN TO UT-S-UKETAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UKETAB-STATUS.
           SELECT MASTINN
               ASSIGN TO UT-S-MASTINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTINN-STATUS.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT SALGTOT
               ASSIGN TO UT-S-SALGTOT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SALGTOT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD UKETAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  UKETAB-IO-AREA.
           05  UKETAB-IO-AREA-X            PICTURE X(80).
       FD MASTINN
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  MASTINN-IO-AREA.
           05  MASTINN-IO-AREA-X           PICTURE X(100).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD SALGTOT
               BLOCK CONTAINS 42
               RECORD CONTAINS 21.
       01  SALGTOT-IO-AREA.
           05  SALGTOT-IO-AREA-X           PICTURE X(21).
       WORKING-STORAGE SECTION.
       77  TABDTO-MAX   VALUE 740          PICTURE 9(4) USAGE BINARY.
       77  TABUKE-MAX   VALUE 740          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABDTO-TABLE.
               10  TABDTO-ENTRY
                                           OCCURS 740 TIMES
                                           INDEXED BY TABDTO-I
                                                      TABDTO-S
                                                      TABUKE-I
                                                      TABUKE-S.
                   15  TABDTO              PICTURE X(6).
                   15  TABUKE              PICTURE X(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  UKETAB-STATUS               PICTURE 99 VALUE 0.
           10  MASTINN-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  SALGTOT-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  UKETAB-EOF-OFF          VALUE '0'.
               88  UKETAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-EOF-OFF         VALUE '0'.
               88  MASTINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-READ-OFF        VALUE '0'.
               88  MASTINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTINN-PROCESS-OFF     VALUE '0'.
               88  MASTINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MASTINN-LEVEL-INIT-OFF  VALUE '0'.
               88  MASTINN-LEVEL-INIT      VALUE '1'.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  MASTINN-LEVEL-01.
               10  MASTINN-01-L1.
                   15  MASTINN-01-L1-FIRMA PICTURE X(3).
           05  MASTINN-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  O1SIFF                  PICTURE X(1).
               10  DAG-IO.
                   15  DAG                 PICTURE S9(2).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  ODATO                   PICTURE X(6).
               10  STATUS-X                PICTURE X(1).
               10  RUT                     PICTURE X(1).
               10  LK                      PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  AVDEL                   PICTURE X(1).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ORDBEL-IO.
                   15  ORDBEL              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REOBEL-IO.
                   15  REOBEL              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTVL-IO.
                   15  ANTVL               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  ANTVLR-IO.
                   15  ANTVLR              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  AUTOPAR-DATA-FIELDS.
               10  APDATO-IO.
                   15  APDATO              PICTURE S9(6).
               10  APDAG-IO.
                   15  APDAG               PICTURE S9(2).
               10  APMND-IO.
                   15  APMND               PICTURE S9(2).
               10  APAAR-IO.
                   15  APAAR               PICTURE S9(4).
      *****************************************************************
      * OPPSLAG MOT PARAMFIL.AUTODATA FOR HENTING AV AKTUELL DATO.    *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  APKEY                   PICTURE X(3).
               10  FAAR-IO.
                   15  FAAR                PICTURE S9(4).
               10  FFAAR-IO.
                   15  FFAAR               PICTURE S9(4).
               10  FIRNR                   PICTURE X(3).
               10  AVD                     PICTURE X(1).
               10  OAAR-IO.
                   15  OAAR                PICTURE S9(4).
               10  NTOBEL-IO.
                   15  NTOBEL              PICTURE S9(7)V9(2).
               10  BELDPA-ELG-IO.
                   15  BELDPA-ELG          PICTURE S9(9)V9(2).
               10  BELDPF-IO.
                   15  BELDPF              PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  MASTINN-PROCESS
               SET MASTINN-PROCESS-OFF     TO TRUE
               SET MASTINN-READ            TO TRUE
           END-IF
 
           IF  MASTINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM MASTINN-GET
               SET MASTINN-READ-OFF        TO TRUE
               IF  NOT MASTINN-EOF
                   SET MASTINN-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-IDSET
           END-IF
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-CHK-LEVEL
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
 
           IF  MASTINN-PROCESS
               PERFORM MASTINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  MASTINN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-09)
               MOVE 'B01'                  TO APKEY
               MOVE APKEY                  TO AUTOPAR-KEY1
               READ AUTOPAR RECORD KEY IS AUTOPAR-KEY1
               INVALID KEY
                   SET I-99                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-99            TO TRUE
                   PERFORM AUTOPAR-FLDSET
                   PERFORM AUTOPAR-IDSET
               END-READ
               SUBTRACT 1 FROM APAAR   GIVING FAAR
               SUBTRACT 2 FROM APAAR   GIVING FFAAR
               SET I-09                    TO TRUE
      *****************************************************************
      * OMDØPING AV FIRMANUMMER P.G.A. SAMMENSLÅING AV FIRMA.         *
      *          VENG KAN FJERNES ETTER 1.01.2003                     *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO FIRNR
               SET NOT-I-77                TO TRUE
               IF  FIRMA = '957'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-77)
               MOVE '956'                  TO FIRNR
      *****************************************************************
      * OMDØPING AV AVDELING.                                         *
      *          VENG KAN FJERNES ETTER 1.01.2003                     *
      *****************************************************************
           END-IF
           MOVE AVDEL                      TO AVD
           SET NOT-I-78                    TO TRUE
           IF  FIRMA = '957'
               SET I-78                    TO TRUE
           END-IF
           IF  (I-78)
               SET NOT-I-61                TO TRUE
               IF  AVDEL = '1'
                   SET I-61                TO TRUE
               END-IF
               SET NOT-I-62                TO TRUE
               IF  AVDEL = '2'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-78 AND I-61)
               MOVE '2'                    TO AVD
           END-IF
           IF  (I-78 AND I-62)
               MOVE '3'                    TO AVD
           END-IF
           SET NOT-I-78                    TO TRUE
           IF  FIRMA = '956'
               SET I-78                    TO TRUE
           END-IF
           IF  (I-78)
               SET NOT-I-61                TO TRUE
               IF  AVDEL = '3'
                   SET I-61                TO TRUE
               END-IF
               SET NOT-I-62                TO TRUE
               IF  AAR = 01
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-78 AND I-61 AND I-62)
               MOVE '1'                    TO AVD
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           END-IF
           SET NOT-I-15                    TO TRUE
           SET NOT-I-50                    TO TRUE
           SET NOT-I-80                    TO TRUE
           IF  AAR NOT < 80
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               ADD AAR TO 1900         GIVING OAAR
           END-IF
           IF  (NOT-I-80)
               ADD AAR TO 2000         GIVING OAAR
           END-IF
           IF  (I-U1)
               GO TO HSTRUT-T
      *****************************************************************
      * RUTINE FOR TEST PÅ SALG.TOTAL.FILE.                           *
      *****************************************************************
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  OAAR = APAAR
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  OAAR = FAAR
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           IF  MND < APMND
               SET I-22                    TO TRUE
           END-IF
           IF  MND = APMND
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  DAG NOT > APDAG
               SET I-24                    TO TRUE
           END-IF
           IF  (I-21 AND I-22)
               OR  (I-21 AND I-23 AND I-24)
               SET I-15                    TO TRUE
           END-IF
           GO TO HSTEND-T
      *****************************************************************
      * RUTINE FOR TEST PÅ SALG.TOTAL.HIST.FILE.                      *
      *****************************************************************
           .
 
       HSTRUT-T.
           SET NOT-I-13                    TO TRUE
           IF  OAAR = FAAR
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  OAAR = FFAAR
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               SET I-15                    TO TRUE
           END-IF.
 
       HSTEND-T.
      *****************************************************************
           ADD ORDBEL TO ZERO          GIVING NTOBEL
      *****************************************************************
      *          RUTINE FOR Å NULLSTILLE SUMMER.                      *
      *****************************************************************
           IF  (I-01)
               MOVE 0,00                   TO BELDPA-ELG
               MOVE 0,00                   TO BELDPF
      *  01                Z-ADD0,00      KELDPÅ           = BEL. DENNE PER I ÅR
      *  01                Z-ADD0,00      KELDPF           = BEL. DENNE PER I FJ
      *  01                Z-ADD0,00      OELDPÅ           = BEL. DENNE PER I ÅR
      *  01                Z-ADD0,00      OELDPF           = BEL. DENNE PER I FJ
      *****************************************************************
      *         RUTINE FOR Å SJEKKE OM DATA SKAL VÆRE MED.            *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  RUT = 'L'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  STATUS-X = 'L'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  STATUS-X = 'U'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  BK = 'V'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  BK = 'W'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-51)
               GO TO SLUTT-T
      *****************************************************************
      *          RUTINE FOR Å DANNE SUMMER.                           *
      ****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-53                TO TRUE
               IF  O1SIFF = '9'
                   SET I-53                TO TRUE
               END-IF
      *  01N53   O1SIFF    COMP "9"                  55    = ORDRE
           END-IF
           IF  (I-01 AND I-13)
               ADD NTOBEL TO ZERO      GIVING BELDPA-ELG
           END-IF
           IF  (I-01 AND I-15)
               ADD NTOBEL TO ZERO      GIVING BELDPF
      *  01 13 53          Z-ADDNTOBEL    KELDPÅ 112       = ORDRESUM IDAG
      *  01 15 53          Z-ADDNTOBEL    KELDPF 112       = REST.SUM IDAG
      *  01 13 55          Z-ADDNTOBEL    OELDPÅ 112       = ORDRESUM IDAG
      *  01 15 55          Z-ADDNTOBEL    OELDPF 112       = REST.SUM IDAG
           END-IF
           IF  (I-01 AND I-13)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND I-15)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND I-50)
               SET NOT-I-83                TO TRUE
               SET TABDTO-S                TO TABDTO-I
               PERFORM WITH TEST AFTER
                       VARYING TABDTO-I FROM 1 BY 1
                         UNTIL TABDTO-I >= TABDTO-MAX
                            OR I-83
                   IF  ODATO = TABDTO (TABDTO-I)
                       SET I-83            TO TRUE
                       SET TABDTO-S        TO TABDTO-I
                   END-IF
               END-PERFORM
               SET TABDTO-I                TO TABDTO-S
               IF  I-83
               AND TABDTO-I NOT > TABUKE-MAX
                   SET TABUKE-I            TO TABDTO-I
               END-IF
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
      *****************************************************************
      * FIRNRTOTALER.  DAG OG MND.                                    *
      *****************************************************************
           CONTINUE.
 
       MASTINN-GET SECTION.
       MASTINN-GET-P.
           IF  MASTINN-EOF-OFF
               READ MASTINN
               AT END
                   SET MASTINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MASTINN-FLDSET SECTION.
       MASTINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MASTINN-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE MASTINN-IO-AREA (5:1)  TO O1SIFF (1:1)
               MOVE MASTINN-IO-AREA (11:2) TO DAG-IO
               INSPECT DAG-IO REPLACING ALL ' ' BY '0'
               MOVE MASTINN-IO-AREA (13:2) TO MND-IO
               INSPECT MND-IO REPLACING ALL ' ' BY '0'
               MOVE MASTINN-IO-AREA (15:2) TO AAR-IO
               INSPECT AAR-IO REPLACING ALL ' ' BY '0'
               MOVE MASTINN-IO-AREA (11:6) TO ODATO (1:6)
               MOVE MASTINN-IO-AREA (25:1) TO STATUS-X (1:1)
               MOVE MASTINN-IO-AREA (26:1) TO RUT (1:1)
               MOVE MASTINN-IO-AREA (29:2) TO LK (1:2)
               MOVE MASTINN-IO-AREA (31:1) TO BK (1:1)
               MOVE MASTINN-IO-AREA (35:1) TO AVDEL (1:1)
               MOVE MASTINN-IO-AREA (51:4) TO ANTORD-IO
               MOVE MASTINN-IO-AREA (55:6) TO ORDBEL-IO
               MOVE MASTINN-IO-AREA (67:6) TO REOBEL-IO
               MOVE MASTINN-IO-AREA (73:5) TO ANTVL-IO
               MOVE MASTINN-IO-AREA (78:5) TO ANTVLR-IO
           END-EVALUATE.
 
       MASTINN-IDSET SECTION.
       MASTINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       MASTINN-CHK-LEVEL SECTION.
       MASTINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO MASTINN-LEVEL-01
               MOVE MASTINN-IO-AREA (2:3)  TO MASTINN-01-L1-FIRMA
               IF  MASTINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MASTINN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MASTINN-01-L1         TO THE-PRIOR-L1
               SET MASTINN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (4:6)  TO APDATO-IO
               INSPECT APDATO-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (4:2)  TO APDAG-IO
               INSPECT APDAG-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (6:2)  TO APMND-IO
               INSPECT APMND-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (26:4) TO APAAR-IO
               INSPECT APAAR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       UKETAB-LOAD SECTION.
       UKETAB-LOAD-P.
           OPEN INPUT UKETAB
           SET TABDTO-I                    TO 1
           PERFORM UNTIL UKETAB-EOF
               READ UKETAB
               AT END
                   SET UKETAB-EOF          TO TRUE
               NOT AT END
                   MOVE UKETAB-IO-AREA (1:8) TO TABDTO-ENTRY (TABDTO-I)
                   SET TABDTO-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE UKETAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'D'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE DAG-IO                 TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'M'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-83)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'U'                    TO SALGTOT-IO-AREA (5:1)
               MOVE TABUKE (TABUKE-I)      TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
      *****************************************************************
      * AVDELINGSTOTALER. DAG OG MND.                                 *
      *****************************************************************
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'D'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE DAG-IO                 TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'M'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-83)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'U'                    TO SALGTOT-IO-AREA (5:1)
               MOVE TABUKE (TABUKE-I)      TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'F'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE DAG-IO                 TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'P'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-83)
           AND (I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'W'                    TO SALGTOT-IO-AREA (5:1)
               MOVE TABUKE (TABUKE-I)      TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
      *****************************************************************
      * AVDELINGSTOTALER. DAG OG MND.                                 *
      *****************************************************************
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'F'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE DAG-IO                 TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'P'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-83)
           AND (I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'W'                    TO SALGTOT-IO-AREA (5:1)
               MOVE TABUKE (TABUKE-I)      TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND NOT-I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'E'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE DAG-IO                 TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'O'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-83)
           AND (NOT-I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE '*'                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'V'                    TO SALGTOT-IO-AREA (5:1)
               MOVE TABUKE (TABUKE-I)      TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
      *****************************************************************
      * AVDELINGSTOTALER. DAG OG MND.                                 *
      *****************************************************************
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND NOT-I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'E'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE DAG-IO                 TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'O'                    TO SALGTOT-IO-AREA (5:1)
               MOVE MND-IO                 TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-83)
           AND (NOT-I-53)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRNR                  TO SALGTOT-IO-AREA (1:3)
               MOVE AVD                    TO SALGTOT-IO-AREA (4:1)
               MOVE 'V'                    TO SALGTOT-IO-AREA (5:1)
               MOVE TABUKE (TABUKE-I)      TO SALGTOT-IO-AREA (6:2)
               MOVE '  '                   TO SALGTOT-IO-AREA (8:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (10:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO SALGTOT-IO-AREA (16:6)
               WRITE SALGTOT-IO-AREA
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
           PERFORM UKETAB-LOAD
           SET MASTINN-LEVEL-INIT          TO TRUE
           INITIALIZE MASTINN-DATA-FIELDS
           SET MASTINN-EOF-OFF             TO TRUE
           SET MASTINN-PROCESS             TO TRUE
           OPEN INPUT MASTINN
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           OPEN OUTPUT SALGTOT.
           SET TABDTO-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE MASTINN
           CLOSE AUTOPAR
           CLOSE SALGTOT.
 
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
