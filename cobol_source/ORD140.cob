       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD140R.
      **********************************************  Z-WIN-RPG2   ****
      ** XX.XX.20XX  LEGGER UT SEMIKOLONDELT FIL MEKONOMEN            **
      ******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD140.rpg
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
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(100).
       FD OUTFIL
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  INNFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  ORDNR1                  PICTURE X(1).
               10  KTSIFF                  PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDDAG-IO.
                   15  ORDDAG              PICTURE S9(2).
               10  ORDMND-IO.
                   15  ORDMND              PICTURE S9(2).
               10  ORDAAR-IO.
                   15  ORDAAR              PICTURE S9(2).
               10  KUNDNR                  PICTURE X(6).
               10  KNAVN1                  PICTURE X(30).
               10  ORDMOT                  PICTURE X(2).
               10  STATUS-X                PICTURE X(1).
               10  ORDSUM-IO.
                   15  ORDSUM              PICTURE S9(11).
               10  ORDSVS-IO.
                   15  ORDSVS              PICTURE S9(11).
               10  ORDTYP                  PICTURE X(1).
               10  SKAF                    PICTURE X(1).
           05  EDITTING-FIELDS.
               10  XO-110YN9               PICTURE ZZZZZZZZZZ9.
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   SET INNFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
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
           SET NOT-I-50                    TO TRUE
           IF  (I-01)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'R'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'K'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'P'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'S'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'J'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X < 'A'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               GO TO UT-T
           END-IF
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               IF  STATUS-X = 'R'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  STATUS-X = 'K'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  STATUS-X = 'P'
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  STATUS-X = 'S'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  STATUS-X = 'J'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-25)
               SET NOT-I-26                TO TRUE
               IF  SKAF = 'P'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-25 AND NOT-I-26)
               SET NOT-I-26                TO TRUE
               IF  SKAF = 'V'
                   SET I-26                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-01)
               SET NOT-I-45                TO TRUE
               IF  ORDNR1 = '9'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           SET I-50                        TO TRUE.
 
       UT-T.
           CONTINUE.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO ORDNR (1:6)
               MOVE INNFIL-IO-AREA (4:1)   TO ORDNR1 (1:1)
               MOVE INNFIL-IO-AREA (10:1)  TO KTSIFF (1:1)
               MOVE INNFIL-IO-AREA (11:1)  TO AVD (1:1)
               MOVE INNFIL-IO-AREA (12:6)  TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (12:2)  TO ORDDAG-IO
               INSPECT ORDDAG-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (14:2)  TO ORDMND-IO
               INSPECT ORDMND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (16:2)  TO ORDAAR-IO
               INSPECT ORDAAR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (18:6)  TO KUNDNR (1:6)
               MOVE INNFIL-IO-AREA (24:30) TO KNAVN1 (1:30)
               MOVE INNFIL-IO-AREA (54:2)  TO ORDMOT (1:2)
               MOVE INNFIL-IO-AREA (56:1)  TO STATUS-X (1:1)
               MOVE INNFIL-IO-AREA (57:11) TO ORDSUM-IO
               INSPECT ORDSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (79:11) TO ORDSVS-IO
               INSPECT ORDSVS-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (90:1)  TO ORDTYP (1:1)
               MOVE INNFIL-IO-AREA (91:1)  TO SKAF (1:1)
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE FIRMA                  TO OUTFIL-IO-AREA (1:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (4:1)
               MOVE ORDNR                  TO OUTFIL-IO-AREA (5:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (11:1)
               MOVE KTSIFF                 TO OUTFIL-IO-AREA (12:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (14:1)
               MOVE AVD                    TO OUTFIL-IO-AREA (17:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (18:1)
      *                        ORDATOX   24
               MOVE ORDDAG-IO              TO OUTFIL-IO-AREA (19:2)
               MOVE '.'                    TO OUTFIL-IO-AREA (21:1)
               MOVE ORDMND-IO              TO OUTFIL-IO-AREA (22:2)
               MOVE '.'                    TO OUTFIL-IO-AREA (24:1)
               MOVE ORDAAR-IO              TO OUTFIL-IO-AREA (25:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (27:1)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (28:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (34:1)
               MOVE KNAVN1                 TO OUTFIL-IO-AREA (35:30)
               MOVE ';'                    TO OUTFIL-IO-AREA (65:1)
               MOVE ORDMOT                 TO OUTFIL-IO-AREA (66:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (69:1)
               MOVE STATUS-X               TO OUTFIL-IO-AREA (71:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (73:1)
               IF  (I-45)
                   MOVE '-'                TO OUTFIL-IO-AREA (74:1)
               END-IF
               MOVE ORDSUM                 TO XO-110YN9
               MOVE XO-110YN9              TO OUTFIL-IO-AREA (78:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (89:1)
               IF  (I-45)
                   MOVE '-'                TO OUTFIL-IO-AREA (90:1)
               END-IF
               MOVE ORDSVS                 TO XO-110YN9
               MOVE XO-110YN9              TO OUTFIL-IO-AREA (94:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (105:1)
               MOVE '                    ' TO OUTFIL-IO-AREA (106:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (126:20)
               IF  (I-21)
                   MOVE 'FERDIG REGISTERT    ' TO OUTFIL-IO-AREA
                                                              (106:20)
               END-IF
               IF  (I-22)
                   MOVE 'PRINTET             ' TO OUTFIL-IO-AREA
                                                              (106:20)
               END-IF
               IF  (I-23)
                   MOVE 'PAKKET              ' TO OUTFIL-IO-AREA
                                                              (106:20)
               END-IF
               IF  (I-24)
                   MOVE 'KREDIT-STOPP        ' TO OUTFIL-IO-AREA
                                                              (106:20)
               END-IF
               IF  (I-25)
                   MOVE 'PÅBEGYNT,IKKE FULLFØRT' TO OUTFIL-IO-AREA
                                                              (106:22)
               END-IF
               IF  (I-26)
                   MOVE 'VENTEORDRE/PLUKKORDRE ' TO OUTFIL-IO-AREA
                                                              (106:22)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (146:1)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE 'FIR'                  TO OUTFIL-IO-AREA (1:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (4:1)
               MOVE 'ORDRE '               TO OUTFIL-IO-AREA (5:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (11:1)
               MOVE 'KS'                   TO OUTFIL-IO-AREA (12:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (14:1)
               MOVE 'AVD'                  TO OUTFIL-IO-AREA (15:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (18:1)
               MOVE 'O DATO'               TO OUTFIL-IO-AREA (19:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (27:1)
               MOVE 'KUNDE '               TO OUTFIL-IO-AREA (28:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (34:1)
               MOVE 'KUNDENAVN '           TO OUTFIL-IO-AREA (35:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (65:1)
               MOVE 'OM'                   TO OUTFIL-IO-AREA (66:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (69:1)
               MOVE 'STA'                  TO OUTFIL-IO-AREA (70:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (73:1)
               MOVE 'ORDRESUM  '           TO OUTFIL-IO-AREA (74:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (89:1)
               MOVE 'SELVKOSTSUM '         TO OUTFIL-IO-AREA (90:12)
               MOVE ';'                    TO OUTFIL-IO-AREA (105:1)
               MOVE 'STATUS    '           TO OUTFIL-IO-AREA (106:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (146:1)
               WRITE OUTFIL-IO-AREA
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
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
           OPEN OUTPUT OUTFIL.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE OUTFIL.
 
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
