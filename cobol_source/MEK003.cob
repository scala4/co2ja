       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEK003R.
      **********************************************  Z-WIN-RPG2   ****
      * DANNE RECORD FRA VAREKON, MED RUTINEID A1, V2 OG V4           *
      *       -------------------------------------------------       *
      * HENTER DIV OPPLYSNINGER FRA VAREMAS VED CHAIN                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: MEK003.rpg
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
           SELECT RUIDTAB
               ASSIGN TO UT-S-RUIDTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RUIDTAB-STATUS.
           SELECT ORDLIM
               ASSIGN TO ORDLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDLIM-STATUS.
           SELECT VAREKON
               ASSIGN TO VAREKON
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREKON-STATUS
               RECORD KEY IS VAREKON-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT VARKFIL
               ASSIGN TO UT-S-VARKFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARKFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
      *UIDTAB IT  F  80  80           EREAD01 SYSIPT
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD RUIDTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  RUIDTAB-IO-AREA.
           05  RUIDTAB-IO-AREA-X           PICTURE X(80).
       FD ORDLIM
               RECORD CONTAINS 80.
       01  ORDLIM-IO-AREA.
           05  ORDLIM-IO-AREA-X.
               10  ORDLIM-KEY1.
                   15  ORDLIM-KEY1N        PICTURE S9(15).
               10  FILLER                  PICTURE X(65).
       FD VAREKON
               RECORD CONTAINS 100.
       01  VAREKON-IO-AREA.
           05  VAREKON-IO-AREA-X.
               10  VAREKON-KEY1            PICTURE X(15).
               10  FILLER                  PICTURE X(85).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD VARKFIL
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  VARKFIL-IO-AREA.
           05  VARKFIL-IO-AREA-X           PICTURE X(200).
       WORKING-STORAGE SECTION.
       77  TABID-MAX   VALUE 30            PICTURE 9(4) USAGE BINARY.
       77  TABNVN-MAX   VALUE 30           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABID-TABLE.
               10  TABID-ENTRY
                                           OCCURS 30 TIMES
                                           INDEXED BY TABID-I
                                                      TABID-S
                                                      TABNVN-I
                                                      TABNVN-S.
                   15  TABID               PICTURE X(2).
                   15  TABNVN              PICTURE X(40).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  RUIDTAB-STATUS              PICTURE 99 VALUE 0.
           10  ORDLIM-STATUS               PICTURE 99 VALUE 0.
           10  VAREKON-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARKFIL-STATUS              PICTURE 99 VALUE 0.
 
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
               88  RUIDTAB-EOF-OFF         VALUE '0'.
               88  RUIDTAB-EOF             VALUE '1'.
           05  ORDLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDLIM-EOF-OFF          VALUE '0'.
               88  ORDLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDLIM-READ-OFF         VALUE '0'.
               88  ORDLIM-READ             VALUE '1'.
           05  ORDLIM-LOW-KEY              PICTURE X(15).
           05  ORDLIM-HIGH-KEY             PICTURE X(15).
           05  VAREKON-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREKON-EOF-OFF         VALUE '0'.
               88  VAREKON-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREKON-READ-OFF        VALUE '0'.
               88  VAREKON-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREKON-PROCESS-OFF     VALUE '0'.
               88  VAREKON-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREKON-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREKON-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PARAM-DATA-FIELDS.
               10  PAAR-IO.
                   15  PAAR                PICTURE S9(2).
               10  LK                      PICTURE X(2).
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           05  VAREKON-LEVEL-01.
               10  VAREKON-01-L2.
                   15  VAREKON-01-L2-FIRMA PICTURE X(3).
               10  VAREKON-01-L1.
                   15  VAREKON-01-L1-EDBNR PICTURE X(7).
           05  VAREKON-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(5).
               10  OPPDTO-IO.
                   15  OPPDTO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  OPPTID-IO.
                   15  OPPTID              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RUTID                   PICTURE X(2).
               10  OPPANT-IO.
                   15  OPPANT              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  OSIGN                   PICTURE X(1).
               10  REFNR-IO.
                   15  REFNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  LAGERK                  PICTURE X(2).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
               10  VGR                     PICTURE X(5).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(6).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DDMM-IO.
                   15  DDMM                PICTURE S9(4).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  MM-IO.
                   15  MM                  PICTURE S9(2).
               10  OTID-IO.
                   15  OTID                PICTURE S9(6).
               10  SEK-IO.
                   15  SEK                 PICTURE S9(2).
               10  TTMM-IO.
                   15  TTMM                PICTURE S9(4).
               10  TIME-X-IO.
                   15  TIME-X              PICTURE S9(2).
               10  MIN-IO.
                   15  MIN                 PICTURE S9(2).
               10  NVNID                   PICTURE X(40).
               10  KEY10                   PICTURE X(10).
               10  SVSSUM-IO.
                   15  SVSSUM              PICTURE S9(10)V9(2).
           05  EDITTING-FIELDS.
               10  EDIT-REFNR              PICTURE Z999999.
               10  EDIT-OPPANT             PICTURE Z999999,99.
               10  EDIT-SELVK              PICTURE Z999999,99.
               10  EDIT-SVSSUM             PICTURE Z999999999,99.
               10  EDIT-EDBNR              PICTURE Z999999,99.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
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
 
           IF  VAREKON-PROCESS
               SET VAREKON-PROCESS-OFF     TO TRUE
               SET VAREKON-READ            TO TRUE
           END-IF
 
           IF  VAREKON-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREKON-GET
               SET VAREKON-READ-OFF        TO TRUE
               IF  NOT VAREKON-EOF
                   SET VAREKON-PROCESS     TO TRUE
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
 
           IF  VAREKON-PROCESS
               PERFORM VAREKON-IDSET
           END-IF
 
           IF  VAREKON-PROCESS
               PERFORM VAREKON-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  VAREKON-PROCESS
               PERFORM VAREKON-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREKON-PROCESS
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
           SET NOT-I-97                    TO TRUE
           IF  LK = '10'
               SET I-97                    TO TRUE
           END-IF
           IF  (NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  LK = '13'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  LK = '18'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  LK = '17'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  LK = '93'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  LK = '92'
                   SET I-97                TO TRUE
               END-IF
      *  03                GOTO SLUTT                       NEI
      *
           END-IF
           ADD OPPDTO TO ZERO          GIVING ODATO
           MOVE ODATO (5:2)                TO AA-IO
           MOVE ODATO (1:4)                TO DDMM
           MOVE DDMM (1:2)                 TO DD
           MOVE DDMM (3:2)                 TO MM-IO
      *
           ADD OPPTID TO ZERO          GIVING OTID
           MOVE OTID (5:2)                 TO SEK-IO
           MOVE OTID (1:4)                 TO TTMM
           MOVE TTMM (1:2)                 TO TIME-X
           MOVE TTMM (3:2)                 TO MIN-IO
      *
           SET NOT-I-19                    TO TRUE
           IF  PAAR = AA
               SET I-19                    TO TRUE
           END-IF
           IF  (NOT-I-19)
               GO TO SLUTT-T
           END-IF
           IF  (I-97)
               SET NOT-I-19                TO TRUE
               IF  LAGERK = LK
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-97 AND NOT-I-19)
               GO TO SLUTT-T
      *          RUTID     COMP "A1"                     15  VARETELLING
      * N15      RUTID     COMP "V2"                     15  VBEK+VATI
      * N15      RUTID     COMP "V4"                     15  VBEK+KORR
      * N15                GOTO SLUTT                       NEI
           END-IF
           SET NOT-I-15                    TO TRUE
           SET TABID-S                     TO TABID-I
           PERFORM WITH TEST AFTER
                   VARYING TABID-I FROM 1 BY 1
                     UNTIL TABID-I >= TABID-MAX
                        OR I-15
               IF  RUTID = TABID (TABID-I)
                   SET I-15                TO TRUE
                   SET TABID-S             TO TABID-I
               END-IF
           END-PERFORM
           SET TABID-I                     TO TABID-S
           IF  I-15
           AND TABID-I NOT > TABNVN-MAX
               SET TABNVN-I                TO TABID-I
           END-IF
           IF  (I-15)
               MOVE TABNVN(TABNVN-I)       TO NVNID
           END-IF
           IF  (NOT-I-15)
               GO TO SLUTT-T
      *****************************************************************
      * CHAINE MOT VAREMAS FOR Å FINNE DIV OPPLYSNINGER               *
      *****************************************************************
           END-IF
           MOVE FIRMA                      TO KEY10 (1:3)
           MOVE EDBNR                      TO KEY10 (4:7)
           MOVE KEY10                      TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-10                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-10                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
      *****************************************************************
           MULTIPLY OPPANT BY SELVK    GIVING SVSSUM
           SET NOT-I-17                    TO TRUE
           IF  OSIGN = '+'
               SET I-17                    TO TRUE
           END-IF
           SET I-50                        TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
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
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A' )
               MOVE PARAM-IO-AREA (12:2)   TO PAAR-IO
               INSPECT PAAR-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (20:2)   TO LK (1:2)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = 'A' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       VAREKON-GET SECTION.
       VAREKON-GET-P.
           IF  VAREKON-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ORDLIM-READ-OFF
                    OR ORDLIM-EOF
                   IF  ORDLIM-READ
                       SET ORDLIM-READ-OFF TO TRUE
                       READ ORDLIM
                       AT END
                           SET ORDLIM-EOF  TO TRUE
                           SET VAREKON-EOF TO TRUE
                           SUBTRACT 1    FROM LR-CHECK
                       NOT AT END
                           MOVE ORDLIM-IO-AREA (1:4) TO VAREKON-KEY1
                       END-READ
                   END-IF
                   IF  ORDLIM-EOF-OFF
                   AND ORDLIM-READ-OFF
                       READ VAREKON
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       VAREKON-FLDSET SECTION.
       VAREKON-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREKON-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE VAREKON-IO-AREA (4:7)  TO EDBNR (1:7)
               MOVE VAREKON-IO-AREA (11:5) TO SEQNR-IO
               INSPECT SEQNR-IO REPLACING ALL ' ' BY '0'
               MOVE VAREKON-IO-AREA (19:4) TO OPPDTO-IO
               MOVE VAREKON-IO-AREA (23:4) TO OPPTID-IO
               MOVE VAREKON-IO-AREA (27:2) TO RUTID (1:2)
               MOVE VAREKON-IO-AREA (29:5) TO OPPANT-IO
               MOVE VAREKON-IO-AREA (34:1) TO OSIGN (1:1)
               MOVE VAREKON-IO-AREA (35:4) TO REFNR-IO
               MOVE VAREKON-IO-AREA (49:2) TO LAGERK (1:2)
           END-EVALUATE.
 
       VAREKON-IDSET SECTION.
       VAREKON-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREKON-CHK-LEVEL SECTION.
       VAREKON-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREKON-LEVEL-01
               MOVE VAREKON-IO-AREA (1:3)  TO VAREKON-01-L2-FIRMA
               MOVE VAREKON-IO-AREA (4:7)  TO VAREKON-01-L1-EDBNR
               IF  VAREKON-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREKON-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREKON-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREKON-01-L2         TO THE-PRIOR-L2
               MOVE  VAREKON-01-L1         TO THE-PRIOR-L1
               SET VAREKON-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VNAVN (1:30)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       RUIDTAB-LOAD SECTION.
       RUIDTAB-LOAD-P.
           OPEN INPUT RUIDTAB
           SET TABID-I                     TO 1
           PERFORM UNTIL RUIDTAB-EOF
               READ RUIDTAB
               AT END
                   SET RUIDTAB-EOF         TO TRUE
               NOT AT END
                   MOVE RUIDTAB-IO-AREA (1:42) TO TABID-ENTRY (TABID-I)
                   SET TABID-I             UP BY 1
               END-READ
           END-PERFORM
           CLOSE RUIDTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO VARKFIL-IO-AREA
               INITIALIZE VARKFIL-IO-AREA
               MOVE FIRMA                  TO VARKFIL-IO-AREA (1:3)
               MOVE ';'                    TO VARKFIL-IO-AREA (4:1)
               IF  (NOT-I-10)
                   MOVE VGR                TO VARKFIL-IO-AREA (5:5)
               END-IF
               MOVE ';'                    TO VARKFIL-IO-AREA (10:1)
               MOVE DD-IO                  TO VARKFIL-IO-AREA (11:2)
               MOVE '.'                    TO VARKFIL-IO-AREA (13:1)
               MOVE MM-IO                  TO VARKFIL-IO-AREA (14:2)
               MOVE '.'                    TO VARKFIL-IO-AREA (16:1)
               MOVE '20'                   TO VARKFIL-IO-AREA (17:2)
               MOVE AA-IO                  TO VARKFIL-IO-AREA (19:2)
      *                        OPPDTO    18 "0      "
               MOVE ';'                    TO VARKFIL-IO-AREA (21:1)
               MOVE TIME-X-IO              TO VARKFIL-IO-AREA (22:2)
               MOVE ':'                    TO VARKFIL-IO-AREA (24:1)
               MOVE MIN-IO                 TO VARKFIL-IO-AREA (25:2)
               MOVE ':'                    TO VARKFIL-IO-AREA (27:1)
               MOVE SEK-IO                 TO VARKFIL-IO-AREA (28:2)
      *                        OPPTID    27 "0      "
               MOVE ';'                    TO VARKFIL-IO-AREA (30:1)
               IF  (NOT-I-10)
                   MOVE ALFA               TO VARKFIL-IO-AREA (31:3)
               END-IF
               MOVE ';'                    TO VARKFIL-IO-AREA (34:1)
               IF  (NOT-I-10)
                   MOVE ARTNR              TO VARKFIL-IO-AREA (35:20)
               END-IF
               MOVE ';'                    TO VARKFIL-IO-AREA (55:1)
               IF  (NOT-I-10)
                   MOVE VNAVN              TO VARKFIL-IO-AREA (56:30)
               END-IF
               MOVE ';'                    TO VARKFIL-IO-AREA (86:1)
               MOVE REFNR                  TO EDIT-REFNR
               MOVE EDIT-REFNR             TO VARKFIL-IO-AREA (88:7)
               MOVE ';'                    TO VARKFIL-IO-AREA (95:1)
               MOVE RUTID                  TO VARKFIL-IO-AREA (96:2)
               MOVE ';'                    TO VARKFIL-IO-AREA (98:1)
               MOVE NVNID                  TO VARKFIL-IO-AREA (99:40)
               MOVE ';'                    TO VARKFIL-IO-AREA (139:1)
               MOVE OSIGN                  TO VARKFIL-IO-AREA (140:1)
               MOVE ';'                    TO VARKFIL-IO-AREA (141:1)
               MOVE OPPANT                 TO EDIT-OPPANT
               MOVE EDIT-OPPANT            TO VARKFIL-IO-AREA (142:10)
               IF  (NOT-I-17)
                   MOVE '-'                TO VARKFIL-IO-AREA (142:1)
               END-IF
               MOVE ';'                    TO VARKFIL-IO-AREA (152:1)
               IF  (NOT-I-10)
                   MOVE SELVK              TO EDIT-SELVK
                   MOVE EDIT-SELVK         TO VARKFIL-IO-AREA (153:10)
               END-IF
               IF  (NOT-I-17)
                   MOVE '-'                TO VARKFIL-IO-AREA (153:1)
               END-IF
               MOVE ';'                    TO VARKFIL-IO-AREA (163:1)
               MOVE SVSSUM                 TO EDIT-SVSSUM
               MOVE EDIT-SVSSUM            TO VARKFIL-IO-AREA (165:13)
               IF  (NOT-I-17)
                   MOVE '-'                TO VARKFIL-IO-AREA (164:1)
               END-IF
               MOVE ';'                    TO VARKFIL-IO-AREA (178:1)
               MOVE EDBNR                  TO EDIT-EDBNR
               MOVE EDIT-EDBNR             TO VARKFIL-IO-AREA (176:10)
               MOVE ';'                    TO VARKFIL-IO-AREA (186:1)
               WRITE VARKFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO VARKFIL-IO-AREA
               INITIALIZE VARKFIL-IO-AREA
               MOVE 'FNR'                  TO VARKFIL-IO-AREA (1:3)
               MOVE ';'                    TO VARKFIL-IO-AREA (4:1)
               MOVE 'VGR  '                TO VARKFIL-IO-AREA (5:5)
               MOVE ';'                    TO VARKFIL-IO-AREA (10:1)
               MOVE 'OPPD DTO'             TO VARKFIL-IO-AREA (13:8)
               MOVE ';'                    TO VARKFIL-IO-AREA (21:1)
               MOVE 'OPPD TID'             TO VARKFIL-IO-AREA (22:8)
               MOVE ';'                    TO VARKFIL-IO-AREA (30:1)
               MOVE 'ALF'                  TO VARKFIL-IO-AREA (31:3)
               MOVE ';'                    TO VARKFIL-IO-AREA (34:1)
               MOVE 'ARTNR     '           TO VARKFIL-IO-AREA (35:10)
               MOVE ';'                    TO VARKFIL-IO-AREA (55:1)
               MOVE 'VARENAVN  '           TO VARKFIL-IO-AREA (56:10)
               MOVE ';'                    TO VARKFIL-IO-AREA (86:1)
               MOVE 'REFNR   '             TO VARKFIL-IO-AREA (87:8)
               MOVE ';'                    TO VARKFIL-IO-AREA (95:1)
      *                                  97 "ID"
      *                                  98 ";"
               MOVE 'RUTINE; NAVN'         TO VARKFIL-IO-AREA (99:12)
               MOVE ';'                    TO VARKFIL-IO-AREA (139:1)
               MOVE 'S'                    TO VARKFIL-IO-AREA (140:1)
               MOVE ';'                    TO VARKFIL-IO-AREA (141:1)
               MOVE 'OPPD ANTAL'           TO VARKFIL-IO-AREA (142:10)
               MOVE ';'                    TO VARKFIL-IO-AREA (152:1)
               MOVE 'SELVKOST  '           TO VARKFIL-IO-AREA (153:10)
               MOVE ';'                    TO VARKFIL-IO-AREA (163:1)
               MOVE 'VERDI  ,  '           TO VARKFIL-IO-AREA (164:10)
               MOVE ';'                    TO VARKFIL-IO-AREA (174:1)
               WRITE VARKFIL-IO-AREA
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
           PERFORM RUIDTAB-LOAD
           SET ORDLIM-EOF-OFF              TO TRUE
           SET ORDLIM-READ                 TO TRUE
           OPEN INPUT ORDLIM
           SET VAREKON-LEVEL-INIT          TO TRUE
           INITIALIZE VAREKON-DATA-FIELDS
           SET VAREKON-EOF-OFF             TO TRUE
           SET VAREKON-PROCESS             TO TRUE
           OPEN INPUT VAREKON
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT VARKFIL.
           SET TABID-I                     TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE ORDLIM
           CLOSE VAREKON
           CLOSE VAREMAS
           CLOSE VARKFIL.
 
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
