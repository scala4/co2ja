       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO510R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RKO510, PLUKKER UT RESKONTRO TIL DOWNLOAD.   *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: RES99UB,RESMEK1,                             *
      *  LAGET DATO....: 21.11.97                                     *
      *  ENDRET........: 21.06.04 LAGET KOLONNEHEADINGER              *
      *                  27.08.04 TAR MED SALDO OG NAVN NÅR UPSI-2 PÅ *
      *                  07.04.09 TATT UT UPSI/LISTER                 *
      *                           TAR MED LANGT TEKSTFELT             *
      *                           REDIGERT HEADINGER OVER KOLONNER    *
      *                           OG BELØPSKOLONNER.                  *
      *                  05.01.10 LAGT INN HND ETTER KUNDENAVN.       *
      *  RETTET........:                                              *
      *  INPUT.........: FIRMAPARAMETER (FIRTAB),                     *
      *                  SEKVENSIELL RESKONTRO MASTER (RESKFIL).      *
      *  BEHANDLING....: PAKKER OPP PAKKEDE FELTER OG REDIGERER PC-   *
      *                  FILE FOR FIRMA I PARAMETER.                  *
      *  OUTPUT........: RESKONTROFILE                                *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO510.rpg
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
           SELECT FIRTAB
               ASSIGN TO UT-S-FIRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRTAB-STATUS.
           SELECT RESKINN
               ASSIGN TO UT-S-RESKINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKINN-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT RESKUT
               ASSIGN TO RESKUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FIRTAB-IO-AREA.
           05  FIRTAB-IO-AREA-X            PICTURE X(80).
       FD RESKINN
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RESKINN-IO-AREA-2.
           05  RESKINN-IO-AREA-X           PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD RESKUT
               RECORD CONTAINS 150.
       01  RESKUT-IO-AREA.
           05  RESKUT-IO-AREA-X            PICTURE X(150).
       WORKING-STORAGE SECTION.
       77  TABFIR-MAX   VALUE 120          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFIR-TABLE.
               10  TABFIR-ENTRY
                                           OCCURS 120 TIMES
                                           INDEXED BY TABFIR-I
                                                      TABFIR-S.
                   15  TABFIR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FIRTAB-STATUS               PICTURE 99 VALUE 0.
           10  RESKINN-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  RESKUT-STATUS               PICTURE 99 VALUE 0.
           10  TKDATA-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRTAB-EOF-OFF          VALUE '0'.
               88  FIRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKINN-EOF-OFF         VALUE '0'.
               88  RESKINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKINN-READ-OFF        VALUE '0'.
               88  RESKINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKINN-PROCESS-OFF     VALUE '0'.
               88  RESKINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKINN-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKINN-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKINN-AHEAD-EOF-OFF   VALUE '0'.
               88  RESKINN-AHEAD-EOF       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKINN-AHEAD-READ-OFF  VALUE '0'.
               88  RESKINN-AHEAD-READ      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
      *DSDS: DATA STRUCTURE FIELDS
           05  TKDATA-XX-DATA-FIELDS.
               10  TKTK                    PICTURE X(2).
               10  FILLER                  PICTURE X(19).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  TKTEXT                  PICTURE X(7).
               10  FILLER                  PICTURE X(12).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  TKBILN                  PICTURE X(6).
               10  FILLER                  PICTURE X(6).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  TKREFN                  PICTURE X(6).
           05  RESKINN-LEVEL-01.
               10  RESKINN-01-L2.
                   15  RESKINN-01-L2-FIRMA PICTURE X(3).
               10  RESKINN-01-L1.
                   15  RESKINN-01-L1-KNRKEY PICTURE X(9).
           05  RESKINN-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KNRKEY                  PICTURE X(9).
               10  RESKNR                  PICTURE X(6).
               10  BILDTO                  PICTURE X(6).
               10  TRKODE                  PICTURE X(2).
               10  REFNR                   PICTURE X(6).
               10  BILNR                   PICTURE X(6).
               10  FFDATO                  PICTURE X(6).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  BILTXT                  PICTURE X(24).
               10  NXTKEY                  PICTURE X(9).
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN                    PICTURE X(30).
               10  KAT-IO.
                   15  KAT                 PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  KJO-ELGPP-IO.
                   15  KJO-ELGPP           PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  HND                     PICTURE X(3).
      *****************************************************************
      * HOUSEKEEPING.                                                 *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(6).
               10  SALDOU-IO.
                   15  SALDOU              PICTURE S9(9)V9(2).
               10  BELN-IO.
                   15  BELN                PICTURE S9(9)V9(2).
               10  SALDO-IO.
                   15  SALDO               PICTURE S9(9)V9(2).
               10  KJO-ELGPN-IO.
                   15  KJO-ELGPN           PICTURE S9(9)V9(2).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(6).
               10  TEKST                   PICTURE X(7).
           05  EDITTING-FIELDS.
               10  EDIT-BELN               PICTURE Z999999999,99.
               10  EDIT-SALDOU             PICTURE Z999999999,99.
               10  EDIT-KJO-ELGPN          PICTURE Z999999999,99.
               10  XO-30D                  PICTURE S9(3).
               10  XO-30U                  PICTURE 9(3).
           05  RESKINN-IO-AREA.
               10  FILLER                  PICTURE X(200).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESKINN-PROCESS
               SET RESKINN-PROCESS-OFF     TO TRUE
               SET RESKINN-READ            TO TRUE
           END-IF
 
           IF  RESKINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKINN-GET
               SET RESKINN-READ-OFF        TO TRUE
               IF  NOT RESKINN-EOF
                   SET RESKINN-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESKINN-PROCESS
               PERFORM RESKINN-IDSET
           END-IF
 
           IF  RESKINN-PROCESS
               PERFORM RESKINN-CHK-LEVEL
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
 
           IF  RESKINN-PROCESS
               PERFORM RESKINN-FLDOFF
               PERFORM RESKINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKINN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-99)
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET I-98                    TO TRUE
               SET I-99                    TO TRUE
      *  99                TIME           TIDSP   60       TIDSPUNKT
      *****************************************************************
      * LEGGER UT RESKINN FOR DOWNLOAD TIL PC.                        *
      *****************************************************************
           END-IF
           ADD 1                           TO ANTINN
           SET NOT-I-50                    TO TRUE
           IF  (I-L1)
               SUBTRACT SALDO              FROM SALDO
               SUBTRACT SALDOU             FROM SALDOU
           END-IF
           SET NOT-I-10                    TO TRUE
           SET TABFIR-S                    TO TABFIR-I
           PERFORM WITH TEST AFTER
                   VARYING TABFIR-I FROM 1 BY 1
                     UNTIL TABFIR-I >= TABFIR-MAX
                        OR I-10
               IF  FIRMA = TABFIR (TABFIR-I)
                   SET I-10                TO TRUE
                   SET TABFIR-S            TO TABFIR-I
               END-IF
           END-PERFORM
           IF  (NOT-I-10)
               GO TO SLUTT-T
           END-IF
           ADD BELO-ELGP TO ZERO       GIVING BELN
           SET NOT-I-12                    TO TRUE
           IF  BELN < 0
               SET I-12                    TO TRUE
           END-IF
           ADD BELO-ELGP                   TO SALDO
           SET NOT-I-11                    TO TRUE
           IF  KNRKEY NOT = NXTKEY
               SET I-11                    TO TRUE
           END-IF
           IF  (I-11)
               ADD SALDO TO ZERO       GIVING SALDOU
               SET NOT-I-14                TO TRUE
               IF  SALDOU < 0
                   SET I-14                TO TRUE
               END-IF
      *                    MLLZO"0"       BELN              FJERN ZONE
           END-IF
           IF  (I-L1)
               MOVE KNRKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-11)
               MOVE 0                      TO KJO-ELGPN
               SET NOT-I-13                TO TRUE
               IF  KJO-ELGPN < 0
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-11 AND I-02)
               ADD KJO-ELGPP TO ZERO   GIVING KJO-ELGPN
               SET NOT-I-13                TO TRUE
               IF  KJO-ELGPN < 0
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-11)
      ** MLLzo
               IF KJO-EL < 0
                   MULTIPLY -1 BY KJO-EL
               END-IF
      *  L1N11             MOVE KJØPN     KJØPX  11        ALFA
           END-IF
           ADD 1                           TO ANTUT
           SET I-50                        TO TRUE
           IF  (I-15)
               PERFORM TKRUT-S
           END-IF.
 
       SLUTT-T.
      *0 01                MOVE "XXXXXXXX"BUGFL1  8        DISPLAY FIELD
      *0 01      BUGFL1    DEBUGFLISTEO   XXXXXX           VIS INDIKATOR
      ******************************************************
      *    SUBRUTINE FOR CALL AV COBOL SUBRUTINE RESTRAN.  *
      *    DENNE RUTINE HENTER RESKONTRO TEKST.            *
      ******************************************************
           CONTINUE.
 
       TKRUT-S SECTION.
       TKRUT-S-P.
           MOVE TRKODE                     TO TKTK
           MOVE '       '                  TO TKTEXT
           MOVE BILNR                      TO TKBILN
           MOVE REFNR                      TO TKREFN
           CALL 'RESTRAN' USING TKDATA-XX-DATA-FIELDS
           MOVE TKTEXT                     TO TEKST.
      *****************************************************************
      * PC-FILE                                                       *
      *****************************************************************
 
       RESKINN-GET SECTION.
       RESKINN-GET-P.
           IF  RESKINN-EOF-OFF
               IF  RESKINN-AHEAD-EOF-OFF
                   IF  RESKINN-AHEAD-READ-OFF
                       SET RESKINN-AHEAD-READ TO TRUE
                       READ RESKINN
                       AT END
                           SET RESKINN-AHEAD-EOF TO TRUE
                           INITIALIZE RESKINN-IO-AREA-2
                       END-READ
                   END-IF
                   MOVE RESKINN-IO-AREA-2  TO RESKINN-IO-AREA
                   IF  RESKINN-AHEAD-EOF-OFF
                       READ RESKINN
                       AT END
                           SET RESKINN-AHEAD-EOF TO TRUE
                           INITIALIZE RESKINN-IO-AREA-2
                       END-READ
                   ELSE
                       SET RESKINN-EOF     TO TRUE
                       SUBTRACT 1        FROM LR-CHECK
                   END-IF
                   PERFORM RESKINN-AHEAD-FLDSET
               ELSE
                   SET RESKINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-IF
           END-IF.
 
       RESKINN-FLDOFF SECTION.
       RESKINN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       RESKINN-FLDSET SECTION.
       RESKINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKINN-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE RESKINN-IO-AREA (3:9)  TO KNRKEY (1:9)
               MOVE RESKINN-IO-AREA (6:6)  TO RESKNR (1:6)
               MOVE RESKINN-IO-AREA (24:6) TO BILDTO (1:6)
               MOVE RESKINN-IO-AREA (22:2) TO TRKODE (1:2)
               MOVE RESKINN-IO-AREA (36:6) TO REFNR (1:6)
               MOVE RESKINN-IO-AREA (30:6) TO BILNR (1:6)
               MOVE RESKINN-IO-AREA (42:6) TO FFDATO (1:6)
               MOVE RESKINN-IO-AREA (48:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE RESKINN-IO-AREA (90:24) TO BILTXT (1:24)
               IF  BILTXT = SPACES
                   SET I-15                TO TRUE
               END-IF
           END-EVALUATE.
 
       RESKINN-IDSET SECTION.
       RESKINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESKINN-CHK-LEVEL SECTION.
       RESKINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKINN-LEVEL-01
               MOVE RESKINN-IO-AREA (3:3)  TO RESKINN-01-L2-FIRMA
               MOVE RESKINN-IO-AREA (3:9)  TO RESKINN-01-L1-KNRKEY
               IF  RESKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKINN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKINN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKINN-01-L2         TO THE-PRIOR-L2
               MOVE  RESKINN-01-L1         TO THE-PRIOR-L1
               SET RESKINN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESKINN-AHEAD-FLDSET SECTION.
       RESKINN-AHEAD-FLDSET-P.
           MOVE RESKINN-IO-AREA-2 (3:9)    TO NXTKEY (1:9).
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN (1:30)
               MOVE KUNDEMA-IO-AREA (162:2) TO KAT-IO
               MOVE KUNDEMA-IO-AREA (150:6) TO KJO-ELGPP-IO
               MOVE KUNDEMA-IO-AREA (185:3) TO HND (1:3)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       FIRTAB-LOAD SECTION.
       FIRTAB-LOAD-P.
           OPEN INPUT FIRTAB
           SET TABFIR-I                    TO 1
           PERFORM UNTIL FIRTAB-EOF
               READ FIRTAB
               AT END
                   SET FIRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FIRTAB-IO-AREA (1:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FIRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO RESKUT-IO-AREA
               INITIALIZE RESKUT-IO-AREA
               MOVE 'FIR'                  TO RESKUT-IO-AREA (2:3)
               MOVE ';'                    TO RESKUT-IO-AREA (5:1)
               MOVE 'RESKNR'               TO RESKUT-IO-AREA (6:6)
               MOVE ';'                    TO RESKUT-IO-AREA (12:1)
               MOVE '------------ NAVN'    TO RESKUT-IO-AREA (13:17)
               MOVE ' ------------'        TO RESKUT-IO-AREA (30:13)
               MOVE ';'                    TO RESKUT-IO-AREA (43:1)
               MOVE 'HND'                  TO RESKUT-IO-AREA (44:3)
               MOVE ';'                    TO RESKUT-IO-AREA (47:1)
               MOVE 'REF.NR'               TO RESKUT-IO-AREA (48:6)
               MOVE ';'                    TO RESKUT-IO-AREA (54:1)
               MOVE '----------- TEKST'    TO RESKUT-IO-AREA (55:17)
               MOVE ' ------------'        TO RESKUT-IO-AREA (72:13)
               MOVE ';'                    TO RESKUT-IO-AREA (85:1)
               MOVE 'BILDTO'               TO RESKUT-IO-AREA (86:6)
               MOVE ';'                    TO RESKUT-IO-AREA (92:1)
               MOVE '--- BELØP ---'        TO RESKUT-IO-AREA (93:13)
               MOVE ';'                    TO RESKUT-IO-AREA (106:1)
               MOVE 'FFDATO'               TO RESKUT-IO-AREA (107:6)
               MOVE ';'                    TO RESKUT-IO-AREA (113:1)
               MOVE '-RESK.-SALDO-'        TO RESKUT-IO-AREA (114:13)
               MOVE ';'                    TO RESKUT-IO-AREA (127:1)
               MOVE '- KJØP I ÅR -'        TO RESKUT-IO-AREA (128:13)
               MOVE ';'                    TO RESKUT-IO-AREA (141:1)
               MOVE 'KAT'                  TO RESKUT-IO-AREA (142:3)
               MOVE ';'                    TO RESKUT-IO-AREA (145:1)
               WRITE RESKUT-IO-AREA
           END-IF
           IF  (I-01 AND I-50)
               MOVE SPACES TO RESKUT-IO-AREA
               INITIALIZE RESKUT-IO-AREA
               MOVE FIRMA                  TO RESKUT-IO-AREA (2:3)
               MOVE ';'                    TO RESKUT-IO-AREA (5:1)
               MOVE RESKNR                 TO RESKUT-IO-AREA (6:6)
               MOVE ';'                    TO RESKUT-IO-AREA (12:1)
               MOVE NAVN                   TO RESKUT-IO-AREA (13:30)
               MOVE ';'                    TO RESKUT-IO-AREA (43:1)
               MOVE HND                    TO RESKUT-IO-AREA (44:3)
               MOVE ';'                    TO RESKUT-IO-AREA (47:1)
               MOVE REFNR                  TO RESKUT-IO-AREA (48:6)
               MOVE ';'                    TO RESKUT-IO-AREA (54:1)
               IF  (NOT-I-15)
                   MOVE BILTXT             TO RESKUT-IO-AREA (55:24)
               END-IF
               IF  (I-15)
                   MOVE TEKST              TO RESKUT-IO-AREA (55:7)
               END-IF
               MOVE ';'                    TO RESKUT-IO-AREA (85:1)
               MOVE BILDTO                 TO RESKUT-IO-AREA (86:6)
               MOVE ';'                    TO RESKUT-IO-AREA (92:1)
               MOVE BELN                   TO EDIT-BELN
               MOVE EDIT-BELN              TO RESKUT-IO-AREA (93:13)
               IF  (NOT-I-12)
                   MOVE ' '                TO RESKUT-IO-AREA (93:1)
               END-IF
               IF  (I-12)
                   MOVE '-'                TO RESKUT-IO-AREA (93:1)
               END-IF
               MOVE ';'                    TO RESKUT-IO-AREA (106:1)
               MOVE FFDATO                 TO RESKUT-IO-AREA (107:6)
               MOVE ';'                    TO RESKUT-IO-AREA (113:1)
               MOVE SALDOU                 TO EDIT-SALDOU
               MOVE EDIT-SALDOU            TO RESKUT-IO-AREA (114:13)
               IF  (NOT-I-14)
                   MOVE ' '                TO RESKUT-IO-AREA (114:1)
               END-IF
               IF  (I-14)
                   MOVE '-'                TO RESKUT-IO-AREA (114:1)
               END-IF
               MOVE ';'                    TO RESKUT-IO-AREA (127:1)
               MOVE KJO-ELGPN              TO EDIT-KJO-ELGPN
               MOVE EDIT-KJO-ELGPN         TO RESKUT-IO-AREA (128:13)
               IF  (NOT-I-13)
                   MOVE ' '                TO RESKUT-IO-AREA (128:1)
               END-IF
               IF  (I-13)
                   MOVE '-'                TO RESKUT-IO-AREA (128:1)
               END-IF
               MOVE ';'                    TO RESKUT-IO-AREA (141:1)
               MOVE KAT                    TO XO-30U
               MOVE XO-30U (1:3)           TO RESKUT-IO-AREA (142:3)
               MOVE ';'                    TO RESKUT-IO-AREA (145:1)
               WRITE RESKUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U7 AND I-U8)
               MOVE SPACES TO RESKUT-IO-AREA
               INITIALIZE RESKUT-IO-AREA
               MOVE TKTK                   TO RESKUT-IO-AREA (139:2)
               MOVE TKBILN                 TO RESKUT-IO-AREA (135:6)
               MOVE TKREFN                 TO RESKUT-IO-AREA (135:6)
               WRITE RESKUT-IO-AREA
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
           PERFORM FIRTAB-LOAD
           SET RESKINN-LEVEL-INIT          TO TRUE
           SET RESKINN-AHEAD-EOF-OFF       TO TRUE
           SET RESKINN-AHEAD-READ-OFF      TO TRUE
           INITIALIZE RESKINN-DATA-FIELDS
           SET RESKINN-EOF-OFF             TO TRUE
           SET RESKINN-PROCESS             TO TRUE
           OPEN INPUT RESKINN
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT RESKUT.
           SET TABFIR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKINN
           CLOSE KUNDEMA
           CLOSE RESKUT.
 
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
