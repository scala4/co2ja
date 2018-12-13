       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK001R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK001, INNLEGGING AV RESKONTROPARAMETER.    *
      *  ENDR 01.07.98.: TILPASSET ÅR 2000.                           *
      *                  TATT UT DATO FOR VEKSEL/AKSEPT.              *
      *                  TATT UT DATO FOR ASA/GRAD 4.                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK001.rpg
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
           SELECT KORT
               ASSIGN TO UT-S-KORT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORT-STATUS.
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT TFILE
               ASSIGN TO UT-S-TFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TFILE-STATUS.
           SELECT TFILE1
               ASSIGN TO UT-S-TFILE1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TFILE1-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORT
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KORT-IO-AREA.
           05  KORT-IO-AREA-X              PICTURE X(80).
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD TFILE
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TFILE-IO-AREA.
           05  TFILE-IO-AREA-X             PICTURE X(80).
       FD TFILE1
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TFILE1-IO-AREA.
           05  TFILE1-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  TABPER-MAX   VALUE 192          PICTURE 9(4) USAGE BINARY.
       77  TABDTO-MAX   VALUE 192          PICTURE 9(4) USAGE BINARY.
       77  TABNR-MAX   VALUE 48            PICTURE 9(4) USAGE BINARY.
       77  TABSDT-MAX   VALUE 48           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABPER-TABLE.
               10  TABPER-ENTRY
                                           OCCURS 192 TIMES
                                           INDEXED BY TABPER-I
                                                      TABPER-S
                                                      TABDTO-I
                                                      TABDTO-S.
                   15  TABPER              PICTURE S9(4).
                   15  TABDTO              PICTURE X(6).
           05  TABNR-TABLE.
               10  TABNR-ENTRY
                                           OCCURS 48 TIMES
                                           INDEXED BY TABNR-I
                                                      TABNR-S
                                                      TABSDT-I
                                                      TABSDT-S.
                   15  TABNR               PICTURE S9(2).
                   15  TABSDT              PICTURE X(15).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KORT-STATUS                 PICTURE 99 VALUE 0.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  TFILE-STATUS                PICTURE 99 VALUE 0.
           10  TFILE1-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORT-EOF-OFF            VALUE '0'.
               88  KORT-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORT-READ-OFF           VALUE '0'.
               88  KORT-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORT-PROCESS-OFF        VALUE '0'.
               88  KORT-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-EOF-OFF          VALUE '0'.
               88  RESPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-READ-OFF         VALUE '0'.
               88  RESPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-PROCESS-OFF      VALUE '0'.
               88  RESPAR-PROCESS          VALUE '1'.
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
           05  FILLER                      PIC X VALUE '0'.
               88  TFILE-EOF-OFF           VALUE '0'.
               88  TFILE-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TFILE1-EOF-OFF          VALUE '0'.
               88  TFILE1-EOF              VALUE '1'.
           05  KORT-DATA-FIELDS.
               10  OMGNR-IO.
                   15  OMGNR               PICTURE S9(4).
      *                                      17  220OMGNR8
               10  SLTDTO-IO.
                   15  SLTDTO              PICTURE S9(6).
               10  SLTDT8-IO.
                   15  SLTDT8              PICTURE S9(8).
      *                                      60  650VAKDTO
               10  PERA-ELGR-IO.
                   15  PERA-ELGR           PICTURE S9(2).
               10  PERA-ELGR8-IO.
                   15  PERA-ELGR8          PICTURE S9(4).
               10  PEROMG-IO.
                   15  PEROMG              PICTURE S9(2).
               10  SLTDAG                  PICTURE X(2).
               10  SLTMND                  PICTURE X(2).
      *                                      64  65 VAKDAG
      *                                      62  63 VAKMND
           05  RESPAR-DATA-FIELDS.
      *
               10  FILLER                  PICTURE X.
           05  TEMPORARY-FIELDS.
               10  PERNR-IO.
                   15  PERNR               PICTURE S9(2).
               10  UPERNR-IO.
                   15  UPERNR              PICTURE S9(2).
               10  UPERA-ELGR-IO.
                   15  UPERA-ELGR          PICTURE S9(2).
               10  UPERA-ELG8              PICTURE X(4).
               10  PERIOD-IO.
                   15  PERIOD              PICTURE S9(4).
               10  UPER-IO.
                   15  UPER                PICTURE S9(4).
               10  TABUDT                  PICTURE X(15).
               10  PERNUM-IO.
                   15  PERNUM              PICTURE S9(4).
               10  UPERD-IO.
                   15  UPERD               PICTURE S9(6).
               10  UPERD8-IO.
                   15  UPERD8              PICTURE S9(8).
               10  USPERD-IO.
                   15  USPERD              PICTURE S9(6).
               10  USPER8-IO.
                   15  USPER8              PICTURE S9(8).
               10  UINK15-IO.
                   15  UINK15              PICTURE S9(6).
               10  UIN158-IO.
                   15  UIN158              PICTURE S9(8).
               10  UINK-IO.
                   15  UINK                PICTURE S9(6).
               10  UINK8-IO.
                   15  UINK8               PICTURE S9(8).
               10  DPER-IO.
                   15  DPER                PICTURE S9(6).
               10  DPER8-IO.
                   15  DPER8               PICTURE S9(8).
               10  DSPER-IO.
                   15  DSPER               PICTURE S9(6).
               10  DSPER8-IO.
                   15  DSPER8              PICTURE S9(8).
               10  PERANT-IO.
                   15  PERANT              PICTURE S9(2).
               10  FF30-IO.
                   15  FF30                PICTURE S9(6).
               10  FF308-IO.
                   15  FF308               PICTURE S9(8).
               10  FF60-IO.
                   15  FF60                PICTURE S9(6).
               10  FF608-IO.
                   15  FF608               PICTURE S9(8).
               10  FF90-IO.
                   15  FF90                PICTURE S9(6).
               10  FF908-IO.
                   15  FF908               PICTURE S9(8).
               10  FF180-IO.
                   15  FF180               PICTURE S9(6).
               10  FF1808-IO.
                   15  FF1808              PICTURE S9(8).
               10  UF15-IO.
                   15  UF15                PICTURE S9(6).
               10  UF158-IO.
                   15  UF158               PICTURE S9(8).
               10  UF30-IO.
                   15  UF30                PICTURE S9(6).
               10  UF308-IO.
                   15  UF308               PICTURE S9(8).
               10  UF60-IO.
                   15  UF60                PICTURE S9(6).
               10  UF608-IO.
                   15  UF608               PICTURE S9(8).
               10  UF90-IO.
                   15  UF90                PICTURE S9(6).
               10  UF908-IO.
                   15  UF908               PICTURE S9(8).
               10  UF180-IO.
                   15  UF180               PICTURE S9(6).
               10  UF1808-IO.
                   15  UF1808              PICTURE S9(8).
               10  NSPER-IO.
                   15  NSPER               PICTURE S9(6).
               10  INK15-IO.
                   15  INK15               PICTURE S9(6).
               10  INK158-IO.
                   15  INK158              PICTURE S9(8).
               10  INKPER-IO.
                   15  INKPER              PICTURE S9(6).
               10  INKPE8-IO.
                   15  INKPE8              PICTURE S9(8).
               10  DTOP1-IO.
                   15  DTOP1               PICTURE S9(6).
               10  DTOP18-IO.
                   15  DTOP18              PICTURE S9(8).
               10  DTOP2-IO.
                   15  DTOP2               PICTURE S9(6).
               10  DTOP28-IO.
                   15  DTOP28              PICTURE S9(8).
               10  DTOP3-IO.
                   15  DTOP3               PICTURE S9(6).
               10  DTOP38-IO.
                   15  DTOP38              PICTURE S9(8).
               10  PERN2-IO.
                   15  PERN2               PICTURE S9(2).
               10  PERN3-IO.
                   15  PERN3               PICTURE S9(3).
               10  TABDT8                  PICTURE X(8).
               10  TABDT2                  PICTURE X(2).
               10  TABDT4                  PICTURE X(4).
               10  PERN1-IO.
                   15  PERN1               PICTURE S9(2).
               10  VEND1-IO.
                   15  VEND1               PICTURE S9(4).
               10  VEND2-IO.
                   15  VEND2               PICTURE S9(2).
               10  VEND3-IO.
                   15  VEND3               PICTURE S9(2).
               10  VEND4-IO.
                   15  VEND4               PICTURE S9(2).
               10  VEND5-IO.
                   15  VEND5               PICTURE S9(4).
               10  VEND6-IO.
                   15  VEND6               PICTURE S9(6).
               10  SNUDTO-IO.
                   15  SNUDTO              PICTURE S9(6).
               10  SNUDT8-IO.
                   15  SNUDT8              PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  EDIT-DPER8              PICTURE Z99.99.9999.
               10  EDIT-DSPER8             PICTURE Z9999.99.99.
               10  EDIT-UPERD8             PICTURE Z99.99.9999.
               10  EDIT-USPER8             PICTURE Z9999.99.99.
               10  EDIT-INKPE8             PICTURE Z9999.99.99.
               10  EDIT-INK158             PICTURE Z9999.99.99.
               10  EDIT-UINK8              PICTURE Z9999.99.99.
               10  EDIT-UIN158             PICTURE Z9999.99.99.
               10  EDIT-DTOP18             PICTURE Z9999.99.99.
               10  EDIT-DTOP28             PICTURE Z9999.99.99.
               10  EDIT-DTOP38             PICTURE Z9999.99.99.
               10  EDIT-SLTDT8             PICTURE Z9999.99.99.
               10  EDIT-FF308              PICTURE Z9999.99.99.
               10  EDIT-FF608              PICTURE Z9999.99.99.
               10  EDIT-FF908              PICTURE Z9999.99.99.
               10  EDIT-FF1808             PICTURE Z9999.99.99.
               10  EDIT-UF308              PICTURE Z9999.99.99.
               10  EDIT-UF608              PICTURE Z9999.99.99.
               10  EDIT-UF908              PICTURE Z9999.99.99.
               10  EDIT-UF1808             PICTURE Z9999.99.99.
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KORT-PROCESS
               SET KORT-PROCESS-OFF        TO TRUE
               SET KORT-READ               TO TRUE
           END-IF
 
           IF  KORT-READ
           AND RECORD-SELECTED-OFF
               PERFORM KORT-GET
               SET KORT-READ-OFF           TO TRUE
               IF  NOT KORT-EOF
                   PERFORM KORT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET KORT-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RESPAR-PROCESS
               SET RESPAR-PROCESS-OFF      TO TRUE
               SET RESPAR-READ             TO TRUE
           END-IF
 
           IF  RESPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESPAR-GET
               SET RESPAR-READ-OFF         TO TRUE
               IF  NOT RESPAR-EOF
                   SET RESPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KORT-PROCESS
               PERFORM KORT-IDSET
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
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
 
           IF  KORT-PROCESS
               PERFORM KORT-FLDSET
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
           IF  (I-02)
               GO TO END-X-T
      *
           END-IF
           IF  (I-01)
               SET NOT-I-16                TO TRUE
               IF  PEROMG < 01
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  PEROMG > 24
                   SET I-16                TO TRUE
               END-IF
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  SLTDAG < '01'
               SET I-17                    TO TRUE
           END-IF
           IF  (NOT-I-17)
               SET NOT-I-17                TO TRUE
               IF  SLTDAG > '31'
                   SET I-17                TO TRUE
               END-IF
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  SLTMND < '01'
               SET I-18                    TO TRUE
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  SLTMND > '12'
                   SET I-18                TO TRUE
               END-IF
      *          VAKDAG    COMP "01"                   19
      * N19      VAKDAG    COMP "31"                 19
      *          VAKMND    COMP "01"                   20
      * N20      VAKMND    COMP "12"                 20
           END-IF
           IF  (I-16)
               GO TO END-X-T
      **                                                                **
      **  ER DET SISTE INKASSO-KJØRING I MND.
      **
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PEROMG = 02
               SET I-24                    TO TRUE
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 04
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 06
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 08
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 10
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 12
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 14
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 16
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 18
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 20
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 22
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  PEROMG = 24
                   SET I-24                TO TRUE
               END-IF
      **                                                                **
      **  BEREGNING AV INKASSO-PERIODER.
      **
           END-IF
           MULTIPLY 2 BY PEROMG        GIVING PERNR
           ADD 1 TO PERNR              GIVING UPERNR
           SET NOT-I-80                    TO TRUE
           IF  UPERNR > 48
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               SUBTRACT 48                 FROM UPERNR
               ADD 1 TO PERA-ELGR      GIVING UPERA-ELGR
           END-IF
           IF  (NOT-I-80)
               ADD PERA-ELGR TO ZERO   GIVING UPERA-ELGR
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  UPERA-ELGR > 80
               SET I-51                    TO TRUE
           END-IF
           MOVE UPERA-ELGR                 TO UPERA-ELG8 (3:2)
           IF  (I-51)
               MOVE '19'                   TO UPERA-ELG8 (1:2)
           END-IF
           IF  (NOT-I-51)
               MOVE '20'                   TO UPERA-ELG8 (1:2)
      *                    MOVE "UPERÅR  "BUGFL1  8        LEDETXT DEBUG
      *          BUGFL1    DEBUGBUGFILO   UPERÅR           VIS FELT/IND
      *
           END-IF
           MOVE OMGNR                      TO PERIOD-IO
           MOVE PERNR                      TO PERIOD-IO (3:2)
           MULTIPLY UPERA-ELGR BY 100  GIVING UPER
           MOVE UPERNR                     TO UPER-IO (3:2)
      **
      **  BEREGNING AV PERIODE-DATOER FOR UKENTLIG INKASSO.
      **
           SET NOT-I-21                    TO TRUE
           SET TABNR-S                     TO TABNR-I
           PERFORM WITH TEST AFTER
                   VARYING TABNR-I FROM 1 BY 1
                     UNTIL TABNR-I >= TABNR-MAX
                        OR I-21
               IF  UPERNR = TABNR (TABNR-I)
                   SET I-21                TO TRUE
                   SET TABNR-S             TO TABNR-I
               END-IF
           END-PERFORM
           SET TABNR-I                     TO TABNR-S
           IF  I-21
           AND TABNR-I NOT > TABSDT-MAX
               SET TABSDT-I                TO TABNR-I
           END-IF
           IF  (I-21)
               MOVE TABSDT(TABSDT-I)       TO TABUDT
           END-IF
           MOVE UPER                       TO PERNUM-IO
           PERFORM DTOFOR-S
           IF  (I-90)
               MOVE TABDTO(TABDTO-I)       TO UPERD-IO
               MOVE TABDT8                 TO UPERD8-IO
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO USPERD-IO
               MOVE SNUDT8                 TO USPER8-IO
      **
           END-IF
           ADD 2                           TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO UINK15-IO
               MOVE SNUDT8                 TO UIN158-IO
      **
           END-IF
           ADD 2                           TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO UINK-IO
               MOVE SNUDT8                 TO UINK8-IO
      **
      **  BEREGNING AV DATO"ER PR 15. OG SISTE.
      **
           END-IF
           SET NOT-I-21                    TO TRUE
           SET TABNR-S                     TO TABNR-I
           PERFORM WITH TEST AFTER
                   VARYING TABNR-I FROM 1 BY 1
                     UNTIL TABNR-I >= TABNR-MAX
                        OR I-21
               IF  PERNR = TABNR (TABNR-I)
                   SET I-21                TO TRUE
                   SET TABNR-S             TO TABNR-I
               END-IF
           END-PERFORM
           SET TABNR-I                     TO TABNR-S
           IF  I-21
           AND TABNR-I NOT > TABSDT-MAX
               SET TABSDT-I                TO TABNR-I
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           PERFORM DTOFOR-S
           IF  (I-90)
               MOVE TABDTO(TABDTO-I)       TO DPER-IO
               MOVE TABDT8                 TO DPER8-IO
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO DSPER-IO
               MOVE SNUDT8                 TO DSPER8-IO
               SET NOT-I-22                TO TRUE
               IF  SLTDT8 < SNUDT8
                   SET I-22                TO TRUE
               END-IF
           END-IF
           MOVE PERNR                      TO PERANT-IO
           SUBTRACT 4                      FROM PERANT
           PERFORM DTOBAK-S
           IF  (I-91)
               PERFORM SNU-S
           END-IF
           IF  (I-91)
               MOVE SNUDTO                 TO FF30-IO
               MOVE SNUDT8                 TO FF308-IO
           END-IF
           MOVE PERNR                      TO PERANT-IO
           SUBTRACT 8                      FROM PERANT
           PERFORM DTOBAK-S
           IF  (I-91)
               PERFORM SNU-S
           END-IF
           IF  (I-91)
               MOVE SNUDTO                 TO FF60-IO
               MOVE SNUDT8                 TO FF608-IO
           END-IF
           MOVE PERNR                      TO PERANT-IO
           SUBTRACT 12                     FROM PERANT
           PERFORM DTOBAK-S
           IF  (I-91)
               PERFORM SNU-S
           END-IF
           IF  (I-91)
               MOVE SNUDTO                 TO FF90-IO
               MOVE SNUDT8                 TO FF908-IO
           END-IF
           MOVE PERNR                      TO PERANT-IO
           SUBTRACT 24                     FROM PERANT
           PERFORM DTOBAK-S
           IF  (I-91)
               PERFORM SNU-S
           END-IF
           IF  (I-91)
               MOVE SNUDTO                 TO FF180-IO
               MOVE SNUDT8                 TO FF1808-IO
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           ADD 2                           TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO UF15-IO
               MOVE SNUDT8                 TO UF158-IO
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           ADD 4                           TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO UF30-IO
               MOVE SNUDT8                 TO UF308-IO
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           ADD 8                           TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO UF60-IO
               MOVE SNUDT8                 TO UF608-IO
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           ADD 12                          TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO UF90-IO
               MOVE SNUDT8                 TO UF908-IO
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           ADD 24                          TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO UF180-IO
               MOVE SNUDT8                 TO UF1808-IO
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           ADD 2                           TO PERNUM
           PERFORM DTOFOR-S
           IF  (I-90)
               PERFORM SNU-S
           END-IF
           IF  (I-90)
               MOVE SNUDTO                 TO NSPER-IO
           END-IF
           MOVE UF15                       TO INK15-IO
           MOVE UF158                      TO INK158-IO
           MOVE UF30                       TO INKPER-IO
           MOVE UF308                      TO INKPE8-IO
           MOVE DSPER                      TO DTOP1-IO
           MOVE DSPER8                     TO DTOP18-IO
           MOVE PERNR                      TO PERANT-IO
           SUBTRACT 2                      FROM PERANT
           PERFORM DTOBAK-S
           IF  (I-91)
               PERFORM SNU-S
           END-IF
           IF  (I-91)
               MOVE SNUDTO                 TO DTOP2-IO
               MOVE SNUDT8                 TO DTOP28-IO
           END-IF
           MOVE FF30                       TO DTOP3-IO
           MOVE FF308                      TO DTOP38-IO
           MOVE PERNR                      TO PERANT-IO
           SUBTRACT 6                      FROM PERANT
           PERFORM DTOBAK-S
           IF  (I-91)
               PERFORM SNU-S
      *  91                MOVE SNUDTO    DTOP4   60
      *  91                MOVE SNUDT8    DTOP48  80
           END-IF
           MOVE PERNR                      TO PERANT-IO
           SUBTRACT 10                     FROM PERANT
           PERFORM DTOBAK-S.
 
       END-X-T.
      *****************************************************************
           CONTINUE.
 
       DTOFOR-S SECTION.
       DTOFOR-S-P.
           MOVE PERNUM (3:2)               TO PERN2-IO
           SUBTRACT 48 FROM PERN2      GIVING PERN3
           SET NOT-I-30                    TO TRUE
           SET NOT-I-31                    TO TRUE
           IF  PERN3 > 0
               SET I-30                    TO TRUE
           END-IF
           IF  PERN3 = 0
               SET I-31                    TO TRUE
           END-IF
           IF  (I-30 AND NOT-I-31)
               ADD 100                     TO PERN3
               SUBTRACT PERN2              FROM PERNUM
               ADD PERN3                   TO PERNUM
           END-IF
           SET NOT-I-90                    TO TRUE
           SET TABPER-S                    TO TABPER-I
           PERFORM WITH TEST AFTER
                   VARYING TABPER-I FROM 1 BY 1
                     UNTIL TABPER-I >= TABPER-MAX
                        OR I-90
               IF  PERNUM = TABPER (TABPER-I)
                   SET I-90                TO TRUE
                   SET TABPER-S            TO TABPER-I
               END-IF
           END-PERFORM
           SET TABPER-I                    TO TABPER-S
           IF  I-90
           AND TABPER-I NOT > TABDTO-MAX
               SET TABDTO-I                TO TABPER-I
           END-IF
           IF  (I-90)
               MOVE TABDTO(TABDTO-I)       TO TABDT8 (1:6)
               MOVE TABDTO(TABDTO-I) (5:2)  TO TABDT2
               SET NOT-I-52                TO TRUE
               IF  TABDT2 > '80'
                   SET I-52                TO TRUE
               END-IF
               MOVE TABDT2                 TO TABDT4 (3:2)
           END-IF
           IF  (I-90 AND I-52)
               MOVE '19'                   TO TABDT4 (1:2)
           END-IF
           IF  (I-90 AND NOT-I-52)
               MOVE '20'                   TO TABDT4 (1:2)
           END-IF
           IF  (I-90)
               MOVE TABDT4                 TO TABDT8 (5:4)
           END-IF.
      *****************************************************************
 
       DTOBAK-S SECTION.
       DTOBAK-S-P.
           MOVE PERA-ELGR                  TO PERN1-IO
           SET NOT-I-20                    TO TRUE
           IF  PERANT NOT > 0
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               ADD 100                     TO PERANT
               SUBTRACT 1                  FROM PERN1
               SET NOT-I-53                TO TRUE
               IF  PERN1 < 0
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-53)
               ADD 100                     TO PERN1
           END-IF
           IF  (I-20)
               ADD 48                      TO PERANT
           END-IF
           MOVE PERANT                     TO PERNUM-IO (3:2)
           MOVE PERN1                      TO PERNUM (1:2)
           SET NOT-I-91                    TO TRUE
           SET TABPER-S                    TO TABPER-I
           PERFORM WITH TEST AFTER
                   VARYING TABPER-I FROM 1 BY 1
                     UNTIL TABPER-I >= TABPER-MAX
                        OR I-91
               IF  PERNUM = TABPER (TABPER-I)
                   SET I-91                TO TRUE
                   SET TABPER-S            TO TABPER-I
               END-IF
           END-PERFORM
           SET TABPER-I                    TO TABPER-S
           IF  I-91
           AND TABPER-I NOT > TABDTO-MAX
               SET TABDTO-I                TO TABPER-I
           END-IF.
      *****************************************************************
 
       SNU-S SECTION.
       SNU-S-P.
           MOVE TABDTO(TABDTO-I) (3:4)     TO VEND1-IO
           MOVE VEND1 (1:2)                TO VEND2
           MOVE VEND1 (3:2)                TO VEND3-IO
           SET NOT-I-50                    TO TRUE
           IF  VEND3 > 80
               SET I-50                    TO TRUE
           END-IF
           MOVE TABDTO(TABDTO-I) (1:2)     TO VEND4
           MOVE VEND4                      TO VEND5-IO (3:2)
           MOVE VEND2                      TO VEND5 (1:2)
           MOVE VEND5                      TO VEND6-IO (3:4)
           MOVE VEND3                      TO VEND6 (1:2)
           MOVE VEND6                      TO SNUDTO-IO
           MOVE VEND6                      TO SNUDT8-IO (3:6)
           IF  (I-50)
               MOVE '19'                   TO SNUDT8 (1:2)
           END-IF
           IF  (NOT-I-50)
               MOVE '20'                   TO SNUDT8 (1:2)
           END-IF.
      *****************************************************************
 
       KORT-GET SECTION.
       KORT-GET-P.
           IF  KORT-EOF-OFF
               READ KORT
               AT END
                   SET KORT-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORT-FLDSET SECTION.
       KORT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '9'
            AND   KORT-IO-AREA (2:1) = '0' )
               MOVE KORT-IO-AREA (19:4)    TO OMGNR-IO
               INSPECT OMGNR-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (38:6)    TO SLTDTO-IO
               INSPECT SLTDTO-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (36:8)    TO SLTDT8-IO
               INSPECT SLTDT8-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (19:2)    TO PERA-ELGR-IO
               INSPECT PERA-ELGR-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (17:4)    TO PERA-ELGR8-IO
               INSPECT PERA-ELGR8-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (21:2)    TO PEROMG-IO
               INSPECT PEROMG-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (42:2)    TO SLTDAG (1:2)
               MOVE KORT-IO-AREA (40:2)    TO SLTMND (1:2)
           END-EVALUATE.
 
       KORT-IDCHK SECTION.
       KORT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '9'
            AND   KORT-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KORT-IDSET SECTION.
       KORT-IDSET-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '9'
            AND   KORT-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       RESPAR-GET SECTION.
       RESPAR-GET-P.
           IF  RESPAR-EOF-OFF
               READ RESPAR
               AT END
                   SET RESPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESPAR-IDSET SECTION.
       RESPAR-IDSET-P.
           SET I-02                        TO TRUE.
 
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
               MOVE 7                      TO LISTE-AFTER-SKIP
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
 
       TFILE-LOAD SECTION.
       TFILE-LOAD-P.
           OPEN INPUT TFILE
           SET TABPER-I                    TO 1
           PERFORM UNTIL TFILE-EOF
               READ TFILE
               AT END
                   SET TFILE-EOF           TO TRUE
               NOT AT END
                   MOVE TFILE-IO-AREA (1:10) TO TABPER-ENTRY (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE TFILE-IO-AREA (11:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE TFILE-IO-AREA (21:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE TFILE-IO-AREA (31:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE TFILE-IO-AREA (41:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE TFILE-IO-AREA (51:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE TFILE-IO-AREA (61:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE TFILE-IO-AREA (71:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TFILE.
 
       TFILE1-LOAD SECTION.
       TFILE1-LOAD-P.
           OPEN INPUT TFILE1
           SET TABNR-I                     TO 1
           PERFORM UNTIL TFILE1-EOF
               READ TFILE1
               AT END
                   SET TFILE1-EOF          TO TRUE
               NOT AT END
                   MOVE TFILE1-IO-AREA (1:17) TO TABNR-ENTRY (TABNR-I)
                   SET TABNR-I             UP BY 1
                   MOVE TFILE1-IO-AREA (18:17) TO TABNR-ENTRY (TABNR-I)
                   SET TABNR-I             UP BY 1
                   MOVE TFILE1-IO-AREA (35:17) TO TABNR-ENTRY (TABNR-I)
                   SET TABNR-I             UP BY 1
                   MOVE TFILE1-IO-AREA (52:17) TO TABNR-ENTRY (TABNR-I)
                   SET TABNR-I             UP BY 1
               END-READ
           END-PERFORM
           CLOSE TFILE1.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PERIODE NR/ÅR'        TO LISTE-IO-AREA (5:13)
               MOVE PERNR-IO               TO LISTE-IO-AREA (24:2)
               MOVE '.'                    TO LISTE-IO-AREA (26:1)
               MOVE PERA-ELGR-IO           TO LISTE-IO-AREA (27:2)
               MOVE PERA-ELGR8-IO          TO LISTE-IO-AREA (27:4)
               IF  (I-24 AND NOT-I-16)
                   MOVE 'SISTE INKASSO DENNE MND.' TO LISTE-IO-AREA
                                                               (51:24)
               END-IF
               IF  (I-16)
                   MOVE 'UGYLDIG PERIODE  PARAMET' TO LISTE-IO-AREA
                                                               (51:24)
               END-IF
               IF  (I-16)
                   MOVE 'ERKORT MÅ RETTES OG RES.' TO LISTE-IO-AREA
                                                               (75:24)
               END-IF
               IF  (I-16)
                   MOVE '001 MÅ KJØRES OM' TO LISTE-IO-AREA (99:16)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* UKENTLIG * '        TO LISTE-IO-AREA (5:13)
               MOVE UPERNR-IO              TO LISTE-IO-AREA (24:2)
               MOVE '.'                    TO LISTE-IO-AREA (26:1)
               MOVE UPERA-ELGR-IO          TO LISTE-IO-AREA (27:2)
               MOVE UPERA-ELG8             TO LISTE-IO-AREA (27:4)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PERIODE DATO'         TO LISTE-IO-AREA (5:12)
               MOVE DPER                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (23:8)
               MOVE DPER8                  TO EDIT-DPER8
               MOVE EDIT-DPER8             TO LISTE-IO-AREA (20:11)
               MOVE DSPER                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE DSPER8                 TO EDIT-DSPER8
               MOVE EDIT-DSPER8            TO LISTE-IO-AREA (35:11)
               MOVE TABSDT (TABSDT-I)      TO LISTE-IO-AREA (50:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* UKENTLIG *'         TO LISTE-IO-AREA (5:12)
               MOVE UPERD                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (23:8)
               MOVE UPERD8                 TO EDIT-UPERD8
               MOVE EDIT-UPERD8            TO LISTE-IO-AREA (20:11)
               MOVE USPERD                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE USPER8                 TO EDIT-USPER8
               MOVE EDIT-USPER8            TO LISTE-IO-AREA (35:11)
               MOVE TABUDT                 TO LISTE-IO-AREA (50:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INNKASSO'             TO LISTE-IO-AREA (5:8)
               MOVE 'FORFALLSDATO'         TO LISTE-IO-AREA (14:12)
               MOVE INKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE INKPE8                 TO EDIT-INKPE8
               MOVE EDIT-INKPE8            TO LISTE-IO-AREA (35:11)
               MOVE 'INKASSO FORFALLSDATO 15' TO LISTE-IO-AREA (49:23)
               MOVE ' DAGER.'              TO LISTE-IO-AREA (72:7)
               MOVE INK15                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (86:8)
               MOVE INK158                 TO EDIT-INK158
               MOVE EDIT-INK158            TO LISTE-IO-AREA (83:11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* UKENTLIG *'         TO LISTE-IO-AREA (14:12)
               MOVE UINK                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE UINK8                  TO EDIT-UINK8
               MOVE EDIT-UINK8             TO LISTE-IO-AREA (35:11)
               MOVE '* UKENTLIG *'         TO LISTE-IO-AREA (66:12)
               MOVE UINK15                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (86:8)
               MOVE UIN158                 TO EDIT-UIN158
               MOVE EDIT-UIN158            TO LISTE-IO-AREA (83:11)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PURRE DATO'           TO LISTE-IO-AREA (5:10)
               MOVE '1. GRAD'              TO LISTE-IO-AREA (24:7)
               MOVE DTOP1                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE DTOP18                 TO EDIT-DTOP18
               MOVE EDIT-DTOP18            TO LISTE-IO-AREA (35:11)
               MOVE '2. GRAD'              TO LISTE-IO-AREA (49:7)
               MOVE DTOP2                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (61:8)
               MOVE DTOP28                 TO EDIT-DTOP28
               MOVE EDIT-DTOP28            TO LISTE-IO-AREA (58:11)
               MOVE '3. GRAD'              TO LISTE-IO-AREA (74:7)
               MOVE DTOP3                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (86:8)
               MOVE DTOP38                 TO EDIT-DTOP38
               MOVE EDIT-DTOP38            TO LISTE-IO-AREA (83:11)
      *                                 110 "4. GRAD KUN ASA"
      *                        DTOP4 Y  120
      *                        DTOP48   120 "0    .  .  "
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PERIODENS AVSLUTNINGS' TO LISTE-IO-AREA (5:21)
               MOVE 'DATO'                 TO LISTE-IO-AREA (27:4)
               MOVE SLTDTO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE SLTDT8                 TO EDIT-SLTDT8
               MOVE EDIT-SLTDT8            TO LISTE-IO-AREA (35:11)
               IF  (I-17)
                   MOVE 'UGYLDIG DAG/MÅNED I AVSL' TO LISTE-IO-AREA
                                                               (51:24)
               END-IF
               IF  (I-17)
                   MOVE 'UTNINGS DATO I PARAMETER' TO LISTE-IO-AREA
                                                               (75:24)
               END-IF
               IF  (I-17)
                   MOVE 'KORT RES 001 MÅ KJØRES' TO LISTE-IO-AREA
                                                               (99:22)
               END-IF
               IF  (I-17)
                   MOVE 'OM'               TO LISTE-IO-AREA (121:2)
               END-IF
               IF  (I-22 AND NOT-I-17)
                   MOVE '** FEIL **          AVSL' TO LISTE-IO-AREA
                                                               (51:24)
               END-IF
               IF  (I-22 AND NOT-I-17)
                   MOVE 'UTNINGS DATO ER LAVERE E' TO LISTE-IO-AREA
                                                               (75:24)
               END-IF
               IF  (I-22 AND NOT-I-17)
                   MOVE 'NN PERIODEDATO.  KJØR ' TO LISTE-IO-AREA
                                                               (99:22)
               END-IF
               IF  (I-22 AND NOT-I-17)
                   MOVE 'OM'               TO LISTE-IO-AREA (121:2)
      *       D  2     02
      *                                  23 "VEKSEL FORFALLSDATO"
      *                        VAKDTOY   43
      *                      19          75 "UGYDIG DAG/MÅNED I VEKSE"
      *                      19          89 "L FORFALLSDATO"
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALLS DAGER'       TO LISTE-IO-AREA (5:14)
               MOVE '30 DAGER'             TO LISTE-IO-AREA (24:8)
               MOVE FF30                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE FF308                  TO EDIT-FF308
               MOVE EDIT-FF308             TO LISTE-IO-AREA (35:11)
               MOVE '60 DAGER'             TO LISTE-IO-AREA (49:8)
               MOVE FF60                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (61:8)
               MOVE FF608                  TO EDIT-FF608
               MOVE EDIT-FF608             TO LISTE-IO-AREA (58:11)
               MOVE '90 DAGER'             TO LISTE-IO-AREA (74:8)
               MOVE FF90                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (86:8)
               MOVE FF908                  TO EDIT-FF908
               MOVE EDIT-FF908             TO LISTE-IO-AREA (83:11)
               MOVE '180 DAGER'            TO LISTE-IO-AREA (99:9)
               MOVE FF180                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (112:8)
               MOVE FF1808                 TO EDIT-FF1808
               MOVE EDIT-FF1808            TO LISTE-IO-AREA (109:11)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'UFORFALTE DAGER'      TO LISTE-IO-AREA (5:15)
               MOVE '30 DAGER'             TO LISTE-IO-AREA (24:8)
               MOVE UF30                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (36:8)
               MOVE UF308                  TO EDIT-UF308
               MOVE EDIT-UF308             TO LISTE-IO-AREA (35:11)
               MOVE '60 DAGER'             TO LISTE-IO-AREA (49:8)
               MOVE UF60                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (61:8)
               MOVE UF608                  TO EDIT-UF608
               MOVE EDIT-UF608             TO LISTE-IO-AREA (58:11)
               MOVE '90 DAGER'             TO LISTE-IO-AREA (74:8)
               MOVE UF90                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (86:8)
               MOVE UF908                  TO EDIT-UF908
               MOVE EDIT-UF908             TO LISTE-IO-AREA (83:11)
               MOVE '180 DAGER'            TO LISTE-IO-AREA (99:9)
               MOVE UF180                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (112:8)
               MOVE UF1808                 TO EDIT-UF1808
               MOVE EDIT-UF1808            TO LISTE-IO-AREA (109:11)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** UKENTLIGE PERIODER S' TO LISTE-IO-AREA (5:24)
               MOVE 'KAL VÆRE 8 DAGER SENERE ' TO LISTE-IO-AREA (29:24)
               MOVE 'ENN PERIODE-DATOER  **  ' TO LISTE-IO-AREA (53:24)
               MOVE 'RELATERINGS-POSTER ELDRE' TO LISTE-IO-AREA (77:24)
               MOVE ' ENN 3 MNDER FJERNES ***' TO LISTE-IO-AREA
                                                              (101:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '........................' TO LISTE-IO-AREA (77:24)
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GODKJENT'             TO LISTE-IO-AREA (85:8)
               MOVE 01                     TO LISTE-AFTER-SKIP
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02)
               MOVE '90'                   TO RESPAR-IO-AREA (1:2)
               MOVE PERNR-IO               TO RESPAR-IO-AREA (3:2)
               MOVE PERA-ELGR-IO           TO RESPAR-IO-AREA (5:2)
               MOVE DPER-IO                TO RESPAR-IO-AREA (7:6)
               MOVE DSPER-IO               TO RESPAR-IO-AREA (13:6)
               MOVE TABSDT (TABSDT-I)      TO RESPAR-IO-AREA (19:15)
               MOVE FF30-IO                TO RESPAR-IO-AREA (34:6)
               MOVE FF60-IO                TO RESPAR-IO-AREA (40:6)
               MOVE FF90-IO                TO RESPAR-IO-AREA (46:6)
               MOVE FF180-IO               TO RESPAR-IO-AREA (52:6)
               MOVE UF30-IO                TO RESPAR-IO-AREA (58:6)
               MOVE UF60-IO                TO RESPAR-IO-AREA (64:6)
               MOVE UF90-IO                TO RESPAR-IO-AREA (70:6)
               MOVE UF180-IO               TO RESPAR-IO-AREA (76:6)
               MOVE NSPER-IO               TO RESPAR-IO-AREA (82:6)
      *                        VAKDTOX   93
               MOVE '      '               TO RESPAR-IO-AREA (88:6)
               MOVE INKPER-IO              TO RESPAR-IO-AREA (94:6)
               MOVE DTOP1-IO               TO RESPAR-IO-AREA (101:6)
               MOVE DTOP2-IO               TO RESPAR-IO-AREA (107:6)
               MOVE DTOP3-IO               TO RESPAR-IO-AREA (113:6)
               MOVE SLTDTO-IO              TO RESPAR-IO-AREA (119:6)
               MOVE INK15-IO               TO RESPAR-IO-AREA (137:6)
      *                        DTOP4 X  148
               MOVE '      '               TO RESPAR-IO-AREA (143:6)
               MOVE UPERNR-IO              TO RESPAR-IO-AREA (155:2)
               MOVE UPERA-ELGR-IO          TO RESPAR-IO-AREA (157:2)
               MOVE UPERD-IO               TO RESPAR-IO-AREA (159:6)
               MOVE USPERD-IO              TO RESPAR-IO-AREA (165:6)
               MOVE UINK-IO                TO RESPAR-IO-AREA (171:6)
               MOVE UINK15-IO              TO RESPAR-IO-AREA (177:6)
               MOVE TABUDT                 TO RESPAR-IO-AREA (183:15)
               IF  (I-24)
                   MOVE 'M'                TO RESPAR-IO-AREA (200:1)
               END-IF
               IF  (NOT-I-24)
                   MOVE ' '                TO RESPAR-IO-AREA (200:1)
               END-IF
               REWRITE RESPAR-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'R E S K O N T R O  P A R' TO LISTE-IO-AREA (11:24)
               MOVE 'A M E T E R F I L E  K O' TO LISTE-IO-AREA (36:24)
               MOVE 'R R I G E R T'        TO LISTE-IO-AREA (61:13)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 'RES 001.'             TO LISTE-IO-AREA (110:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
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
           INITIALIZE KORT-DATA-FIELDS
           SET KORT-EOF-OFF                TO TRUE
           SET KORT-PROCESS                TO TRUE
           OPEN INPUT KORT
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN I-O RESPAR
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           PERFORM TFILE-LOAD
           PERFORM TFILE1-LOAD.
           SET TABPER-I                    TO 1
           SET TABNR-I                     TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KORT
           CLOSE RESPAR
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
