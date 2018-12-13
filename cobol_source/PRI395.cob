       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI395R.
      **********************************************  Z-WIN-RPG2   ****
      *  UTPLUKK TIL PRISLISTER(TRYKKING), MED NYE PRISER **
      *  DERSOM FLERE PRISTRANSER PÅ SAMME ARTIKKEL, BLIR **
      *  PRIS HENTET FRA SISTE TRANS MED UTSALGSPRIS.     **
      *  030915 LAGT TIL KAMPPR PÅ UTFIL               RE **
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI395.rpg
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
           SELECT PRISF
               ASSIGN TO UT-S-PRISF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRISF-STATUS.
           SELECT VAREMAS
               ASSIGN TO UT-S-VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT TABVGR
               ASSIGN TO UT-S-TABVGR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABVGR-STATUS.
           SELECT TABALF
               ASSIGN TO UT-S-TABALF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABALF-STATUS.
           SELECT TABVGA
               ASSIGN TO UT-S-TABVGA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABVGA-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT PRTFILE
               ASSIGN TO UT-S-PRTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PRISF
               BLOCK CONTAINS 4080
               RECORD CONTAINS 85.
       01  PRISF-IO-AREA.
           05  PRISF-IO-AREA-X             PICTURE X(85).
       FD VAREMAS
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD TABVGR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABVGR-IO-AREA.
           05  TABVGR-IO-AREA-X            PICTURE X(80).
       FD TABALF
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABALF-IO-AREA.
           05  TABALF-IO-AREA-X            PICTURE X(80).
       FD TABVGA
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABVGA-IO-AREA.
           05  TABVGA-IO-AREA-X            PICTURE X(80).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD PRTFILE
               BLOCK CONTAINS 4200
               RECORD CONTAINS 120.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(120).
       WORKING-STORAGE SECTION.
       77  TABA-MAX   VALUE 150            PICTURE 9(4) USAGE BINARY.
       77  TABB-MAX   VALUE 150            PICTURE 9(4) USAGE BINARY.
       77  TABC-MAX   VALUE 600            PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABA-TABLE.
               10  TABA-ENTRY
                                           OCCURS 150 TIMES
                                           INDEXED BY TABA-I
                                                      TABA-S.
                   15  TABA                PICTURE X(5).
           05  TABB-TABLE.
               10  TABB-ENTRY
                                           OCCURS 150 TIMES
                                           INDEXED BY TABB-I
                                                      TABB-S.
                   15  TABB                PICTURE X(3).
           05  TABC-TABLE.
               10  TABC-ENTRY
                                           OCCURS 600 TIMES
                                           INDEXED BY TABC-I
                                                      TABC-S.
                   15  TABC                PICTURE X(8).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PRISF-STATUS                PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  TABVGR-STATUS               PICTURE 99 VALUE 0.
           10  TABALF-STATUS               PICTURE 99 VALUE 0.
           10  TABVGA-STATUS               PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISF-EOF-OFF           VALUE '0'.
               88  PRISF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISF-READ-OFF          VALUE '0'.
               88  PRISF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISF-PROCESS-OFF       VALUE '0'.
               88  PRISF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PRISF-LEVEL-INIT-OFF    VALUE '0'.
               88  PRISF-LEVEL-INIT        VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREMAS-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TABVGR-EOF-OFF          VALUE '0'.
               88  TABVGR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TABALF-EOF-OFF          VALUE '0'.
               88  TABALF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TABVGA-EOF-OFF          VALUE '0'.
               88  TABVGA-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  PRISF-LEVEL-02.
               10  PRISF-02-L2.
                   15  PRISF-02-L2-FNR     PICTURE X(3).
               10  PRISF-02-L1.
                   15  PRISF-02-L1-EDBNR   PICTURE X(7).
           05  PRISF-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  VGR                     PICTURE X(5).
               10  VAVD                    PICTURE X(1).
               10  ALFA                    PICTURE X(3).
               10  EDATO                   PICTURE X(6).
               10  NYPRIS-IO.
                   15  NYPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  PRISF-MP                    PICTURE X(10).
           05  PRISF-MC                    PICTURE X(10).
           05  PRISF-M-02              REDEFINES PRISF-MC.
               10  PRISF-M-02-M2.
                   15  PRISF-M-02-M2-FNR-G.
                       20  PRISF-M-02-M2-FNR PICTURE X(3).
               10  PRISF-M-02-M1.
                   15  PRISF-M-02-M1-EDBNR-G.
                       20  PRISF-M-02-M1-EDBNR PICTURE X(7).
           05  VAREMAS-LEVEL-03.
               10  VAREMAS-03-L2.
                   15  VAREMAS-03-L2-FNR   PICTURE X(3).
               10  VAREMAS-03-L1.
                   15  VAREMAS-03-L1-EDBNR PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  ARTNR                   PICTURE X(20).
               10  VAREN                   PICTURE X(30).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  PEDATO-IO.
                   15  PEDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ALTEDB                  PICTURE X(7).
               10  NPRIS                   PICTURE X(1).
               10  PTEK                    PICTURE X(1).
               10  KINNH-IO.
                   15  KINNH               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  KAMPPR                  PICTURE X(1).
               10  MRK                     PICTURE X(1).
               10  SLETT                   PICTURE X(1).
               10  LOC                     PICTURE X(6).
               10  PTYPE                   PICTURE X(1).
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *
           05  VAREMAS-MP                  PICTURE X(10).
           05  VAREMAS-MC                  PICTURE X(10).
           05  VAREMAS-M-03            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-03-M2.
                   15  VAREMAS-M-03-M2-FNR-G.
                       20  VAREMAS-M-03-M2-FNR PICTURE X(3).
               10  VAREMAS-M-03-M1.
                   15  VAREMAS-M-03-M1-EDBNR-G.
                       20  VAREMAS-M-03-M1-EDBNR PICTURE X(7).
           05  PARAM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  TABELL                  PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  NYKUNN                  PICTURE X(1).
               10  PRTYP                   PICTURE X(1).
               10  UTGA-ELGR               PICTURE X(1).
               10  PDATO                   PICTURE X(6).
               10  SKJO-ELGRT              PICTURE X(6).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  SDATO                   PICTURE X(6).
               10  PDATO8                  PICTURE X(8).
               10  SKJO-ELGR8              PICTURE X(8).
               10  VGRALF                  PICTURE X(8).
               10  EDATO8                  PICTURE X(8).
               10  PRIS2-IO.
                   15  PRIS2               PICTURE S9(6)V9(2).
               10  PEDAT1                  PICTURE X(6).
               10  PEDATO-N-IO.
                   15  PEDATO-N            PICTURE S9(7).
               10  PEDAT8                  PICTURE X(8).
               10  DAG                     PICTURE X(2).
               10  DDMM                    PICTURE X(4).
               10  MM                      PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  AAR4                    PICTURE X(4).
               10  AAR8                    PICTURE X(8).
               10  MNDDD                   PICTURE X(4).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PRISF-PROCESS
               SET PRISF-PROCESS-OFF       TO TRUE
               SET PRISF-READ              TO TRUE
           END-IF
 
           IF  PRISF-READ
               PERFORM PRISF-GET
               SET PRISF-READ-OFF          TO TRUE
               IF  NOT PRISF-EOF
                   PERFORM PRISF-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   PERFORM VAREMAS-MATCH-SET
               END-IF
           END-IF
 
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
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  PRISF-PROCESS
               PERFORM PRISF-IDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  PRISF-PROCESS
               PERFORM PRISF-CHK-LEVEL
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-CHK-LEVEL
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
 
           IF  PRISF-PROCESS
               PERFORM PRISF-FLDOFF
               PERFORM PRISF-FLDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDOFF
               PERFORM PARAM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  PRISF-PROCESS
           OR  VAREMAS-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-30                    TO TRUE
           IF  (I-L1)
               SET NOT-I-93                TO TRUE
               SET NOT-I-85                TO TRUE
      *
      *  BESTEMMELSE AV UTPLUKKS-KRITERIER UTFRA PARAMETER.
      *
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-05)
               SET NOT-I-10                TO TRUE
               IF  TABELL = 'A'
                   SET I-10                TO TRUE
               END-IF
               SET NOT-I-11                TO TRUE
               IF  TABELL = 'B'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-14                TO TRUE
               IF  TABELL = 'C'
                   SET I-14                TO TRUE
               END-IF
               SET NOT-I-50                TO TRUE
               IF  AVD = ' '
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-15                TO TRUE
               IF  NYKUNN = 'J'
                   SET I-15                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           IF  (I-06)
               SET NOT-I-60                TO TRUE
               IF  PRTYP = ' '
                   SET I-60                TO TRUE
               END-IF
               SET NOT-I-94                TO TRUE
               IF  UTGA-ELGR = 'J'
                   SET I-94                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           IF  (I-07 AND NOT-I-70)
               MOVE PDATO                  TO SDATO
               PERFORM SNU-S
           END-IF
           IF  (I-07 AND NOT-I-70)
               MOVE SDATO                  TO PDATO
               MOVE AAR8                   TO PDATO8
           END-IF
           IF  (I-07 AND NOT-I-71)
               MOVE SKJO-ELGRT             TO SDATO
               PERFORM SNU-S
           END-IF
           IF  (I-07 AND NOT-I-71)
               MOVE SDATO                  TO SKJO-ELGRT
               MOVE AAR8                   TO SKJO-ELGR8
           END-IF
           IF  (I-07)
               GO TO SLUTT-T
      ****************************************************************
      *
      *  TEST OM FIRMA SKAL MED.
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-LR                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  FNR > FIRMA
                   SET I-LR                TO TRUE
               END-IF
               IF  FNR = FIRMA
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20)
               GO TO SLUTT-T
      *
      *  TEST OM ARTIKKEL SKAL MED.
      *
           END-IF
           IF  (I-02 AND NOT-I-MR)
               OR  (I-03 AND I-15 AND NOT-I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-09                TO TRUE
               IF  SLETT = 'S'
                   SET I-09                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-09)
               GO TO SLUTT-T
      *
      *
           END-IF
           IF  (I-03)
               SET NOT-I-09                TO TRUE
               IF  MRK = '5'
                   SET I-09                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-09)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-40                TO TRUE
               IF  EDBNR NOT < '9000000'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               GO TO SLUTT-T
      *
      **********   UTPLUKK PR. TABELL.  ***********
      *
           END-IF
           IF  (I-L1 AND I-10)
               SET NOT-I-12                TO TRUE
               SET TABA-S                  TO TABA-I
               PERFORM WITH TEST AFTER
                       VARYING TABA-I FROM 1 BY 1
                         UNTIL TABA-I >= TABA-MAX
                            OR I-12
                   IF  VGR = TABA (TABA-I)
                       SET I-12            TO TRUE
                       SET TABA-S          TO TABA-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-L1 AND I-11)
               SET NOT-I-13                TO TRUE
               SET TABB-S                  TO TABB-I
               PERFORM WITH TEST AFTER
                       VARYING TABB-I FROM 1 BY 1
                         UNTIL TABB-I >= TABB-MAX
                            OR I-13
                   IF  ALFA = TABB (TABB-I)
                       SET I-13            TO TRUE
                       SET TABB-S          TO TABB-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-L1 AND I-11 AND I-13)
               SET NOT-I-13                TO TRUE
               IF  ALFA NOT = '   '
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-14)
               MOVE VGR                    TO VGRALF (1:5)
               MOVE ALFA                   TO VGRALF (6:3)
               SET NOT-I-16                TO TRUE
               SET TABC-S                  TO TABC-I
               PERFORM WITH TEST AFTER
                       VARYING TABC-I FROM 1 BY 1
                         UNTIL TABC-I >= TABC-MAX
                            OR I-16
                   IF  VGRALF = TABC (TABC-I)
                       SET I-16            TO TRUE
                       SET TABC-S          TO TABC-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-10 AND NOT-I-12)
               OR  (I-11 AND NOT-I-13)
               OR  (I-14 AND NOT-I-16)
               GO TO SLUTT-T
           END-IF
           IF  (I-50)
               GO TO ENDAVD-T
           END-IF
           IF  (I-L1)
               SET NOT-I-55                TO TRUE
               IF  AVD = VAVD
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-55)
               GO TO SLUTT-T
           END-IF.
 
       ENDAVD-T.
      *
      ***UTPLUKK PÅ PRODTYPE  ( FØRSTE SIFFER I PRODUKT GRUPPE )
           IF  (I-60)
               GO TO ENDPT-T
           END-IF
           IF  (I-02)
               GO TO ENDPT-T
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  PTYPE = PRTYP
               SET I-61                    TO TRUE
           END-IF
           IF  (NOT-I-61)
               GO TO SLUTT-T
           END-IF.
 
       ENDPT-T.
      *
      *  TEST OM PRISENDRING SKAL MED.
      *
           IF  (I-02 AND I-38)
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND NOT-I-70)
               MOVE EDATO                  TO SDATO
               PERFORM SNU-S
           END-IF
           IF  (I-02 AND NOT-I-70)
               MOVE SDATO                  TO EDATO
               MOVE AAR8                   TO EDATO8
      *
           END-IF
           IF  (I-02 AND NOT-I-70)
               SET NOT-I-91                TO TRUE
               IF  EDATO8 > PDATO8
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-91)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET I-85                    TO TRUE
               ADD NYPRIS TO ZERO      GIVING PRIS2
      *
      *  TEST OM PRISENDRING INNLAGT SIDEN SISTE KJØRING.
      *
           END-IF
           IF  (I-03 AND NOT-I-71)
               MOVE PEDATO                 TO PEDATO-N
               MOVE PEDATO-N-IO (2:6)      TO PEDAT1
               MOVE PEDAT1                 TO SDATO
               PERFORM SNU-S
           END-IF
           IF  (I-03 AND NOT-I-71)
               MOVE SDATO                  TO PEDAT1
               MOVE AAR8                   TO PEDAT8
               SET NOT-I-93                TO TRUE
               IF  PEDAT8 NOT < SKJO-ELGR8
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-85)
               SET I-93                    TO TRUE
      *
           END-IF
           IF  (I-03 AND NOT-I-MR)
               OR  (I-03 AND I-MR AND NOT-I-85)
               ADD PRIS TO ZERO        GIVING PRIS2
           END-IF
           IF  (I-03)
               SET NOT-I-90                TO TRUE
               IF  NPRIS = 'N'
                   SET I-90                TO TRUE
               END-IF
      *
      *  SETTE PÅ PRINTINDIKATOR DERSOM UTSKRIFT.
      *
           END-IF
           IF  (I-03 AND I-15 AND I-93)
               OR  (I-03 AND NOT-I-15)
               OR  (I-03 AND I-MR AND I-85)
               SET I-30                    TO TRUE
           END-IF.
 
       SLUTT-T.
      ******************************************************
      *  SUBRUTINE FOR Å SNU DATO TIL FORMATET ÅR-MND-DAG  *
      ******************************************************
           CONTINUE.
 
       SNU-S SECTION.
       SNU-S-P.
           MOVE SDATO (1:2)                TO DAG
           MOVE SDATO (1:4)                TO DDMM
           MOVE DDMM (3:2)                 TO MM
           MOVE SDATO (5:2)                TO AAR
           SET NOT-I-98                    TO TRUE
           IF  AAR < '80'
               SET I-98                    TO TRUE
           END-IF
           IF  (I-98)
               MOVE '20'                   TO AAR4 (1:2)
           END-IF
           IF  (NOT-I-98)
               MOVE '19'                   TO AAR4 (1:2)
           END-IF
           MOVE AAR                        TO AAR4 (3:2)
           MOVE AAR4                       TO AAR8 (1:4)
           MOVE MM                         TO MNDDD (1:2)
           MOVE DAG                        TO MNDDD (3:2)
           MOVE MNDDD                      TO AAR8 (5:4)
           MOVE AAR                        TO SDATO (1:2)
           MOVE DAG                        TO SDATO (5:2).
      ******************************************************
 
       PRISF-GET SECTION.
       PRISF-GET-P.
           IF  PRISF-EOF-OFF
               READ PRISF
               AT END
                   SET PRISF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PRISF-FLDOFF SECTION.
       PRISF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-38                TO TRUE
           END-EVALUATE.
 
       PRISF-FLDSET SECTION.
       PRISF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRISF-IO-AREA (3:3)    TO FNR (1:3)
               MOVE PRISF-IO-AREA (6:7)    TO EDBNR (1:7)
               MOVE PRISF-IO-AREA (19:5)   TO VGR (1:5)
               MOVE PRISF-IO-AREA (19:1)   TO VAVD (1:1)
               MOVE PRISF-IO-AREA (24:3)   TO ALFA (1:3)
               MOVE PRISF-IO-AREA (33:6)   TO EDATO (1:6)
               MOVE PRISF-IO-AREA (44:5)   TO NYPRIS-IO
               IF  NYPRIS = ZERO
                   SET I-38                TO TRUE
               END-IF
           END-EVALUATE.
 
       PRISF-IDSET SECTION.
       PRISF-IDSET-P.
           SET I-02                        TO TRUE.
 
       PRISF-CHK-LEVEL SECTION.
       PRISF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PRISF-LEVEL-02
               MOVE PRISF-IO-AREA (3:3)    TO PRISF-02-L2-FNR
               MOVE PRISF-IO-AREA (6:7)    TO PRISF-02-L1-EDBNR
               IF  PRISF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PRISF-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PRISF-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PRISF-02-L2           TO THE-PRIOR-L2
               MOVE  PRISF-02-L1           TO THE-PRIOR-L1
               SET PRISF-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       PRISF-MATCH-SET SECTION.
       PRISF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE PRISF-IO-AREA (3:3)    TO PRISF-M-02-M2-FNR
               MOVE PRISF-IO-AREA (6:7)    TO PRISF-M-02-M1-EDBNR
           END-EVALUATE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO FNR (1:3)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VAREN (1:30)
               MOVE VAREMAS-IO-AREA (75:9) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (84:4) TO PEDATO-IO
               MOVE VAREMAS-IO-AREA (88:7) TO ALTEDB (1:7)
               MOVE VAREMAS-IO-AREA (95:1) TO NPRIS (1:1)
               MOVE VAREMAS-IO-AREA (107:1) TO PTEK (1:1)
               MOVE VAREMAS-IO-AREA (108:3) TO KINNH-IO
               MOVE VAREMAS-IO-AREA (117:1) TO KAMPPR (1:1)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (118:1) TO VAVD (1:1)
               MOVE VAREMAS-IO-AREA (127:1) TO MRK (1:1)
               MOVE VAREMAS-IO-AREA (128:1) TO SLETT (1:1)
               MOVE VAREMAS-IO-AREA (140:6) TO LOC (1:6)
               MOVE VAREMAS-IO-AREA (146:1) TO PTYPE (1:1)
               MOVE VAREMAS-IO-AREA (161:4) TO PRITIL-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       VAREMAS-CHK-LEVEL SECTION.
       VAREMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREMAS-LEVEL-03
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-03-L2-FNR
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-03-L1-EDBNR
               IF  VAREMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREMAS-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREMAS-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREMAS-03-L2         TO THE-PRIOR-L2
               MOVE  VAREMAS-03-L1         TO THE-PRIOR-L1
               SET VAREMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-M-03-M2-FNR
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-M-03-M1-EDBNR
           END-EVALUATE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDOFF SECTION.
       PARAM-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '2' )
               SET NOT-I-70                TO TRUE
               SET NOT-I-71                TO TRUE
           END-EVALUATE.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               MOVE PARAM-IO-AREA (11:3)   TO FIRMA (1:3)
               MOVE PARAM-IO-AREA (24:1)   TO TABELL (1:1)
               MOVE PARAM-IO-AREA (38:1)   TO AVD (1:1)
               MOVE PARAM-IO-AREA (60:1)   TO NYKUNN (1:1)
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '1' )
               MOVE PARAM-IO-AREA (32:1)   TO PRTYP (1:1)
               MOVE PARAM-IO-AREA (73:1)   TO UTGA-ELGR (1:1)
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '2' )
               MOVE PARAM-IO-AREA (33:6)   TO PDATO (1:6)
               IF  PDATO = SPACES
                   SET I-70                TO TRUE
               END-IF
               MOVE PARAM-IO-AREA (58:6)   TO SKJO-ELGRT (1:6)
               IF  SKJO-ELGRT = SPACES
                   SET I-71                TO TRUE
               END-IF
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '*' )
             OR ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
             OR ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '1' )
             OR ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '*' )
               SET I-01                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               SET I-05                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '1' )
               SET I-06                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '2' )
               SET I-07                    TO TRUE
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  PRISF-EOF
               MOVE HIGH-VALUES            TO PRISF-MC
                                              PRISF-MP
           END-IF
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  PRISF-MC < PRISF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  PRISF-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PRISF-PROCESS       TO TRUE
                   MOVE PRISF-MC           TO PRISF-MP
                   IF  PRISF-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC < PRISF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = PRISF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  PRISF-MC = VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PRISF-PROCESS       TO TRUE
                   MOVE PRISF-MC           TO PRISF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TABVGR-LOAD SECTION.
       TABVGR-LOAD-P.
           OPEN INPUT TABVGR
           SET TABA-I                      TO 1
           PERFORM UNTIL TABVGR-EOF
               READ TABVGR
               AT END
                   SET TABVGR-EOF          TO TRUE
               NOT AT END
                   MOVE TABVGR-IO-AREA (1:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (6:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (11:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (16:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (21:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (26:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (31:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (36:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (41:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (46:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (51:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (56:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (61:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (66:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
                   MOVE TABVGR-IO-AREA (71:5) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABVGR.
 
       TABALF-LOAD SECTION.
       TABALF-LOAD-P.
           OPEN INPUT TABALF
           SET TABB-I                      TO 1
           PERFORM UNTIL TABALF-EOF
               READ TABALF
               AT END
                   SET TABALF-EOF          TO TRUE
               NOT AT END
                   MOVE TABALF-IO-AREA (1:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (4:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (7:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (10:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (13:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (16:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (19:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (22:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (25:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (28:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (31:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (34:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (37:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (40:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (43:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (46:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (49:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (52:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (55:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (58:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (61:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (64:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (67:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (70:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
                   MOVE TABALF-IO-AREA (73:3) TO TABB-ENTRY (TABB-I)
                   SET TABB-I              UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABALF.
 
       TABVGA-LOAD SECTION.
       TABVGA-LOAD-P.
           OPEN INPUT TABVGA
           SET TABC-I                      TO 1
           PERFORM UNTIL TABVGA-EOF
               READ TABVGA
               AT END
                   SET TABVGA-EOF          TO TRUE
               NOT AT END
                   MOVE TABVGA-IO-AREA (1:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (9:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (17:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (25:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (33:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (41:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (49:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (57:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
                   MOVE TABVGA-IO-AREA (65:8) TO TABC-ENTRY (TABC-I)
                   SET TABC-I              UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABVGA.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-30)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE PRTYP                  TO PRTFILE-IO-AREA (1:1)
               MOVE ALFA                   TO PRTFILE-IO-AREA (2:3)
               MOVE ARTNR                  TO PRTFILE-IO-AREA (5:20)
               MOVE VAREN                  TO PRTFILE-IO-AREA (25:30)
               MOVE PRIS2-IO               TO PRTFILE-IO-AREA (55:8)
               MOVE FNR                    TO PRTFILE-IO-AREA (63:3)
               IF  (I-90)
                   MOVE 'N'                TO PRTFILE-IO-AREA (66:1)
               END-IF
               MOVE VGR                    TO PRTFILE-IO-AREA (67:5)
               IF  (I-93)
                   MOVE 'E'                TO PRTFILE-IO-AREA (72:1)
               END-IF
               IF  (I-94)
                   MOVE MRK                TO PRTFILE-IO-AREA (73:1)
               END-IF
               MOVE PEDATO                 TO XO-70P
               MOVE XO-70P-EF              TO PRTFILE-IO-AREA (74:4)
               MOVE ALTEDB                 TO PRTFILE-IO-AREA (78:7)
               MOVE PRITIL                 TO XO-52P
               MOVE XO-52P-EF              TO PRTFILE-IO-AREA (85:4)
               MOVE EDBNR                  TO PRTFILE-IO-AREA (89:7)
               MOVE KINNH                  TO XO-50P
               MOVE XO-50P-EF              TO PRTFILE-IO-AREA (96:3)
               MOVE KAMPPR                 TO PRTFILE-IO-AREA (113:1)
               MOVE PTEK                   TO PRTFILE-IO-AREA (114:1)
               MOVE LOC                    TO PRTFILE-IO-AREA (115:6)
               WRITE PRTFILE-IO-AREA
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
           MOVE 3                          TO LR-CHECK
           SET PRISF-LEVEL-INIT            TO TRUE
           INITIALIZE PRISF-DATA-FIELDS
           SET PRISF-EOF-OFF               TO TRUE
           SET PRISF-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO PRISF-MC
                                              PRISF-MP
           OPEN INPUT PRISF
           SET VAREMAS-LEVEL-INIT          TO TRUE
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN INPUT VAREMAS
           PERFORM TABVGR-LOAD
           PERFORM TABALF-LOAD
           PERFORM TABVGA-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           OPEN OUTPUT PRTFILE.
           SET TABA-I                      TO 1
           SET TABB-I                      TO 1
           SET TABC-I                      TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PRISF
           CLOSE VAREMAS
           CLOSE PARAM
           CLOSE PRTFILE.
 
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
