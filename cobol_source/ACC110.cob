       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACC110R.
      **********************************************  Z-WIN-RPG2      *
      ***********************************************************
      *       PROGRAMMET DANNER EN LISTE SOM VISER HVOR MYE     *
      *       HVERT ENKELT FIRMA HAR KJØRT FOR DENNE PERIODE.   *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ACC110.rpg
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
           SELECT ACCOUNT
               ASSIGN TO UT-S-ACCOUNT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ACCOUNT-STATUS.
           SELECT PRISER
               ASSIGN TO UT-S-PRISER
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRISER-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT
               BLOCK CONTAINS 4050
               RECORD CONTAINS 90.
       01  ACCOUNT-IO-AREA.
           05  ACCOUNT-IO-AREA-X           PICTURE X(90).
       FD PRISER
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PRISER-IO-AREA.
           05  PRISER-IO-AREA-X            PICTURE X(80).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABBET-MAX   VALUE 600          PICTURE 9(4) USAGE BINARY.
       77  TABPRI-MAX   VALUE 600          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABBET-TABLE.
               10  TABBET-ENTRY
                                           OCCURS 600 TIMES
                                           INDEXED BY TABBET-I
                                                      TABBET-S
                                                      TABPRI-I
                                                      TABPRI-S.
                   15  TABBET              PICTURE X(5).
                   15  TABPRI              PICTURE X(23).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ACCOUNT-STATUS              PICTURE 99 VALUE 0.
           10  PRISER-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ACCOUNT-EOF-OFF         VALUE '0'.
               88  ACCOUNT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ACCOUNT-READ-OFF        VALUE '0'.
               88  ACCOUNT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ACCOUNT-PROCESS-OFF     VALUE '0'.
               88  ACCOUNT-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ACCOUNT-LEVEL-INIT-OFF  VALUE '0'.
               88  ACCOUNT-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISER-EOF-OFF          VALUE '0'.
               88  PRISER-EOF              VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  ACCOUNT-LEVEL-01.
               10  ACCOUNT-01-L2.
                   15  ACCOUNT-01-L2-FIRMA PICTURE X(3).
               10  ACCOUNT-01-L1.
                   15  ACCOUNT-01-L1-NUMMER PICTURE S9(4).
           05  ACCOUNT-LEVEL-02.
               10  ACCOUNT-02-L2.
                   15  ACCOUNT-02-L2-FIRMA PICTURE X(3).
               10  ACCOUNT-02-L1.
                   15  ACCOUNT-02-L1-NUMMER PICTURE S9(4).
           05  ACCOUNT-DATA-FIELDS.
               10  RA                      PICTURE X(1).
               10  DATO                    PICTURE X(8).
               10  START-X-IO.
                   15  START-X             PICTURE S9(6).
               10  TIM-IO.
                   15  TIM                 PICTURE S9(2).
               10  MIN-IO.
                   15  MIN                 PICTURE S9(2).
               10  SEK-IO.
                   15  SEK                 PICTURE S9(2).
               10  STOP-X-IO.
                   15  STOP-X              PICTURE S9(6).
               10  TIM1-IO.
                   15  TIM1                PICTURE S9(2).
               10  MIN1-IO.
                   15  MIN1                PICTURE S9(2).
               10  SEK1-IO.
                   15  SEK1                PICTURE S9(2).
               10  USER-X                  PICTURE X(16).
               10  KOPI-IO.
                   15  KOPI                PICTURE S9(1).
               10  SIGN-X                  PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  OPPSL                   PICTURE X(5).
               10  BETM                    PICTURE X(1).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(5).
               10  BELKO                   PICTURE X(1).
               10  NAVN                    PICTURE X(8).
               10  NAVN1                   PICTURE X(2).
               10  NUMMER-IO.
                   15  NUMMER              PICTURE S9(4).
               10  ESIDE-IO.
                   15  ESIDE               PICTURE S9(4).
               10  LINJE-IO.
                   15  LINJE               PICTURE S9(8).
               10  SIDE-IO.
                   15  SIDE                PICTURE S9(4).
               10  KOPIL-IO.
                   15  KOPIL               PICTURE S9(8).
               10  KOPIS-IO.
                   15  KOPIS               PICTURE S9(4).
               10  FORM                    PICTURE X(4).
               10  PRT                     PICTURE X(3).
           05  FIRMAF-DATA-FIELDS.
               10  FINAV                   PICTURE X(30).
      ***********************************************************
      **  TEST PÅ OM FIRMA LIGGER I FIRMAFILEN,OG OM DET       **
      **  FINNES MAKKER I TABELLEN.                            **
      ***********************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(8)V9(2).
               10  BEGYNN-IO.
                   15  BEGYNN              PICTURE S9(6).
               10  STANS-IO.
                   15  STANS               PICTURE S9(6).
               10  S1-IO.
                   15  S1                  PICTURE S9(6).
               10  S2-IO.
                   15  S2                  PICTURE S9(4).
               10  S5-IO.
                   15  S5                  PICTURE S9(6).
               10  S6-IO.
                   15  S6                  PICTURE S9(4).
               10  SEKUN-IO.
                   15  SEKUN               PICTURE S9(10).
               10  SEKUN2-IO.
                   15  SEKUN2              PICTURE S9(10).
               10  ENH-IO.
                   15  ENH                 PICTURE S9(6)V9(2).
               10  ENH2-IO.
                   15  ENH2                PICTURE S9(6)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(8)V9(2).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(4)V9(3).
               10  TEKST                   PICTURE X(16).
               10  ENHET2-IO.
                   15  ENHET2              PICTURE S9(6)V9(2).
               10  ENHET-IO.
                   15  ENHET               PICTURE S9(6)V9(2).
               10  SLUTT-IO.
                   15  SLUTT               PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-62YYZR               PICTURE ZZZ.ZZZ,ZZ-.
               10  XO-43YYZR               PICTURE Z.ZZZ,ZZZ-.
               10  XO-82YYZR               PICTURE ZZ.ZZZ.ZZZ,ZZ-.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
 
           PERFORM HEADING-OUTPUT
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ACCOUNT-PROCESS
               SET ACCOUNT-PROCESS-OFF     TO TRUE
               SET ACCOUNT-READ            TO TRUE
           END-IF
 
           IF  ACCOUNT-READ
           AND RECORD-SELECTED-OFF
               PERFORM ACCOUNT-GET
               SET ACCOUNT-READ-OFF        TO TRUE
               IF  NOT ACCOUNT-EOF
                   PERFORM ACCOUNT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ACCOUNT-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ACCOUNT-PROCESS
               PERFORM ACCOUNT-IDSET
           END-IF
 
           IF  ACCOUNT-PROCESS
               PERFORM ACCOUNT-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  ACCOUNT-PROCESS
               PERFORM ACCOUNT-FLDOFF
               PERFORM ACCOUNT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ACCOUNT-PROCESS
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
               SET NOT-I-35                TO TRUE
               SET NOT-I-36                TO TRUE
               SET NOT-I-81                TO TRUE
               SET NOT-I-82                TO TRUE
               SET NOT-I-19                TO TRUE
      *
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-40                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-40            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               MOVE 0                      TO TOTSUM
           END-IF
           IF  (I-40)
               GO TO END-X-T
      **      NULLSTILLING AV FELTER.
      *
      *
           END-IF
           IF  (I-L1)
               ADD START-X TO ZERO     GIVING BEGYNN
               MOVE 0                      TO STANS
               MOVE 0                      TO S1
               MOVE 0                      TO S2
               MOVE 0                      TO S5
               MOVE 0                      TO S6
               MOVE 0                      TO SEKUN
               MOVE 0                      TO SEKUN2
               MOVE 0                      TO TID
               MOVE 0                      TO ENH
               MOVE 0                      TO ENH2
               MOVE 0                      TO SUM-X
      **      SETTE AV INDIKATORER.
           END-IF
           IF  (I-L1)
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
               SET NOT-I-14                TO TRUE
               SET NOT-I-15                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-30                TO TRUE
               SET NOT-I-36                TO TRUE
               SET NOT-I-19                TO TRUE
               SET NOT-I-89                TO TRUE
               SET NOT-I-27                TO TRUE
               SET NOT-I-29                TO TRUE
      ***********************************************************
      ** GJØR OPPSLAG MOT TABELL FOR Å HENTE PRIS.             **
      ***********************************************************
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-10                TO TRUE
               SET TABBET-S                TO TABBET-I
               PERFORM WITH TEST AFTER
                       VARYING TABBET-I FROM 1 BY 1
                         UNTIL TABBET-I >= TABBET-MAX
                            OR I-10
                   IF  OPPSL = TABBET (TABBET-I)
                       SET I-10            TO TRUE
                       SET TABBET-S        TO TABBET-I
                   END-IF
               END-PERFORM
               SET TABBET-I                TO TABBET-S
               IF  I-10
               AND TABBET-I NOT > TABPRI-MAX
                   SET TABPRI-I            TO TABBET-I
               END-IF
           END-IF
           IF  (I-L1 AND I-10)
               MOVE TABPRI(TABPRI-I) (1:7) TO PRIS
               SET NOT-I-90                TO TRUE
               IF  PRIS < 0
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-10 AND I-90)
               MULTIPLY -1 BY PRIS     GIVING PRIS
           END-IF
           IF  (I-L1 AND I-10)
               MOVE TABBET(TABBET-I)       TO TEKST (12:5)
      ***********************************************************
      **    TESTER PÅ BETALINGSMÅTE FOR VIDERE UTREGNING       **
      **    AV PRIS.                                           **
      ***********************************************************
           END-IF
           IF  (I-L1 AND I-01)
               SET NOT-I-14                TO TRUE
               IF  BETM = 'C'
                   SET I-14                TO TRUE
               END-IF
               SET NOT-I-29                TO TRUE
               IF  BETM = 'R'
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-15                TO TRUE
               IF  BETM = 'L'
                   SET I-15                TO TRUE
               END-IF
               SET NOT-I-16                TO TRUE
               IF  BETM = 'S'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  BETM = 'P'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  BETM = 'E'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-15)
               OR  (I-L1 AND I-29)
               OR  (I-L1 AND I-16)
               SET I-19                    TO TRUE
           END-IF
           IF  (I-L1 AND I-14)
               MULTIPLY 3600 BY TIM    GIVING S5
               MULTIPLY 60 BY MIN      GIVING S6
               ADD S6 TO S5            GIVING SEKUN
               ADD SEK                     TO SEKUN
      ***********************************************************
           END-IF
           IF  (I-01)
               MOVE 0                      TO ENHET2
               ADD STOP-X TO ZERO      GIVING STANS
           END-IF
           IF  (I-01 AND I-29)
               ADD ESIDE                   TO ENHET2
           END-IF
           IF  (I-01 AND I-29 AND NOT-I-28)
               PERFORM SID-S
           END-IF
           IF  (I-01 AND I-29 AND NOT-I-28)
               SET I-27                    TO TRUE
           END-IF
           IF  (I-01 AND I-29)
               ADD ENHET2                  TO ENH2
           END-IF
           IF  (I-01)
               GO TO END-X-T
      ***********************************************************
      **     UTREGNING AV ANTALL LINJER X ANTALL KOPIER +      **
      **     ANTALL PRØVELINJER.  ELLER SIDER.                 **
      ***********************************************************
           END-IF
           IF  (I-02)
               MOVE 0                      TO ENHET
           END-IF
           IF  (I-02 AND I-15)
               ADD LINJE                   TO ENHET
           END-IF
           IF  (I-02 AND I-15 AND NOT-I-17)
               PERFORM LIN-S
           END-IF
           IF  (I-02)
               ADD ENHET                   TO ENH
           END-IF.
 
       END-X-T.
      ***********************************************************
      **     SETTER PÅ PRINTINDIKATORER.                       **
      ***********************************************************
           CONTINUE.
 
       REC02-S SECTION.
       REC02-S-P.
           SET NOT-I-11                    TO TRUE
           IF  FORM = 'LB02'
               SET I-11                    TO TRUE
           END-IF
           IF  (NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FORM = 'LS02'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  FORM = 'LB03'
               SET I-12                    TO TRUE
           END-IF
           IF  (NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  FORM = 'LS03'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  FORM = 'LB04'
               SET I-13                    TO TRUE
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  FORM = 'LS04'
                   SET I-13                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-11)
               MULTIPLY 2 BY ENH       GIVING ENH
           END-IF
           IF  (I-12)
               MULTIPLY 3 BY ENH       GIVING ENH
           END-IF
           IF  (I-13)
               MULTIPLY 4 BY ENH       GIVING ENH
           END-IF
           IF  (I-11)
               MULTIPLY 2 BY ENH2      GIVING ENH2
           END-IF
           IF  (I-12)
               MULTIPLY 3 BY ENH2      GIVING ENH2
           END-IF
           IF  (I-13)
               MULTIPLY 4 BY ENH2      GIVING ENH2
           END-IF.
      ***********************************************************
      **         SUBRUTINE FOR Å SE OM DET ER SKREVET UT FLERE  *
      **         KOPIER.   LINJER                               *
      ***********************************************************
 
       LIN-S SECTION.
       LIN-S-P.
      *          KOPIL     SUB  LINJE     KOPIL
           CONTINUE.
 
       LOOPA-T.
           SET NOT-I-20                    TO TRUE
           IF  KOPIL NOT < LINJE
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               ADD LINJE                   TO ENHET
               ADD LINJE                   TO ENHET2
               SUBTRACT LINJE              FROM KOPIL
               SET NOT-I-21                TO TRUE
               IF  KOPIL = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-20 AND NOT-I-21)
               GO TO LOOPA-T
           END-IF.
      ***********************************************************
      **         SUBRUTINE FOR Å SE OM DET ER SKREVET UT FLERE  *
      **         KOPIER.   SIDER.                               *
      ***********************************************************
 
       SID-S SECTION.
       SID-S-P.
      *          KOPIS     SUB  ESIDE     KOPIS
           CONTINUE.
 
       LOOPB-T.
           SET NOT-I-22                    TO TRUE
           IF  KOPIS NOT < ESIDE
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               ADD ESIDE                   TO ENHET2
               SUBTRACT ESIDE              FROM KOPIS
               SET NOT-I-23                TO TRUE
               IF  KOPIS = 0
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-23)
               GO TO LOOPB-T
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-40)
               GO TO BUNN-T
           END-IF
           IF  (I-L1 AND I-14)
               SET I-35                    TO TRUE
               SET I-36                    TO TRUE
           END-IF
           IF  (I-L1 AND I-19)
               SET I-35                    TO TRUE
               SET I-36                    TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-35)
               GO TO BUNN-T
      ***********************************************************
      ****  FLYTTER STOP TIDSPUNKT CPU TIL SLUTT.              **
      ***********************************************************
           END-IF
           IF  (I-L1)
               ADD STANS TO ZERO       GIVING SLUTT
               SET NOT-I-88                TO TRUE
               IF  SLUTT = 0
                   SET I-88                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-19)
               PERFORM REC02-S
      ******************************************************************
      **    TESTER PÅ OM DET ER FAST AVTALT PRIS,TILLEGG,ELLER ORDINÆR.*
      ******************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-24                TO TRUE
               IF  BELKO = 'F'
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-24)
               SET NOT-I-25                TO TRUE
               IF  BELKO = '+'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-26                TO TRUE
               IF  BELKO = 'O'
                   SET I-26                TO TRUE
               END-IF
      **
           END-IF
           IF  (I-L1 AND I-24)
               ADD BEL TO ZERO         GIVING SUM-X
           END-IF
           IF  (I-L1 AND I-24 AND NOT-I-88)
               ADD BEL                     TO TOTSUM
           END-IF
           IF  (I-L1 AND I-24)
               GO TO BUNN-T
      ***********************************************************
      **     UTREGNING AV ANTALL SEKUNDER FOR SLUTT TIDSPUNKT  **
      **     OG REGNER UT ANTALL ENHETER.                      **
      ***********************************************************
           END-IF
           IF  (I-L1 AND I-14)
               SET NOT-I-30                TO TRUE
               IF  SLUTT < BEGYNN
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-14 AND I-30)
               ADD 24                      TO TIM1
           END-IF
           IF  (I-L1 AND I-14)
               MULTIPLY 3600 BY TIM1   GIVING S1
               MULTIPLY 60 BY MIN1     GIVING S2
               ADD S2 TO S1            GIVING SEKUN2
               ADD SEK1                    TO SEKUN2
               SUBTRACT SEKUN FROM SEKUN2 GIVING TID
               DIVIDE TID BY 60        GIVING ENH
               DIVIDE TID BY 60        GIVING ENH2
      ***********************************************************
      **         UTREGNING AV PRIS OG FIRMATOTAL.              **
      **                                                       **
      ***********************************************************
           END-IF
           IF  (I-L1 AND NOT-I-17 AND I-16)
               MULTIPLY PRIS BY ENH    GIVING SUM-X ROUNDED
           END-IF
           IF  (I-L1 AND NOT-I-28 AND I-29)
               MULTIPLY PRIS BY ENH2   GIVING SUM-X ROUNDED
           END-IF
           IF  (I-L1 AND I-25)
               ADD BEL                     TO SUM-X
           END-IF
           IF  (I-L1)
               ADD SUM-X                   TO TOTSUM
      **
           END-IF
           .
 
       BUNN-T.
      ***********************************************************
      **   SUBRUTINE FOR SJEKKE FORMULAR                       **
      ***********************************************************
           CONTINUE.
 
       ACCOUNT-GET SECTION.
       ACCOUNT-GET-P.
           IF  ACCOUNT-EOF-OFF
               READ ACCOUNT
               AT END
                   SET ACCOUNT-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ACCOUNT-FLDOFF SECTION.
       ACCOUNT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'E' )
               SET NOT-I-28                TO TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'L' )
               SET NOT-I-17                TO TRUE
               SET NOT-I-18                TO TRUE
           END-EVALUATE.
 
       ACCOUNT-FLDSET SECTION.
       ACCOUNT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'E' )
               MOVE ACCOUNT-IO-AREA (1:1)  TO RA (1:1)
               MOVE ACCOUNT-IO-AREA (2:8)  TO DATO (1:8)
               MOVE ACCOUNT-IO-AREA (11:6) TO START-X-IO
               INSPECT START-X-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (11:2) TO TIM-IO
               INSPECT TIM-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (13:2) TO MIN-IO
               INSPECT MIN-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (15:2) TO SEK-IO
               INSPECT SEK-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (18:6) TO STOP-X-IO
               INSPECT STOP-X-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (18:2) TO TIM1-IO
               INSPECT TIM1-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (20:2) TO MIN1-IO
               INSPECT MIN1-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (22:2) TO SEK1-IO
               INSPECT SEK1-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (24:16) TO USER-X (1:16)
               MOVE ACCOUNT-IO-AREA (35:1) TO KOPI-IO
               INSPECT KOPI-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (38:2) TO SIGN-X (1:2)
               MOVE ACCOUNT-IO-AREA (24:3) TO FIRMA (1:3)
               MOVE ACCOUNT-IO-AREA (24:5) TO OPPSL (1:5)
               MOVE ACCOUNT-IO-AREA (28:1) TO BETM (1:1)
               MOVE ACCOUNT-IO-AREA (30:5) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (36:1) TO BELKO (1:1)
               MOVE ACCOUNT-IO-AREA (40:8) TO NAVN (1:8)
               MOVE ACCOUNT-IO-AREA (40:2) TO NAVN1 (1:2)
               MOVE ACCOUNT-IO-AREA (48:4) TO NUMMER-IO
               INSPECT NUMMER-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (77:4) TO ESIDE-IO
               INSPECT ESIDE-IO REPLACING ALL ' ' BY '0'
               IF  ESIDE = ZERO
                   SET I-28                TO TRUE
               END-IF
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'L' )
               MOVE ACCOUNT-IO-AREA (1:1)  TO RA (1:1)
               MOVE ACCOUNT-IO-AREA (2:8)  TO DATO (1:8)
               MOVE ACCOUNT-IO-AREA (24:16) TO USER-X (1:16)
               MOVE ACCOUNT-IO-AREA (35:1) TO KOPI-IO
               INSPECT KOPI-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (24:3) TO FIRMA (1:3)
               MOVE ACCOUNT-IO-AREA (24:5) TO OPPSL (1:5)
               MOVE ACCOUNT-IO-AREA (28:1) TO BETM (1:1)
               MOVE ACCOUNT-IO-AREA (30:5) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (36:1) TO BELKO (1:1)
               MOVE ACCOUNT-IO-AREA (38:2) TO SIGN-X (1:2)
               MOVE ACCOUNT-IO-AREA (40:8) TO NAVN (1:8)
               MOVE ACCOUNT-IO-AREA (40:2) TO NAVN1 (1:2)
               MOVE ACCOUNT-IO-AREA (48:4) TO NUMMER-IO
               INSPECT NUMMER-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (52:8) TO LINJE-IO
               INSPECT LINJE-IO REPLACING ALL ' ' BY '0'
               IF  LINJE = ZERO
                   SET I-17                TO TRUE
               END-IF
               MOVE ACCOUNT-IO-AREA (60:4) TO SIDE-IO
               INSPECT SIDE-IO REPLACING ALL ' ' BY '0'
               IF  SIDE = ZERO
                   SET I-18                TO TRUE
               END-IF
               MOVE ACCOUNT-IO-AREA (64:8) TO KOPIL-IO
               INSPECT KOPIL-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (72:4) TO KOPIS-IO
               INSPECT KOPIS-IO REPLACING ALL ' ' BY '0'
               MOVE ACCOUNT-IO-AREA (76:4) TO FORM (1:4)
               MOVE ACCOUNT-IO-AREA (80:3) TO PRT (1:3)
           END-EVALUATE.
 
       ACCOUNT-IDCHK SECTION.
       ACCOUNT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'E' )
             OR ( ACCOUNT-IO-AREA (1:1) = 'L' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ACCOUNT-IDSET SECTION.
       ACCOUNT-IDSET-P.
           EVALUATE TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'E' )
               SET I-01                    TO TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'L' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       ACCOUNT-CHK-LEVEL SECTION.
       ACCOUNT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'E' )
               MOVE LOW-VALUES             TO ACCOUNT-LEVEL-01
               MOVE ACCOUNT-IO-AREA (24:3) TO ACCOUNT-01-L2-FIRMA
               MOVE ACCOUNT-IO-AREA (48:4) TO ACCOUNT-01-L1-NUMMER
               IF  ACCOUNT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ACCOUNT-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ACCOUNT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ACCOUNT-01-L2         TO THE-PRIOR-L2
               MOVE  ACCOUNT-01-L1         TO THE-PRIOR-L1
               SET ACCOUNT-LEVEL-INIT      TO TRUE
           WHEN ( ACCOUNT-IO-AREA (1:1) = 'L' )
               MOVE LOW-VALUES             TO ACCOUNT-LEVEL-02
               MOVE ACCOUNT-IO-AREA (24:3) TO ACCOUNT-02-L2-FIRMA
               MOVE ACCOUNT-IO-AREA (48:4) TO ACCOUNT-02-L1-NUMMER
               IF  ACCOUNT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ACCOUNT-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ACCOUNT-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ACCOUNT-02-L2         TO THE-PRIOR-L2
               MOVE  ACCOUNT-02-L1         TO THE-PRIOR-L1
               SET ACCOUNT-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAV (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
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
 
       PRISER-LOAD SECTION.
       PRISER-LOAD-P.
           OPEN INPUT PRISER
           SET TABBET-I                    TO 1
           PERFORM UNTIL PRISER-EOF
               READ PRISER
               AT END
                   SET PRISER-EOF          TO TRUE
               NOT AT END
                   MOVE PRISER-IO-AREA (1:28) TO TABBET-ENTRY
                                                            (TABBET-I)
                   SET TABBET-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE PRISER.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-40)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AUTO DATA A/S'        TO LISTE-IO-AREA (1:13)
               MOVE '** ACCOUNTINGLISTE   LEV' TO LISTE-IO-AREA (34:24)
               MOVE 'ERES SVERRE/KNUT  ** ' TO LISTE-IO-AREA (58:21)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (100:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (110:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (114:4)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (1:9)
               MOVE 'DATO     JOBNAVN  BEST. ' TO LISTE-IO-AREA (32:24)
               MOVE 'FAKT.MÅTE         START' TO LISTE-IO-AREA (58:23)
               MOVE 'STOP'                 TO LISTE-IO-AREA (86:4)
               MOVE 'ANT.ENH.'             TO LISTE-IO-AREA (96:8)
               MOVE 'ENH.PRIS     PRIS PR JOB' TO LISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AUTO DATA A/S'        TO LISTE-IO-AREA (1:13)
               MOVE '** ACCOUNTINGLISTE   LEV' TO LISTE-IO-AREA (34:24)
               MOVE 'ERES SVERRE/KNUT  ** ' TO LISTE-IO-AREA (58:21)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (90:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (100:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (110:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (114:4)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (1:9)
               MOVE 'DATO     JOBNAVN  BEST. ' TO LISTE-IO-AREA (32:24)
               MOVE 'FAKT.MÅTE         START' TO LISTE-IO-AREA (58:23)
               MOVE 'STOP'                 TO LISTE-IO-AREA (86:4)
               MOVE 'ANT.ENH.'             TO LISTE-IO-AREA (96:8)
               MOVE 'ENH.PRIS     PRIS PR JOB' TO LISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-14 AND I-36)
           OR  (I-L1 AND I-19 AND I-36)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAV                  TO LISTE-IO-AREA (1:30)
               MOVE DATO                   TO LISTE-IO-AREA (32:8)
               MOVE NAVN                   TO LISTE-IO-AREA (41:8)
               MOVE SIGN-X                 TO LISTE-IO-AREA (52:2)
               IF  (I-10)
                   MOVE TEKST              TO LISTE-IO-AREA (58:16)
               END-IF
               IF  (I-15)
                   MOVE 'FAKT. PR LINJE  ' TO LISTE-IO-AREA (58:16)
               END-IF
               IF  (I-24)
                   MOVE 'FAST PRIS AVTALT' TO LISTE-IO-AREA (58:16)
               END-IF
               MOVE BEGYNN                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (76:8)
               INITIALIZE BEGYNN
               MOVE SLUTT                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (86:8)
               INITIALIZE SLUTT
               IF  (NOT-I-24 AND I-14)
                   MOVE ENH                TO XO-62YYZR
                   MOVE XO-62YYZR          TO LISTE-IO-AREA (95:11)
                   INITIALIZE ENH
               END-IF
               IF  (NOT-I-24 AND I-14 AND I-29)
                   MOVE ENH2               TO XO-62YYZR
                   MOVE XO-62YYZR          TO LISTE-IO-AREA (95:11)
                   INITIALIZE ENH2
               END-IF
               IF  (NOT-I-24 AND I-14 AND I-10)
                   MOVE PRIS               TO XO-43YYZR
                   MOVE XO-43YYZR          TO LISTE-IO-AREA (107:10)
      *               N24 14 10PRIS     116 " . 0 ,   "
               END-IF
               IF  (I-14)
                   MOVE SUM-X              TO XO-82YYZR
                   MOVE XO-82YYZR          TO LISTE-IO-AREA (118:14)
                   INITIALIZE SUM-X
               END-IF
               EVALUATE TRUE
               WHEN (I-L1 AND I-14 AND I-36)
                   MOVE 2                  TO LISTE-AFTER-SPACE
               WHEN (I-L1 AND I-19 AND I-36)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-36 AND I-19)
      *                        FINAV     30
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DATO                   TO LISTE-IO-AREA (32:8)
               MOVE NAVN                   TO LISTE-IO-AREA (41:8)
               MOVE SIGN-X                 TO LISTE-IO-AREA (52:2)
               IF  (I-10)
                   MOVE TEKST              TO LISTE-IO-AREA (58:16)
               END-IF
               IF  (I-15)
                   MOVE 'FAKT. PR LINJE  ' TO LISTE-IO-AREA (58:16)
               END-IF
               IF  (I-24)
                   MOVE 'FAST PRIS AVTALT' TO LISTE-IO-AREA (58:16)
               END-IF
               MOVE PRT                    TO LISTE-IO-AREA (78:3)
               INITIALIZE PRT
               IF  (NOT-I-24)
                   MOVE ENH                TO XO-62YYZR
                   MOVE XO-62YYZR          TO LISTE-IO-AREA (95:11)
                   INITIALIZE ENH
               END-IF
               IF  (NOT-I-24 AND I-29)
                   MOVE ENH2               TO XO-62YYZR
                   MOVE XO-62YYZR          TO LISTE-IO-AREA (95:11)
                   INITIALIZE ENH2
               END-IF
               IF  (NOT-I-24 AND I-10)
                   MOVE PRIS               TO XO-43YYZR
                   MOVE XO-43YYZR          TO LISTE-IO-AREA (107:10)
      *                  N24 10PRIS     116 " . 0 ,   "
               END-IF
               MOVE SUM-X                  TO XO-82YYZR
               MOVE XO-82YYZR              TO LISTE-IO-AREA (118:14)
               INITIALIZE SUM-X
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT PR. FIRMA:  '  TO LISTE-IO-AREA (1:19)
               MOVE TOTSUM                 TO XO-82YYZR
               MOVE XO-82YYZR              TO LISTE-IO-AREA (119:14)
               INITIALIZE TOTSUM
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-05 AND I-18)
           AND (I-81 AND I-82 AND I-26)
           AND (I-89 AND I-27)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RA                     TO LISTE-IO-AREA (80:1)
               MOVE USER-X                 TO LISTE-IO-AREA (65:16)
               MOVE KOPI-IO                TO LISTE-IO-AREA (80:1)
               MOVE NAVN1                  TO LISTE-IO-AREA (79:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           SET ACCOUNT-LEVEL-INIT          TO TRUE
           INITIALIZE ACCOUNT-DATA-FIELDS
           SET ACCOUNT-EOF-OFF             TO TRUE
           SET ACCOUNT-PROCESS             TO TRUE
           OPEN INPUT ACCOUNT
           PERFORM PRISER-LOAD
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABBET-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ACCOUNT
           CLOSE FIRMAF
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
