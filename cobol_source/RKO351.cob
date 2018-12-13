       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO351R.
      **********************************************  Z-WIN-RPG2P     *
      *    KONV. IFRA RSK351 UTVIDET RECORD.     ***TXT***ok ss***    *
      *  PROGRAM.......: RKO351, MERKE RECORDS SOM SKAL FAKTURERES OG *
      *                          DANNE KUNDETOTAL.                    *
      *  PROGRAMMERER..:                                              *
      *  KJØRES I JOBB.: RES18UM                                      *
      *  LAGET DATO....:                                              *
      *  ENDRET........: 23.09.96 EKSKLUDERER GRUNNLAG NÅR RESKONTRO- *
      *                           SALDO ER MINDRE ENN NULL.           *
      *                  18.09.98 TAKLER ÅR 2000.                     *
      *                           KANSELLERER OG SKRIVER MELDING PÅ   *
      *                           KONSOLLET VED FEIL I PARAM.-DATO.   *
      *                           SKRIVER RECORDS MED FEIL I DATO PÅ  *
      *                           KVITT.-LISTEN.                      *
      *  INPUT.........: RESKONTROPARAMETER (RESPAR),                 *
      *                  RENTEGRUNNLAGSMASTER (RENTEG),               *
      *                  FIRMAFILE (FIRMAF),                          *
      *  BEHANDLING....: BEREGNER RENTEBELØP PR POST FOR NORMAL OG    *
      *                  AVVIKENDE RENTEPROSENT.                      *
      *  OUTPUT........: KUNDETOTAL (KUNDET) ,                        *
      *                  KVITTERINGSLISTE FOR AUTO DATA               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO351.rpg
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
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT RENTEG
               ASSIGN TO UT-S-RENTEG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RENTEG-STATUS.
           SELECT RESKSUM
               ASSIGN TO UT-S-RESKSUM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKSUM-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT RENTUT
               ASSIGN TO UT-S-RENTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RENTUT-STATUS.
           SELECT KUNDET
               ASSIGN TO UT-S-KUNDET
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KUNDET-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
       FD RENTEG
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RENTEG-IO-AREA.
           05  RENTEG-IO-AREA-X            PICTURE X(200).
       FD RESKSUM
               BLOCK CONTAINS 2000
               RECORD CONTAINS 20.
       01  RESKSUM-IO-AREA.
           05  RESKSUM-IO-AREA-X           PICTURE X(20).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD RENTUT
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RENTUT-IO-AREA.
           05  RENTUT-IO-AREA-X            PICTURE X(200).
       FD KUNDET
               BLOCK CONTAINS 4080
               RECORD CONTAINS 30.
       01  KUNDET-IO-AREA.
           05  KUNDET-IO-AREA-X            PICTURE X(30).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  RENTEG-STATUS               PICTURE 99 VALUE 0.
           10  RESKSUM-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  RENTUT-STATUS               PICTURE 99 VALUE 0.
           10  KUNDET-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-EOF-OFF          VALUE '0'.
               88  RESPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-READ-OFF         VALUE '0'.
               88  RESPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-PROCESS-OFF      VALUE '0'.
               88  RESPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEG-EOF-OFF          VALUE '0'.
               88  RENTEG-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEG-READ-OFF         VALUE '0'.
               88  RENTEG-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEG-PROCESS-OFF      VALUE '0'.
               88  RENTEG-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RENTEG-LEVEL-INIT-OFF   VALUE '0'.
               88  RENTEG-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKSUM-EOF-OFF         VALUE '0'.
               88  RESKSUM-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKSUM-READ-OFF        VALUE '0'.
               88  RESKSUM-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKSUM-PROCESS-OFF     VALUE '0'.
               88  RESKSUM-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKSUM-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKSUM-LEVEL-INIT      VALUE '1'.
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
           05  CONSOLE-IO-AREA.
               10  CONSOLE-IO-AREA-X       PICTURE X(800).
      *DSDS: DATA STRUCTURE FIELDS
           05  DTOPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTO8SI                  PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AAR4SI-IO.
                   15  AAR4SI              PICTURE S9(4).
               10  FILLER                  PICTURE X(61).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
           05  RESPAR-DATA-FIELDS.
               10  RESPER-IO.
                   15  RESPER              PICTURE S9(6).
           05  RENTEG-LEVEL-02.
               10  RENTEG-02-L2.
                   15  RENTEG-02-L2-FIRMA  PICTURE X(3).
               10  RENTEG-02-L1.
                   15  RENTEG-02-L1-RESKNR PICTURE X(6).
           05  RENTEG-DATA-FIELDS.
               10  REC60                   PICTURE X(60).
               10  FIRMA                   PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  BILDTO-IO.
                   15  BILDTO              PICTURE S9(6).
               10  BILA-ELGRX              PICTURE X(2).
               10  BILMND-IO.
                   15  BILMND              PICTURE S9(2).
               10  BILDAG-IO.
                   15  BILDAG              PICTURE S9(2).
               10  FORFAL-IO.
                   15  FORFAL              PICTURE S9(6).
               10  FORA-ELGRX              PICTURE X(2).
               10  FORMND-IO.
                   15  FORMND              PICTURE S9(2).
               10  FORDAG-IO.
                   15  FORDAG              PICTURE S9(2).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  REC                     PICTURE X(200).
           05  RENTEG-MP                   PICTURE X(9).
           05  RENTEG-MC                   PICTURE X(9).
           05  RENTEG-M-02             REDEFINES RENTEG-MC.
               10  RENTEG-M-02-M2.
                   15  RENTEG-M-02-M2-FIRMA-G.
                       20  RENTEG-M-02-M2-FIRMA PICTURE X(3).
               10  RENTEG-M-02-M1.
                   15  RENTEG-M-02-M1-RESKNR-G.
                       20  RENTEG-M-02-M1-RESKNR PICTURE X(6).
           05  RESKSUM-LEVEL-04.
               10  RESKSUM-04-L2.
                   15  RESKSUM-04-L2-FIRMA PICTURE X(3).
               10  RESKSUM-04-L1.
                   15  RESKSUM-04-L1-RESKNR PICTURE X(6).
           05  RESKSUM-DATA-FIELDS.
               10  SALDO-IO.
                   15  SALDO               PICTURE S9(9)V9(2).
           05  RESKSUM-MP                  PICTURE X(9).
           05  RESKSUM-MC                  PICTURE X(9).
           05  RESKSUM-M-04            REDEFINES RESKSUM-MC.
               10  RESKSUM-M-04-M2.
                   15  RESKSUM-M-04-M2-FIRMA-G.
                       20  RESKSUM-M-04-M2-FIRMA PICTURE X(3).
               10  RESKSUM-M-04-M1.
                   15  RESKSUM-M-04-M1-RESKNR-G.
                       20  RESKSUM-M-04-M1-RESKNR PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  RPROSN-IO.
                   15  RPROSN              PICTURE S9(1)V9(2).
               10  RRUT                    PICTURE X(1).
               10  RPOSTB-IO.
                   15  RPOSTB              PICTURE S9(3).
               10  RDAGER-IO.
                   15  RDAGER              PICTURE S9(2).
               10  RPROSA-IO.
                   15  RPROSA              PICTURE S9(1)V9(2).
      **   GJØR OM TIL 4-SIFRET ÅRSTALL
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  RESPE8-IO.
                   15  RESPE8              PICTURE S9(8).
               10  TOTANT-IO.
                   15  TOTANT              PICTURE S9(7).
               10  NEGSAL-IO.
                   15  NEGSAL              PICTURE S9(7).
               10  RBELO-ELGP-IO.
                   15  RBELO-ELGP          PICTURE S9(7)V9(2).
               10  RBELAV-IO.
                   15  RBELAV              PICTURE S9(7)V9(2).
               10  BILA-ELGRN-IO.
                   15  BILA-ELGRN          PICTURE S9(4).
               10  BILDT8-IO.
                   15  BILDT8              PICTURE S9(8).
               10  HJ4X                    PICTURE X(4).
               10  MERKE                   PICTURE X(1).
               10  NESTEM-IO.
                   15  NESTEM              PICTURE S9(5).
               10  FORA-ELGRN-IO.
                   15  FORA-ELGRN          PICTURE S9(4).
               10  DAG1-IO.
                   15  DAG1                PICTURE S9(6).
               10  DAG2-IO.
                   15  DAG2                PICTURE S9(3).
               10  FDAG-IO.
                   15  FDAG                PICTURE S9(6).
               10  INN1-IO.
                   15  INN1                PICTURE S9(6).
               10  INN2-IO.
                   15  INN2                PICTURE S9(3).
               10  INNDAG-IO.
                   15  INNDAG              PICTURE S9(6).
               10  RDAG-IO.
                   15  RDAG                PICTURE S9(4).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(8)V9(2).
               10  SUMA-IO.
                   15  SUMA                PICTURE S9(10).
               10  SUMB-IO.
                   15  SUMB                PICTURE S9(13).
               10  RENTEB-IO.
                   15  RENTEB              PICTURE S9(10)V9(1).
               10  TEST1-IO.
                   15  TEST1               PICTURE S9(9)V9(2).
               10  RENTEN-IO.
                   15  RENTEN              PICTURE S9(7)V9(2).
               10  RENTEA-IO.
                   15  RENTEA              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-70YYZ                PICTURE Z.ZZZ.ZZZ.
               10  XO-50YYZ                PICTURE ZZ.ZZZ.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RENTEG-PROCESS
               SET RENTEG-PROCESS-OFF      TO TRUE
               SET RENTEG-READ             TO TRUE
           END-IF
 
           IF  RENTEG-READ
               PERFORM RENTEG-GET
               SET RENTEG-READ-OFF         TO TRUE
               IF  NOT RENTEG-EOF
                   PERFORM RENTEG-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM RENTEG-MATCH-SET
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
                   PERFORM RESPAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RESPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RESKSUM-PROCESS
               SET RESKSUM-PROCESS-OFF     TO TRUE
               SET RESKSUM-READ            TO TRUE
           END-IF
 
           IF  RESKSUM-READ
               PERFORM RESKSUM-GET
               SET RESKSUM-READ-OFF        TO TRUE
               IF  NOT RESKSUM-EOF
                   PERFORM RESKSUM-MATCH-SET
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
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
           END-IF
 
           IF  RENTEG-PROCESS
               PERFORM RENTEG-IDSET
           END-IF
 
           IF  RESKSUM-PROCESS
               PERFORM RESKSUM-IDSET
           END-IF
 
           IF  RENTEG-PROCESS
               PERFORM RENTEG-CHK-LEVEL
           END-IF
 
           IF  RESKSUM-PROCESS
               PERFORM RESKSUM-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-FLDSET
           END-IF
 
           IF  RENTEG-PROCESS
               PERFORM RENTEG-FLDSET
           END-IF
 
           IF  RESKSUM-PROCESS
               PERFORM RESKSUM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RENTEG-PROCESS
           OR  RESKSUM-PROCESS
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
               SET I-H0                    TO TRUE
           END-IF
           IF  (I-01)
               MOVE 'B'                    TO DTOKOD
               MOVE RESPER                 TO DTODTO
               PERFORM DTORUT-S
      *                    SETON                     96     TEST FEILRUT
           END-IF
           IF  (I-01 AND NOT-I-96)
               MOVE DTO8SI                 TO RESPE8-IO
      *  01N96             MOVE "20000131"RESPE8  80        TEST ÅRSSKIF
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-20                    TO TRUE
           SET NOT-I-21                    TO TRUE
           IF  (I-02)
               ADD 1                       TO TOTANT
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO NEGSAL
           END-IF
           IF  (I-L1)
               MOVE 0                      TO RBELO-ELGP
               MOVE 0                      TO RBELAV
               SET NOT-I-17                TO TRUE
               SET NOT-I-18                TO TRUE
           END-IF
           IF  (I-MR)
               SET I-17                    TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-16                TO TRUE
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               SET NOT-I-16                TO TRUE
               IF  RRUT = 'F'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           MOVE 'F'                        TO MERKE
           IF  (NOT-I-16)
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-96                    TO TRUE
           SET NOT-I-98                    TO TRUE
      **   GJØR OM TIL 4-SIFRET ÅRSTALL
           MOVE 'B'                        TO DTOKOD
           MOVE BILDTO                     TO DTODTO
           PERFORM DTORUT-S
           IF  (NOT-I-96)
               MOVE AAR4SI                 TO BILA-ELGRN-IO
               MOVE DTO8SI                 TO BILDT8-IO
           END-IF
           IF  (I-96)
               MOVE '00'                   TO BILDT8 (1:2)
               MOVE BILDTO                 TO BILDT8-IO (3:6)
               MOVE '00'                   TO HJ4X (1:2)
               MOVE BILA-ELGRX             TO HJ4X (3:2)
               MOVE HJ4X                   TO BILA-ELGRN-IO
               SET I-98                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  BILDT8 > RESPE8
               SET I-15                    TO TRUE
           END-IF
           IF  (I-15)
               MOVE 'A'                    TO MERKE
               ADD 1                       TO NESTEM
               GO TO SLUTT-T
           END-IF
           IF  (I-17)
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND I-16 AND NOT-I-15)
               SET I-18                    TO TRUE
      *****************************************************
      *       BEREGNING  AV  RENTE-DAGER          *********
      *****************************************************
      **   GJØR OM TIL 4-SIFRET ÅRSTALL
           END-IF
           MOVE 'B'                        TO DTOKOD
           MOVE FORFAL                     TO DTODTO
           PERFORM DTORUT-S
           IF  (NOT-I-96)
               MOVE AAR4SI                 TO FORA-ELGRN-IO
           END-IF
           IF  (I-96)
               MOVE '00'                   TO HJ4X (1:2)
               MOVE FORA-ELGRX             TO HJ4X (3:2)
               MOVE HJ4X                   TO FORA-ELGRN-IO
               SET I-98                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  FORMND = 2
               SET I-32                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  FORDAG = 28
               SET I-38                    TO TRUE
           END-IF
           SET NOT-I-39                    TO TRUE
           IF  FORDAG = 29
               SET I-39                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  FORDAG = 31
               SET I-31                    TO TRUE
           END-IF
      *****************************************************
           MULTIPLY 360 BY FORA-ELGRN  GIVING DAG1
           MULTIPLY 30 BY FORMND       GIVING DAG2
           ADD DAG1 TO ZERO            GIVING FDAG
           ADD DAG2                        TO FDAG
           ADD FORDAG                      TO FDAG
           IF  (I-32 AND I-38)
               ADD 2                       TO FDAG
           END-IF
           IF  (I-32 AND I-39)
               ADD 1                       TO FDAG
           END-IF
           IF  (I-31)
               SUBTRACT 1                  FROM FDAG
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           MULTIPLY 360 BY BILA-ELGRN  GIVING INN1
           MULTIPLY 30 BY BILMND       GIVING INN2
           ADD INN1 TO ZERO            GIVING INNDAG
           ADD INN2                        TO INNDAG
           ADD BILDAG                      TO INNDAG
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           SUBTRACT FDAG FROM INNDAG   GIVING RDAG
           SET NOT-I-20                    TO TRUE
           IF  RDAGER NOT < RDAG
               SET I-20                    TO TRUE
           END-IF
      *                    MOVE "RESKNR  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGBUGFILO   RESKNR           VIS INDIKATOR
      *                    MOVE "RDAG    "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGBUGFILO   RDAG             VIS INDIKATOR
           IF  (I-20)
               SET I-21                    TO TRUE
               GO TO SLUTT-T
      *****************************************************
      *       BEREGNING  AV  NORMALT RENTE-BELØP          *
      *****************************************************
           END-IF
           MULTIPLY RPROSN BY BELO-ELGP GIVING SUM-X ROUNDED
           MULTIPLY 12 BY SUM-X        GIVING SUMA ROUNDED
           MULTIPLY RDAG BY SUMA       GIVING SUMB ROUNDED
           DIVIDE SUMB BY 36000        GIVING RENTEB ROUNDED
           ADD RENTEB TO ZERO          GIVING TEST1
           ADD RENTEB TO ZERO          GIVING RENTEN
      ** MLLzo
           IF TEST1 < 0
               MULTIPLY -1 BY TEST1
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RPOSTB NOT < TEST1
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               ADD RENTEN                  TO RBELO-ELGP
      *****************************************************
      *       BEREGNING  AV  AVIKENDE RENTE-BELØP.        *
      *****************************************************
           END-IF
           MULTIPLY RPROSA BY BELO-ELGP GIVING SUM-X ROUNDED
           MULTIPLY 12 BY SUM-X        GIVING SUMA ROUNDED
           MULTIPLY RDAG BY SUMA       GIVING SUMB ROUNDED
           DIVIDE SUMB BY 36000        GIVING RENTEB ROUNDED
           ADD RENTEB TO ZERO          GIVING TEST1
           ADD RENTEB TO ZERO          GIVING RENTEA
      ** MLLzo
           IF TEST1 < 0
               MULTIPLY -1 BY TEST1
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  RPOSTB NOT < TEST1
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               ADD RENTEA                  TO RBELAV
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
           CONTINUE.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-96                    TO TRUE
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-96                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-96                    TO TRUE
           END-IF.
 
       RESPAR-GET SECTION.
       RESPAR-GET-P.
           IF  RESPAR-EOF-OFF
               READ RESPAR
               AT END
                   SET RESPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESPAR-FLDSET SECTION.
       RESPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               MOVE RESPAR-IO-AREA (13:6)  TO RESPER-IO
               INSPECT RESPER-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RESPAR-IDCHK SECTION.
       RESPAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESPAR-IDSET SECTION.
       RESPAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       RENTEG-GET SECTION.
       RENTEG-GET-P.
           IF  RENTEG-EOF-OFF
               READ RENTEG
               AT END
                   SET RENTEG-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RENTEG-FLDSET SECTION.
       RENTEG-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RENTEG-IO-AREA (1:1) = '3'
            AND   RENTEG-IO-AREA (2:1) = '1' )
               MOVE RENTEG-IO-AREA (1:60)  TO REC60 (1:60)
               MOVE RENTEG-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE RENTEG-IO-AREA (6:6)   TO RESKNR (1:6)
               MOVE RENTEG-IO-AREA (20:6)  TO BILDTO-IO
               INSPECT BILDTO-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEG-IO-AREA (20:2)  TO BILA-ELGRX (1:2)
               MOVE RENTEG-IO-AREA (22:2)  TO BILMND-IO
               INSPECT BILMND-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEG-IO-AREA (24:2)  TO BILDAG-IO
               INSPECT BILDAG-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEG-IO-AREA (33:6)  TO FORFAL-IO
               INSPECT FORFAL-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEG-IO-AREA (33:2)  TO FORA-ELGRX (1:2)
               MOVE RENTEG-IO-AREA (35:2)  TO FORMND-IO
               INSPECT FORMND-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEG-IO-AREA (37:2)  TO FORDAG-IO
               INSPECT FORDAG-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEG-IO-AREA (39:9)  TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEG-IO-AREA (1:200) TO REC (1:200)
           END-EVALUATE.
 
       RENTEG-IDCHK SECTION.
       RENTEG-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RENTEG-IO-AREA (1:1) = '3'
            AND   RENTEG-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RENTEG-IDSET SECTION.
       RENTEG-IDSET-P.
           EVALUATE TRUE
           WHEN ( RENTEG-IO-AREA (1:1) = '3'
            AND   RENTEG-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       RENTEG-CHK-LEVEL SECTION.
       RENTEG-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RENTEG-IO-AREA (1:1) = '3'
            AND   RENTEG-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO RENTEG-LEVEL-02
               MOVE RENTEG-IO-AREA (3:3)   TO RENTEG-02-L2-FIRMA
               MOVE RENTEG-IO-AREA (6:6)   TO RENTEG-02-L1-RESKNR
               IF  RENTEG-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RENTEG-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RENTEG-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RENTEG-02-L2          TO THE-PRIOR-L2
               MOVE  RENTEG-02-L1          TO THE-PRIOR-L1
               SET RENTEG-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       RENTEG-MATCH-SET SECTION.
       RENTEG-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( RENTEG-IO-AREA (1:1) = '3'
            AND   RENTEG-IO-AREA (2:1) = '1' )
               MOVE RENTEG-IO-AREA (3:3)   TO RENTEG-M-02-M2-FIRMA
               MOVE RENTEG-IO-AREA (6:6)   TO RENTEG-M-02-M1-RESKNR
           END-EVALUATE.
 
       RESKSUM-GET SECTION.
       RESKSUM-GET-P.
           IF  RESKSUM-EOF-OFF
               READ RESKSUM
               AT END
                   SET RESKSUM-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKSUM-FLDSET SECTION.
       RESKSUM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKSUM-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE RESKSUM-IO-AREA (4:6)  TO RESKNR (1:6)
               MOVE RESKSUM-IO-AREA (10:11) TO SALDO-IO
               INSPECT SALDO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RESKSUM-IDSET SECTION.
       RESKSUM-IDSET-P.
           SET I-04                        TO TRUE.
 
       RESKSUM-CHK-LEVEL SECTION.
       RESKSUM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKSUM-LEVEL-04
               MOVE RESKSUM-IO-AREA (1:3)  TO RESKSUM-04-L2-FIRMA
               MOVE RESKSUM-IO-AREA (4:6)  TO RESKSUM-04-L1-RESKNR
               IF  RESKSUM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKSUM-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKSUM-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKSUM-04-L2         TO THE-PRIOR-L2
               MOVE  RESKSUM-04-L1         TO THE-PRIOR-L1
               SET RESKSUM-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESKSUM-MATCH-SET SECTION.
       RESKSUM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKSUM-IO-AREA (1:3)  TO RESKSUM-M-04-M2-FIRMA
               MOVE RESKSUM-IO-AREA (4:6)  TO RESKSUM-M-04-M1-RESKNR
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (136:3) TO RPROSN-IO
               INSPECT RPROSN-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (139:1) TO RRUT (1:1)
               MOVE FIRMAF-IO-AREA (149:3) TO RPOSTB-IO
               INSPECT RPOSTB-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (152:2) TO RDAGER-IO
               INSPECT RDAGER-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (154:3) TO RPROSA-IO
               INSPECT RPROSA-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  RENTEG-EOF
               MOVE HIGH-VALUES            TO RENTEG-MC
                                              RENTEG-MP
           END-IF
           IF  RESKSUM-EOF
               MOVE HIGH-VALUES            TO RESKSUM-MC
                                              RESKSUM-MP
           END-IF
           IF  RENTEG-MC < RENTEG-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESKSUM-MC < RESKSUM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RENTEG-MC < RESKSUM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RENTEG-PROCESS      TO TRUE
                   MOVE RENTEG-MC          TO RENTEG-MP
                   IF  RENTEG-MC = RESKSUM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESKSUM-MC < RENTEG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKSUM-PROCESS     TO TRUE
                   MOVE RESKSUM-MC         TO RESKSUM-MP
                   IF  RESKSUM-MC = RENTEG-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RENTEG-MC = RESKSUM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RENTEG-PROCESS      TO TRUE
                   MOVE RENTEG-MC          TO RENTEG-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-16 AND NOT-I-15)
           AND (NOT-I-17)
               MOVE SPACES TO RENTUT-IO-AREA
               INITIALIZE RENTUT-IO-AREA
               MOVE REC                    TO RENTUT-IO-AREA (1:200)
               MOVE FORFAL-IO              TO RENTUT-IO-AREA (33:6)
               IF  (I-20)
                   MOVE 'U'                TO RENTUT-IO-AREA (48:1)
               END-IF
               IF  (I-21)
                   MOVE 'U'                TO RENTUT-IO-AREA (49:1)
               END-IF
               MOVE RENTEN-IO              TO RENTUT-IO-AREA (61:9)
               INITIALIZE RENTEN-IO
               MOVE RDAG-IO                TO RENTUT-IO-AREA (71:4)
               MOVE RENTEA-IO              TO RENTUT-IO-AREA (78:9)
               INITIALIZE RENTEA-IO
               WRITE RENTUT-IO-AREA
           END-IF
           IF  (I-02)
               MOVE MERKE                  TO RENTEG-IO-AREA (89:1)
               REWRITE RENTEG-IO-AREA
           END-IF
           IF  (I-98)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> PROGRAM: RSK351 ' TO LISTE-IO-AREA (1:20)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> '                 TO LISTE-IO-AREA (1:4)
               MOVE DTOMEL                 TO LISTE-IO-AREA (14:57)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> REC :'            TO LISTE-IO-AREA (1:9)
               MOVE REC60                  TO LISTE-IO-AREA (11:60)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-99)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> CANCEL RSK351  ==>  ' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE 'FEIL I DATO (SUBRUT DATO' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE '8SIF)       '         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'RESPER: '             TO CONSOLE-IO-AREA (5:8)
               MOVE RESPER-IO              TO CONSOLE-IO-AREA (15:6)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (4:57)
               DISPLAY CONSOLE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAM RSK351.         ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDER MED NEGATIV SALDO' TO LISTE-IO-AREA (1:24)
               MOVE ' OG RENTEBEREGNING NÅ.  ' TO LISTE-IO-AREA (25:24)
               MOVE ' FREMSTILT.'          TO LISTE-IO-AREA (59:11)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (70:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA KUNDE'          TO LISTE-IO-AREA (1:11)
               MOVE 'TOTALSALDO'           TO LISTE-IO-AREA (21:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAM RSK351.         ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDER MED NEGATIV SALDO' TO LISTE-IO-AREA (1:24)
               MOVE ' OG RENTEBEREGNING NÅ.  ' TO LISTE-IO-AREA (25:24)
               MOVE ' FREMSTILT.'          TO LISTE-IO-AREA (59:11)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (70:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA KUNDE'          TO LISTE-IO-AREA (1:11)
               MOVE 'TOTALSALDO'           TO LISTE-IO-AREA (21:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-18)
               MOVE SPACES TO KUNDET-IO-AREA
               INITIALIZE KUNDET-IO-AREA
               MOVE '31'                   TO KUNDET-IO-AREA (1:2)
               MOVE FIRMA                  TO KUNDET-IO-AREA (3:3)
               MOVE RESKNR                 TO KUNDET-IO-AREA (6:6)
               MOVE RBELO-ELGP-IO          TO KUNDET-IO-AREA (12:9)
               MOVE RBELAV-IO              TO KUNDET-IO-AREA (22:9)
               WRITE KUNDET-IO-AREA
           END-IF
           IF  (I-L1 AND I-17 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (2:3)
               MOVE RESKNR                 TO LISTE-IO-AREA (7:6)
               MOVE SALDO                  TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (16:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '******   TOTALER FRA' TO LISTE-IO-AREA (3:20)
               MOVE 'RSK351   *******'     TO LISTE-IO-AREA (24:16)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '********************' TO LISTE-IO-AREA (3:20)
               MOVE '*******************'  TO LISTE-IO-AREA (21:19)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC LEST.'        TO LISTE-IO-AREA (6:13)
               MOVE TOTANT                 TO XO-70YYZ
               MOVE XO-70YYZ               TO LISTE-IO-AREA (28:9)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC TIL NESTE MND.' TO LISTE-IO-AREA (6:22)
               MOVE NESTEM                 TO XO-50YYZ
               MOVE XO-50YYZ               TO LISTE-IO-AREA (31:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT MED NEGATIV SALDO ' TO LISTE-IO-AREA (6:22)
               MOVE NEGSAL                 TO XO-70YYZ
               MOVE XO-70YYZ               TO LISTE-IO-AREA (28:9)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '********************' TO LISTE-IO-AREA (3:20)
               MOVE '*******************'  TO LISTE-IO-AREA (21:19)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 01                     TO LISTE-AFTER-SKIP
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-U2 AND I-U3)
           AND (I-03 AND I-97)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DTODTO                 TO LISTE-IO-AREA (75:6)
      *****************************************************************
      * FEILMELDINGER PÅ KONSOLLET:                                   *
      *****************************************************************
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
           MOVE 3                          TO LR-CHECK
           INITIALIZE RESPAR-DATA-FIELDS
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN INPUT RESPAR
           SET RENTEG-LEVEL-INIT           TO TRUE
           INITIALIZE RENTEG-DATA-FIELDS
           SET RENTEG-EOF-OFF              TO TRUE
           SET RENTEG-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO RENTEG-MC
                                              RENTEG-MP
           OPEN I-O RENTEG
           SET RESKSUM-LEVEL-INIT          TO TRUE
           INITIALIZE RESKSUM-DATA-FIELDS
           SET RESKSUM-EOF-OFF             TO TRUE
           SET RESKSUM-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESKSUM-MC
                                              RESKSUM-MP
           OPEN INPUT RESKSUM
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT RENTUT
           OPEN OUTPUT KUNDET
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESPAR
           CLOSE RENTEG
           CLOSE RESKSUM
           CLOSE FIRMAF
           CLOSE RENTUT
           CLOSE KUNDET
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
