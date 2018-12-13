       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD107R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: ORD107                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMERT.: 25.08.95                                        *
      *  SIST RETTET: 21.07.05                                        *
      *                                                               *
      *  PROGRAMMET DANNE NY ORDRE.HIST.FILE                          *
      * 25.08.95  ENDRET SLETTEGRENSEN FRA 2 TIL 3 MND.               *
      * 20.10.98  TATT I BRUK 8 SIFFERET DATO VIA SUBRUT: DATO8SIF    *
      * 11.01.00  VED FEIL ORDREDATO, RETTES DETTE TIL DAGENS DATO.   *
      *  5.02.00  ENDRET SLETTEGRENSE FRA 3 TIL 12 MND.               *
      *  5.09.01  TEST PÅ OM FIRMA ER SLETTET.                        *
      * 28.02.05  ENDRET SLETTEGRENSE FRA 12 TIL 36 MND (FULLBRUKERE) *
      * 01.03.05  KUNDER MED EGET ANLEGG SLETTES ETTER 12 MND.        *
      * 20.09.06  ENDRET SLETTEGRENSE FRA 12 TIL 60 MND (FULLBRUKERE) *
      ******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD107.rpg
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
           SELECT ORDSEL
               ASSIGN TO UT-S-ORDSEL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSEL-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT ORDHIST
               ASSIGN TO ORDHIST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDHIST-STATUS
               RECORD KEY IS ORDHIST-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD ORDSEQ
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDSEQ-IO-AREA.
           05  ORDSEQ-IO-AREA-X            PICTURE X(164).
       FD ORDSEL
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDSEL-IO-AREA.
           05  ORDSEL-IO-AREA-X            PICTURE X(164).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD ORDHIST
               RECORD CONTAINS 164.
       01  ORDHIST-IO-AREA.
           05  ORDHIST-IO-AREA-X.
               10  ORDHIST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDSEQ-STATUS               PICTURE 99 VALUE 0.
           10  ORDSEL-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  ORDHIST-STATUS              PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
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
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSEL-EOF-OFF          VALUE '0'.
               88  ORDSEL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSEL-READ-OFF         VALUE '0'.
               88  ORDSEL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSEL-PROCESS-OFF      VALUE '0'.
               88  ORDSEL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDSEL-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDSEL-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  ORDHIST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
      *PSDS: DATA STRUCTURE FIELDS
           05  PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(28).
               10  R                       PICTURE X(8).
               10  FILLER                  PICTURE X(44).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  P-IO.
                   15  P                   PICTURE S9(3).
               10  FILLER                  PICTURE X(41).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(10).
               10  S-IO.
                   15  S                   PICTURE S9(5).
               10  FILLER                  PICTURE X(65).
      *DSDS: DATA STRUCTURE FIELDS
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  ORDSEQ-LEVEL-01.
               10  ORDSEQ-01-L1.
                   15  ORDSEQ-01-L1-FIRMA  PICTURE X(3).
           05  ORDSEQ-DATA-FIELDS.
               10  ORDKEY                  PICTURE X(20).
               10  FIRMA                   PICTURE X(3).
               10  ORDATO                  PICTURE X(6).
               10  OHREC1                  PICTURE X(164).
               10  OHREC2                  PICTURE X(164).
               10  OHREC3                  PICTURE X(164).
               10  OVREC                   PICTURE X(164).
           05  ORDSEQ-MP                   PICTURE X(20).
           05  ORDSEQ-MC                   PICTURE X(20).
           05  ORDSEQ-M-01             REDEFINES ORDSEQ-MC.
               10  ORDSEQ-M-01-M1.
                   15  ORDSEQ-M-01-M1-ORDKEY-G.
                       20  ORDSEQ-M-01-M1-ORDKEY PICTURE X(20).
           05  ORDSEQ-M-02             REDEFINES ORDSEQ-MC.
               10  ORDSEQ-M-02-M1.
                   15  ORDSEQ-M-02-M1-ORDKEY-G.
                       20  ORDSEQ-M-02-M1-ORDKEY PICTURE X(20).
           05  ORDSEQ-M-03             REDEFINES ORDSEQ-MC.
               10  ORDSEQ-M-03-M1.
                   15  ORDSEQ-M-03-M1-ORDKEY-G.
                       20  ORDSEQ-M-03-M1-ORDKEY PICTURE X(20).
           05  ORDSEQ-M-04             REDEFINES ORDSEQ-MC.
               10  ORDSEQ-M-04-M1.
                   15  ORDSEQ-M-04-M1-ORDKEY-G.
                       20  ORDSEQ-M-04-M1-ORDKEY PICTURE X(20).
           05  ORDSEL-LEVEL-06.
               10  ORDSEL-06-L1.
                   15  ORDSEL-06-L1-FIRMA  PICTURE X(3).
           05  ORDSEL-DATA-FIELDS.
               10  SHREC1                  PICTURE X(164).
               10  SHREC2                  PICTURE X(164).
               10  SHREC3                  PICTURE X(164).
               10  SVREC                   PICTURE X(164).
           05  ORDSEL-MP                   PICTURE X(20).
           05  ORDSEL-MC                   PICTURE X(20).
           05  ORDSEL-M-06             REDEFINES ORDSEL-MC.
               10  ORDSEL-M-06-M1.
                   15  ORDSEL-M-06-M1-ORDKEY-G.
                       20  ORDSEL-M-06-M1-ORDKEY PICTURE X(20).
           05  ORDSEL-M-07             REDEFINES ORDSEL-MC.
               10  ORDSEL-M-07-M1.
                   15  ORDSEL-M-07-M1-ORDKEY-G.
                       20  ORDSEL-M-07-M1-ORDKEY PICTURE X(20).
           05  ORDSEL-M-08             REDEFINES ORDSEL-MC.
               10  ORDSEL-M-08-M1.
                   15  ORDSEL-M-08-M1-ORDKEY-G.
                       20  ORDSEL-M-08-M1-ORDKEY PICTURE X(20).
           05  ORDSEL-M-09             REDEFINES ORDSEL-MC.
               10  ORDSEL-M-09-M1.
                   15  ORDSEL-M-09-M1-ORDKEY-G.
                       20  ORDSEL-M-09-M1-ORDKEY PICTURE X(20).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
               10  FIRMBT                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  OSDATO                  PICTURE X(8).
               10  FFDAG                   PICTURE X(2).
               10  FFMND                   PICTURE X(2).
               10  FFAAR                   PICTURE X(2).
               10  FF4                     PICTURE X(4).
               10  DDATO6                  PICTURE X(6).
               10  DDDATO-IO.
                   15  DDDATO              PICTURE S9(8).
               10  DDATO8                  PICTURE X(8).
               10  FFDATO                  PICTURE X(8).
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
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
               PERFORM ORDSEQ-GET
               SET ORDSEQ-READ-OFF         TO TRUE
               IF  NOT ORDSEQ-EOF
                   PERFORM ORDSEQ-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM ORDSEQ-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDSEL-PROCESS
               SET ORDSEL-PROCESS-OFF      TO TRUE
               SET ORDSEL-READ             TO TRUE
           END-IF
 
           IF  ORDSEL-READ
               PERFORM ORDSEL-GET
               SET ORDSEL-READ-OFF         TO TRUE
               IF  NOT ORDSEL-EOF
                   PERFORM ORDSEL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM ORDSEL-MATCH-SET
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
 
           IF  ORDSEQ-PROCESS
               PERFORM ORDSEQ-IDSET
           END-IF
 
           IF  ORDSEL-PROCESS
               PERFORM ORDSEL-IDSET
           END-IF
 
           IF  ORDSEQ-PROCESS
               PERFORM ORDSEQ-CHK-LEVEL
           END-IF
 
           IF  ORDSEL-PROCESS
               PERFORM ORDSEL-CHK-LEVEL
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
 
           IF  ORDSEQ-PROCESS
               PERFORM ORDSEQ-FLDSET
           END-IF
 
           IF  ORDSEL-PROCESS
               PERFORM ORDSEL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDSEQ-PROCESS
           OR  ORDSEL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               SET NOT-I-10                TO TRUE
           END-IF
           IF  (I-06)
               SET NOT-I-10                TO TRUE
           END-IF
           IF  (NOT-I-01 AND NOT-I-06)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               PERFORM FISLET-S
           END-IF
           IF  (I-98)
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR Å SE OM ORDREDATO ER ELDERE ENN 24 MND.           *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-97                TO TRUE
               IF  FIRMA = '918'
                   SET I-97                TO TRUE
               END-IF
               PERFORM FFRUT-S
           END-IF
           PERFORM DATRUT-S
           SET NOT-I-23                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           IF  OSDATO > FFDATO
               SET I-23                    TO TRUE
           END-IF
           IF  OSDATO < FFDATO
               SET I-21                    TO TRUE
           END-IF
           IF  OSDATO = FFDATO
               SET I-22                    TO TRUE
           END-IF
           IF  (I-23)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-22 AND NOT-I-21)
               SET I-10                    TO TRUE
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *  SUBRUTINE FOR Å SNU ORDREDATO SAMT OG HENTE 4 SIFFERER ÅR.
      *  SER ORDREDATO FEIL, SETTES INN DAGENS DATO I ORDREDATO.
      *****************************************************************
           CONTINUE.
 
       DATRUT-S SECTION.
       DATRUT-S-P.
           MOVE 'A'                        TO DATOK
           MOVE ORDATO                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           SET NOT-I-44                    TO TRUE
           IF  DATOK = 'F'
               SET I-44                    TO TRUE
           END-IF
           IF  (NOT-I-44)
               MOVE AMD8                   TO OSDATO
           END-IF
           IF  (I-44)
               MOVE DDATO8                 TO OSDATO
           END-IF.
      *****************************************************************
      *  RUTINE FOR Å BEREGNE DATO - 24 MND FOR FULLBRUKERE.          *
      *                            - 12 MND FOR KUNDER MED EGET ANLEGG*
      *****************************************************************
 
       FFRUT-S SECTION.
       FFRUT-S-P.
           MOVE UDAY                       TO FFDAG
           MOVE UMONTH                     TO FFMND
           MOVE UYEAR                      TO FFAAR
           MOVE FFDAG                      TO FF4 (1:2)
           MOVE FFMND                      TO FF4 (3:2)
           MOVE FF4                        TO DATO6 (1:4)
           MOVE FFAAR                      TO DATO6 (5:2)
           MOVE 'A'                        TO DATOK
           MOVE DATO6                      TO DDATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO DDDATO-IO
           MOVE AMD8                       TO DDATO8
           SET NOT-I-45                    TO TRUE
           IF  FIRMBT = 'E'
               SET I-45                    TO TRUE
           END-IF
           IF  (NOT-I-45 AND NOT-I-97)
               SUBTRACT 50000              FROM DDDATO
           END-IF
           IF  (NOT-I-45 AND I-97)
               SUBTRACT 50000              FROM DDDATO
           END-IF
           IF  (I-45)
               SUBTRACT 10000              FROM DDDATO
           END-IF
           MOVE DDDATO                     TO FFDATO
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE FFDATO (8:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO FFDATO (8:1).
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
      *****************************************************************
 
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
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEQ-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDSEQ-IO-AREA (136:6) TO ORDATO (1:6)
               MOVE ORDSEQ-IO-AREA (1:164) TO OHREC1 (1:164)
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEQ-IO-AREA (1:164) TO OHREC2 (1:164)
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEQ-IO-AREA (1:164) TO OHREC3 (1:164)
           WHEN ( ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEQ-IO-AREA (1:164) TO OVREC (1:164)
           END-EVALUATE.
 
       ORDSEQ-IDCHK SECTION.
       ORDSEQ-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
             OR ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
             OR ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
             OR ( ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDSEQ-IDSET SECTION.
       ORDSEQ-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDSEQ-CHK-LEVEL SECTION.
       ORDSEQ-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDSEQ-LEVEL-01
               MOVE ORDSEQ-IO-AREA (2:3)   TO ORDSEQ-01-L1-FIRMA
               IF  ORDSEQ-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSEQ-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSEQ-01-L1          TO THE-PRIOR-L1
               SET ORDSEQ-LEVEL-INIT       TO TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
               CONTINUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
               CONTINUE
           WHEN ( ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           END-EVALUATE.
 
       ORDSEQ-MATCH-SET SECTION.
       ORDSEQ-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '1' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDSEQ-M-01-M1-ORDKEY
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '2' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDSEQ-M-02-M1-ORDKEY
           WHEN ( ORDSEQ-IO-AREA (19:1) = ' '
            AND   ORDSEQ-IO-AREA (20:1) = '3' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDSEQ-M-03-M1-ORDKEY
           WHEN ( ORDSEQ-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDSEQ-IO-AREA (1:20)  TO ORDSEQ-M-04-M1-ORDKEY
           END-EVALUATE.
 
       ORDSEL-GET SECTION.
       ORDSEL-GET-P.
           IF  ORDSEL-EOF-OFF
               READ ORDSEL
               AT END
                   SET ORDSEL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDSEL-FLDSET SECTION.
       ORDSEL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '1' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEL-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDSEL-IO-AREA (136:6) TO ORDATO (1:6)
               MOVE ORDSEL-IO-AREA (1:164) TO SHREC1 (1:164)
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '2' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEL-IO-AREA (1:164) TO SHREC2 (1:164)
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '3' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEL-IO-AREA (1:164) TO SHREC3 (1:164)
           WHEN ( ORDSEL-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDKEY (1:20)
               MOVE ORDSEL-IO-AREA (1:164) TO SVREC (1:164)
           END-EVALUATE.
 
       ORDSEL-IDCHK SECTION.
       ORDSEL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '1' )
             OR ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '2' )
             OR ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '3' )
             OR ( ORDSEL-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDSEL-IDSET SECTION.
       ORDSEL-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '1' )
               SET I-06                    TO TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '2' )
               SET I-07                    TO TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '3' )
               SET I-08                    TO TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) NOT = ' ' )
               SET I-09                    TO TRUE
           END-EVALUATE.
 
       ORDSEL-CHK-LEVEL SECTION.
       ORDSEL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDSEL-LEVEL-06
               MOVE ORDSEL-IO-AREA (2:3)   TO ORDSEL-06-L1-FIRMA
               IF  ORDSEL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSEL-06-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSEL-06-L1          TO THE-PRIOR-L1
               SET ORDSEL-LEVEL-INIT       TO TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '2' )
               CONTINUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '3' )
               CONTINUE
           WHEN ( ORDSEL-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           END-EVALUATE.
 
       ORDSEL-MATCH-SET SECTION.
       ORDSEL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '1' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDSEL-M-06-M1-ORDKEY
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '2' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDSEL-M-07-M1-ORDKEY
           WHEN ( ORDSEL-IO-AREA (19:1) = ' '
            AND   ORDSEL-IO-AREA (20:1) = '3' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDSEL-M-08-M1-ORDKEY
           WHEN ( ORDSEL-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDSEL-IO-AREA (1:20)  TO ORDSEL-M-09-M1-ORDKEY
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
               MOVE FIRMAF-IO-AREA (956:1) TO FIRMBT (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  ORDSEQ-EOF
               MOVE HIGH-VALUES            TO ORDSEQ-MC
                                              ORDSEQ-MP
           END-IF
           IF  ORDSEL-EOF
               MOVE HIGH-VALUES            TO ORDSEL-MC
                                              ORDSEL-MP
           END-IF
           IF  ORDSEQ-MC < ORDSEQ-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDSEL-MC < ORDSEL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  ORDSEQ-MC < ORDSEL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDSEQ-PROCESS      TO TRUE
                   MOVE ORDSEQ-MC          TO ORDSEQ-MP
                   IF  ORDSEQ-MC = ORDSEL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDSEL-MC < ORDSEQ-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDSEL-PROCESS      TO TRUE
                   MOVE ORDSEL-MC          TO ORDSEL-MP
                   IF  ORDSEL-MC = ORDSEQ-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDSEQ-MC = ORDSEL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDSEQ-PROCESS      TO TRUE
                   MOVE ORDSEQ-MC          TO ORDSEQ-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-10 AND NOT-I-MR)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE OHREC1                 TO ORDHIST-IO-AREA (1:164)
               IF  (I-44)
                   MOVE DDATO6             TO ORDHIST-IO-AREA (136:6)
               END-IF
               WRITE ORDHIST-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-MR)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE OHREC2                 TO ORDHIST-IO-AREA (1:164)
               WRITE ORDHIST-IO-AREA
           END-IF
           IF  (I-03 AND I-10 AND NOT-I-MR)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE OHREC3                 TO ORDHIST-IO-AREA (1:164)
               WRITE ORDHIST-IO-AREA
           END-IF
           IF  (I-04 AND I-10 AND NOT-I-MR)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE OVREC                  TO ORDHIST-IO-AREA (1:164)
               WRITE ORDHIST-IO-AREA
           END-IF
           IF  (I-06 AND I-10)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE SHREC1                 TO ORDHIST-IO-AREA (1:164)
               IF  (I-44)
                   MOVE DDATO6             TO ORDHIST-IO-AREA (136:6)
               END-IF
               WRITE ORDHIST-IO-AREA
           END-IF
           IF  (I-07 AND I-10)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE SHREC2                 TO ORDHIST-IO-AREA (1:164)
               WRITE ORDHIST-IO-AREA
           END-IF
           IF  (I-08 AND I-10)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE SHREC3                 TO ORDHIST-IO-AREA (1:164)
               WRITE ORDHIST-IO-AREA
           END-IF
           IF  (I-09 AND I-10)
               MOVE SPACES TO ORDHIST-IO-AREA
               INITIALIZE ORDHIST-IO-AREA
               MOVE SVREC                  TO ORDHIST-IO-AREA (1:164)
               WRITE ORDHIST-IO-AREA
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
           SET ORDSEQ-LEVEL-INIT           TO TRUE
           INITIALIZE ORDSEQ-DATA-FIELDS
           SET ORDSEQ-EOF-OFF              TO TRUE
           SET ORDSEQ-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO ORDSEQ-MC
                                              ORDSEQ-MP
           OPEN INPUT ORDSEQ
           SET ORDSEL-LEVEL-INIT           TO TRUE
           INITIALIZE ORDSEL-DATA-FIELDS
           SET ORDSEL-EOF-OFF              TO TRUE
           SET ORDSEL-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO ORDSEL-MC
                                              ORDSEL-MP
           OPEN INPUT ORDSEL
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT ORDHIST.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDSEQ
           CLOSE ORDSEL
           CLOSE FIRMAF
           CLOSE ORDHIST.
 
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
