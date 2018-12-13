       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO276R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM   : RKO276 - MERKER REF.NR SOM ER UTLIGNET            *
      *                    - MERKER RESK.NR MED SALDO=0               *
      * LAGET AV  : MORTEN TUVRØNNINGEN JUL 2013                      *
      * E 30.01.18: BH - FEIL VED UTSKRIFT TIL REFNULL L2 -> L1       *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO276.rpg
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
           SELECT RESKFIL
               ASSIGN TO UT-S-RESKFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKFIL-STATUS.
           SELECT RSKNULL
               ASSIGN TO UT-S-RSKNULL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RSKNULL-STATUS.
           SELECT REFNULL
               ASSIGN TO UT-S-REFNULL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REFNULL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESKFIL
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  RESKFIL-IO-AREA.
           05  RESKFIL-IO-AREA-X           PICTURE X(200).
       FD RSKNULL
               BLOCK CONTAINS 9
               RECORD CONTAINS 9.
       01  RSKNULL-IO-AREA.
           05  RSKNULL-IO-AREA-X           PICTURE X(9).
       FD REFNULL
               BLOCK CONTAINS 15
               RECORD CONTAINS 15.
       01  REFNULL-IO-AREA.
           05  REFNULL-IO-AREA-X           PICTURE X(15).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESKFIL-STATUS              PICTURE 99 VALUE 0.
           10  RSKNULL-STATUS              PICTURE 99 VALUE 0.
           10  REFNULL-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKFIL-EOF-OFF         VALUE '0'.
               88  RESKFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKFIL-READ-OFF        VALUE '0'.
               88  RESKFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKFIL-PROCESS-OFF     VALUE '0'.
               88  RESKFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKFIL-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKFIL-LEVEL-INIT      VALUE '1'.
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
           05  RESKFIL-LEVEL-01.
               10  RESKFIL-01-L3.
                   15  RESKFIL-01-L3-FIRMA PICTURE X(3).
               10  RESKFIL-01-L2.
                   15  RESKFIL-01-L2-RESKNR PICTURE X(6).
               10  RESKFIL-01-L1.
                   15  RESKFIL-01-L1-REFNR PICTURE X(6).
           05  RESKFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  REFNR                   PICTURE X(6).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KLOKKE-IO.
                   15  KLOKKE              PICTURE S9(6).
               10  FIRDEB-IO.
                   15  FIRDEB              PICTURE S9(9)V9(2).
               10  FIRKRE-IO.
                   15  FIRKRE              PICTURE S9(9)V9(2).
               10  FIRANT-IO.
                   15  FIRANT              PICTURE S9(7).
               10  RSKDEB-IO.
                   15  RSKDEB              PICTURE S9(9)V9(2).
               10  RSKKRE-IO.
                   15  RSKKRE              PICTURE S9(9)V9(2).
               10  RSKANT-IO.
                   15  RSKANT              PICTURE S9(7).
               10  REFDEB-IO.
                   15  REFDEB              PICTURE S9(9)V9(2).
               10  REFKRE-IO.
                   15  REFKRE              PICTURE S9(9)V9(2).
               10  REFANT-IO.
                   15  REFANT              PICTURE S9(7).
               10  TOTDEB-IO.
                   15  TOTDEB              PICTURE S9(9)V9(2).
               10  TOTKRE-IO.
                   15  TOTKRE              PICTURE S9(9)V9(2).
               10  TOTANT-IO.
                   15  TOTANT              PICTURE S9(7).
               10  REFBEL-IO.
                   15  REFBEL              PICTURE S9(9)V9(2).
               10  RSKBEL-IO.
                   15  RSKBEL              PICTURE S9(9)V9(2).
               10  FIRBEL-IO.
                   15  FIRBEL              PICTURE S9(9)V9(2).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESKFIL-PROCESS
               SET RESKFIL-PROCESS-OFF     TO TRUE
               SET RESKFIL-READ            TO TRUE
           END-IF
 
           IF  RESKFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKFIL-GET
               SET RESKFIL-READ-OFF        TO TRUE
               IF  NOT RESKFIL-EOF
                   SET RESKFIL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESKFIL-PROCESS
               PERFORM RESKFIL-IDSET
           END-IF
 
           IF  RESKFIL-PROCESS
               PERFORM RESKFIL-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  RESKFIL-PROCESS
               PERFORM RESKFIL-FLDOFF
               PERFORM RESKFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-98)
               SET NOT-I-97                TO TRUE
           END-IF
           IF  (NOT-I-98)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO KLOKKE (1:6)
               SET I-98                    TO TRUE
               SET I-97                    TO TRUE
           END-IF
           IF  (I-L3)
               MOVE 0                      TO FIRDEB
               MOVE 0                      TO FIRKRE
               MOVE 0                      TO FIRANT
           END-IF
           IF  (I-L2)
               MOVE 0                      TO RSKDEB
               MOVE 0                      TO RSKKRE
               MOVE 0                      TO RSKANT
           END-IF
           IF  (I-L1)
               MOVE 0                      TO REFDEB
               MOVE 0                      TO REFKRE
               MOVE 0                      TO REFANT
           END-IF
           IF  (I-31)
               ADD BELO-ELGP               TO TOTDEB
           END-IF
           IF  (I-30)
               ADD BELO-ELGP               TO TOTKRE
           END-IF
           IF  (I-31)
               ADD BELO-ELGP               TO FIRDEB
           END-IF
           IF  (I-30)
               ADD BELO-ELGP               TO FIRKRE
           END-IF
           IF  (I-31)
               ADD BELO-ELGP               TO RSKDEB
           END-IF
           IF  (I-30)
               ADD BELO-ELGP               TO RSKKRE
           END-IF
           IF  (I-31)
               ADD BELO-ELGP               TO REFDEB
           END-IF
           IF  (I-30)
               ADD BELO-ELGP               TO REFKRE
      *
           END-IF
           IF  (I-01)
               ADD 1                       TO TOTANT
               ADD 1                       TO FIRANT
               ADD 1                       TO RSKANT
               ADD 1                       TO REFANT
      *
           END-IF
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD REFKRE TO REFDEB    GIVING REFBEL
               SET NOT-I-21                TO TRUE
               IF  REFBEL = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               ADD RSKKRE TO RSKDEB    GIVING RSKBEL
               SET NOT-I-22                TO TRUE
               IF  RSKBEL = 0
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-L3)
               ADD FIRKRE TO FIRDEB    GIVING FIRBEL
               SET NOT-I-23                TO TRUE
               IF  FIRBEL NOT < 0
                   SET I-23                TO TRUE
               END-IF
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD TOTKRE TO TOTDEB        GIVING TOTBEL
           SET NOT-I-24                    TO TRUE
           IF  TOTBEL NOT < 0
               SET I-24                    TO TRUE
           END-IF.
 
       RESKFIL-GET SECTION.
       RESKFIL-GET-P.
           IF  RESKFIL-EOF-OFF
               READ RESKFIL
               AT END
                   SET RESKFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKFIL-FLDOFF SECTION.
       RESKFIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-31                TO TRUE
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
           END-EVALUATE.
 
       RESKFIL-FLDSET SECTION.
       RESKFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKFIL-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE RESKFIL-IO-AREA (6:6)  TO RESKNR (1:6)
               MOVE RESKFIL-IO-AREA (36:6) TO REFNR (1:6)
               MOVE RESKFIL-IO-AREA (48:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               IF  BELO-ELGP = ZERO
                   SET I-31                TO TRUE
               END-IF
               IF  BELO-ELGP > ZERO
                   SET I-31                TO TRUE
               END-IF
               IF  BELO-ELGP < ZERO
                   SET I-30                TO TRUE
               END-IF
           END-EVALUATE.
 
       RESKFIL-IDSET SECTION.
       RESKFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESKFIL-CHK-LEVEL SECTION.
       RESKFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKFIL-LEVEL-01
               MOVE RESKFIL-IO-AREA (3:3)  TO RESKFIL-01-L3-FIRMA
               MOVE RESKFIL-IO-AREA (6:6)  TO RESKFIL-01-L2-RESKNR
               MOVE RESKFIL-IO-AREA (36:6) TO RESKFIL-01-L1-REFNR
               IF  RESKFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESKFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKFIL-01-L3         TO THE-PRIOR-L3
               MOVE  RESKFIL-01-L2         TO THE-PRIOR-L2
               MOVE  RESKFIL-01-L1         TO THE-PRIOR-L1
               SET RESKFIL-LEVEL-INIT      TO TRUE
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-97)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FJERNE POSTER FRA """"""""""""""""""AD"T IT-OAE
                                                                   12)
               MOVE 'SPESIFIKASJON"""""""""""""""""""""     
                                                          OLSEI-RA(52)
               MOVE 'PROGRAM RKO276'       TO LISTE-IO-AREA (48:14)
               MOVE 'DATO   :'             TO LISTE-IO-AREA (63:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               IF  I-U6
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MERKER REF.NR OG RESK.NR' TO LISTE-IO-AREA (1:24)
               MOVE ' SOM GÅR I NULL         ' TO LISTE-IO-AREA (25:24)
               MOVE 'KLOKKE :'             TO LISTE-IO-AREA (63:8)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SIDE   :'             TO LISTE-IO-AREA (63:8)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (77:4)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FJERNE POSTER FRA """"""""""""""""""AD"T IT-OAE
                                                                   12)
               MOVE 'SPESIFIKASJON"""""""""""""""""""""     
                                                          OLSEI-RA(52)
               MOVE 'PROGRAM RKO276'       TO LISTE-IO-AREA (48:14)
               MOVE 'DATO   :'             TO LISTE-IO-AREA (63:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               IF  I-U6
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MERKER REF.NR OG RESK.NR' TO LISTE-IO-AREA (1:24)
               MOVE ' SOM GÅR I NULL         ' TO LISTE-IO-AREA (25:24)
               MOVE 'KLOKKE :'             TO LISTE-IO-AREA (63:8)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SIDE   :'             TO LISTE-IO-AREA (63:8)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (77:4)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-21)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE RESKNR                 TO LISTE-IO-AREA (5:6)
               MOVE REFNR                  TO LISTE-IO-AREA (12:6)
               MOVE REFDEB                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (26:15)
               MOVE REFKRE                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (46:15)
               MOVE REFANT                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (61:10)
               MOVE '* REF'                TO LISTE-IO-AREA (76:5)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF
           IF  (I-L2 AND I-22 AND I-U7)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE RESKNR                 TO LISTE-IO-AREA (5:6)
               MOVE RSKDEB                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (26:15)
               MOVE RSKKRE                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (46:15)
               MOVE RSKANT                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (61:10)
               MOVE '* RSK'                TO LISTE-IO-AREA (76:5)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF
           IF  (I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE 'D/K'                  TO LISTE-IO-AREA (8:3)
               MOVE FIRDEB                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (26:15)
               MOVE FIRKRE                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (46:15)
               MOVE FIRANT                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (61:10)
               MOVE '* FIR'                TO LISTE-IO-AREA (76:5)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE 'NET'                  TO LISTE-IO-AREA (8:3)
               IF  (I-23)
                   MOVE FIRBEL             TO XO-92YY9R
                   MOVE XO-92YY9R          TO LISTE-IO-AREA (26:15)
               END-IF
               IF  (NOT-I-23)
                   MOVE FIRBEL             TO XO-92YY9R
                   MOVE XO-92YY9R          TO LISTE-IO-AREA (46:15)
               END-IF
               MOVE '* FIR'                TO LISTE-IO-AREA (76:5)
               IF  I-U6
                   MOVE 2                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOT'                  TO LISTE-IO-AREA (1:3)
               MOVE 'D/K'                  TO LISTE-IO-AREA (8:3)
               MOVE TOTDEB                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (26:15)
               MOVE TOTKRE                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (46:15)
               MOVE TOTANT                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (61:10)
               MOVE '* TOT'                TO LISTE-IO-AREA (76:5)
               IF  I-U6
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOT'                  TO LISTE-IO-AREA (1:3)
               MOVE 'NET'                  TO LISTE-IO-AREA (8:3)
               IF  (I-24)
                   MOVE TOTBEL             TO XO-92YY9R
                   MOVE XO-92YY9R          TO LISTE-IO-AREA (26:15)
               END-IF
               IF  (NOT-I-24)
                   MOVE TOTBEL             TO XO-92YY9R
                   MOVE XO-92YY9R          TO LISTE-IO-AREA (46:15)
               END-IF
               MOVE '* TOT'                TO LISTE-IO-AREA (76:5)
               IF  I-U6
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF
           IF  (I-L1 AND I-21)
               MOVE SPACES TO REFNULL-IO-AREA
               INITIALIZE REFNULL-IO-AREA
               MOVE FIRMA                  TO REFNULL-IO-AREA (1:3)
               MOVE RESKNR                 TO REFNULL-IO-AREA (4:6)
               MOVE REFNR                  TO REFNULL-IO-AREA (10:6)
               IF  I-U8
                   WRITE REFNULL-IO-AREA
               END-IF
           END-IF
           IF  (I-L2 AND I-22)
               MOVE SPACES TO RSKNULL-IO-AREA
               INITIALIZE RSKNULL-IO-AREA
               MOVE FIRMA                  TO RSKNULL-IO-AREA (1:3)
               MOVE RESKNR                 TO RSKNULL-IO-AREA (4:6)
               IF  I-U7
                   WRITE RSKNULL-IO-AREA
               END-IF
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
           SET RESKFIL-LEVEL-INIT          TO TRUE
           INITIALIZE RESKFIL-DATA-FIELDS
           SET RESKFIL-EOF-OFF             TO TRUE
           SET RESKFIL-PROCESS             TO TRUE
           OPEN INPUT RESKFIL
           IF I-U7
               OPEN OUTPUT RSKNULL
           END-IF
           IF I-U8
               OPEN OUTPUT REFNULL
           END-IF
           IF I-U6
               OPEN OUTPUT LISTE
           END-IF
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKFIL
           IF I-U7
               CLOSE RSKNULL
           END-IF
           IF I-U8
               CLOSE REFNULL
           END-IF
           IF I-U6
               CLOSE LISTE
           END-IF.
 
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
