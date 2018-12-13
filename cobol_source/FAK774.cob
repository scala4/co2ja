       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK774R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: FAK774                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMT...: 30.10.2000                                      *
      *  RETTET.....: 17.01.2001                                      *
      *                                                               *
      *  U1 PRINTER PRINTER KONTROLLISTE.                             *
      *  MERGER OG OPPDATERER FAKTURA.SALGSDATA OG -                  *
      *  DANNER VSAM/KSDS FAKTURA.SALGSDATA, MED SEQ.NR. PR BRUDD.    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK774.rpg
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
           SELECT GMLFIL
               ASSIGN TO UT-S-GMLFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLFIL-STATUS.
           SELECT NYFIL
               ASSIGN TO UT-S-NYFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYFIL-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT FAKSALG
               ASSIGN TO FAKSALG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FAKSALG-STATUS
               RECORD KEY IS FAKSALG-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GMLFIL
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  GMLFIL-IO-AREA.
           05  GMLFIL-IO-AREA-X            PICTURE X(160).
       FD NYFIL
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  NYFIL-IO-AREA.
           05  NYFIL-IO-AREA-X             PICTURE X(160).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FAKSALG
               RECORD CONTAINS 160.
       01  FAKSALG-IO-AREA.
           05  FAKSALG-IO-AREA-X.
               10  FAKSALG-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(140).
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
           10  GMLFIL-STATUS               PICTURE 99 VALUE 0.
           10  NYFIL-STATUS                PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FAKSALG-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFIL-EOF-OFF          VALUE '0'.
               88  GMLFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFIL-READ-OFF         VALUE '0'.
               88  GMLFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFIL-PROCESS-OFF      VALUE '0'.
               88  GMLFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  GMLFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  GMLFIL-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYFIL-EOF-OFF           VALUE '0'.
               88  NYFIL-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYFIL-READ-OFF          VALUE '0'.
               88  NYFIL-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYFIL-PROCESS-OFF       VALUE '0'.
               88  NYFIL-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  NYFIL-LEVEL-INIT-OFF    VALUE '0'.
               88  NYFIL-LEVEL-INIT        VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FAKSALG-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  GMLFIL-LEVEL-01.
               10  GMLFIL-01-L3.
                   15  GMLFIL-01-L3-KONSER PICTURE X(3).
               10  GMLFIL-01-L2.
                   15  GMLFIL-01-L2-FAKMND PICTURE X(6).
               10  GMLFIL-01-L1.
                   15  GMLFIL-01-L1-KUNDNR PICTURE X(6).
           05  GMLFIL-DATA-FIELDS.
               10  KONSER                  PICTURE X(3).
               10  FAKMND                  PICTURE X(6).
               10  FAKMN1-IO.
                   15  FAKMN1              PICTURE S9(6).
               10  KUNDNR                  PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  REC1                    PICTURE X(160).
           05  GMLFIL-MP                   PICTURE X(15).
           05  GMLFIL-MC                   PICTURE X(15).
           05  GMLFIL-M-01             REDEFINES GMLFIL-MC.
               10  GMLFIL-M-01-M3.
                   15  GMLFIL-M-01-M3-KONSER-G.
                       20  GMLFIL-M-01-M3-KONSER PICTURE X(3).
               10  GMLFIL-M-01-M2.
                   15  GMLFIL-M-01-M2-FAKMND-G.
                       20  GMLFIL-M-01-M2-FAKMND PICTURE X(6).
               10  GMLFIL-M-01-M1.
                   15  GMLFIL-M-01-M1-KUNDNR-G.
                       20  GMLFIL-M-01-M1-KUNDNR PICTURE X(6).
           05  NYFIL-LEVEL-02.
               10  NYFIL-02-L3.
                   15  NYFIL-02-L3-KONSER  PICTURE X(3).
               10  NYFIL-02-L2.
                   15  NYFIL-02-L2-FAKMND  PICTURE X(6).
               10  NYFIL-02-L1.
                   15  NYFIL-02-L1-KUNDNR  PICTURE X(6).
           05  NYFIL-DATA-FIELDS.
               10  REC2                    PICTURE X(160).
           05  NYFIL-MP                    PICTURE X(15).
           05  NYFIL-MC                    PICTURE X(15).
           05  NYFIL-M-02              REDEFINES NYFIL-MC.
               10  NYFIL-M-02-M3.
                   15  NYFIL-M-02-M3-KONSER-G.
                       20  NYFIL-M-02-M3-KONSER PICTURE X(3).
               10  NYFIL-M-02-M2.
                   15  NYFIL-M-02-M2-FAKMND-G.
                       20  NYFIL-M-02-M2-FAKMND PICTURE X(6).
               10  NYFIL-M-02-M1.
                   15  NYFIL-M-02-M1-KUNDNR-G.
                       20  NYFIL-M-02-M1-KUNDNR PICTURE X(6).
           05  PARAM-DATA-FIELDS.
               10  SLMND-IO.
                   15  SLMND               PICTURE S9(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FAKPFJ-IO.
                   15  FAKPFJ              PICTURE S9(6).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(5).
               10  ANTGML-IO.
                   15  ANTGML              PICTURE S9(8).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(8).
               10  ANTSL-IO.
                   15  ANTSL               PICTURE S9(8).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
               10  EDIT-FAKPFJ             PICTURE ZZZZ.ZZ.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  GMLFIL-PROCESS
               SET GMLFIL-PROCESS-OFF      TO TRUE
               SET GMLFIL-READ             TO TRUE
           END-IF
 
           IF  GMLFIL-READ
               PERFORM GMLFIL-GET
               SET GMLFIL-READ-OFF         TO TRUE
               IF  NOT GMLFIL-EOF
                   PERFORM GMLFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  NYFIL-PROCESS
               SET NYFIL-PROCESS-OFF       TO TRUE
               SET NYFIL-READ              TO TRUE
           END-IF
 
           IF  NYFIL-READ
               PERFORM NYFIL-GET
               SET NYFIL-READ-OFF          TO TRUE
               IF  NOT NYFIL-EOF
                   PERFORM NYFIL-MATCH-SET
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
 
           IF  GMLFIL-PROCESS
               PERFORM GMLFIL-IDSET
           END-IF
 
           IF  NYFIL-PROCESS
               PERFORM NYFIL-IDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  GMLFIL-PROCESS
               PERFORM GMLFIL-CHK-LEVEL
           END-IF
 
           IF  NYFIL-PROCESS
               PERFORM NYFIL-CHK-LEVEL
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
 
           IF  GMLFIL-PROCESS
               PERFORM GMLFIL-FLDSET
           END-IF
 
           IF  NYFIL-PROCESS
               PERFORM NYFIL-FLDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  GMLFIL-PROCESS
           OR  NYFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å BEREGNE PERIODE 12 MND. TILBAKE.                 *
      *****************************************************************
           IF  (I-03)
               ADD SLMND TO ZERO       GIVING FAKPFJ
               GO TO SLUTT-T
      *****************************************************************
      * DANNE FORTLØPENDE SEQ.NR. PR. KONSERN,FAKMND OG KUNDENR.      *
      * OG Å SLETTE RECORDS SOM ER MERE EN 12 MND GAMLE.              *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SUBTRACT SEQNR              FROM SEQNR
           END-IF
           IF  (I-01)
               SET NOT-I-67                TO TRUE
               IF  FAKMN1 NOT > FAKPFJ
                   SET I-67                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-67)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND I-MR)
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO SEQNR
           SET I-10                        TO TRUE.
 
       SLUTT-T.
           IF  (I-01)
               ADD 1                       TO ANTGML
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTNYE
           END-IF
           IF  (I-01 AND I-67)
               ADD 1                       TO ANTSL
           END-IF
           IF  (I-01 AND NOT-I-67 AND I-MR)
               ADD 1                       TO ANTSL
           END-IF
           IF  (I-01 AND I-10)
               ADD 1                       TO ANTUT
           END-IF
           IF  (I-02 AND I-10)
               ADD 1                       TO ANTUT
           END-IF.
 
       GMLFIL-GET SECTION.
       GMLFIL-GET-P.
           IF  GMLFIL-EOF-OFF
               READ GMLFIL
               AT END
                   SET GMLFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLFIL-FLDSET SECTION.
       GMLFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLFIL-IO-AREA (1:3)   TO KONSER (1:3)
               MOVE GMLFIL-IO-AREA (4:6)   TO FAKMND (1:6)
               MOVE GMLFIL-IO-AREA (4:6)   TO FAKMN1-IO
               INSPECT FAKMN1-IO REPLACING ALL ' ' BY '0'
               MOVE GMLFIL-IO-AREA (10:6)  TO KUNDNR (1:6)
               MOVE GMLFIL-IO-AREA (39:3)  TO FIRMA (1:3)
               MOVE GMLFIL-IO-AREA (33:6)  TO ORDNR (1:6)
               MOVE GMLFIL-IO-AREA (1:160) TO REC1 (1:160)
           END-EVALUATE.
 
       GMLFIL-IDSET SECTION.
       GMLFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLFIL-CHK-LEVEL SECTION.
       GMLFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO GMLFIL-LEVEL-01
               MOVE GMLFIL-IO-AREA (1:3)   TO GMLFIL-01-L3-KONSER
               MOVE GMLFIL-IO-AREA (4:6)   TO GMLFIL-01-L2-FAKMND
               MOVE GMLFIL-IO-AREA (10:6)  TO GMLFIL-01-L1-KUNDNR
               IF  GMLFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  GMLFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  GMLFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  GMLFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  GMLFIL-01-L3          TO THE-PRIOR-L3
               MOVE  GMLFIL-01-L2          TO THE-PRIOR-L2
               MOVE  GMLFIL-01-L1          TO THE-PRIOR-L1
               SET GMLFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       GMLFIL-MATCH-SET SECTION.
       GMLFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLFIL-IO-AREA (1:3)   TO GMLFIL-M-01-M3-KONSER
               MOVE GMLFIL-IO-AREA (4:6)   TO GMLFIL-M-01-M2-FAKMND
               MOVE GMLFIL-IO-AREA (10:6)  TO GMLFIL-M-01-M1-KUNDNR
           END-EVALUATE.
 
       NYFIL-GET SECTION.
       NYFIL-GET-P.
           IF  NYFIL-EOF-OFF
               READ NYFIL
               AT END
                   SET NYFIL-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYFIL-FLDSET SECTION.
       NYFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NYFIL-IO-AREA (1:3)    TO KONSER (1:3)
               MOVE NYFIL-IO-AREA (4:6)    TO FAKMND (1:6)
               MOVE NYFIL-IO-AREA (10:6)   TO KUNDNR (1:6)
               MOVE NYFIL-IO-AREA (39:3)   TO FIRMA (1:3)
               MOVE NYFIL-IO-AREA (33:6)   TO ORDNR (1:6)
               MOVE NYFIL-IO-AREA (1:160)  TO REC2 (1:160)
           END-EVALUATE.
 
       NYFIL-IDSET SECTION.
       NYFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       NYFIL-CHK-LEVEL SECTION.
       NYFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO NYFIL-LEVEL-02
               MOVE NYFIL-IO-AREA (1:3)    TO NYFIL-02-L3-KONSER
               MOVE NYFIL-IO-AREA (4:6)    TO NYFIL-02-L2-FAKMND
               MOVE NYFIL-IO-AREA (10:6)   TO NYFIL-02-L1-KUNDNR
               IF  NYFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYFIL-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  NYFIL-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  NYFIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYFIL-02-L3           TO THE-PRIOR-L3
               MOVE  NYFIL-02-L2           TO THE-PRIOR-L2
               MOVE  NYFIL-02-L1           TO THE-PRIOR-L1
               SET NYFIL-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       NYFIL-MATCH-SET SECTION.
       NYFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE NYFIL-IO-AREA (1:3)    TO NYFIL-M-02-M3-KONSER
               MOVE NYFIL-IO-AREA (4:6)    TO NYFIL-M-02-M2-FAKMND
               MOVE NYFIL-IO-AREA (10:6)   TO NYFIL-M-02-M1-KUNDNR
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
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARAM-IO-AREA (21:6)   TO SLMND-IO
               INSPECT SLMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
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
           IF  GMLFIL-EOF
               MOVE HIGH-VALUES            TO GMLFIL-MC
                                              GMLFIL-MP
           END-IF
           IF  NYFIL-EOF
               MOVE HIGH-VALUES            TO NYFIL-MC
                                              NYFIL-MP
           END-IF
           IF  GMLFIL-MC < GMLFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  NYFIL-MC < NYFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLFIL-MC < NYFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLFIL-PROCESS      TO TRUE
                   MOVE GMLFIL-MC          TO GMLFIL-MP
                   IF  GMLFIL-MC = NYFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  NYFIL-MC < GMLFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET NYFIL-PROCESS       TO TRUE
                   MOVE NYFIL-MC           TO NYFIL-MP
                   IF  NYFIL-MC = GMLFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLFIL-MC = NYFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLFIL-PROCESS      TO TRUE
                   MOVE GMLFIL-MC          TO GMLFIL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-10 AND NOT-I-MR)
               MOVE SPACES TO FAKSALG-IO-AREA
               INITIALIZE FAKSALG-IO-AREA
               MOVE REC1                   TO FAKSALG-IO-AREA (1:160)
               MOVE SEQNR-IO               TO FAKSALG-IO-AREA (16:5)
               WRITE FAKSALG-IO-AREA
           END-IF
           IF  (I-02 AND I-10)
               MOVE SPACES TO FAKSALG-IO-AREA
               INITIALIZE FAKSALG-IO-AREA
               MOVE REC2                   TO FAKSALG-IO-AREA (1:160)
               MOVE SEQNR-IO               TO FAKSALG-IO-AREA (16:5)
      *****************************************************************
      * KONTROLLISTE.                                                 *
      *****************************************************************
               WRITE FAKSALG-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROG.'                TO LISTE-IO-AREA (2:5)
               MOVE 'FAK774 '              TO LISTE-IO-AREA (7:7)
               MOVE 'OPPD. FAKTURA.SALGDATA' TO LISTE-IO-AREA (17:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
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
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROG.'                TO LISTE-IO-AREA (2:5)
               MOVE 'FAK774 '              TO LISTE-IO-AREA (7:7)
               MOVE 'OPPD. FAKTURA.SALGDATA' TO LISTE-IO-AREA (17:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
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
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT.REC. FRA FØR.   ' TO LISTE-IO-AREA (11:20)
               MOVE ANTGML                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (32:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT.REC. SLETTET.   ' TO LISTE-IO-AREA (11:20)
               MOVE ANTSL                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (32:10)
               MOVE FAKPFJ                 TO EDIT-FAKPFJ
               MOVE EDIT-FAKPFJ            TO LISTE-IO-AREA (45:7)
               MOVE '= FAKT.ÅR/MND SLETTES' TO LISTE-IO-AREA (52:21)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT.NYE REC.        ' TO LISTE-IO-AREA (11:20)
               MOVE ANTNYE                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (32:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT.REC. NY MASTER  ' TO LISTE-IO-AREA (11:20)
               MOVE ANTUT                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (32:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           SET GMLFIL-LEVEL-INIT           TO TRUE
           INITIALIZE GMLFIL-DATA-FIELDS
           SET GMLFIL-EOF-OFF              TO TRUE
           SET GMLFIL-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO GMLFIL-MC
                                              GMLFIL-MP
           OPEN INPUT GMLFIL
           SET NYFIL-LEVEL-INIT            TO TRUE
           INITIALIZE NYFIL-DATA-FIELDS
           SET NYFIL-EOF-OFF               TO TRUE
           SET NYFIL-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO NYFIL-MC
                                              NYFIL-MP
           OPEN INPUT NYFIL
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           OPEN OUTPUT FAKSALG
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLFIL
           CLOSE NYFIL
           CLOSE PARAM
           CLOSE FAKSALG
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
