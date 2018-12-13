       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK125R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK125.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FRAFILE
               ASSIGN TO UT-S-FRAFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRAFILE-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FRAFILE
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  FRAFILE-IO-AREA.
           05  FRAFILE-IO-AREA-X           PICTURE X(100).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FRAFILE-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAFILE-EOF-OFF         VALUE '0'.
               88  FRAFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAFILE-READ-OFF        VALUE '0'.
               88  FRAFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAFILE-PROCESS-OFF     VALUE '0'.
               88  FRAFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FRAFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  FRAFILE-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  LDATA-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
           05  FAKPAR-DATA-FIELDS.
               10  FAKPER-IO.
                   15  FAKPER              PICTURE S9(6).
           05  FRAFILE-LEVEL-01.
               10  FRAFILE-01-L3.
                   15  FRAFILE-01-L3-FIRMA PICTURE X(3).
               10  FRAFILE-01-L2.
                   15  FRAFILE-01-L2-KUNDE PICTURE X(6).
               10  FRAFILE-01-L1.
                   15  FRAFILE-01-L1-ORDNR PICTURE X(6).
           05  FRAFILE-DATA-FIELDS.
               10  FRAKTT                  PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
               10  ORDNR                   PICTURE X(6).
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(6).
               10  ARTNR                   PICTURE X(10).
               10  FRAKT-IO.
                   15  FRAKT               PICTURE S9(7)V9(2).
               10  ORDSUM-IO.
                   15  ORDSUM              PICTURE S9(7)V9(2).
               10  BESSUM-IO.
                   15  BESSUM              PICTURE S9(7)V9(2).
               10  BSFREK                  PICTURE X(1).
               10  ORDFGR-IO.
                   15  ORDFGR              PICTURE S9(7)V9(2).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
           05  KUNDEMA-DATA-FIELDS.
               10  CNAVN1                  PICTURE X(30).
               10  CPNR                    PICTURE X(4).
               10  CPSTED                  PICTURE X(15).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  L3BELA-IO.
                   15  L3BELA              PICTURE S9(7)V9(2).
               10  L3BELF-IO.
                   15  L3BELF              PICTURE S9(7)V9(2).
               10  RESKEY                  PICTURE X(9).
               10  FAKTGR-IO.
                   15  FAKTGR              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FRAFILE-PROCESS
               SET FRAFILE-PROCESS-OFF     TO TRUE
               SET FRAFILE-READ            TO TRUE
           END-IF
 
           IF  FRAFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM FRAFILE-GET
               SET FRAFILE-READ-OFF        TO TRUE
               IF  NOT FRAFILE-EOF
                   SET FRAFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FRAFILE-PROCESS
               PERFORM FRAFILE-IDSET
           END-IF
 
           IF  FRAFILE-PROCESS
               PERFORM FRAFILE-CHK-LEVEL
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
 
           IF  FRAFILE-PROCESS
               PERFORM FRAFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FRAFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3 AND NOT-I-99)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-L3)
               SET I-99                    TO TRUE
               SUBTRACT L3BELA             FROM L3BELA
               SUBTRACT L3BELF             FROM L3BELF
           END-IF
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  FRAKTT = 'A'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  FRAKTT = 'F'
                   SET I-12                TO TRUE
               END-IF
      *****************************************************************
      * HENTE KUNDEOPPLYSNINGER.                                      *
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-55)
               SET NOT-I-25                TO TRUE
               IF  KONFNR > '000'
                   SET I-25                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO RESKEY (1:3)
           END-IF
           IF  (I-L2 AND NOT-I-55 AND I-25)
               MOVE KONFNR                 TO RESKEY (1:3)
           END-IF
           IF  (I-L2)
               MOVE KUNDE                  TO RESKEY (4:6)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-91                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-91            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      *****************************************************
      *   SUMERING AV ALLE FRAKTBELØP                     *
      *****************************************************
           END-IF
           IF  (I-01 AND I-11)
               ADD FRAKT                   TO L3BELA
           END-IF
           IF  (I-01 AND I-12)
               ADD FRAKT                   TO L3BELF
           END-IF
           IF  (I-01)
               ADD ORDFGR TO ZERO      GIVING FAKTGR ROUNDED
           END-IF.
 
       SLUTT-T.
      *****************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'FAK07'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK125  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (12:6)  TO FAKPER-IO
               INSPECT FAKPER-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-04                        TO TRUE.
 
       FRAFILE-GET SECTION.
       FRAFILE-GET-P.
           IF  FRAFILE-EOF-OFF
               READ FRAFILE
               AT END
                   SET FRAFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FRAFILE-FLDSET SECTION.
       FRAFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FRAFILE-IO-AREA (1:1)  TO FRAKTT (1:1)
               MOVE FRAFILE-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE FRAFILE-IO-AREA (5:6)  TO KUNDE (1:6)
               MOVE FRAFILE-IO-AREA (11:6) TO ORDNR (1:6)
               MOVE FRAFILE-IO-AREA (17:6) TO ODATO-IO
               INSPECT ODATO-IO REPLACING ALL ' ' BY '0'
               MOVE FRAFILE-IO-AREA (26:10) TO ARTNR (1:10)
               MOVE FRAFILE-IO-AREA (53:9) TO FRAKT-IO
               INSPECT FRAKT-IO REPLACING ALL ' ' BY '0'
               MOVE FRAFILE-IO-AREA (62:9) TO ORDSUM-IO
               INSPECT ORDSUM-IO REPLACING ALL ' ' BY '0'
               MOVE FRAFILE-IO-AREA (71:9) TO BESSUM-IO
               INSPECT BESSUM-IO REPLACING ALL ' ' BY '0'
               MOVE FRAFILE-IO-AREA (80:1) TO BSFREK (1:1)
               MOVE FRAFILE-IO-AREA (81:9) TO ORDFGR-IO
               INSPECT ORDFGR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FRAFILE-IDSET SECTION.
       FRAFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       FRAFILE-CHK-LEVEL SECTION.
       FRAFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FRAFILE-LEVEL-01
               MOVE FRAFILE-IO-AREA (2:3)  TO FRAFILE-01-L3-FIRMA
               MOVE FRAFILE-IO-AREA (5:6)  TO FRAFILE-01-L2-KUNDE
               MOVE FRAFILE-IO-AREA (11:6) TO FRAFILE-01-L1-ORDNR
               IF  FRAFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FRAFILE-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FRAFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FRAFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FRAFILE-01-L3         TO THE-PRIOR-L3
               MOVE  FRAFILE-01-L2         TO THE-PRIOR-L2
               MOVE  FRAFILE-01-L1         TO THE-PRIOR-L1
               SET FRAFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO CNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (121:4) TO CPNR (1:4)
               MOVE KUNDEMA-IO-AREA (106:15) TO CPSTED (1:15)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KUNDE                  TO LISTE-IO-AREA (1:6)
               MOVE BSFREK                 TO LISTE-IO-AREA (8:1)
               IF  (NOT-I-91)
                   MOVE CNAVN1             TO LISTE-IO-AREA (10:30)
               END-IF
               IF  (I-91)
                   MOVE 'KUNDENUMMER UKJENT. ' TO LISTE-IO-AREA (10:20)
               END-IF
               IF  (NOT-I-91)
                   MOVE CPNR               TO LISTE-IO-AREA (41:4)
               END-IF
               IF  (NOT-I-91)
                   MOVE CPSTED             TO LISTE-IO-AREA (46:15)
               END-IF
               MOVE ORDNR                  TO LISTE-IO-AREA (62:6)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (69:8)
               MOVE BESSUM                 TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (77:12)
               MOVE FAKTGR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (90:6)
               MOVE FRAKT                  TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (96:12)
               IF  (I-12)
                   MOVE 'FRAKT FJERNET'    TO LISTE-IO-AREA (109:13)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (75:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE '               TO LISTE-IO-AREA (1:6)
               MOVE 'F'                    TO LISTE-IO-AREA (8:1)
               MOVE 'KUNDENAVN           ' TO LISTE-IO-AREA (10:20)
               MOVE 'PNR.'                 TO LISTE-IO-AREA (41:4)
               MOVE 'POSTSTED       '      TO LISTE-IO-AREA (46:15)
               MOVE 'ORD.NR'               TO LISTE-IO-AREA (62:6)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (69:8)
               MOVE '  B.ORDRESUM'         TO LISTE-IO-AREA (77:12)
               MOVE 'ORD.GR'               TO LISTE-IO-AREA (90:6)
               MOVE '  FRAKT.BEL.'         TO LISTE-IO-AREA (96:12)
               MOVE 'MERKNADER    '        TO LISTE-IO-AREA (109:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (75:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE '               TO LISTE-IO-AREA (1:6)
               MOVE 'F'                    TO LISTE-IO-AREA (8:1)
               MOVE 'KUNDENAVN           ' TO LISTE-IO-AREA (10:20)
               MOVE 'PNR.'                 TO LISTE-IO-AREA (41:4)
               MOVE 'POSTSTED       '      TO LISTE-IO-AREA (46:15)
               MOVE 'ORD.NR'               TO LISTE-IO-AREA (62:6)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (69:8)
               MOVE '  B.ORDRESUM'         TO LISTE-IO-AREA (77:12)
               MOVE 'ORD.GR'               TO LISTE-IO-AREA (90:6)
               MOVE '  FRAKT.BEL.'         TO LISTE-IO-AREA (96:12)
               MOVE 'MERKNADER    '        TO LISTE-IO-AREA (109:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' **'                  TO LISTE-IO-AREA (4:3)
               MOVE 'TOTALT FRAKT FAKTURERT  ' TO LISTE-IO-AREA (53:24)
               MOVE L3BELA                 TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (87:12)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT FRAKT FJERNET.   ' TO LISTE-IO-AREA (53:24)
               MOVE L3BELF                 TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (87:12)
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
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET FRAFILE-LEVEL-INIT          TO TRUE
           INITIALIZE FRAFILE-DATA-FIELDS
           SET FRAFILE-EOF-OFF             TO TRUE
           SET FRAFILE-PROCESS             TO TRUE
           OPEN INPUT FRAFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKPAR
           CLOSE FRAFILE
           CLOSE FIRMAF
           CLOSE KUNDEMA
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
