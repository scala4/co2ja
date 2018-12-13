       IDENTIFICATION DIVISION.
       PROGRAM-ID. RES505R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RES505 . JCL=DOP.XDOP16NY                    *
      *********************************************************************
      **  KVITTERINGSLISTE KUNDEARKIVET,- ÅPNINGER/ENDRINGER/SLETTINGER. **
      *                                          XX2000XXIRXXMT       *
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RES505.rpg
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
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 4000
               RECORD CONTAINS 200.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(200).
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
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
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
           05  INFILE-LEVEL-01.
               10  INFILE-01-L1.
                   15  INFILE-01-L1-FIRMA  PICTURE X(3).
           05  INFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  RESASA                  PICTURE X(1).
               10  RESK                    PICTURE X(6).
               10  ALFA                    PICTURE X(4).
               10  NAVN1                   PICTURE X(30).
               10  NAVN2                   PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  POST                    PICTURE X(15).
               10  POSTNR                  PICTURE X(4).
               10  RG                      PICTURE X(2).
               10  BM                      PICTURE X(2).
               10  KRGR                    PICTURE X(4).
               10  BANK                    PICTURE X(11).
               10  IP                      PICTURE X(1).
               10  IR                      PICTURE X(1).
               10  FAKUT                   PICTURE X(1).
               10  FM                      PICTURE X(1).
               10  ALTNR                   PICTURE X(6).
               10  VADR                    PICTURE X(1).
               10  RO                      PICTURE X(1).
               10  IG                      PICTURE X(1).
               10  HND                     PICTURE X(3).
               10  KODE                    PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTA-ELG-IO.
                   15  ANTA-ELG            PICTURE S9(5).
               10  ANTE-IO.
                   15  ANTE                PICTURE S9(5).
               10  ANTS-IO.
                   15  ANTS                PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-50YNZ                PICTURE ZZZZZ.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   SET INFILE-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDOFF
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-20                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           IF  (I-L1)
               SET NOT-I-26                TO TRUE
               MOVE 0                      TO ANTA-ELG
               MOVE 0                      TO ANTE
               MOVE 0                      TO ANTS
               PERFORM RBSRUT-S
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  (I-40 AND I-45)
               SET I-50                    TO TRUE
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  KODE = 'Å'
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-21                TO TRUE
               IF  KODE = 'E'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20 AND NOT-I-21)
               SET NOT-I-22                TO TRUE
               IF  KODE = 'S'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               ADD 1                       TO ANTA-ELG
           END-IF
           IF  (I-21)
               ADD 1                       TO ANTE
           END-IF
           IF  (I-22)
               ADD 1                       TO ANTS
      *
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'RES01'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RES505  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-U2)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 2
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (I-U3)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 3
                   SET I-86                TO TRUE
               END-IF
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-23                TO TRUE
               IF  ANTA-ELG = 0
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  ANTE = 0
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  ANTS = 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-23 AND I-24 AND I-25)
               SET I-26                    TO TRUE
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDOFF SECTION.
       INFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-40                TO TRUE
               SET NOT-I-45                TO TRUE
           END-EVALUATE.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (6:1)   TO RESASA (1:1)
               MOVE INFILE-IO-AREA (6:6)   TO RESK (1:6)
               MOVE INFILE-IO-AREA (12:4)  TO ALFA (1:4)
               MOVE INFILE-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE INFILE-IO-AREA (46:30) TO NAVN2 (1:30)
               IF  NAVN2 = SPACES
                   SET I-40                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (76:30) TO ADR (1:30)
               IF  ADR = SPACES
                   SET I-45                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (106:15) TO POST (1:15)
               MOVE INFILE-IO-AREA (121:4) TO POSTNR (1:4)
               MOVE INFILE-IO-AREA (125:2) TO RG (1:2)
               MOVE INFILE-IO-AREA (127:2) TO BM (1:2)
               MOVE INFILE-IO-AREA (129:4) TO KRGR (1:4)
               MOVE INFILE-IO-AREA (133:11) TO BANK (1:11)
               MOVE INFILE-IO-AREA (164:1) TO IP (1:1)
               MOVE INFILE-IO-AREA (165:1) TO IR (1:1)
               MOVE INFILE-IO-AREA (170:1) TO FAKUT (1:1)
               MOVE INFILE-IO-AREA (171:1) TO FM (1:1)
               MOVE INFILE-IO-AREA (172:6) TO ALTNR (1:6)
               MOVE INFILE-IO-AREA (178:1) TO VADR (1:1)
               MOVE INFILE-IO-AREA (179:1) TO RO (1:1)
               MOVE INFILE-IO-AREA (180:1) TO IG (1:1)
               MOVE INFILE-IO-AREA (185:3) TO HND (1:3)
               MOVE INFILE-IO-AREA (200:1) TO KODE (1:1)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (3:3)   TO INFILE-01-L1-FIRMA
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
               MOVE RESK                   TO LISTE-IO-AREA (1:6)
               MOVE NAVN1                  TO LISTE-IO-AREA (8:30)
               MOVE ADR                    TO LISTE-IO-AREA (39:30)
               IF  (I-50)
                   MOVE POSTNR             TO LISTE-IO-AREA (39:4)
               END-IF
               IF  (I-50)
                   MOVE POST               TO LISTE-IO-AREA (44:15)
               END-IF
               MOVE ALFA                   TO LISTE-IO-AREA (70:4)
               MOVE ALTNR                  TO LISTE-IO-AREA (76:6)
               MOVE BANK                   TO LISTE-IO-AREA (83:11)
               MOVE RG                     TO LISTE-IO-AREA (95:2)
               MOVE BM                     TO LISTE-IO-AREA (98:2)
               MOVE KRGR                   TO LISTE-IO-AREA (101:4)
               MOVE HND                    TO LISTE-IO-AREA (106:3)
               MOVE IP                     TO LISTE-IO-AREA (111:1)
               MOVE IR                     TO LISTE-IO-AREA (114:1)
               MOVE FAKUT                  TO LISTE-IO-AREA (117:1)
               MOVE FM                     TO LISTE-IO-AREA (120:1)
               MOVE RO                     TO LISTE-IO-AREA (123:1)
               MOVE IG                     TO LISTE-IO-AREA (126:1)
               MOVE VADR                   TO LISTE-IO-AREA (129:1)
               MOVE KODE                   TO LISTE-IO-AREA (132:1)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE NAVN2                  TO LISTE-IO-AREA (8:30)
               MOVE POSTNR                 TO LISTE-IO-AREA (39:4)
               MOVE POST                   TO LISTE-IO-AREA (44:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'K V I T T E R I N G S L' TO LISTE-IO-AREA (36:23)
               MOVE 'I S T E   K U N D E'  TO LISTE-IO-AREA (60:19)
               MOVE 'A R K I V E T'        TO LISTE-IO-AREA (80:13)
               MOVE 'DATO'                 TO LISTE-IO-AREA (111:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (116:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L1)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESKNR  NAVN'         TO LISTE-IO-AREA (1:12)
               MOVE 'ADRESSE'              TO LISTE-IO-AREA (41:7)
               MOVE 'ALFA  ALT.NR  BANKGIRONR' TO LISTE-IO-AREA (70:24)
               MOVE 'RG BM KRGR HOM IP IR 3F' TO LISTE-IO-AREA (95:23)
               MOVE 'FM RO IG ADR T'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV AND NOT-I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'K V I T T E R I N G S L' TO LISTE-IO-AREA (36:23)
               MOVE 'I S T E   K U N D E'  TO LISTE-IO-AREA (60:19)
               MOVE 'A R K I V E T'        TO LISTE-IO-AREA (80:13)
               MOVE 'DATO'                 TO LISTE-IO-AREA (111:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (116:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L1)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESKNR  NAVN'         TO LISTE-IO-AREA (1:12)
               MOVE 'ADRESSE'              TO LISTE-IO-AREA (41:7)
               MOVE 'ALFA  ALT.NR  BANKGIRONR' TO LISTE-IO-AREA (70:24)
               MOVE 'RG BM KRGR HOM IP IR 3F' TO LISTE-IO-AREA (95:23)
               MOVE 'FM RO IG ADR T'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86 AND NOT-I-26)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL ENDRINGER='    TO LISTE-IO-AREA (10:17)
               MOVE ANTE                   TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (28:5)
               IF  (I-24)
                   MOVE '0'                TO LISTE-IO-AREA (32:1)
               END-IF
               MOVE 'ANTALL SLETTINGER='   TO LISTE-IO-AREA (40:18)
               MOVE ANTS                   TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (59:5)
               IF  (I-25)
                   MOVE '0'                TO LISTE-IO-AREA (63:1)
               END-IF
               MOVE 'ANTALL ÅPNINGER='     TO LISTE-IO-AREA (71:16)
               MOVE ANTA-ELG               TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (88:5)
               IF  (I-23)
                   MOVE '0'                TO LISTE-IO-AREA (92:1)
               END-IF
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
           MOVE 1                          TO LR-CHECK
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
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
