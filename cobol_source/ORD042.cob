       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD042R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM......:ORD042                                          *
      * PROGRAMERER..:ESPEN LARSEN                                    *
      * PROGRAMERT...:30.01.1995                                      *
      * OPPDAVE NR...: ORD40                                          *
      * UTLIST AV DAGENS ORDRE SOM IKKE ER FULL-LEVERT.               *
      *                                                               *
      *  6/11-95 OPPSLAG MOT VARE.MASTER FOR Å HENTE ANT. I BEST.     *
      *          OG FORV. LEVERING.                                   *
      * 23/09-97 MERKNAD OM VAREN UTGÅR ELLER PÅ BESTILLING.          *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD042.rpg
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
           SELECT ORDUTF
               ASSIGN TO UT-S-ORDUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDUTF-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDUTF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  ORDUTF-IO-AREA.
           05  ORDUTF-IO-AREA-X            PICTURE X(200).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
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
           10  ORDUTF-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDUTF-EOF-OFF          VALUE '0'.
               88  ORDUTF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDUTF-READ-OFF         VALUE '0'.
               88  ORDUTF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDUTF-PROCESS-OFF      VALUE '0'.
               88  ORDUTF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDUTF-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDUTF-LEVEL-INIT       VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  ORDUTF-LEVEL-01.
               10  ORDUTF-01-L2.
                   15  ORDUTF-01-L2-FIRM   PICTURE X(3).
               10  ORDUTF-01-L1.
                   15  ORDUTF-01-L1-KUNDNR PICTURE X(6).
           05  ORDUTF-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  KUNDNR                  PICTURE X(6).
               10  ORDNR                   PICTURE X(6).
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KADR                    PICTURE X(30).
               10  POSTNR                  PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  AVD                     PICTURE X(1).
               10  ORDMOT                  PICTURE X(2).
               10  REST                    PICTURE X(1).
               10  FERDIM                  PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALF                     PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  DIRREG                  PICTURE X(1).
               10  BK                      PICTURE X(1).
               10  NOREST                  PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  MERKN                   PICTURE X(1).
               10  AIBEST-IO.
                   15  AIBEST              PICTURE S9(7).
               10  LEVAAR                  PICTURE X(2).
               10  LEVUKE                  PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTB-IO.
                   15  ANTB                PICTURE S9(5).
               10  ANTL-IO.
                   15  ANTL                PICTURE S9(5).
               10  NEDBNR                  PICTURE X(10).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDUTF-PROCESS
               SET ORDUTF-PROCESS-OFF      TO TRUE
               SET ORDUTF-READ             TO TRUE
           END-IF
 
           IF  ORDUTF-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDUTF-GET
               SET ORDUTF-READ-OFF         TO TRUE
               IF  NOT ORDUTF-EOF
                   SET ORDUTF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDUTF-PROCESS
               PERFORM ORDUTF-IDSET
           END-IF
 
           IF  ORDUTF-PROCESS
               PERFORM ORDUTF-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
 
           IF  ORDUTF-PROCESS
               PERFORM ORDUTF-FLDOFF
               PERFORM ORDUTF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDUTF-PROCESS
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
               PERFORM RBSRUT-S
           END-IF
           ADD ANTBES TO ZERO          GIVING ANTB
           ADD ANTLEV TO ZERO          GIVING ANTL
           SET NOT-I-11                    TO TRUE
           IF  DIRREG = 'J'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  REST = '1'
               SET I-12                    TO TRUE
           END-IF
           IF  (NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  NOREST = '0'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  STATUS-X = 'M'
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  STATUS-X = 'A'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  FERDIM = '*'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  EDBNR > 9000000
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  STATUS-X = 'J'
               SET I-16                    TO TRUE
           END-IF
      ******************************************************
      *  RUTINE FOR Å FINNE ANT. I BEST I VAREMASTER.      *
      ******************************************************
           MOVE FIRM                       TO NEDBNR (1:3)
           MOVE EDBNR                      TO EDBNR-N
           MOVE EDBNR-N-IO                 TO NEDBNR (4:7)
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE NEDBNR (10:1)              TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO NEDBNR (10:1)
           IF  (NOT-I-86)
               MOVE NEDBNR                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-51                TO TRUE
               IF  MERKN = '1'
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  MERKN = '2'
                   SET I-52                TO TRUE
               END-IF
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
      *RN89                MOVE " "       BBEST             NO OVERSTYRING
           MOVE ' '                        TO BBEST
           MOVE 'ORD40'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD042  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
 
       ORDUTF-GET SECTION.
       ORDUTF-GET-P.
           IF  ORDUTF-EOF-OFF
               READ ORDUTF
               AT END
                   SET ORDUTF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDUTF-FLDOFF SECTION.
       ORDUTF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-70                TO TRUE
               SET NOT-I-71                TO TRUE
           END-EVALUATE.
 
       ORDUTF-FLDSET SECTION.
       ORDUTF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDUTF-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE ORDUTF-IO-AREA (5:6)   TO KUNDNR (1:6)
               MOVE ORDUTF-IO-AREA (11:6)  TO ORDNR (1:6)
               MOVE ORDUTF-IO-AREA (17:30) TO KNAVN1 (1:30)
               MOVE ORDUTF-IO-AREA (47:30) TO KNAVN2 (1:30)
               IF  KNAVN2 = SPACES
                   SET I-70                TO TRUE
               END-IF
               MOVE ORDUTF-IO-AREA (77:30) TO KADR (1:30)
               IF  KADR = SPACES
                   SET I-71                TO TRUE
               END-IF
               MOVE ORDUTF-IO-AREA (107:4) TO POSTNR (1:4)
               MOVE ORDUTF-IO-AREA (111:15) TO PSTED (1:15)
               MOVE ORDUTF-IO-AREA (126:1) TO AVD (1:1)
               MOVE ORDUTF-IO-AREA (127:2) TO ORDMOT (1:2)
               MOVE ORDUTF-IO-AREA (129:1) TO REST (1:1)
               MOVE ORDUTF-IO-AREA (130:1) TO FERDIM (1:1)
               MOVE ORDUTF-IO-AREA (131:1) TO STATUS-X (1:1)
               MOVE ORDUTF-IO-AREA (132:4) TO ANTBES-IO
               MOVE ORDUTF-IO-AREA (136:4) TO ANTLEV-IO
               MOVE ORDUTF-IO-AREA (140:3) TO ALF (1:3)
               MOVE ORDUTF-IO-AREA (143:20) TO ARTNR (1:20)
               MOVE ORDUTF-IO-AREA (163:30) TO VARBET (1:30)
               MOVE ORDUTF-IO-AREA (193:4) TO EDBNR-IO
               MOVE ORDUTF-IO-AREA (197:1) TO DIRREG (1:1)
               MOVE ORDUTF-IO-AREA (198:1) TO BK (1:1)
               MOVE ORDUTF-IO-AREA (199:1) TO NOREST (1:1)
           END-EVALUATE.
 
       ORDUTF-IDSET SECTION.
       ORDUTF-IDSET-P.
           SET I-01                        TO TRUE.
 
       ORDUTF-CHK-LEVEL SECTION.
       ORDUTF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDUTF-LEVEL-01
               MOVE ORDUTF-IO-AREA (2:3)   TO ORDUTF-01-L2-FIRM
               MOVE ORDUTF-IO-AREA (5:6)   TO ORDUTF-01-L1-KUNDNR
               IF  ORDUTF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDUTF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDUTF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDUTF-01-L2          TO THE-PRIOR-L2
               MOVE  ORDUTF-01-L1          TO THE-PRIOR-L1
               SET ORDUTF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (127:1) TO MERKN (1:1)
               MOVE VAREMAS-IO-AREA (129:7) TO AIBEST-IO
               INSPECT AIBEST-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (136:2) TO LEVAAR (1:2)
               MOVE VAREMAS-IO-AREA (138:2) TO LEVUKE (1:2)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
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
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KUNDNR                 TO LISTE-IO-AREA (3:6)
               MOVE KNAVN1                 TO LISTE-IO-AREA (10:30)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND NOT-I-70 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KNAVN2                 TO LISTE-IO-AREA (10:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND NOT-I-71 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KADR                   TO LISTE-IO-AREA (10:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE POSTNR                 TO LISTE-IO-AREA (10:4)
               MOVE PSTED                  TO LISTE-IO-AREA (15:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDNR                  TO LISTE-IO-AREA (3:6)
               MOVE ANTB                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (11:6)
               MOVE ANTL                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (18:6)
               MOVE ALF                    TO LISTE-IO-AREA (25:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (29:20)
               MOVE VARBET                 TO LISTE-IO-AREA (50:30)
               MOVE ORDMOT                 TO LISTE-IO-AREA (81:2)
               IF  (I-11)
                   MOVE 'JA'               TO LISTE-IO-AREA (84:2)
               END-IF
               IF  (NOT-I-12)
                   MOVE ' JA'              TO LISTE-IO-AREA (88:3)
               END-IF
               IF  (I-12)
                   MOVE 'NEI'              TO LISTE-IO-AREA (88:3)
               END-IF
               IF  (I-14)
                   MOVE ' JA'              TO LISTE-IO-AREA (93:3)
               END-IF
               IF  (NOT-I-14)
                   MOVE 'NEI'              TO LISTE-IO-AREA (93:3)
               END-IF
               IF  (NOT-I-10)
                   MOVE AIBEST             TO XO-70YY9
                   MOVE XO-70YY9           TO LISTE-IO-AREA (97:9)
               END-IF
               IF  (NOT-I-10)
                   MOVE LEVUKE             TO LISTE-IO-AREA (107:2)
               END-IF
               IF  (NOT-I-10)
                   MOVE '.'                TO LISTE-IO-AREA (109:1)
               END-IF
               IF  (NOT-I-10)
                   MOVE LEVAAR             TO LISTE-IO-AREA (110:2)
               END-IF
               IF  (I-15)
                   MOVE 'IKKE LAGERVARE.'  TO LISTE-IO-AREA (113:15)
               END-IF
               IF  (I-16)
                   MOVE 'IKKE FULLFØRT ORDRE' TO LISTE-IO-AREA (113:19)
               END-IF
               IF  (I-51)
                   MOVE 'MERKET UTGÅR       ' TO LISTE-IO-AREA (113:19)
               END-IF
               IF  (I-52)
                   MOVE 'MERKET PÅ BEST.    ' TO LISTE-IO-AREA (113:19)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * * DAGENS IKKE'    TO LISTE-IO-AREA (36:17)
               MOVE 'FULLEVERTE ORDRELINJER.' TO LISTE-IO-AREA (54:23)
               MOVE '* * *'                TO LISTE-IO-AREA (78:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (96:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L2)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDRE'                TO LISTE-IO-AREA (4:5)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (11:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (18:6)
               MOVE 'REST'                 TO LISTE-IO-AREA (87:4)
               MOVE 'FERD'                 TO LISTE-IO-AREA (92:4)
               MOVE 'ANT.'                 TO LISTE-IO-AREA (102:4)
               MOVE 'FORV.'                TO LISTE-IO-AREA (107:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.'                  TO LISTE-IO-AREA (6:3)
               MOVE ' BEST.'               TO LISTE-IO-AREA (11:6)
               MOVE 'LEVERT'               TO LISTE-IO-AREA (18:6)
               MOVE 'ALF'                  TO LISTE-IO-AREA (25:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (29:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (50:14)
               MOVE 'OM'                   TO LISTE-IO-AREA (81:2)
               MOVE 'DR'                   TO LISTE-IO-AREA (84:2)
               MOVE 'ORD.'                 TO LISTE-IO-AREA (87:4)
               MOVE 'MELD'                 TO LISTE-IO-AREA (92:4)
               MOVE 'I BEST.'              TO LISTE-IO-AREA (99:7)
               MOVE 'LEV.'                 TO LISTE-IO-AREA (108:4)
               MOVE 'MERKNADER'            TO LISTE-IO-AREA (113:9)
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
           IF  (I-OF AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * * DAGENS IKKE'    TO LISTE-IO-AREA (36:17)
               MOVE 'FULLEVERTE ORDRELINJER.' TO LISTE-IO-AREA (54:23)
               MOVE '* * *'                TO LISTE-IO-AREA (78:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (96:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L2)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDRE'                TO LISTE-IO-AREA (4:5)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (11:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (18:6)
               MOVE 'REST'                 TO LISTE-IO-AREA (87:4)
               MOVE 'FERD'                 TO LISTE-IO-AREA (92:4)
               MOVE 'ANT.'                 TO LISTE-IO-AREA (102:4)
               MOVE 'FORV.'                TO LISTE-IO-AREA (107:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.'                  TO LISTE-IO-AREA (6:3)
               MOVE ' BEST.'               TO LISTE-IO-AREA (11:6)
               MOVE 'LEVERT'               TO LISTE-IO-AREA (18:6)
               MOVE 'ALF'                  TO LISTE-IO-AREA (25:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (29:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (50:14)
               MOVE 'OM'                   TO LISTE-IO-AREA (81:2)
               MOVE 'DR'                   TO LISTE-IO-AREA (84:2)
               MOVE 'ORD.'                 TO LISTE-IO-AREA (87:4)
               MOVE 'MELD'                 TO LISTE-IO-AREA (92:4)
               MOVE 'I BEST.'              TO LISTE-IO-AREA (99:7)
               MOVE 'LEV.'                 TO LISTE-IO-AREA (108:4)
               MOVE 'MERKNADER'            TO LISTE-IO-AREA (113:9)
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
           SET ORDUTF-LEVEL-INIT           TO TRUE
           INITIALIZE ORDUTF-DATA-FIELDS
           SET ORDUTF-EOF-OFF              TO TRUE
           SET ORDUTF-PROCESS              TO TRUE
           OPEN INPUT ORDUTF
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDUTF
           CLOSE VAREMAS
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
