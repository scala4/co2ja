       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG044R.
      **********************************************  Z-WIN-RPG2      *
      *  PROGRAM...... VRG044                              *
      *  UTPLUKK AV ARTIKKLER MED ALTERNATIV NR SOM SKAL   *
      *  OPPDATERES FRA FIRMA TIL FIRMA.                   *
      *  VARER MED ALTERNATIV KOPIERS NÅR DE IKKE ER       *
      *  MERKET MED UTGÅR. ELLER HVIST DE ER MERKET MED    *
      *  UTGÅR OG BEHOLDNING ER 0.                         *
      *  8/12-94 SKREV OM FRA TABELLBRUK TIL MERGING/ARRAY *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG044.rpg
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
           SELECT TILTAB
               ASSIGN TO UT-S-TILTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILTAB-STATUS.
           SELECT VARE
               ASSIGN TO UT-S-VARE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARE-STATUS.
           SELECT VOAUT
               ASSIGN TO UT-S-VOAUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VOAUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TILTAB
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  TILTAB-IO-AREA.
           05  TILTAB-IO-AREA-X            PICTURE X(60).
       FD VARE
               BLOCK CONTAINS 1280
               RECORD CONTAINS 128.
       01  VARE-IO-AREA.
           05  VARE-IO-AREA-X              PICTURE X(128).
       FD VOAUT
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VOAUT-IO-AREA.
           05  VOAUT-IO-AREA-X             PICTURE X(60).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ART-MAX   VALUE 199             PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ART-TABLE.
               10  ART-ENTRY
                                           OCCURS 199 TIMES
                                           INDEXED BY ART-I
                                                      ART-S.
                   15  ART                 PICTURE X(46).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TILTAB-STATUS               PICTURE 99 VALUE 0.
           10  VARE-STATUS                 PICTURE 99 VALUE 0.
           10  VOAUT-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TILTAB-EOF-OFF          VALUE '0'.
               88  TILTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILTAB-READ-OFF         VALUE '0'.
               88  TILTAB-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILTAB-PROCESS-OFF      VALUE '0'.
               88  TILTAB-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  TILTAB-LEVEL-INIT-OFF   VALUE '0'.
               88  TILTAB-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-EOF-OFF            VALUE '0'.
               88  VARE-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-READ-OFF           VALUE '0'.
               88  VARE-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-PROCESS-OFF        VALUE '0'.
               88  VARE-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARE-LEVEL-INIT-OFF     VALUE '0'.
               88  VARE-LEVEL-INIT         VALUE '1'.
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
           05  TILTAB-LEVEL-02.
               10  TILTAB-02-L2.
                   15  TILTAB-02-L2-FRAFIR PICTURE X(3).
               10  TILTAB-02-L1.
                   15  TILTAB-02-L1-FRAVGR PICTURE X(5).
                   15  TILTAB-02-L1-FRAALF PICTURE X(3).
                   15  TILTAB-02-L1-FRAPT  PICTURE X(1).
           05  TILTAB-DATA-FIELDS.
               10  FRAFIR                  PICTURE X(3).
               10  FRAVGR                  PICTURE X(5).
               10  FRAALF                  PICTURE X(3).
               10  FRAPT                   PICTURE X(1).
               10  FRASEQ-IO.
                   15  FRASEQ              PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  FRADAT                  PICTURE X(46).
           05  TILTAB-MP                   PICTURE X(11).
           05  TILTAB-MC                   PICTURE X(11).
           05  TILTAB-M-02             REDEFINES TILTAB-MC.
               10  TILTAB-M-02-M4.
                   15  TILTAB-M-02-M4-FRAFIR-G.
                       20  TILTAB-M-02-M4-FRAFIR PICTURE X(3).
               10  TILTAB-M-02-M3.
                   15  TILTAB-M-02-M3-FRAVGR-G.
                       20  TILTAB-M-02-M3-FRAVGR PICTURE X(5).
               10  TILTAB-M-02-M2.
                   15  TILTAB-M-02-M2-FRAALF-G.
                       20  TILTAB-M-02-M2-FRAALF PICTURE X(3).
           05  VARE-LEVEL-01.
               10  VARE-01-L2.
                   15  VARE-01-L2-FIRMA    PICTURE X(3).
               10  VARE-01-L1.
                   15  VARE-01-L1-ALFA     PICTURE X(3).
                   15  VARE-01-L1-PT       PICTURE X(1).
                   15  VARE-01-L1-VGR      PICTURE X(5).
           05  VARE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  EDBNR1                  PICTURE X(1).
               10  ALFA                    PICTURE X(3).
               10  ALTNR                   PICTURE X(7).
               10  PT                      PICTURE X(1).
               10  AUTOO                   PICTURE X(1).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VGR                     PICTURE X(5).
               10  UTGA-ELGR               PICTURE X(1).
               10  SLETT                   PICTURE X(1).
      **********************************************************
      *  RUTINE FOR Å BYGGE OPP TIL.FIRMA.ARRAY                *
      **********************************************************
           05  VARE-MP                     PICTURE X(11).
           05  VARE-MC                     PICTURE X(11).
           05  VARE-M-01               REDEFINES VARE-MC.
               10  VARE-M-01-M4.
                   15  VARE-M-01-M4-FIRMA-G.
                       20  VARE-M-01-M4-FIRMA PICTURE X(3).
               10  VARE-M-01-M3.
                   15  VARE-M-01-M3-VGR-G.
                       20  VARE-M-01-M3-VGR PICTURE X(5).
               10  VARE-M-01-M2.
                   15  VARE-M-01-M2-ALFA-G.
                       20  VARE-M-01-M2-ALFA PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  Y-IO.
                   15  Y                   PICTURE S9(3).
               10  FRASEQ-N-IO.
                   15  FRASEQ-N            PICTURE S9(3).
               10  BEH-IO.
                   15  BEH                 PICTURE S9(7)V9(2).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7).
               10  FELT46                  PICTURE X(46).
               10  FELT15                  PICTURE X(15).
               10  FELT31                  PICTURE X(31).
               10  NYFNR                   PICTURE X(3).
               10  FELT10                  PICTURE X(10).
               10  VGRALF                  PICTURE X(8).
               10  NYALFA                  PICTURE X(3).
               10  ANTNY-IO.
                   15  ANTNY               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YN9                PICTURE ZZZZZZ9.
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  TILTAB-PROCESS
               SET TILTAB-PROCESS-OFF      TO TRUE
               SET TILTAB-READ             TO TRUE
           END-IF
 
           IF  TILTAB-READ
               PERFORM TILTAB-GET
               SET TILTAB-READ-OFF         TO TRUE
               IF  NOT TILTAB-EOF
                   PERFORM TILTAB-MATCH-SET
               END-IF
           END-IF
 
           IF  VARE-PROCESS
               SET VARE-PROCESS-OFF        TO TRUE
               SET VARE-READ               TO TRUE
           END-IF
 
           IF  VARE-READ
               PERFORM VARE-GET
               SET VARE-READ-OFF           TO TRUE
               IF  NOT VARE-EOF
                   PERFORM VARE-MATCH-SET
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
 
           IF  TILTAB-PROCESS
               PERFORM TILTAB-IDSET
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-IDSET
           END-IF
 
           IF  TILTAB-PROCESS
               PERFORM TILTAB-CHK-LEVEL
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  TILTAB-PROCESS
               PERFORM TILTAB-FLDSET
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  TILTAB-PROCESS
           OR  VARE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02)
               MOVE FRASEQ                 TO FRASEQ-N
               MOVE FRASEQ-N-IO            TO Y-IO
               MOVE FRADAT                 TO ART (Y)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-65                    TO TRUE
      **********************************************************
      *  TEST OM DET SKAL KOPIERES NOE FRA DETTE FIRMA.    *****
      **********************************************************
           IF  (NOT-I-MR)
               GO TO SLUTT-T
      *****************************************
      *    SKAFFEVARER KOPIERES IKKE          *
      *    SLETTEDE    KOPIERES IKKE          *
      *    AUTOOPDATERING=X SKAL IKKE KOPIERES*
      *    VARER UTEN ALTERNATIV KOPERES IKKE *
      *****************************************
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  EDBNR1 = '9'
               SET I-59                    TO TRUE
           END-IF
           IF  (NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  SLETT = 'S'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  AUTOO = 'X'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  ALTNR NOT > '0000000'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-59)
               GO TO SLUTT-T
      ***********************************************************
      *    VARER MERKET MED UTGÅR SKAL KUN OVERFØRE ALTERNATIV  *
      *    HOS MOTTAGER HVIS DET IKKE ER BEHOLDNING.            *
      ***********************************************************
           END-IF
           SUBTRACT ANTUT FROM ANTINN  GIVING BEH
           SET NOT-I-41                    TO TRUE
           IF  BEH > 0
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  UTGA-ELGR = '1'
               SET I-42                    TO TRUE
           END-IF
           IF  (I-42 AND I-41)
               GO TO SLUTT-T
      **********************************************************
      * NULLSTIL INDEX (Y) OG TELL OPP ANTALL KOPIERT FRA      *
      **********************************************************
           END-IF
           MOVE 0                          TO Y
           ADD 1                           TO ANT
      ***********************************************************
      *  LOOP FOR FIRMAER SOM SKAL HA KOPI AV VAREARKIV.    *****
      ***********************************************************
           .
 
       LOOP-T.
           ADD 1                           TO Y
           SET NOT-I-62                    TO TRUE
           IF  Y > FRASEQ
               SET I-62                    TO TRUE
           END-IF
           IF  (I-62)
               GO TO SLUTT-T
           END-IF
           MOVE ART (Y)                    TO FELT46
      ***********************************************************
      *       SPLITTING AV FELTER FRA TABELL NYTT-FIRMA-KOPI.   *
      ***********************************************************
      *  TIL FIRMANR,FAKTOR FOR UTREGNING AV SELVKOST OG UTSALGSPRIS.
           MOVE FELT46 (1:15)              TO FELT15
           MOVE FELT46 (16:31)             TO FELT31
           MOVE FELT15 (1:3)               TO NYFNR
      *  VAREGRUPPE,ALFAKODE,PRISTYPE.
           MOVE FELT31 (1:10)              TO FELT10
           MOVE FELT10 (1:8)               TO VGRALF
           MOVE VGRALF (6:3)               TO NYALFA
      *
           SET I-65                        TO TRUE
           PERFORM EXCEPTION-OUTPUT
           ADD 1                           TO ANTNY
           SET NOT-I-65                    TO TRUE
           GO TO LOOP-T
      *****************************************************
           .
 
       SLUTT-T.
      ******************************************************
           CONTINUE.
 
       TILTAB-GET SECTION.
       TILTAB-GET-P.
           IF  TILTAB-EOF-OFF
               READ TILTAB
               AT END
                   SET TILTAB-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TILTAB-FLDSET SECTION.
       TILTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE TILTAB-IO-AREA (1:3)   TO FRAFIR (1:3)
               MOVE TILTAB-IO-AREA (4:5)   TO FRAVGR (1:5)
               MOVE TILTAB-IO-AREA (9:3)   TO FRAALF (1:3)
               MOVE TILTAB-IO-AREA (12:1)  TO FRAPT (1:1)
               MOVE TILTAB-IO-AREA (13:2)  TO FRASEQ-IO
               MOVE TILTAB-IO-AREA (15:46) TO FRADAT (1:46)
           END-EVALUATE.
 
       TILTAB-IDSET SECTION.
       TILTAB-IDSET-P.
           SET I-02                        TO TRUE.
 
       TILTAB-CHK-LEVEL SECTION.
       TILTAB-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO TILTAB-LEVEL-02
               MOVE TILTAB-IO-AREA (1:3)   TO TILTAB-02-L2-FRAFIR
               MOVE TILTAB-IO-AREA (4:5)   TO TILTAB-02-L1-FRAVGR
               MOVE TILTAB-IO-AREA (9:3)   TO TILTAB-02-L1-FRAALF
               MOVE TILTAB-IO-AREA (12:1)  TO TILTAB-02-L1-FRAPT
               IF  TILTAB-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  TILTAB-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  TILTAB-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  TILTAB-02-L2          TO THE-PRIOR-L2
               MOVE  TILTAB-02-L1          TO THE-PRIOR-L1
               SET TILTAB-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       TILTAB-MATCH-SET SECTION.
       TILTAB-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE TILTAB-IO-AREA (1:3)   TO TILTAB-M-02-M4-FRAFIR
               MOVE TILTAB-IO-AREA (4:5)   TO TILTAB-M-02-M3-FRAVGR
               MOVE TILTAB-IO-AREA (9:3)   TO TILTAB-M-02-M2-FRAALF
           END-EVALUATE.
 
       VARE-GET SECTION.
       VARE-GET-P.
           IF  VARE-EOF-OFF
               READ VARE
               AT END
                   SET VARE-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARE-FLDSET SECTION.
       VARE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE VARE-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE VARE-IO-AREA (6:1)     TO EDBNR1 (1:1)
               MOVE VARE-IO-AREA (13:3)    TO ALFA (1:3)
               MOVE VARE-IO-AREA (88:7)    TO ALTNR (1:7)
               MOVE VARE-IO-AREA (95:1)    TO PT (1:1)
               MOVE VARE-IO-AREA (96:1)    TO AUTOO (1:1)
               MOVE VARE-IO-AREA (97:5)    TO ANTINN-IO
               MOVE VARE-IO-AREA (102:5)   TO ANTUT-IO
               MOVE VARE-IO-AREA (118:5)   TO VGR (1:5)
               MOVE VARE-IO-AREA (127:1)   TO UTGA-ELGR (1:1)
               MOVE VARE-IO-AREA (128:1)   TO SLETT (1:1)
           END-EVALUATE.
 
       VARE-IDSET SECTION.
       VARE-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARE-CHK-LEVEL SECTION.
       VARE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARE-LEVEL-01
               MOVE VARE-IO-AREA (3:3)     TO VARE-01-L2-FIRMA
               MOVE VARE-IO-AREA (13:3)    TO VARE-01-L1-ALFA
               MOVE VARE-IO-AREA (95:1)    TO VARE-01-L1-PT
               MOVE VARE-IO-AREA (118:5)   TO VARE-01-L1-VGR
               IF  VARE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARE-01-L2            TO THE-PRIOR-L2
               MOVE  VARE-01-L1            TO THE-PRIOR-L1
               SET VARE-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       VARE-MATCH-SET SECTION.
       VARE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO VARE-M-01-M4-FIRMA
               MOVE VARE-IO-AREA (118:5)   TO VARE-M-01-M3-VGR
               MOVE VARE-IO-AREA (13:3)    TO VARE-M-01-M2-ALFA
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  TILTAB-EOF
               MOVE HIGH-VALUES            TO TILTAB-MC
                                              TILTAB-MP
           END-IF
           IF  VARE-EOF
               MOVE HIGH-VALUES            TO VARE-MC
                                              VARE-MP
           END-IF
           IF  TILTAB-MC < TILTAB-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VARE-MC < VARE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  TILTAB-MC < VARE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILTAB-PROCESS      TO TRUE
                   MOVE TILTAB-MC          TO TILTAB-MP
                   IF  TILTAB-MC = VARE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARE-MC < TILTAB-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARE-PROCESS        TO TRUE
                   MOVE VARE-MC            TO VARE-MP
                   IF  VARE-MC = TILTAB-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  TILTAB-MC = VARE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILTAB-PROCESS      TO TRUE
                   MOVE TILTAB-MC          TO TILTAB-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-65)
               MOVE SPACES TO VOAUT-IO-AREA
               INITIALIZE VOAUT-IO-AREA
               MOVE FIRMA                  TO VOAUT-IO-AREA (1:3)
               MOVE EDBNR                  TO VOAUT-IO-AREA (4:7)
               MOVE ALTNR                  TO VOAUT-IO-AREA (11:7)
               MOVE NYALFA                 TO VOAUT-IO-AREA (18:3)
               MOVE ' '                    TO VOAUT-IO-AREA (21:1)
               MOVE '                    ' TO VOAUT-IO-AREA (22:20)
               MOVE NYFNR                  TO VOAUT-IO-AREA (42:3)
               MOVE '       '              TO VOAUT-IO-AREA (45:7)
               MOVE '       '              TO VOAUT-IO-AREA (52:7)
               WRITE VOAUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG044 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT RECORDS FRA GIVER' TO LISTE-IO-AREA (1:24)
               MOVE ' LEST.  '             TO LISTE-IO-AREA (25:8)
               MOVE ANT                    TO XO-70YN9
               MOVE XO-70YN9               TO LISTE-IO-AREA (39:7)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT RECORDS TIL MOTTA' TO LISTE-IO-AREA (1:24)
               MOVE 'GER DANNET'           TO LISTE-IO-AREA (25:10)
               MOVE ANTNY                  TO XO-70YN9
               MOVE XO-70YN9               TO LISTE-IO-AREA (39:7)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               MOVE 01                     TO LISTE-AFTER-SKIP
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
           SET TILTAB-LEVEL-INIT           TO TRUE
           INITIALIZE TILTAB-DATA-FIELDS
           SET TILTAB-EOF-OFF              TO TRUE
           SET TILTAB-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO TILTAB-MC
                                              TILTAB-MP
           OPEN INPUT TILTAB
           SET VARE-LEVEL-INIT             TO TRUE
           INITIALIZE VARE-DATA-FIELDS
           SET VARE-EOF-OFF                TO TRUE
           SET VARE-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO VARE-MC
                                              VARE-MP
           OPEN INPUT VARE
           OPEN OUTPUT VOAUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ART-I FROM 1 BY 1
                     UNTIL ART-I > ART-MAX
               INITIALIZE ART (ART-I)
           END-PERFORM
           SET ART-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILTAB
           CLOSE VARE
           CLOSE VOAUT
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
