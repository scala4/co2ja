       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO321R.
      **********************************************  Z-WIN-RPG2   ****
      *  JCL:DOP.XDOP40ME I DOP.XDOP40RD                              *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN 2010                     *
      *  PROGRAM.......: RKO321 - INNBETALINGER TIL MEKONOMEN         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO321.rpg
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
           SELECT RESFILI
               ASSIGN TO UT-S-RESFILI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILI-STATUS.
           SELECT DAGINNI
               ASSIGN TO UT-S-DAGINNI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGINNI-STATUS.
           SELECT DAGINNO
               ASSIGN TO DAGINNO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGINNO-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESFILI
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  RESFILI-IO-AREA.
           05  RESFILI-IO-AREA-X           PICTURE X(80).
       FD DAGINNI
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  DAGINNI-IO-AREA.
           05  DAGINNI-IO-AREA-X           PICTURE X(80).
       FD DAGINNO
               RECORD CONTAINS 77.
       01  DAGINNO-IO-AREA.
           05  DAGINNO-IO-AREA-X           PICTURE X(77).
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
           10  RESFILI-STATUS              PICTURE 99 VALUE 0.
           10  DAGINNI-STATUS              PICTURE 99 VALUE 0.
           10  DAGINNO-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  TKDATA-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILI-EOF-OFF         VALUE '0'.
               88  RESFILI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILI-READ-OFF        VALUE '0'.
               88  RESFILI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILI-PROCESS-OFF     VALUE '0'.
               88  RESFILI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESFILI-LEVEL-INIT-OFF  VALUE '0'.
               88  RESFILI-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGINNI-EOF-OFF         VALUE '0'.
               88  DAGINNI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGINNI-READ-OFF        VALUE '0'.
               88  DAGINNI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGINNI-PROCESS-OFF     VALUE '0'.
               88  DAGINNI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  DAGINNI-LEVEL-INIT-OFF  VALUE '0'.
               88  DAGINNI-LEVEL-INIT      VALUE '1'.
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
           05  TKDATA-XX-DATA-FIELDS.
               10  TKTK                    PICTURE X(2).
               10  FILLER                  PICTURE X(19).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  TKTEXT                  PICTURE X(7).
               10  FILLER                  PICTURE X(12).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  TKBILN                  PICTURE X(6).
               10  FILLER                  PICTURE X(6).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  TKREFN                  PICTURE X(6).
           05  RESFILI-LEVEL-01.
               10  RESFILI-01-L3.
                   15  RESFILI-01-L3-FIRMA PICTURE X(3).
               10  RESFILI-01-L2.
                   15  RESFILI-01-L2-RESK  PICTURE X(6).
               10  RESFILI-01-L1.
                   15  RESFILI-01-L1-REFNR PICTURE X(6).
           05  RESFILI-DATA-FIELDS.
               10  REC801                  PICTURE X(80).
               10  FIRMA                   PICTURE X(3).
               10  RESK                    PICTURE X(6).
               10  REFNR                   PICTURE X(6).
               10  TK                      PICTURE X(2).
               10  BETBET                  PICTURE X(2).
               10  TEKST                   PICTURE X(24).
           05  RESFILI-MP                  PICTURE X(15).
           05  RESFILI-MC                  PICTURE X(15).
           05  RESFILI-M-01            REDEFINES RESFILI-MC.
               10  RESFILI-M-01-M3.
                   15  RESFILI-M-01-M3-FIRMA-G.
                       20  RESFILI-M-01-M3-FIRMA PICTURE X(3).
               10  RESFILI-M-01-M2.
                   15  RESFILI-M-01-M2-RESK-G.
                       20  RESFILI-M-01-M2-RESK PICTURE X(6).
               10  RESFILI-M-01-M1.
                   15  RESFILI-M-01-M1-REFNR-G.
                       20  RESFILI-M-01-M1-REFNR PICTURE X(6).
           05  DAGINNI-LEVEL-02.
               10  DAGINNI-02-L3.
                   15  DAGINNI-02-L3-FIRMA PICTURE X(3).
               10  DAGINNI-02-L2.
                   15  DAGINNI-02-L2-RESK  PICTURE X(6).
               10  DAGINNI-02-L1.
                   15  DAGINNI-02-L1-REFNR PICTURE X(6).
           05  DAGINNI-DATA-FIELDS.
               10  REC802                  PICTURE X(80).
      *                                      16  17 TKODE           25
               10  BILNR                   PICTURE X(6).
               10  BILART                  PICTURE X(1).
               10  BILDAT                  PICTURE X(6).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  PERIOD                  PICTURE X(6).
           05  DAGINNI-MP                  PICTURE X(15).
           05  DAGINNI-MC                  PICTURE X(15).
           05  DAGINNI-M-02            REDEFINES DAGINNI-MC.
               10  DAGINNI-M-02-M3.
                   15  DAGINNI-M-02-M3-FIRMA-G.
                       20  DAGINNI-M-02-M3-FIRMA PICTURE X(3).
               10  DAGINNI-M-02-M2.
                   15  DAGINNI-M-02-M2-RESK-G.
                       20  DAGINNI-M-02-M2-RESK PICTURE X(6).
               10  DAGINNI-M-02-M1.
                   15  DAGINNI-M-02-M1-REFNR-G.
                       20  DAGINNI-M-02-M1-REFNR PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  OVTK                    PICTURE X(2).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(6).
               10  ANTT-IO.
                   15  ANTT                PICTURE S9(6).
               10  NYBEL-IO.
                   15  NYBEL               PICTURE S9(7)V9(2).
               10  BELF-IO.
                   15  BELF                PICTURE S9(7)V9(2).
               10  BELT-IO.
                   15  BELT                PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  EDIT-BELO-ELGP          PICTURE Z999999,9.99.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-60YYZ                PICTURE ZZZ.ZZZ.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESFILI-PROCESS
               SET RESFILI-PROCESS-OFF     TO TRUE
               SET RESFILI-READ            TO TRUE
           END-IF
 
           IF  RESFILI-READ
               PERFORM RESFILI-GET
               SET RESFILI-READ-OFF        TO TRUE
               IF  NOT RESFILI-EOF
                   PERFORM RESFILI-MATCH-SET
               END-IF
           END-IF
 
           IF  DAGINNI-PROCESS
               SET DAGINNI-PROCESS-OFF     TO TRUE
               SET DAGINNI-READ            TO TRUE
           END-IF
 
           IF  DAGINNI-READ
               PERFORM DAGINNI-GET
               SET DAGINNI-READ-OFF        TO TRUE
               IF  NOT DAGINNI-EOF
                   PERFORM DAGINNI-MATCH-SET
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
 
           IF  RESFILI-PROCESS
               PERFORM RESFILI-IDSET
           END-IF
 
           IF  DAGINNI-PROCESS
               PERFORM DAGINNI-IDSET
           END-IF
 
           IF  RESFILI-PROCESS
               PERFORM RESFILI-CHK-LEVEL
           END-IF
 
           IF  DAGINNI-PROCESS
               PERFORM DAGINNI-CHK-LEVEL
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
 
           IF  RESFILI-PROCESS
               PERFORM RESFILI-FLDOFF
               PERFORM RESFILI-FLDSET
           END-IF
 
           IF  DAGINNI-PROCESS
               PERFORM DAGINNI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESFILI-PROCESS
           OR  DAGINNI-PROCESS
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
               SET NOT-I-21                TO TRUE
           END-IF
           SET NOT-I-20                    TO TRUE
           SET NOT-I-30                    TO TRUE
           SET NOT-I-96                    TO TRUE
           SET NOT-I-22                    TO TRUE
           IF  (I-01)
               MOVE '00'                   TO OVTK
           END-IF
           IF  (I-01 AND I-MR)
               MOVE TK                     TO OVTK
      * NMR                GOTO SLUTT
      *     TEST OM BETALINGSMÅTE 07 (KONTANT.)           *
      *     TEST OM BETALINGSMÅTE 14 (OPPKRAV.)           *
           END-IF
           IF  (I-MR)
               SET NOT-I-20                TO TRUE
               IF  BETBET = '07'
                   SET I-20                TO TRUE
               END-IF
      * N20      BETBET    COMP "14"                     20 = OPPKRAV.  .
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
           END-IF
           PERFORM TKRUT-S
      *  02                MOVE "TKTEXT  "BUGFL2  8        LEDETXT DEBUG
      *  02      BUGFL2    DEBUGBUGFILO   TKTEXT           VIS FELT/IND
      *  02                MOVE "TEKST   "BUGFL2  8        LEDETXT DEBUG
      *  02      BUGFL2    DEBUGBUGFILO   TEKST            VIS FELT/IND
           IF  (I-02)
               ADD 1                       TO ANTF
               SET NOT-I-21                TO TRUE
               IF  ANTF > 0
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  ANTF = 1
                   SET I-22                TO TRUE
               END-IF
               ADD 1                       TO ANTT
               DIVIDE BELO-ELGP BY -1  GIVING NYBEL
               SET NOT-I-23                TO TRUE
               IF  NYBEL NOT < 0
                   SET I-23                TO TRUE
               END-IF
               ADD NYBEL                   TO BELF
               ADD NYBEL                   TO BELT
               SET I-30                    TO TRUE
      *  02                MOVE "REC802  "BUGFL2  8        LEDETXT DEBUG
      *  02      BUGFL2    DEBUGBUGFILO   REC802           VIS FELT/IND
      *  02                MOVE "RECA    "BUGFL2  8        LEDETXT DEBUG
      *  02      BUGFL2    DEBUGBUGFILO   RECA             VIS FELT/IND
      *  02                MOVE "ANTF    "BUGFL2  8        LEDETXT DEBUG
      *  02      BUGFL2    DEBUGBUGFILO   ANTF             VIS FELT/IND
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           END-IF
           .
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV COBOL SUBRUTINE RESTRAN.  *
      *    DENNE RUTINE HENTER RESKONTRO TEKST.            *
      ******************************************************
           CONTINUE.
 
       TKRUT-S SECTION.
       TKRUT-S-P.
           MOVE OVTK                       TO TKTK
      *R 25                MOVE OVTK      TKTK              TRANS-KODE.
      *RN25                MOVE TKODE     TKTK              TRANS-KODE.
           MOVE '       '                  TO TKTEXT
           MOVE BILNR                      TO TKBILN
           MOVE REFNR                      TO TKREFN
           CALL 'RESTRAN' USING TKDATA-XX-DATA-FIELDS
      *R                   MOVELTKTEXT    TEKST  24         FLYTT TEKST.
           .
 
       RESFILI-GET SECTION.
       RESFILI-GET-P.
           IF  RESFILI-EOF-OFF
               READ RESFILI
               AT END
                   SET RESFILI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESFILI-FLDOFF SECTION.
       RESFILI-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-24                TO TRUE
           END-EVALUATE.
 
       RESFILI-FLDSET SECTION.
       RESFILI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESFILI-IO-AREA (1:80) TO REC801 (1:80)
               MOVE RESFILI-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE RESFILI-IO-AREA (4:6)  TO RESK (1:6)
               MOVE RESFILI-IO-AREA (10:6) TO REFNR (1:6)
               MOVE RESFILI-IO-AREA (16:2) TO TK (1:2)
               MOVE RESFILI-IO-AREA (18:2) TO BETBET (1:2)
               MOVE RESFILI-IO-AREA (20:24) TO TEKST (1:24)
               IF  TEKST = SPACES
                   SET I-24                TO TRUE
               END-IF
           END-EVALUATE.
 
       RESFILI-IDSET SECTION.
       RESFILI-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESFILI-CHK-LEVEL SECTION.
       RESFILI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESFILI-LEVEL-01
               MOVE RESFILI-IO-AREA (1:3)  TO RESFILI-01-L3-FIRMA
               MOVE RESFILI-IO-AREA (4:6)  TO RESFILI-01-L2-RESK
               MOVE RESFILI-IO-AREA (10:6) TO RESFILI-01-L1-REFNR
               IF  RESFILI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESFILI-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESFILI-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESFILI-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESFILI-01-L3         TO THE-PRIOR-L3
               MOVE  RESFILI-01-L2         TO THE-PRIOR-L2
               MOVE  RESFILI-01-L1         TO THE-PRIOR-L1
               SET RESFILI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESFILI-MATCH-SET SECTION.
       RESFILI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESFILI-IO-AREA (1:3)  TO RESFILI-M-01-M3-FIRMA
               MOVE RESFILI-IO-AREA (4:6)  TO RESFILI-M-01-M2-RESK
               MOVE RESFILI-IO-AREA (10:6) TO RESFILI-M-01-M1-REFNR
           END-EVALUATE.
 
       DAGINNI-GET SECTION.
       DAGINNI-GET-P.
           IF  DAGINNI-EOF-OFF
               READ DAGINNI
               AT END
                   SET DAGINNI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGINNI-FLDSET SECTION.
       DAGINNI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGINNI-IO-AREA (1:80) TO REC802 (1:80)
               MOVE DAGINNI-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE DAGINNI-IO-AREA (4:6)  TO RESK (1:6)
               MOVE DAGINNI-IO-AREA (10:6) TO REFNR (1:6)
               MOVE DAGINNI-IO-AREA (44:6) TO BILNR (1:6)
               MOVE DAGINNI-IO-AREA (50:1) TO BILART (1:1)
               MOVE DAGINNI-IO-AREA (51:6) TO BILDAT (1:6)
               MOVE DAGINNI-IO-AREA (57:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE DAGINNI-IO-AREA (66:6) TO PERIOD (1:6)
           END-EVALUATE.
 
       DAGINNI-IDSET SECTION.
       DAGINNI-IDSET-P.
           SET I-02                        TO TRUE.
 
       DAGINNI-CHK-LEVEL SECTION.
       DAGINNI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGINNI-LEVEL-02
               MOVE DAGINNI-IO-AREA (1:3)  TO DAGINNI-02-L3-FIRMA
               MOVE DAGINNI-IO-AREA (4:6)  TO DAGINNI-02-L2-RESK
               MOVE DAGINNI-IO-AREA (10:6) TO DAGINNI-02-L1-REFNR
               IF  DAGINNI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGINNI-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  DAGINNI-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  DAGINNI-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  DAGINNI-02-L3         TO THE-PRIOR-L3
               MOVE  DAGINNI-02-L2         TO THE-PRIOR-L2
               MOVE  DAGINNI-02-L1         TO THE-PRIOR-L1
               SET DAGINNI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DAGINNI-MATCH-SET SECTION.
       DAGINNI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGINNI-IO-AREA (1:3)  TO DAGINNI-M-02-M3-FIRMA
               MOVE DAGINNI-IO-AREA (4:6)  TO DAGINNI-M-02-M2-RESK
               MOVE DAGINNI-IO-AREA (10:6) TO DAGINNI-M-02-M1-REFNR
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  RESFILI-EOF
               MOVE HIGH-VALUES            TO RESFILI-MC
                                              RESFILI-MP
           END-IF
           IF  DAGINNI-EOF
               MOVE HIGH-VALUES            TO DAGINNI-MC
                                              DAGINNI-MP
           END-IF
           IF  RESFILI-MC < RESFILI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  DAGINNI-MC < DAGINNI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RESFILI-MC < DAGINNI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESFILI-PROCESS     TO TRUE
                   MOVE RESFILI-MC         TO RESFILI-MP
                   IF  RESFILI-MC = DAGINNI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGINNI-MC < RESFILI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGINNI-PROCESS     TO TRUE
                   MOVE DAGINNI-MC         TO DAGINNI-MP
                   IF  DAGINNI-MC = RESFILI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESFILI-MC = DAGINNI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESFILI-PROCESS     TO TRUE
                   MOVE RESFILI-MC         TO RESFILI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-22)
               MOVE SPACES TO DAGINNO-IO-AREA
               INITIALIZE DAGINNO-IO-AREA
               MOVE 'FIR'                  TO DAGINNO-IO-AREA (1:3)
               MOVE 'KUNDE '               TO DAGINNO-IO-AREA (5:6)
               IF  (I-U2)
                   MOVE 'LEVER.'           TO DAGINNO-IO-AREA (5:6)
               END-IF
               MOVE 'BILDTO'               TO DAGINNO-IO-AREA (12:6)
               MOVE 'BELØP'                TO DAGINNO-IO-AREA (19:5)
               MOVE 'BILNR '               TO DAGINNO-IO-AREA (31:6)
               MOVE 'REFNR '               TO DAGINNO-IO-AREA (38:6)
               MOVE 'TEKST'                TO DAGINNO-IO-AREA (45:5)
               MOVE 'PERIOD'               TO DAGINNO-IO-AREA (70:6)
               MOVE 'A'                    TO DAGINNO-IO-AREA (77:1)
               WRITE DAGINNO-IO-AREA
           END-IF
           IF  (I-30)
               MOVE SPACES TO DAGINNO-IO-AREA
               INITIALIZE DAGINNO-IO-AREA
               MOVE FIRMA                  TO DAGINNO-IO-AREA (1:3)
               MOVE ';'                    TO DAGINNO-IO-AREA (4:1)
               MOVE RESK                   TO DAGINNO-IO-AREA (5:6)
               MOVE ';'                    TO DAGINNO-IO-AREA (11:1)
               MOVE BILDAT                 TO DAGINNO-IO-AREA (12:6)
               MOVE ';'                    TO DAGINNO-IO-AREA (18:1)
               MOVE BELO-ELGP              TO EDIT-BELO-ELGP
               MOVE EDIT-BELO-ELGP         TO DAGINNO-IO-AREA (18:12)
               IF  (I-23)
                   MOVE '+'                TO DAGINNO-IO-AREA (19:1)
               END-IF
               IF  (NOT-I-23)
                   MOVE '-'                TO DAGINNO-IO-AREA (19:1)
               END-IF
               MOVE ';'                    TO DAGINNO-IO-AREA (30:1)
               MOVE BILNR                  TO DAGINNO-IO-AREA (31:6)
               MOVE ';'                    TO DAGINNO-IO-AREA (37:1)
               MOVE REFNR                  TO DAGINNO-IO-AREA (38:6)
               MOVE ';'                    TO DAGINNO-IO-AREA (44:1)
               IF  (NOT-I-24)
                   MOVE TEKST              TO DAGINNO-IO-AREA (45:24)
               END-IF
               IF  (I-24)
                   MOVE TKTEXT             TO DAGINNO-IO-AREA (45:7)
               END-IF
               MOVE ';'                    TO DAGINNO-IO-AREA (69:1)
               MOVE PERIOD                 TO DAGINNO-IO-AREA (70:6)
               MOVE ';'                    TO DAGINNO-IO-AREA (76:1)
               MOVE BILART                 TO DAGINNO-IO-AREA (77:1)
               WRITE DAGINNO-IO-AREA
           END-IF
           IF  (I-30 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE RESK                   TO LISTE-IO-AREA (5:6)
               MOVE BILDAT                 TO LISTE-IO-AREA (12:6)
               MOVE BILNR                  TO LISTE-IO-AREA (19:6)
               MOVE NYBEL                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (26:13)
               MOVE REFNR                  TO LISTE-IO-AREA (40:6)
               MOVE OVTK                   TO LISTE-IO-AREA (47:2)
      *                     N25TKODE     48
               IF  (NOT-I-24)
                   MOVE TEKST              TO LISTE-IO-AREA (50:24)
               END-IF
               IF  (I-24)
                   MOVE TKTEXT             TO LISTE-IO-AREA (50:7)
               END-IF
               MOVE PERIOD                 TO LISTE-IO-AREA (75:6)
               MOVE BILART                 TO LISTE-IO-AREA (84:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> PROGRAM: RKO321 ' TO LISTE-IO-AREA (1:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> '                 TO LISTE-IO-AREA (1:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-MR AND I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> REC1:'            TO LISTE-IO-AREA (1:9)
               MOVE REC801                 TO LISTE-IO-AREA (11:80)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-MR AND I-02)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> REC2:'            TO LISTE-IO-AREA (1:9)
               MOVE REC802                 TO LISTE-IO-AREA (11:80)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-22 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONTROLLISTE INNBET TIL ' TO LISTE-IO-AREA (1:24)
               MOVE 'MEKONOMEN        RKO321 ' TO LISTE-IO-AREA (25:24)
               IF  (I-U2)
                   MOVE 'KONTROLLISTE UTBET. TIL ' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-U2)
                   MOVE 'MEKONOMEN        RKO322 ' TO LISTE-IO-AREA
                                                               (25:24)
               END-IF
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (49:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (5:6)
               IF  (I-U2)
                   MOVE 'LEVER.'           TO LISTE-IO-AREA (5:6)
               END-IF
               MOVE 'BILDTO'               TO LISTE-IO-AREA (12:6)
               MOVE 'BILNR '               TO LISTE-IO-AREA (19:6)
               MOVE 'BELØP '               TO LISTE-IO-AREA (33:6)
               MOVE 'REFNR '               TO LISTE-IO-AREA (40:6)
               MOVE 'TK'                   TO LISTE-IO-AREA (47:2)
               MOVE 'TEKST'                TO LISTE-IO-AREA (50:5)
               MOVE 'PERIOD'               TO LISTE-IO-AREA (75:6)
               MOVE 'BILART'               TO LISTE-IO-AREA (82:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONTROLLISTE INNBET TIL ' TO LISTE-IO-AREA (1:24)
               MOVE 'MEKONOMEN        RKO321 ' TO LISTE-IO-AREA (25:24)
               IF  (I-U2)
                   MOVE 'KONTROLLISTE UTBET. TIL ' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-U2)
                   MOVE 'MEKONOMEN        RKO322 ' TO LISTE-IO-AREA
                                                               (25:24)
               END-IF
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (49:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (5:6)
               IF  (I-U2)
                   MOVE 'LEVER.'           TO LISTE-IO-AREA (5:6)
               END-IF
               MOVE 'BILDTO'               TO LISTE-IO-AREA (12:6)
               MOVE 'BILNR '               TO LISTE-IO-AREA (19:6)
               MOVE 'BELØP '               TO LISTE-IO-AREA (33:6)
               MOVE 'REFNR '               TO LISTE-IO-AREA (40:6)
               MOVE 'TK'                   TO LISTE-IO-AREA (47:2)
               MOVE 'TEKST'                TO LISTE-IO-AREA (50:5)
               MOVE 'PERIOD'               TO LISTE-IO-AREA (75:6)
               MOVE 'BILART'               TO LISTE-IO-AREA (82:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND I-21)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INNBETALT   '         TO LISTE-IO-AREA (1:12)
               IF  (I-U2)
                   MOVE 'UTBETALT    '     TO LISTE-IO-AREA (1:12)
               END-IF
               MOVE BELF                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (26:13)
               INITIALIZE BELF
               MOVE ANTF                   TO XO-60YYZ
               MOVE XO-60YYZ               TO LISTE-IO-AREA (40:7)
               INITIALIZE ANTF
               MOVE 'TRANSER'              TO LISTE-IO-AREA (48:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOT INNBETALT   '     TO LISTE-IO-AREA (1:16)
               IF  (I-U2)
                   MOVE 'TOT UTBETALT    ' TO LISTE-IO-AREA (1:16)
               END-IF
               MOVE BELT                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (26:13)
               MOVE ANTT                   TO XO-60YYZ
               MOVE XO-60YYZ               TO LISTE-IO-AREA (40:7)
               MOVE 'TRANSER'              TO LISTE-IO-AREA (48:7)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-U2 AND I-U3)
           AND (I-01 AND I-98)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (53:80)
               MOVE R                      TO LISTE-IO-AREA (125:8)
               MOVE P-IO                   TO LISTE-IO-AREA (130:3)
               MOVE S-IO                   TO LISTE-IO-AREA (128:5)
               MOVE TKTK                   TO LISTE-IO-AREA (131:2)
               MOVE TKBILN                 TO LISTE-IO-AREA (127:6)
               MOVE TKREFN                 TO LISTE-IO-AREA (127:6)
               MOVE TKTK                   TO LISTE-IO-AREA (131:2)
               MOVE TKBILN                 TO LISTE-IO-AREA (127:6)
               MOVE TKREFN                 TO LISTE-IO-AREA (127:6)
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
           MOVE 2                          TO LR-CHECK
           SET RESFILI-LEVEL-INIT          TO TRUE
           INITIALIZE RESFILI-DATA-FIELDS
           SET RESFILI-EOF-OFF             TO TRUE
           SET RESFILI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESFILI-MC
                                              RESFILI-MP
           OPEN INPUT RESFILI
           SET DAGINNI-LEVEL-INIT          TO TRUE
           INITIALIZE DAGINNI-DATA-FIELDS
           SET DAGINNI-EOF-OFF             TO TRUE
           SET DAGINNI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO DAGINNI-MC
                                              DAGINNI-MP
           OPEN INPUT DAGINNI
           OPEN OUTPUT DAGINNO
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESFILI
           CLOSE DAGINNI
           CLOSE DAGINNO
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
