       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO320R.
      **********************************************  Z-WIN-RPG2   ****
      *  JCL:DOP.XDOP40RD                        ***TXT***OK EN       *
      *  PROGRAM.......: RKO320- FØR 0905:RSK320                      *
      *  DANNE NY RENTEGRUNNLAGSMASTER - DAGLIG.                      *
      *E 09.08.96: TAR IKKE MED RENTENOTA - TK=22 - I RENTEGRUNNLAGET *
      *E 04.09.98: TILPASSET ÅR 2000.                                 *
      *E 21.10.98: FLYTTER TKODE FRA 88-89 TIL 72-73 I DAGRESK.       *
      *E SEPT. 05: UTVIDELSE AV RECORDLENGDE FRA 89 -> 200            *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO320.rpg
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
           SELECT RESKMAS
               ASSIGN TO UT-S-RESKMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKMAS-STATUS.
           SELECT DAGINN
               ASSIGN TO UT-S-DAGINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGINN-STATUS.
           SELECT RENTEM
               ASSIGN TO UT-S-RENTEM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RENTEM-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT NYRENTE
               ASSIGN TO UT-S-NYRENTE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYRENTE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESKMAS
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RESKMAS-IO-AREA.
           05  RESKMAS-IO-AREA-X           PICTURE X(200).
       FD DAGINN
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  DAGINN-IO-AREA.
           05  DAGINN-IO-AREA-X            PICTURE X(200).
       FD RENTEM
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RENTEM-IO-AREA.
           05  RENTEM-IO-AREA-X            PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD NYRENTE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  NYRENTE-IO-AREA.
           05  NYRENTE-IO-AREA-X           PICTURE X(200).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
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
           10  RESKMAS-STATUS              PICTURE 99 VALUE 0.
           10  DAGINN-STATUS               PICTURE 99 VALUE 0.
           10  RENTEM-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  NYRENTE-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKMAS-EOF-OFF         VALUE '0'.
               88  RESKMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKMAS-READ-OFF        VALUE '0'.
               88  RESKMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKMAS-PROCESS-OFF     VALUE '0'.
               88  RESKMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKMAS-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGINN-EOF-OFF          VALUE '0'.
               88  DAGINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGINN-READ-OFF         VALUE '0'.
               88  DAGINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGINN-PROCESS-OFF      VALUE '0'.
               88  DAGINN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  DAGINN-LEVEL-INIT-OFF   VALUE '0'.
               88  DAGINN-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEM-EOF-OFF          VALUE '0'.
               88  RENTEM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEM-READ-OFF         VALUE '0'.
               88  RENTEM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEM-PROCESS-OFF      VALUE '0'.
               88  RENTEM-PROCESS          VALUE '1'.
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
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
           05  RESKMAS-LEVEL-01.
               10  RESKMAS-01-L3.
                   15  RESKMAS-01-L3-FIRMA PICTURE X(3).
               10  RESKMAS-01-L2.
                   15  RESKMAS-01-L2-RESK  PICTURE X(6).
               10  RESKMAS-01-L1.
                   15  RESKMAS-01-L1-REFNR PICTURE X(6).
           05  RESKMAS-DATA-FIELDS.
               10  REC601                  PICTURE X(60).
               10  FIRMA                   PICTURE X(3).
               10  RESK                    PICTURE X(6).
               10  TK                      PICTURE X(2).
               10  REFNR                   PICTURE X(6).
               10  FORF-IO.
                   15  FORF                PICTURE S9(6).
               10  BETBET                  PICTURE X(2).
           05  RESKMAS-MP                  PICTURE X(15).
           05  RESKMAS-MC                  PICTURE X(15).
           05  RESKMAS-M-01            REDEFINES RESKMAS-MC.
               10  RESKMAS-M-01-M3.
                   15  RESKMAS-M-01-M3-FIRMA-G.
                       20  RESKMAS-M-01-M3-FIRMA PICTURE X(3).
               10  RESKMAS-M-01-M2.
                   15  RESKMAS-M-01-M2-RESK-G.
                       20  RESKMAS-M-01-M2-RESK PICTURE X(6).
               10  RESKMAS-M-01-M1.
                   15  RESKMAS-M-01-M1-REFNR-G.
                       20  RESKMAS-M-01-M1-REFNR PICTURE X(6).
           05  DAGINN-LEVEL-02.
               10  DAGINN-02-L3.
                   15  DAGINN-02-L3-FIRMA  PICTURE X(3).
               10  DAGINN-02-L2.
                   15  DAGINN-02-L2-RESK   PICTURE X(6).
               10  DAGINN-02-L1.
                   15  DAGINN-02-L1-REFNR  PICTURE X(6).
           05  DAGINN-DATA-FIELDS.
               10  RECA                    PICTURE X(200).
               10  REC602                  PICTURE X(60).
               10  BILDAT-IO.
                   15  BILDAT              PICTURE S9(6).
               10  TKODE                   PICTURE X(2).
               10  BILNR                   PICTURE X(6).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
           05  DAGINN-MP                   PICTURE X(15).
           05  DAGINN-MC                   PICTURE X(15).
           05  DAGINN-M-02             REDEFINES DAGINN-MC.
               10  DAGINN-M-02-M3.
                   15  DAGINN-M-02-M3-FIRMA-G.
                       20  DAGINN-M-02-M3-FIRMA PICTURE X(3).
               10  DAGINN-M-02-M2.
                   15  DAGINN-M-02-M2-RESK-G.
                       20  DAGINN-M-02-M2-RESK PICTURE X(6).
               10  DAGINN-M-02-M1.
                   15  DAGINN-M-02-M1-REFNR-G.
                       20  DAGINN-M-02-M1-REFNR PICTURE X(6).
           05  RENTEM-DATA-FIELDS.
               10  RECB                    PICTURE X(200).
           05  FIRMAF-DATA-FIELDS.
               10  RENTER                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  OVTK                    PICTURE X(2).
               10  ANTB-IO.
                   15  ANTB                PICTURE S9(10).
               10  FORF8                   PICTURE X(8).
               10  BILDA8                  PICTURE X(8).
               10  ANTA-IO.
                   15  ANTA                PICTURE S9(6).
               10  NYBEL-IO.
                   15  NYBEL               PICTURE S9(7)V9(2).
               10  ANTC-IO.
                   15  ANTC                PICTURE S9(12).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-100YYZ               PICTURE Z.ZZZ.ZZZ.ZZZ.
               10  XO-60YYZ                PICTURE ZZZ.ZZZ.
               10  XO-120YYZ               PICTURE ZZZ.ZZZ.ZZZ.ZZZ.
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
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESKMAS-PROCESS
               SET RESKMAS-PROCESS-OFF     TO TRUE
               SET RESKMAS-READ            TO TRUE
           END-IF
 
           IF  RESKMAS-READ
               PERFORM RESKMAS-GET
               SET RESKMAS-READ-OFF        TO TRUE
               IF  NOT RESKMAS-EOF
                   PERFORM RESKMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  DAGINN-PROCESS
               SET DAGINN-PROCESS-OFF      TO TRUE
               SET DAGINN-READ             TO TRUE
           END-IF
 
           IF  DAGINN-READ
               PERFORM DAGINN-GET
               SET DAGINN-READ-OFF         TO TRUE
               IF  NOT DAGINN-EOF
                   PERFORM DAGINN-MATCH-SET
               END-IF
           END-IF
 
           IF  RENTEM-PROCESS
               SET RENTEM-PROCESS-OFF      TO TRUE
               SET RENTEM-READ             TO TRUE
           END-IF
 
           IF  RENTEM-READ
           AND RECORD-SELECTED-OFF
               PERFORM RENTEM-GET
               SET RENTEM-READ-OFF         TO TRUE
               IF  NOT RENTEM-EOF
                   PERFORM RENTEM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RENTEM-PROCESS      TO TRUE
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
 
           IF  RESKMAS-PROCESS
               PERFORM RESKMAS-IDSET
           END-IF
 
           IF  DAGINN-PROCESS
               PERFORM DAGINN-IDSET
           END-IF
 
           IF  RENTEM-PROCESS
               PERFORM RENTEM-IDSET
           END-IF
 
           IF  RESKMAS-PROCESS
               PERFORM RESKMAS-CHK-LEVEL
           END-IF
 
           IF  DAGINN-PROCESS
               PERFORM DAGINN-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
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
 
           IF  RESKMAS-PROCESS
               PERFORM RESKMAS-FLDSET
           END-IF
 
           IF  DAGINN-PROCESS
               PERFORM DAGINN-FLDOFF
               PERFORM DAGINN-FLDSET
           END-IF
 
           IF  RENTEM-PROCESS
               PERFORM RENTEM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKMAS-PROCESS
           OR  DAGINN-PROCESS
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
           SET NOT-I-30                    TO TRUE
           IF  (I-L3)
               SET NOT-I-15                TO TRUE
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *     SAVE FØRSTE TRANSKODE PR REFNR.
      *
           END-IF
           IF  (I-L1)
               MOVE TK                     TO OVTK
      *
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTB
               GO TO SLUTT-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           END-IF
           IF  (I-L3)
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
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-15                TO TRUE
               IF  RENTER = 'F'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-15)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-MR)
               GO TO SLUTT-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *     TEST OM BETALINGSMÅTE 07 (KONTANT.)           *
      *     TEST OM BETALINGSMÅTE 14 (OPPKRAV.)           *
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  BETBET = '07'
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  BETBET = '14'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *     TEST OM INNBETALINGEN ER ETTER FORFALL.       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      **   GJØR OM TIL 8-SIFRET DATO
           END-IF
           IF  (I-01)
               MOVE 'B'                    TO DTOKOD
               MOVE FORF                   TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND NOT-I-96)
               MOVE DTO8SI                 TO FORF8
           END-IF
           IF  (I-01 AND I-96)
               MOVE '00'                   TO BILDA8 (1:2)
               MOVE FORF                   TO BILDA8 (3:6)
      *
           END-IF
           IF  (I-02)
               MOVE 'B'                    TO DTOKOD
               MOVE BILDAT                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-02 AND NOT-I-96)
               MOVE DTO8SI                 TO BILDA8
           END-IF
           IF  (I-02 AND I-96)
               MOVE '00'                   TO BILDA8 (1:2)
               MOVE BILDAT                 TO BILDA8 (3:6)
      *  02                MOVE "FORF8   "BUGFL1  8        LEDETXT DEBUG
      *  02      BUGFL1    DEBUGBUGFILO   FORF8            VIS FELT/IND
      *  02                MOVE "BILDA8  "BUGFL1  8        LEDETXT DEBUG
      *  02      BUGFL1    DEBUGBUGFILO   BILDA8           VIS FELT/IND
           END-IF
           IF  (I-02)
               SET NOT-I-20                TO TRUE
               IF  BILDA8 NOT > FORF8
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *     TEST OM INNBETALINGEN GJELDER RENTENOTA       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  TK = '22'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTA
               DIVIDE BELO-ELGP BY -1  GIVING NYBEL
               SET I-30                    TO TRUE
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-96                    TO TRUE
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-96                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-96                    TO TRUE
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD ANTB TO ANTA            GIVING ANTC
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
           .
 
       RESKMAS-GET SECTION.
       RESKMAS-GET-P.
           IF  RESKMAS-EOF-OFF
               READ RESKMAS
               AT END
                   SET RESKMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKMAS-FLDSET SECTION.
       RESKMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKMAS-IO-AREA (1:60) TO REC601 (1:60)
               MOVE RESKMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE RESKMAS-IO-AREA (6:6)  TO RESK (1:6)
               MOVE RESKMAS-IO-AREA (22:2) TO TK (1:2)
               MOVE RESKMAS-IO-AREA (36:6) TO REFNR (1:6)
               MOVE RESKMAS-IO-AREA (42:6) TO FORF-IO
               INSPECT FORF-IO REPLACING ALL ' ' BY '0'
               MOVE RESKMAS-IO-AREA (61:2) TO BETBET (1:2)
           END-EVALUATE.
 
       RESKMAS-IDSET SECTION.
       RESKMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESKMAS-CHK-LEVEL SECTION.
       RESKMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKMAS-LEVEL-01
               MOVE RESKMAS-IO-AREA (3:3)  TO RESKMAS-01-L3-FIRMA
               MOVE RESKMAS-IO-AREA (6:6)  TO RESKMAS-01-L2-RESK
               MOVE RESKMAS-IO-AREA (36:6) TO RESKMAS-01-L1-REFNR
               IF  RESKMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKMAS-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESKMAS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKMAS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKMAS-01-L3         TO THE-PRIOR-L3
               MOVE  RESKMAS-01-L2         TO THE-PRIOR-L2
               MOVE  RESKMAS-01-L1         TO THE-PRIOR-L1
               SET RESKMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESKMAS-MATCH-SET SECTION.
       RESKMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKMAS-IO-AREA (3:3)  TO RESKMAS-M-01-M3-FIRMA
               MOVE RESKMAS-IO-AREA (6:6)  TO RESKMAS-M-01-M2-RESK
               MOVE RESKMAS-IO-AREA (36:6) TO RESKMAS-M-01-M1-REFNR
           END-EVALUATE.
 
       DAGINN-GET SECTION.
       DAGINN-GET-P.
           IF  DAGINN-EOF-OFF
               READ DAGINN
               AT END
                   SET DAGINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGINN-FLDOFF SECTION.
       DAGINN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-25                TO TRUE
           END-EVALUATE.
 
       DAGINN-FLDSET SECTION.
       DAGINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGINN-IO-AREA (1:200) TO RECA (1:200)
               MOVE DAGINN-IO-AREA (1:60)  TO REC602 (1:60)
               MOVE DAGINN-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE DAGINN-IO-AREA (6:6)   TO RESK (1:6)
               MOVE DAGINN-IO-AREA (12:6)  TO REFNR (1:6)
               MOVE DAGINN-IO-AREA (20:6)  TO BILDAT-IO
               INSPECT BILDAT-IO REPLACING ALL ' ' BY '0'
               MOVE DAGINN-IO-AREA (72:2)  TO TKODE (1:2)
               IF  TKODE = SPACES
                   SET I-25                TO TRUE
               END-IF
               MOVE DAGINN-IO-AREA (26:6)  TO BILNR (1:6)
               MOVE DAGINN-IO-AREA (39:9)  TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       DAGINN-IDSET SECTION.
       DAGINN-IDSET-P.
           SET I-02                        TO TRUE.
 
       DAGINN-CHK-LEVEL SECTION.
       DAGINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGINN-LEVEL-02
               MOVE DAGINN-IO-AREA (3:3)   TO DAGINN-02-L3-FIRMA
               MOVE DAGINN-IO-AREA (6:6)   TO DAGINN-02-L2-RESK
               MOVE DAGINN-IO-AREA (12:6)  TO DAGINN-02-L1-REFNR
               IF  DAGINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGINN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  DAGINN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  DAGINN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  DAGINN-02-L3          TO THE-PRIOR-L3
               MOVE  DAGINN-02-L2          TO THE-PRIOR-L2
               MOVE  DAGINN-02-L1          TO THE-PRIOR-L1
               SET DAGINN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       DAGINN-MATCH-SET SECTION.
       DAGINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGINN-IO-AREA (3:3)   TO DAGINN-M-02-M3-FIRMA
               MOVE DAGINN-IO-AREA (6:6)   TO DAGINN-M-02-M2-RESK
               MOVE DAGINN-IO-AREA (12:6)  TO DAGINN-M-02-M1-REFNR
           END-EVALUATE.
 
       RENTEM-GET SECTION.
       RENTEM-GET-P.
           IF  RENTEM-EOF-OFF
               READ RENTEM
               AT END
                   SET RENTEM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RENTEM-FLDSET SECTION.
       RENTEM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RENTEM-IO-AREA (1:1) = '3'
            AND   RENTEM-IO-AREA (2:1) = '1' )
               MOVE RENTEM-IO-AREA (1:200) TO RECB (1:200)
           END-EVALUATE.
 
       RENTEM-IDCHK SECTION.
       RENTEM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RENTEM-IO-AREA (1:1) = '3'
            AND   RENTEM-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RENTEM-IDSET SECTION.
       RENTEM-IDSET-P.
           EVALUATE TRUE
           WHEN ( RENTEM-IO-AREA (1:1) = '3'
            AND   RENTEM-IO-AREA (2:1) = '1' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (139:1) TO RENTER (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
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
           IF  RESKMAS-EOF
               MOVE HIGH-VALUES            TO RESKMAS-MC
                                              RESKMAS-MP
           END-IF
           IF  DAGINN-EOF
               MOVE HIGH-VALUES            TO DAGINN-MC
                                              DAGINN-MP
           END-IF
           IF  RESKMAS-MC < RESKMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  DAGINN-MC < DAGINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RESKMAS-MC < DAGINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKMAS-PROCESS     TO TRUE
                   MOVE RESKMAS-MC         TO RESKMAS-MP
                   IF  RESKMAS-MC = DAGINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGINN-MC < RESKMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGINN-PROCESS      TO TRUE
                   MOVE DAGINN-MC          TO DAGINN-MP
                   IF  DAGINN-MC = RESKMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESKMAS-MC = DAGINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKMAS-PROCESS     TO TRUE
                   MOVE RESKMAS-MC         TO RESKMAS-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03)
               MOVE SPACES TO NYRENTE-IO-AREA
               INITIALIZE NYRENTE-IO-AREA
               MOVE RECB                   TO NYRENTE-IO-AREA (1:200)
               WRITE NYRENTE-IO-AREA
           END-IF
           IF  (I-30)
               MOVE SPACES TO NYRENTE-IO-AREA
               INITIALIZE NYRENTE-IO-AREA
               MOVE RECA                   TO NYRENTE-IO-AREA (1:200)
               MOVE FORF-IO                TO NYRENTE-IO-AREA (33:6)
               MOVE NYBEL-IO               TO NYRENTE-IO-AREA (39:9)
               IF  (I-25)
                   MOVE OVTK               TO NYRENTE-IO-AREA (87:2)
               END-IF
               IF  (NOT-I-25)
                   MOVE TKODE              TO NYRENTE-IO-AREA (87:2)
               END-IF
               WRITE NYRENTE-IO-AREA
           END-IF
           IF  (I-30 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (2:3)
               MOVE RESK                   TO LISTE-IO-AREA (7:6)
               MOVE BILNR                  TO LISTE-IO-AREA (14:6)
               MOVE BILDA8                 TO LISTE-IO-AREA (21:8)
               MOVE BELO-ELGP              TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (30:13)
               MOVE REFNR                  TO LISTE-IO-AREA (50:6)
               IF  (I-25)
                   MOVE OVTK               TO LISTE-IO-AREA (57:2)
               END-IF
               IF  (NOT-I-25)
                   MOVE TKODE              TO LISTE-IO-AREA (57:2)
               END-IF
               MOVE FORF8                  TO LISTE-IO-AREA (60:8)
               MOVE NYBEL                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (69:13)
               IF  (NOT-I-25)
                   MOVE 'TRANSKODE FRA REL.FILE. ' TO LISTE-IO-AREA
                                                               (84:24)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> PROGRAM: RSK320 ' TO LISTE-IO-AREA (1:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> '                 TO LISTE-IO-AREA (1:4)
               MOVE DTOMEL                 TO LISTE-IO-AREA (14:57)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-MR AND I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> REC1:'            TO LISTE-IO-AREA (1:9)
               MOVE REC601                 TO LISTE-IO-AREA (11:60)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-96 AND I-MR AND I-02)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '==> REC2:'            TO LISTE-IO-AREA (1:9)
               MOVE REC602                 TO LISTE-IO-AREA (11:60)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RSK320 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONTROLLISTE POSTER TIL' TO LISTE-IO-AREA (12:23)
               MOVE 'RENTERUTINEN.'        TO LISTE-IO-AREA (37:13)
               MOVE 'FREMSTILLT.'          TO LISTE-IO-AREA (59:11)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (70:8)
               MOVE 'LEVERES TIL PROG. AVD.' TO LISTE-IO-AREA (84:22)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA KUNDE  BILAGS'  TO LISTE-IO-AREA (1:19)
               MOVE 'BILAGS'               TO LISTE-IO-AREA (22:6)
               MOVE 'BELØP'                TO LISTE-IO-AREA (37:5)
               MOVE 'REF.NR. TK  FORFALL'  TO LISTE-IO-AREA (49:19)
               MOVE 'BELØP'                TO LISTE-IO-AREA (76:5)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.     DATO'         TO LISTE-IO-AREA (14:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RSK320 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONTROLLISTE POSTER TIL' TO LISTE-IO-AREA (12:23)
               MOVE 'RENTERUTINEN.'        TO LISTE-IO-AREA (37:13)
               MOVE 'FREMSTILLT.'          TO LISTE-IO-AREA (59:11)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (70:8)
               MOVE 'LEVERES TIL PROG. AVD.' TO LISTE-IO-AREA (84:22)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA KUNDE  BILAGS'  TO LISTE-IO-AREA (1:19)
               MOVE 'BILAGS'               TO LISTE-IO-AREA (22:6)
               MOVE 'BELØP'                TO LISTE-IO-AREA (37:5)
               MOVE 'REF.NR. TK  FORFALL'  TO LISTE-IO-AREA (49:19)
               MOVE 'BELØP'                TO LISTE-IO-AREA (76:5)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.     DATO'         TO LISTE-IO-AREA (14:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '**********'           TO LISTE-IO-AREA (47:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (56:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* ANT REC PÅ GML RENTE' TO LISTE-IO-AREA (1:22)
               MOVE 'GRUNNLAGSMASTER.'     TO LISTE-IO-AREA (23:16)
               MOVE ANTB                   TO XO-100YYZ
               MOVE XO-100YYZ              TO LISTE-IO-AREA (42:13)
               MOVE '*'                    TO LISTE-IO-AREA (56:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (56:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* ANT REC DANNET IDAG.' TO LISTE-IO-AREA (1:22)
               MOVE ANTA                   TO XO-60YYZ
               MOVE XO-60YYZ               TO LISTE-IO-AREA (48:7)
               MOVE '*'                    TO LISTE-IO-AREA (56:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '---------------- *'   TO LISTE-IO-AREA (39:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* SUM.'               TO LISTE-IO-AREA (1:6)
               MOVE '*'                    TO LISTE-IO-AREA (56:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '---------------- *'   TO LISTE-IO-AREA (39:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* ANT REC PÅ NY RENTE' TO LISTE-IO-AREA (1:21)
               MOVE 'GRUNNLAGSMASTER.'     TO LISTE-IO-AREA (22:16)
               MOVE ANTC                   TO XO-120YYZ
               MOVE XO-120YYZ              TO LISTE-IO-AREA (40:15)
               MOVE '*'                    TO LISTE-IO-AREA (56:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '---------------- *'   TO LISTE-IO-AREA (39:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '**********'           TO LISTE-IO-AREA (47:10)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-U2 AND I-U3)
           AND (I-01 AND I-04 AND I-97)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DTODTO                 TO LISTE-IO-AREA (75:6)
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
           SET RESKMAS-LEVEL-INIT          TO TRUE
           INITIALIZE RESKMAS-DATA-FIELDS
           SET RESKMAS-EOF-OFF             TO TRUE
           SET RESKMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESKMAS-MC
                                              RESKMAS-MP
           OPEN INPUT RESKMAS
           SET DAGINN-LEVEL-INIT           TO TRUE
           INITIALIZE DAGINN-DATA-FIELDS
           SET DAGINN-EOF-OFF              TO TRUE
           SET DAGINN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO DAGINN-MC
                                              DAGINN-MP
           OPEN INPUT DAGINN
           INITIALIZE RENTEM-DATA-FIELDS
           SET RENTEM-EOF-OFF              TO TRUE
           SET RENTEM-PROCESS              TO TRUE
           OPEN INPUT RENTEM
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT NYRENTE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKMAS
           CLOSE DAGINN
           CLOSE RENTEM
           CLOSE FIRMAF
           CLOSE NYRENTE
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
