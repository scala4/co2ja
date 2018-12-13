       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG016R.
      **********************************************  Z-WIN-RPG2      *
      *********************************************************************
      *  MERGE  KOPI VAREMASTER - SAMT KOPIERTE REC                       *
      *  DANNE VAREARKIV-ONLINE, FILE-OPPSLAGSARKIV, OG FILE-SLETTEREC    *
      *                                                                   *
      *  DERSOM 01 OG IKKE MERGING MED 02 LEGGES 01 UT.                   *
      *  DERSOM 01 OG 02 MERGER DANNES OUTPUT PÅ 02.                      *
      *                                                                   *
      *  02 TESTES FOR OM FLERE RECORD"S HAR SAMME KEY                    *
      *  OPPDATERING AV MERKNAD KUN DERSOM J FOR OVERFØRING.              *
      * 26.04.01. ER KODE8=K, SKAL VAREREC MERKES MED K(KONSERNOPPDAT)    *
      * 04.09.02. AUTOOPPDAT = V = IKKE OPPDATERING AV BETEGNELSE         *
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG016.rpg
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
           SELECT VARE
               ASSIGN TO UT-S-VARE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARE-STATUS.
           SELECT KOPI
               ASSIGN TO UT-S-KOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPI-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT SLETTF
               ASSIGN TO UT-S-SLETTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SLETTF-STATUS.
           SELECT OPPSL
               ASSIGN TO UT-S-OPPSL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OPPSL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARE
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  VARE-IO-AREA.
           05  VARE-IO-AREA-X              PICTURE X(200).
       FD KOPI
               BLOCK CONTAINS 280
               RECORD CONTAINS 140.
       01  KOPI-IO-AREA-2.
           05  KOPI-IO-AREA-X              PICTURE X(140).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD SLETTF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  SLETTF-IO-AREA.
           05  SLETTF-IO-AREA-X            PICTURE X(200).
       FD OPPSL
               BLOCK CONTAINS 72
               RECORD CONTAINS 36.
       01  OPPSL-IO-AREA.
           05  OPPSL-IO-AREA-X             PICTURE X(36).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       77  ARO-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(1).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARO-TABLE.
               10  ARO-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARO-I
                                                      ARO-S.
                   15  ARO                 PICTURE X(1).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VARE-STATUS                 PICTURE 99 VALUE 0.
           10  KOPI-STATUS                 PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  SLETTF-STATUS               PICTURE 99 VALUE 0.
           10  OPPSL-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
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
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-EOF-OFF            VALUE '0'.
               88  KOPI-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-READ-OFF           VALUE '0'.
               88  KOPI-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-PROCESS-OFF        VALUE '0'.
               88  KOPI-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOPI-LEVEL-INIT-OFF     VALUE '0'.
               88  KOPI-LEVEL-INIT         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-AHEAD-EOF-OFF      VALUE '0'.
               88  KOPI-AHEAD-EOF          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPI-AHEAD-READ-OFF     VALUE '0'.
               88  KOPI-AHEAD-READ         VALUE '1'.
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
           05  VARE-LEVEL-01.
               10  VARE-01-L1.
                   15  VARE-01-L1-FIRMA    PICTURE X(3).
           05  VARE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  MERGE1                  PICTURE X(10).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ART01                   PICTURE X(20).
               10  ART5                    PICTURE X(15).
               10  ART10                   PICTURE X(10).
               10  ART15                   PICTURE X(5).
               10  BETEGN                  PICTURE X(30).
               10  PRISTY                  PICTURE X(1).
               10  AUTOPD                  PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  UTG                     PICTURE X(1).
               10  SLETT                   PICTURE X(1).
               10  REC1                    PICTURE X(200).
           05  VARE-MP                     PICTURE X(10).
           05  VARE-MC                     PICTURE X(10).
           05  VARE-M-01               REDEFINES VARE-MC.
               10  VARE-M-01-M2.
                   15  VARE-M-01-M2-FIRMA-G.
                       20  VARE-M-01-M2-FIRMA PICTURE X(3).
               10  VARE-M-01-M1.
                   15  VARE-M-01-M1-EDBNR-G.
                       20  VARE-M-01-M1-EDBNR PICTURE X(7).
           05  KOPI-LEVEL-02.
               10  KOPI-02-L1.
                   15  KOPI-02-L1-FIRMA    PICTURE X(3).
           05  KOPI-DATA-FIELDS.
               10  MERGE2                  PICTURE X(10).
               10  PT                      PICTURE X(1).
               10  MERKN                   PICTURE X(1).
               10  REC13                   PICTURE X(13).
               10  REC21                   PICTURE X(33).
               10  REC22                   PICTURE X(81).
               10  KOPBET                  PICTURE X(30).
               10  KOPSKP                  PICTURE X(9).
               10  KOPUTP                  PICTURE X(9).
               10  ENDRET-IO.
                   15  ENDRET              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PRISTT                  PICTURE X(1).
               10  PRISTI-IO.
                   15  PRISTI              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVPRI-IO.
                   15  LEVPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVRAB-IO.
                   15  LEVRAB              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VALG01                  PICTURE X(1).
               10  VALG02                  PICTURE X(1).
               10  VALG03                  PICTURE X(1).
               10  VALG04                  PICTURE X(1).
               10  VALG05                  PICTURE X(1).
               10  VALG06                  PICTURE X(1).
               10  VALG07                  PICTURE X(1).
               10  VALG08                  PICTURE X(1).
               10  VALG09                  PICTURE X(1).
               10  VALG10                  PICTURE X(1).
               10  VALG11                  PICTURE X(1).
               10  NXT02                   PICTURE X(10).
      *
           05  KOPI-MP                     PICTURE X(10).
           05  KOPI-MC                     PICTURE X(10).
           05  KOPI-M-02               REDEFINES KOPI-MC.
               10  KOPI-M-02-M2.
                   15  KOPI-M-02-M2-FIRMA-G.
                       20  KOPI-M-02-M2-FIRMA PICTURE X(3).
               10  KOPI-M-02-M1.
                   15  KOPI-M-02-M1-EDBNR-G.
                       20  KOPI-M-02-M1-EDBNR PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FELT92-IO.
                   15  FELT92              PICTURE S9(7)V9(2).
               10  FELT70-IO.
                   15  FELT70              PICTURE S9(7).
               10  FELT72-IO.
                   15  FELT72              PICTURE S9(5)V9(2).
               10  FELT50-IO.
                   15  FELT50              PICTURE S9(5).
               10  FELT60-IO.
                   15  FELT60              PICTURE S9(6).
               10  FELT31-IO.
                   15  FELT31              PICTURE S9(2)V9(1).
               10  A-ELGR                  PICTURE X(2).
               10  MND                     PICTURE X(2).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(7).
               10  MAXANT-IO.
                   15  MAXANT              PICTURE S9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  Z-IO.
                   15  Z                   PICTURE S9(2).
               10  ANR                     PICTURE X(1).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
           05  KOPI-IO-AREA.
               10  FILLER                  PICTURE X(140).
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
           05  MOVEA-COUNT                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE1                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA1                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE2                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA2                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-LENGTH                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-OFFSET                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-TEMP                  PICTURE X(256).
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
 
           IF  KOPI-PROCESS
               SET KOPI-PROCESS-OFF        TO TRUE
               SET KOPI-READ               TO TRUE
           END-IF
 
           IF  KOPI-READ
               PERFORM KOPI-GET
               SET KOPI-READ-OFF           TO TRUE
               IF  NOT KOPI-EOF
                   PERFORM KOPI-MATCH-SET
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
 
           IF  VARE-PROCESS
               PERFORM VARE-IDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-IDSET
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-CHK-LEVEL
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-CHK-LEVEL
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
 
           IF  VARE-PROCESS
               PERFORM VARE-FLDOFF
               PERFORM VARE-FLDSET
           END-IF
 
           IF  KOPI-PROCESS
               PERFORM KOPI-FLDOFF
               PERFORM KOPI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARE-PROCESS
           OR  KOPI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SUBTRACT ANTF               FROM ANTF
               MOVE 0                      TO FELT92
               MOVE 0                      TO FELT70
               MOVE 0                      TO FELT72
               MOVE 0                      TO FELT50
               MOVE 0                      TO FELT60
               MOVE 0                      TO FELT31
               MOVE UYEAR                  TO A-ELGR
               MOVE UMONTH                 TO MND
               SET NOT-I-71                TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           SET NOT-I-31                    TO TRUE
           SET NOT-I-70                    TO TRUE
           SET NOT-I-90                    TO TRUE
           SET NOT-I-91                    TO TRUE
           SET NOT-I-92                    TO TRUE
           SET NOT-I-93                    TO TRUE
           IF  (I-01)
               SET NOT-I-97                TO TRUE
      *
      * * * * * * * * * *   -VARE REC-    * * * * * * * * * * * * * * * * *
           END-IF
           IF  (I-01 AND NOT-I-85)
               SET I-90                    TO TRUE
      *      NESTE LINJE LAGT INN AV ELIN 10.03.95
           END-IF
           IF  (I-01)
               SET NOT-I-63                TO TRUE
               IF  AUTOPD = 'D'
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-90)
               GO TO DANNE-T
      *
           END-IF
           IF  (I-01)
               SET NOT-I-94                TO TRUE
               IF  UTG = '1'
                   SET I-94                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  UTG > '1'
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-30                TO TRUE
               IF  MERGE1 = NXT02
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-30 AND I-90)
               SET I-97                    TO TRUE
           END-IF
           IF  (I-01)
               SET NOT-I-58                TO TRUE
               IF  PRISTY = 'T'
                   SET I-58                TO TRUE
               END-IF
               SET NOT-I-59                TO TRUE
               IF  AUTOPD = 'X'
                   SET I-59                TO TRUE
               END-IF
               SET NOT-I-60                TO TRUE
               IF  AUTOPD = 'S'
                   SET I-60                TO TRUE
               END-IF
               SET NOT-I-61                TO TRUE
               IF  AUTOPD = 'U'
                   SET I-61                TO TRUE
               END-IF
      *      NESTE LINJE LAGT INN AV ELIN 01.03.95
           END-IF
           IF  (I-01)
               SET NOT-I-62                TO TRUE
               IF  AUTOPD = 'L'
                   SET I-62                TO TRUE
               END-IF
               SET NOT-I-57                TO TRUE
               IF  AUTOPD = 'V'
                   SET I-57                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-01 AND I-90)
               OR  (I-01 AND I-30)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               GO TO DANNE-T
      *
      * * * * * * * * * *   -KOPI REC-    * * * * * * * * * * * * * * * * *
      *
           END-IF
           IF  (I-02)
               SET NOT-I-44                TO TRUE
               IF  VALG03 = 'N'
                   SET I-44                TO TRUE
               END-IF
               SET NOT-I-45                TO TRUE
               IF  VALG04 = 'N'
                   SET I-45                TO TRUE
               END-IF
               SET NOT-I-46                TO TRUE
               IF  VALG05 = 'N'
                   SET I-46                TO TRUE
               END-IF
               SET NOT-I-48                TO TRUE
               IF  VALG07 = 'J'
                   SET I-48                TO TRUE
               END-IF
               SET NOT-I-49                TO TRUE
               IF  VALG08 = 'N'
                   SET I-49                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  VALG08 = 'K'
                   SET I-53                TO TRUE
               END-IF
               SET NOT-I-50                TO TRUE
               IF  VALG09 = 'J'
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-51                TO TRUE
               IF  VALG10 = 'J'
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  VALG11 = 'J'
                   SET I-52                TO TRUE
               END-IF
               SET NOT-I-91                TO TRUE
               IF  MERKN = 'F'
                   SET I-91                TO TRUE
               END-IF
               SET NOT-I-92                TO TRUE
               IF  MERKN = '1'
                   SET I-92                TO TRUE
               END-IF
               SET NOT-I-93                TO TRUE
               IF  MERKN = '2'
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-93)
               SET NOT-I-93                TO TRUE
               IF  MERKN = '4'
                   SET I-93                TO TRUE
               END-IF
      *
      *  TEST OM SAMME KEY PÅ FLERE REC FRA KOPI.
      *  SAMT PRINT AV FEIL-LISTE FRA KOPI.
      *
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  (I-02 AND I-MR AND I-97)
               SET I-90                    TO TRUE
           END-IF
           IF  (I-02 AND I-90)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-02)
               SET NOT-I-54                TO TRUE
               IF  MERGE2 = NXT02
                   SET I-54                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-54)
               ADD 1                       TO ANTF
           END-IF
           IF  (I-02 AND I-54 AND NOT-I-71)
               SET I-70                    TO TRUE
               SET I-71                    TO TRUE
           END-IF
           IF  (I-02 AND I-70)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-02 AND I-54)
               SET I-90                    TO TRUE
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-02)
               GO TO DANNE-T
      *
      * * * * * * *   DANNING AV OPPSLAGSNUMMER     * * * * * * * * * * * *
      *
           END-IF
           .
 
       DANNE-T.
           MOVE 20                         TO MAXANT
           IF  (I-07)
               MOVE 15                     TO MAXANT
           END-IF
           IF  (I-06)
               MOVE 10                     TO MAXANT
           END-IF
           IF  (I-05)
               MOVE 5                      TO MAXANT
           END-IF
           MOVE 1                          TO MOVEA-SA1 MOVEA-SA2
           MOVE 20                         TO MOVEA-SIZE1
           MULTIPLY ARA-MAX BY 1 GIVING MOVEA-SIZE2
           IF  MOVEA-SIZE1 > MOVEA-SIZE2
               MOVE MOVEA-SIZE2            TO MOVEA-SIZE1
           END-IF
           MOVE ARTNR
                    TO ARA-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           SUBTRACT X                      FROM X
           SUBTRACT Z                      FROM Z
      ****************************************************
      *     RUTINE FOR KONTROLL AV ARTIKKELNUMMER        *
      ****************************************************
           .
 
       RUTA-T.
           ADD 1                           TO X
           SET NOT-I-21                    TO TRUE
           IF  X > MAXANT
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO SLUTT-T
           END-IF
           MOVE ARA (X)                    TO ANR
           SET NOT-I-40                    TO TRUE
           IF  ANR = '.'
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '&'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '*'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ','
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '+'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '-'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '_'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '/'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ')'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '('
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '""""
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '='
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '%'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '@'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ' '
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               GO TO RUTA-T
      **********************************************************
      *      RUTINE FOR OPPBYGGING AV OPPSLAGSNUMMER           *
      **********************************************************
           END-IF
           ADD 1                           TO Z
           SET NOT-I-22                    TO TRUE
           IF  Z > MAXANT
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               GO TO SLUTT-T
           END-IF
           MOVE ANR                        TO ARO (Z)
           GO TO RUTA-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           .
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE '  '                       TO BBEST
           MOVE 'VAR29'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'VRG016  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       VARE-GET SECTION.
       VARE-GET-P.
           IF  VARE-EOF-OFF
               READ VARE
               AT END
                   SET VARE-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARE-FLDOFF SECTION.
       VARE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-05                TO TRUE
               SET NOT-I-06                TO TRUE
               SET NOT-I-07                TO TRUE
               SET NOT-I-85                TO TRUE
           END-EVALUATE.
 
       VARE-FLDSET SECTION.
       VARE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE VARE-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE VARE-IO-AREA (3:10)    TO MERGE1 (1:10)
               MOVE VARE-IO-AREA (13:3)    TO ALFA (1:3)
               MOVE VARE-IO-AREA (16:20)   TO ARTNR (1:20)
               MOVE VARE-IO-AREA (16:20)   TO ART01 (1:20)
               MOVE VARE-IO-AREA (21:15)   TO ART5 (1:15)
               IF  ART5 = SPACES
                   SET I-05                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (26:10)   TO ART10 (1:10)
               IF  ART10 = SPACES
                   SET I-06                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (31:5)    TO ART15 (1:5)
               IF  ART15 = SPACES
                   SET I-07                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (36:30)   TO BETEGN (1:30)
               MOVE VARE-IO-AREA (95:1)    TO PRISTY (1:1)
               MOVE VARE-IO-AREA (96:1)    TO AUTOPD (1:1)
               MOVE VARE-IO-AREA (118:5)   TO VGR (1:5)
               MOVE VARE-IO-AREA (127:1)   TO UTG (1:1)
               MOVE VARE-IO-AREA (128:1)   TO SLETT (1:1)
               IF  SLETT = SPACES
                   SET I-85                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (1:200)   TO REC1 (1:200)
           END-EVALUATE.
 
       VARE-IDSET SECTION.
       VARE-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARE-CHK-LEVEL SECTION.
       VARE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARE-LEVEL-01
               MOVE VARE-IO-AREA (3:3)     TO VARE-01-L1-FIRMA
               IF  VARE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARE-01-L1            TO THE-PRIOR-L1
               SET VARE-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       VARE-MATCH-SET SECTION.
       VARE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO VARE-M-01-M2-FIRMA
               MOVE VARE-IO-AREA (6:7)     TO VARE-M-01-M1-EDBNR
           END-EVALUATE.
 
       KOPI-GET SECTION.
       KOPI-GET-P.
           IF  KOPI-EOF-OFF
               IF  KOPI-AHEAD-EOF-OFF
                   IF  KOPI-AHEAD-READ-OFF
                       SET KOPI-AHEAD-READ TO TRUE
                       READ KOPI
                       AT END
                           SET KOPI-AHEAD-EOF TO TRUE
                           INITIALIZE KOPI-IO-AREA-2
                       END-READ
                   END-IF
                   MOVE KOPI-IO-AREA-2     TO KOPI-IO-AREA
                   IF  KOPI-AHEAD-EOF-OFF
                       READ KOPI
                       AT END
                           SET KOPI-AHEAD-EOF TO TRUE
                           INITIALIZE KOPI-IO-AREA-2
                       END-READ
                   ELSE
                       SET KOPI-EOF        TO TRUE
                       SUBTRACT 1        FROM LR-CHECK
                   END-IF
                   PERFORM KOPI-AHEAD-FLDSET
               ELSE
                   SET KOPI-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-IF
           END-IF.
 
       KOPI-FLDOFF SECTION.
       KOPI-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-05                TO TRUE
               SET NOT-I-06                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       KOPI-FLDSET SECTION.
       KOPI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPI-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE KOPI-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE KOPI-IO-AREA (3:10)    TO MERGE2 (1:10)
               MOVE KOPI-IO-AREA (13:3)    TO ALFA (1:3)
               MOVE KOPI-IO-AREA (16:20)   TO ARTNR (1:20)
               MOVE KOPI-IO-AREA (21:15)   TO ART5 (1:15)
               IF  ART5 = SPACES
                   SET I-05                TO TRUE
               END-IF
               MOVE KOPI-IO-AREA (26:10)   TO ART10 (1:10)
               IF  ART10 = SPACES
                   SET I-06                TO TRUE
               END-IF
               MOVE KOPI-IO-AREA (31:5)    TO ART15 (1:5)
               IF  ART15 = SPACES
                   SET I-07                TO TRUE
               END-IF
               MOVE KOPI-IO-AREA (36:30)   TO BETEGN (1:30)
               MOVE KOPI-IO-AREA (84:5)    TO VGR (1:5)
               MOVE KOPI-IO-AREA (89:1)    TO PT (1:1)
               MOVE KOPI-IO-AREA (90:1)    TO MERKN (1:1)
               MOVE KOPI-IO-AREA (3:13)    TO REC13 (1:13)
               MOVE KOPI-IO-AREA (3:33)    TO REC21 (1:33)
               MOVE KOPI-IO-AREA (3:81)    TO REC22 (1:81)
               MOVE KOPI-IO-AREA (36:30)   TO KOPBET (1:30)
               MOVE KOPI-IO-AREA (66:9)    TO KOPSKP (1:9)
               MOVE KOPI-IO-AREA (75:9)    TO KOPUTP (1:9)
               MOVE KOPI-IO-AREA (94:4)    TO ENDRET-IO
               MOVE KOPI-IO-AREA (98:1)    TO PRISTT (1:1)
               MOVE KOPI-IO-AREA (99:4)    TO PRISTI-IO
               MOVE KOPI-IO-AREA (121:5)   TO LEVPRI-IO
               MOVE KOPI-IO-AREA (126:2)   TO LEVRAB-IO
               MOVE KOPI-IO-AREA (103:1)   TO VALG01 (1:1)
               MOVE KOPI-IO-AREA (104:1)   TO VALG02 (1:1)
               MOVE KOPI-IO-AREA (105:1)   TO VALG03 (1:1)
               MOVE KOPI-IO-AREA (106:1)   TO VALG04 (1:1)
               MOVE KOPI-IO-AREA (107:1)   TO VALG05 (1:1)
               MOVE KOPI-IO-AREA (108:1)   TO VALG06 (1:1)
               MOVE KOPI-IO-AREA (109:1)   TO VALG07 (1:1)
               MOVE KOPI-IO-AREA (110:1)   TO VALG08 (1:1)
               MOVE KOPI-IO-AREA (111:1)   TO VALG09 (1:1)
               MOVE KOPI-IO-AREA (112:1)   TO VALG10 (1:1)
               MOVE KOPI-IO-AREA (113:1)   TO VALG11 (1:1)
           END-EVALUATE.
 
       KOPI-IDSET SECTION.
       KOPI-IDSET-P.
           SET I-02                        TO TRUE.
 
       KOPI-CHK-LEVEL SECTION.
       KOPI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KOPI-LEVEL-02
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-02-L1-FIRMA
               IF  KOPI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOPI-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOPI-02-L1            TO THE-PRIOR-L1
               SET KOPI-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       KOPI-MATCH-SET SECTION.
       KOPI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPI-IO-AREA (3:3)     TO KOPI-M-02-M2-FIRMA
               MOVE KOPI-IO-AREA (6:7)     TO KOPI-M-02-M1-EDBNR
           END-EVALUATE.
 
       KOPI-AHEAD-FLDSET SECTION.
       KOPI-AHEAD-FLDSET-P.
           MOVE KOPI-IO-AREA-2 (3:10)      TO NXT02 (1:10).
 
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
           IF  VARE-EOF
               MOVE HIGH-VALUES            TO VARE-MC
                                              VARE-MP
           END-IF
           IF  KOPI-EOF
               MOVE HIGH-VALUES            TO KOPI-MC
                                              KOPI-MP
           END-IF
           IF  VARE-MC < VARE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KOPI-MC < KOPI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VARE-MC < KOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARE-PROCESS        TO TRUE
                   MOVE VARE-MC            TO VARE-MP
                   IF  VARE-MC = KOPI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KOPI-MC < VARE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KOPI-PROCESS        TO TRUE
                   MOVE KOPI-MC            TO KOPI-MP
                   IF  KOPI-MC = VARE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARE-MC = KOPI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARE-PROCESS        TO TRUE
                   MOVE VARE-MC            TO VARE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-MR AND NOT-I-90)
               MOVE SPACES TO VAREMAS-IO-AREA
               INITIALIZE VAREMAS-IO-AREA
               MOVE REC1                   TO VAREMAS-IO-AREA (1:200)
               IF  (NOT-I-63)
                   MOVE ' '                TO VAREMAS-IO-AREA (96:1)
               END-IF
               WRITE VAREMAS-IO-AREA
           END-IF
           IF  (I-01 AND I-MR AND NOT-I-90)
           AND (I-59)
               MOVE SPACES TO VAREMAS-IO-AREA
               INITIALIZE VAREMAS-IO-AREA
               MOVE REC1                   TO VAREMAS-IO-AREA (1:200)
               WRITE VAREMAS-IO-AREA
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-90)
           AND (NOT-I-59)
               MOVE SPACES TO VAREMAS-IO-AREA
               INITIALIZE VAREMAS-IO-AREA
               MOVE REC1                   TO VAREMAS-IO-AREA (1:200)
      *                        REC13     15
               MOVE REC21                  TO VAREMAS-IO-AREA (3:33)
               IF  (NOT-I-57 AND NOT-I-44)
                   MOVE KOPBET             TO VAREMAS-IO-AREA (36:30)
               END-IF
               IF  (NOT-I-60 AND NOT-I-45)
                   MOVE KOPSKP             TO VAREMAS-IO-AREA (66:9)
               END-IF
               IF  (NOT-I-61 AND NOT-I-58 AND NOT-I-46)
                   MOVE KOPUTP             TO VAREMAS-IO-AREA (75:9)
               END-IF
               MOVE ENDRET                 TO XO-70P
               MOVE XO-70P-EF              TO VAREMAS-IO-AREA (84:4)
               IF  (NOT-I-58)
                   MOVE PT                 TO VAREMAS-IO-AREA (95:1)
               END-IF
               IF  (NOT-I-60 AND NOT-I-61 AND NOT-I-49)
                   MOVE 'A'                TO VAREMAS-IO-AREA (96:1)
               END-IF
               IF  (I-62 AND NOT-I-49)
                   MOVE 'L'                TO VAREMAS-IO-AREA (96:1)
               END-IF
               IF  (NOT-I-60 AND NOT-I-61 AND I-53)
                   MOVE 'K'                TO VAREMAS-IO-AREA (96:1)
               END-IF
               IF  (NOT-I-60 AND NOT-I-61 AND I-49)
                   MOVE ' '                TO VAREMAS-IO-AREA (96:1)
               END-IF
               IF  (I-48)
                   MOVE PRISTT             TO VAREMAS-IO-AREA (107:1)
               END-IF
               MOVE VGR                    TO VAREMAS-IO-AREA (118:5)
               IF  (I-50 AND NOT-I-75 AND I-91)
                   MOVE ' '                TO VAREMAS-IO-AREA (127:1)
               END-IF
               IF  (I-50 AND I-92)
                   MOVE MERKN              TO VAREMAS-IO-AREA (127:1)
               END-IF
               IF  (I-50 AND I-93 AND NOT-I-75)
                   MOVE MERKN              TO VAREMAS-IO-AREA (127:1)
               END-IF
               MOVE ' '                    TO VAREMAS-IO-AREA (128:1)
               IF  (NOT-I-62 AND I-52)
                   MOVE LEVRAB             TO XO-21P
                   MOVE XO-21P-EF          TO VAREMAS-IO-AREA (153:2)
               END-IF
               IF  (I-48)
                   MOVE PRISTI             TO XO-52P
                   MOVE XO-52P-EF          TO VAREMAS-IO-AREA (161:4)
               END-IF
               IF  (NOT-I-62 AND I-51)
                   MOVE LEVPRI             TO XO-72P
                   MOVE XO-72P-EF          TO VAREMAS-IO-AREA (165:5)
               END-IF
               WRITE VAREMAS-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR AND NOT-I-90)
               MOVE SPACES TO VAREMAS-IO-AREA
               INITIALIZE VAREMAS-IO-AREA
               MOVE '70'                   TO VAREMAS-IO-AREA (1:2)
               MOVE REC22                  TO VAREMAS-IO-AREA (3:81)
               MOVE ENDRET                 TO XO-70P
               MOVE XO-70P-EF              TO VAREMAS-IO-AREA (84:4)
               MOVE PT                     TO VAREMAS-IO-AREA (95:1)
               IF  (NOT-I-49)
                   MOVE 'A'                TO VAREMAS-IO-AREA (96:1)
               END-IF
               IF  (I-53)
                   MOVE 'K'                TO VAREMAS-IO-AREA (96:1)
               END-IF
               IF  (I-49)
                   MOVE ' '                TO VAREMAS-IO-AREA (96:1)
               END-IF
               MOVE FELT92                 TO XO-72P
               MOVE XO-72P-EF              TO VAREMAS-IO-AREA (97:5)
               MOVE FELT92                 TO XO-72P
               MOVE XO-72P-EF              TO VAREMAS-IO-AREA (102:5)
               IF  (I-48)
                   MOVE PRISTT             TO VAREMAS-IO-AREA (107:1)
               END-IF
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (108:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (111:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (114:3)
               MOVE VGR                    TO VAREMAS-IO-AREA (118:5)
               IF  (NOT-I-50)
                   MOVE ' '                TO VAREMAS-IO-AREA (127:1)
               END-IF
               IF  (I-50 AND I-91)
                   MOVE ' '                TO VAREMAS-IO-AREA (127:1)
               END-IF
               IF  (I-50 AND I-92)
                   MOVE MERKN              TO VAREMAS-IO-AREA (127:1)
               END-IF
               IF  (I-50 AND I-93)
                   MOVE MERKN              TO VAREMAS-IO-AREA (127:1)
               END-IF
               MOVE '0000000'              TO VAREMAS-IO-AREA (129:7)
               IF  (I-52)
                   MOVE LEVRAB             TO XO-21P
                   MOVE XO-21P-EF          TO VAREMAS-IO-AREA (153:2)
               END-IF
               IF  (NOT-I-52)
                   MOVE FELT31             TO XO-21P
                   MOVE XO-21P-EF          TO VAREMAS-IO-AREA (153:2)
               END-IF
               MOVE FELT60                 TO XO-60P
               MOVE XO-60P-EF              TO VAREMAS-IO-AREA (156:4)
               MOVE A-ELGR                 TO VAREMAS-IO-AREA (171:2)
               MOVE MND                    TO VAREMAS-IO-AREA (173:2)
               IF  (I-48)
                   MOVE PRISTI             TO XO-52P
                   MOVE XO-52P-EF          TO VAREMAS-IO-AREA (161:4)
               END-IF
               IF  (NOT-I-48)
                   MOVE FELT72             TO XO-52P
                   MOVE XO-52P-EF          TO VAREMAS-IO-AREA (161:4)
               END-IF
               IF  (I-51)
                   MOVE LEVPRI             TO XO-72P
                   MOVE XO-72P-EF          TO VAREMAS-IO-AREA (165:5)
               END-IF
               IF  (NOT-I-51)
                   MOVE FELT92             TO XO-72P
                   MOVE XO-72P-EF          TO VAREMAS-IO-AREA (165:5)
               END-IF
               MOVE FELT70                 TO XO-70P
               MOVE XO-70P-EF              TO VAREMAS-IO-AREA (175:4)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (179:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (182:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (185:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (188:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (191:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (194:3)
               MOVE FELT50                 TO XO-50P
               MOVE XO-50P-EF              TO VAREMAS-IO-AREA (197:3)
               WRITE VAREMAS-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-90)
           OR  (I-01 AND I-MR AND NOT-I-30)
           AND (I-90)
               MOVE SPACES TO SLETTF-IO-AREA
               INITIALIZE SLETTF-IO-AREA
               MOVE REC1                   TO SLETTF-IO-AREA (1:200)
               WRITE SLETTF-IO-AREA
           END-IF
           IF  (I-02 AND I-MR AND I-90)
               MOVE SPACES TO SLETTF-IO-AREA
               INITIALIZE SLETTF-IO-AREA
               MOVE REC1                   TO SLETTF-IO-AREA (1:200)
               MOVE REC22                  TO SLETTF-IO-AREA (3:81)
               WRITE SLETTF-IO-AREA
           END-IF
           IF  (I-02 AND I-MR AND I-92)
           AND (NOT-I-90 AND NOT-I-94)
               MOVE SPACES TO SLETTF-IO-AREA
               INITIALIZE SLETTF-IO-AREA
               MOVE REC1                   TO SLETTF-IO-AREA (1:200)
               MOVE REC22                  TO SLETTF-IO-AREA (3:81)
               MOVE 'UU'                   TO SLETTF-IO-AREA (127:2)
               WRITE SLETTF-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-90)
           OR  (I-01 AND I-MR AND I-31)
           OR  (I-02 AND NOT-I-90)
               MOVE SPACES TO OPPSL-IO-AREA
               INITIALIZE OPPSL-IO-AREA
               MOVE '1'                    TO OPPSL-IO-AREA (1:1)
               MOVE FIRMA                  TO OPPSL-IO-AREA (2:3)
               MOVE ALFA                   TO OPPSL-IO-AREA (5:3)
               MOVE ' '                    TO OPPSL-IO-AREA (8:1)
               MOVE 29                     TO BW-A
               PERFORM VARYING ARO-I FROM ARO-MAX BY -1
                         UNTIL ARO-I < 1
                   SUBTRACT 1            FROM BW-A
                   MOVE ARO-ENTRY (ARO-I)  TO OPPSL-IO-AREA (BW-A:1)
                   INITIALIZE ARO-ENTRY (ARO-I)
               END-PERFORM
               MOVE EDBNR                  TO OPPSL-IO-AREA (29:7)
               WRITE OPPSL-IO-AREA
           END-IF
           IF  (I-02 AND I-54 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE EDBNR                  TO LISTE-IO-AREA (1:7)
               MOVE VGR                    TO LISTE-IO-AREA (9:5)
               MOVE ALFA                   TO LISTE-IO-AREA (15:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (19:20)
               MOVE BETEGN                 TO LISTE-IO-AREA (40:30)
               IF  (I-54)
                   MOVE 'TO MED SAMME EDBNR FRA K' TO LISTE-IO-AREA
                                                              (105:24)
               END-IF
               IF  (I-54)
                   MOVE 'OPI.'             TO LISTE-IO-AREA (129:4)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-02 AND I-70 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'DATO'                 TO LISTE-IO-AREA (94:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (99:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' EDBNR   VGR  ALF ARTIKK' TO LISTE-IO-AREA (1:24)
               MOVE 'ELNUMMER       VAREBETEG' TO LISTE-IO-AREA (25:24)
               MOVE 'NELSE                   ' TO LISTE-IO-AREA (49:24)
               MOVE '                        ' TO LISTE-IO-AREA (73:24)
               MOVE '        MERKNAD         ' TO LISTE-IO-AREA (97:24)
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
           IF  (I-L1 AND I-71 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTF                   TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (17:9)
               INITIALIZE ANTF
               MOVE 'FEIL.'                TO LISTE-IO-AREA (27:5)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           SET VARE-LEVEL-INIT             TO TRUE
           INITIALIZE VARE-DATA-FIELDS
           SET VARE-EOF-OFF                TO TRUE
           SET VARE-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO VARE-MC
                                              VARE-MP
           OPEN INPUT VARE
           SET KOPI-LEVEL-INIT             TO TRUE
           SET KOPI-AHEAD-EOF-OFF          TO TRUE
           SET KOPI-AHEAD-READ-OFF         TO TRUE
           INITIALIZE KOPI-DATA-FIELDS
           SET KOPI-EOF-OFF                TO TRUE
           SET KOPI-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO KOPI-MC
                                              KOPI-MP
           OPEN INPUT KOPI
           OPEN OUTPUT VAREMAS
           OPEN OUTPUT SLETTF
           OPEN OUTPUT OPPSL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               INITIALIZE ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARE
           CLOSE KOPI
           CLOSE VAREMAS
           CLOSE SLETTF
           CLOSE OPPSL
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
