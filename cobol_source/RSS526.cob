       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSS526R.
      *    KONV. IFRA RES526 UTVIDET RECORD.     ***TXT***ok ss***    *
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.: RSS526                                             *
      *E 30.04.01: BLANKER IKKE SLETTEKODE NÅR DET LIGGER ""I"" I KODEN *
      *E 17.06.03: ""I"" BLE BLANKET KUN VED NMR.                       *
      *E 28.11.03: LAGT INN ""NY HEADING"" (RBS)                        *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSS526.rpg
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
           SELECT KUNDEIN
               ASSIGN TO UT-S-KUNDEIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KUNDEIN-STATUS.
           SELECT FAKTREC
               ASSIGN TO UT-S-FAKTREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTREC-STATUS.
           SELECT ORDFILE
               ASSIGN TO UT-S-ORDFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDFILE-STATUS.
           SELECT RESKOMA
               ASSIGN TO RESKOMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKOMA-STATUS
               RECORD KEY IS RESKOMA-KEY1.
           SELECT KONTKU
               ASSIGN TO UT-S-KONTKU
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KONTKU-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS SEQUENTIAL
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
       FD KUNDEIN
               BLOCK CONTAINS 8200
               RECORD CONTAINS 200.
       01  KUNDEIN-IO-AREA.
           05  KUNDEIN-IO-AREA-X           PICTURE X(200).
       FD FAKTREC
               BLOCK CONTAINS 8360
               RECORD CONTAINS 10.
       01  FAKTREC-IO-AREA.
           05  FAKTREC-IO-AREA-X           PICTURE X(10).
       FD ORDFILE
               BLOCK CONTAINS 8360
               RECORD CONTAINS 10.
       01  ORDFILE-IO-AREA.
           05  ORDFILE-IO-AREA-X           PICTURE X(10).
       FD RESKOMA
               RECORD CONTAINS 200.
       01  RESKOMA-IO-AREA.
           05  RESKOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  RESKOMA-KEY1.
                   15  RESKOMA-KEY1N       PICTURE S9(14).
               10  FILLER                  PICTURE X(185).
       FD KONTKU
               BLOCK CONTAINS 8360
               RECORD CONTAINS 10.
       01  KONTKU-IO-AREA.
           05  KONTKU-IO-AREA-X            PICTURE X(10).
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
               10  KUNDEMA-KEY1.
                   15  KUNDEMA-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(190).
      **************************************************************
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
           10  KUNDEIN-STATUS              PICTURE 99 VALUE 0.
           10  FAKTREC-STATUS              PICTURE 99 VALUE 0.
           10  ORDFILE-STATUS              PICTURE 99 VALUE 0.
           10  RESKOMA-STATUS              PICTURE 99 VALUE 0.
           10  KONTKU-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEIN-EOF-OFF         VALUE '0'.
               88  KUNDEIN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEIN-READ-OFF        VALUE '0'.
               88  KUNDEIN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEIN-PROCESS-OFF     VALUE '0'.
               88  KUNDEIN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KUNDEIN-LEVEL-INIT-OFF  VALUE '0'.
               88  KUNDEIN-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTREC-EOF-OFF         VALUE '0'.
               88  FAKTREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTREC-READ-OFF        VALUE '0'.
               88  FAKTREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTREC-PROCESS-OFF     VALUE '0'.
               88  FAKTREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFILE-EOF-OFF         VALUE '0'.
               88  ORDFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFILE-READ-OFF        VALUE '0'.
               88  ORDFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFILE-PROCESS-OFF     VALUE '0'.
               88  ORDFILE-PROCESS         VALUE '1'.
           05  RESKOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-EOF-OFF         VALUE '0'.
               88  RESKOMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-READ-OFF        VALUE '0'.
               88  RESKOMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-PROCESS-OFF     VALUE '0'.
               88  RESKOMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTKU-EOF-OFF          VALUE '0'.
               88  KONTKU-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTKU-READ-OFF         VALUE '0'.
               88  KONTKU-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTKU-PROCESS-OFF      VALUE '0'.
               88  KONTKU-PROCESS          VALUE '1'.
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
           05  KUNDEIN-LEVEL-01.
               10  KUNDEIN-01-L1.
                   15  KUNDEIN-01-L1-KFIRM PICTURE X(3).
           05  KUNDEIN-DATA-FIELDS.
               10  KFIRM                   PICTURE X(3).
               10  KKNR                    PICTURE X(6).
               10  KNAVN                   PICTURE X(30).
               10  KADR                    PICTURE X(30).
               10  KPADR                   PICTURE X(15).
               10  KPNR                    PICTURE X(4).
               10  KSLETT                  PICTURE X(1).
               10  KREC                    PICTURE X(200).
           05  KUNDEIN-MP                  PICTURE X(9).
           05  KUNDEIN-MC                  PICTURE X(9).
           05  KUNDEIN-M-01            REDEFINES KUNDEIN-MC.
               10  KUNDEIN-M-01-M2.
                   15  KUNDEIN-M-01-M2-KFIRM-G.
                       20  KUNDEIN-M-01-M2-KFIRM PICTURE X(3).
               10  KUNDEIN-M-01-M1.
                   15  KUNDEIN-M-01-M1-KKNR-G.
                       20  KUNDEIN-M-01-M1-KKNR PICTURE X(6).
           05  FAKTREC-DATA-FIELDS.
               10  FFIRM                   PICTURE X(3).
               10  FKNR                    PICTURE X(6).
           05  FAKTREC-MP                  PICTURE X(9).
           05  FAKTREC-MC                  PICTURE X(9).
           05  FAKTREC-M-02            REDEFINES FAKTREC-MC.
               10  FAKTREC-M-02-M2.
                   15  FAKTREC-M-02-M2-FFIRM-G.
                       20  FAKTREC-M-02-M2-FFIRM PICTURE X(3).
               10  FAKTREC-M-02-M1.
                   15  FAKTREC-M-02-M1-FKNR-G.
                       20  FAKTREC-M-02-M1-FKNR PICTURE X(6).
           05  ORDFILE-DATA-FIELDS.
               10  OFIRM                   PICTURE X(3).
               10  OKNR                    PICTURE X(6).
           05  ORDFILE-MP                  PICTURE X(9).
           05  ORDFILE-MC                  PICTURE X(9).
           05  ORDFILE-M-03            REDEFINES ORDFILE-MC.
               10  ORDFILE-M-03-M2.
                   15  ORDFILE-M-03-M2-OFIRM-G.
                       20  ORDFILE-M-03-M2-OFIRM PICTURE X(3).
               10  ORDFILE-M-03-M1.
                   15  ORDFILE-M-03-M1-OKNR-G.
                       20  ORDFILE-M-03-M1-OKNR PICTURE X(6).
           05  RESKOMA-DATA-FIELDS.
               10  RFIRM                   PICTURE X(3).
               10  RKNR                    PICTURE X(6).
           05  RESKOMA-MP                  PICTURE X(9).
           05  RESKOMA-MC                  PICTURE X(9).
           05  RESKOMA-M-04            REDEFINES RESKOMA-MC.
               10  RESKOMA-M-04-M2.
                   15  RESKOMA-M-04-M2-RFIRM-G.
                       20  RESKOMA-M-04-M2-RFIRM PICTURE X(3).
               10  RESKOMA-M-04-M1.
                   15  RESKOMA-M-04-M1-RKNR-G.
                       20  RESKOMA-M-04-M1-RKNR PICTURE X(6).
           05  KONTKU-DATA-FIELDS.
               10  UFIRM                   PICTURE X(3).
               10  UKNR                    PICTURE X(6).
           05  KONTKU-MP                   PICTURE X(9).
           05  KONTKU-MC                   PICTURE X(9).
           05  KONTKU-M-05             REDEFINES KONTKU-MC.
               10  KONTKU-M-05-M2.
                   15  KONTKU-M-05-M2-UFIRM-G.
                       20  KONTKU-M-05-M2-UFIRM PICTURE X(3).
               10  KONTKU-M-05-M1.
                   15  KONTKU-M-05-M1-UKNR-G.
                       20  KONTKU-M-05-M1-UKNR PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
      *                                       8  37 FINAVN
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  BLANK-X                 PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  SANT-IO.
                   15  SANT                PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YY9R               PICTURE ZZZ.ZZ9-.
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KUNDEIN-PROCESS
               SET KUNDEIN-PROCESS-OFF     TO TRUE
               SET KUNDEIN-READ            TO TRUE
           END-IF
 
           IF  KUNDEIN-READ
               PERFORM KUNDEIN-GET
               SET KUNDEIN-READ-OFF        TO TRUE
               IF  NOT KUNDEIN-EOF
                   PERFORM KUNDEIN-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKTREC-PROCESS
               SET FAKTREC-PROCESS-OFF     TO TRUE
               SET FAKTREC-READ            TO TRUE
           END-IF
 
           IF  FAKTREC-READ
               PERFORM FAKTREC-GET
               SET FAKTREC-READ-OFF        TO TRUE
               IF  NOT FAKTREC-EOF
                   PERFORM FAKTREC-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDFILE-PROCESS
               SET ORDFILE-PROCESS-OFF     TO TRUE
               SET ORDFILE-READ            TO TRUE
           END-IF
 
           IF  ORDFILE-READ
               PERFORM ORDFILE-GET
               SET ORDFILE-READ-OFF        TO TRUE
               IF  NOT ORDFILE-EOF
                   PERFORM ORDFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  RESKOMA-PROCESS
               SET RESKOMA-PROCESS-OFF     TO TRUE
               SET RESKOMA-READ            TO TRUE
           END-IF
 
           IF  RESKOMA-READ
               PERFORM RESKOMA-GET
               SET RESKOMA-READ-OFF        TO TRUE
               IF  NOT RESKOMA-EOF
                   PERFORM RESKOMA-MATCH-SET
               END-IF
           END-IF
 
           IF  KONTKU-PROCESS
               SET KONTKU-PROCESS-OFF      TO TRUE
               SET KONTKU-READ             TO TRUE
           END-IF
 
           IF  KONTKU-READ
               PERFORM KONTKU-GET
               SET KONTKU-READ-OFF         TO TRUE
               IF  NOT KONTKU-EOF
                   PERFORM KONTKU-MATCH-SET
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
 
           IF  KUNDEIN-PROCESS
               PERFORM KUNDEIN-IDSET
           END-IF
 
           IF  FAKTREC-PROCESS
               PERFORM FAKTREC-IDSET
           END-IF
 
           IF  ORDFILE-PROCESS
               PERFORM ORDFILE-IDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-IDSET
           END-IF
 
           IF  KONTKU-PROCESS
               PERFORM KONTKU-IDSET
           END-IF
 
           IF  KUNDEIN-PROCESS
               PERFORM KUNDEIN-CHK-LEVEL
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
 
           IF  KUNDEIN-PROCESS
               PERFORM KUNDEIN-FLDSET
           END-IF
 
           IF  FAKTREC-PROCESS
               PERFORM FAKTREC-FLDSET
           END-IF
 
           IF  ORDFILE-PROCESS
               PERFORM ORDFILE-FLDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-FLDSET
           END-IF
 
           IF  KONTKU-PROCESS
               PERFORM KONTKU-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KUNDEIN-PROCESS
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
               SET NOT-I-11                TO TRUE
               SET NOT-I-84                TO TRUE
               SET NOT-I-12                TO TRUE
           END-IF
           IF  (I-L1)
               MOVE KFIRM                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-90                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-90            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET I-85                    TO TRUE
               PERFORM FISLET-S
           END-IF
           IF  (I-01)
               MOVE ' '                    TO BLANK-X
           END-IF
           IF  (I-01 AND NOT-I-MR)
               SET NOT-I-11                TO TRUE
               IF  KSLETT = 'S'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-11)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-98)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-10)
               SET NOT-I-12                TO TRUE
               IF  KSLETT = 'I'
                   SET I-12                TO TRUE
               END-IF
               ADD 1                       TO ANT
           END-IF
           IF  (I-01 AND I-10)
               ADD 1                       TO SANT
           END-IF
           IF  (I-01 AND I-10 AND I-85)
               SET I-84                    TO TRUE
           END-IF
           IF  (I-01 AND I-84)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-01 AND I-84)
               SET NOT-I-85                TO TRUE
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'KUN02'                    TO LONR
           MOVE KFIRM                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RSS526  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           IF  (NOT-I-90)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
      ******************************************************
 
       KUNDEIN-GET SECTION.
       KUNDEIN-GET-P.
           IF  KUNDEIN-EOF-OFF
               READ KUNDEIN
               AT END
                   SET KUNDEIN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEIN-FLDSET SECTION.
       KUNDEIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEIN-IO-AREA (3:3)  TO KFIRM (1:3)
               MOVE KUNDEIN-IO-AREA (6:6)  TO KKNR (1:6)
               MOVE KUNDEIN-IO-AREA (16:30) TO KNAVN (1:30)
               MOVE KUNDEIN-IO-AREA (76:30) TO KADR (1:30)
               MOVE KUNDEIN-IO-AREA (106:15) TO KPADR (1:15)
               MOVE KUNDEIN-IO-AREA (121:4) TO KPNR (1:4)
               MOVE KUNDEIN-IO-AREA (189:1) TO KSLETT (1:1)
               MOVE KUNDEIN-IO-AREA (1:200) TO KREC (1:200)
           END-EVALUATE.
 
       KUNDEIN-IDSET SECTION.
       KUNDEIN-IDSET-P.
           SET I-01                        TO TRUE.
 
       KUNDEIN-CHK-LEVEL SECTION.
       KUNDEIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KUNDEIN-LEVEL-01
               MOVE KUNDEIN-IO-AREA (3:3)  TO KUNDEIN-01-L1-KFIRM
               IF  KUNDEIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KUNDEIN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KUNDEIN-01-L1         TO THE-PRIOR-L1
               SET KUNDEIN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEIN-MATCH-SET SECTION.
       KUNDEIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEIN-IO-AREA (3:3)  TO KUNDEIN-M-01-M2-KFIRM
               MOVE KUNDEIN-IO-AREA (6:6)  TO KUNDEIN-M-01-M1-KKNR
           END-EVALUATE.
 
       FAKTREC-GET SECTION.
       FAKTREC-GET-P.
           IF  FAKTREC-EOF-OFF
               READ FAKTREC
               AT END
                   SET FAKTREC-EOF         TO TRUE
               END-READ
           END-IF.
 
       FAKTREC-FLDSET SECTION.
       FAKTREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTREC-IO-AREA (2:3)  TO FFIRM (1:3)
               MOVE FAKTREC-IO-AREA (5:6)  TO FKNR (1:6)
           END-EVALUATE.
 
       FAKTREC-IDSET SECTION.
       FAKTREC-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKTREC-MATCH-SET SECTION.
       FAKTREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTREC-IO-AREA (2:3)  TO FAKTREC-M-02-M2-FFIRM
               MOVE FAKTREC-IO-AREA (5:6)  TO FAKTREC-M-02-M1-FKNR
           END-EVALUATE.
 
       ORDFILE-GET SECTION.
       ORDFILE-GET-P.
           IF  ORDFILE-EOF-OFF
               READ ORDFILE
               AT END
                   SET ORDFILE-EOF         TO TRUE
               END-READ
           END-IF.
 
       ORDFILE-FLDSET SECTION.
       ORDFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDFILE-IO-AREA (2:3)  TO OFIRM (1:3)
               MOVE ORDFILE-IO-AREA (5:6)  TO OKNR (1:6)
           END-EVALUATE.
 
       ORDFILE-IDSET SECTION.
       ORDFILE-IDSET-P.
           SET I-03                        TO TRUE.
 
       ORDFILE-MATCH-SET SECTION.
       ORDFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDFILE-IO-AREA (2:3)  TO ORDFILE-M-03-M2-OFIRM
               MOVE ORDFILE-IO-AREA (5:6)  TO ORDFILE-M-03-M1-OKNR
           END-EVALUATE.
 
       RESKOMA-GET SECTION.
       RESKOMA-GET-P.
           IF  RESKOMA-EOF-OFF
               READ RESKOMA
               AT END
                   SET RESKOMA-EOF         TO TRUE
               END-READ
           END-IF.
 
       RESKOMA-FLDSET SECTION.
       RESKOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (3:3)  TO RFIRM (1:3)
               MOVE RESKOMA-IO-AREA (6:6)  TO RKNR (1:6)
           END-EVALUATE.
 
       RESKOMA-IDSET SECTION.
       RESKOMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       RESKOMA-MATCH-SET SECTION.
       RESKOMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (3:3)  TO RESKOMA-M-04-M2-RFIRM
               MOVE RESKOMA-IO-AREA (6:6)  TO RESKOMA-M-04-M1-RKNR
           END-EVALUATE.
 
       KONTKU-GET SECTION.
       KONTKU-GET-P.
           IF  KONTKU-EOF-OFF
               READ KONTKU
               AT END
                   SET KONTKU-EOF          TO TRUE
               END-READ
           END-IF.
 
       KONTKU-FLDSET SECTION.
       KONTKU-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTKU-IO-AREA (2:3)   TO UFIRM (1:3)
               MOVE KONTKU-IO-AREA (5:6)   TO UKNR (1:6)
           END-EVALUATE.
 
       KONTKU-IDSET SECTION.
       KONTKU-IDSET-P.
           SET I-05                        TO TRUE.
 
       KONTKU-MATCH-SET SECTION.
       KONTKU-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTKU-IO-AREA (2:3)   TO KONTKU-M-05-M2-UFIRM
               MOVE KONTKU-IO-AREA (5:6)   TO KONTKU-M-05-M1-UKNR
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-07                        TO TRUE.
 
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
           IF  KUNDEIN-EOF
               MOVE HIGH-VALUES            TO KUNDEIN-MC
                                              KUNDEIN-MP
           END-IF
           IF  FAKTREC-EOF
               MOVE HIGH-VALUES            TO FAKTREC-MC
                                              FAKTREC-MP
           END-IF
           IF  ORDFILE-EOF
               MOVE HIGH-VALUES            TO ORDFILE-MC
                                              ORDFILE-MP
           END-IF
           IF  RESKOMA-EOF
               MOVE HIGH-VALUES            TO RESKOMA-MC
                                              RESKOMA-MP
           END-IF
           IF  KONTKU-EOF
               MOVE HIGH-VALUES            TO KONTKU-MC
                                              KONTKU-MP
           END-IF
           IF  KUNDEIN-MC < KUNDEIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FAKTREC-MC < FAKTREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDFILE-MC < ORDFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESKOMA-MC < RESKOMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KONTKU-MC < KONTKU-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KUNDEIN-MC < FAKTREC-MC
            AND  KUNDEIN-MC < ORDFILE-MC
            AND  KUNDEIN-MC < RESKOMA-MC
            AND  KUNDEIN-MC < KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEIN-PROCESS     TO TRUE
                   MOVE KUNDEIN-MC         TO KUNDEIN-MP
                   IF  KUNDEIN-MC = FAKTREC-MP
                     OR  KUNDEIN-MC = ORDFILE-MP
                     OR  KUNDEIN-MC = RESKOMA-MP
                     OR  KUNDEIN-MC = KONTKU-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKTREC-MC < KUNDEIN-MC
            AND  FAKTREC-MC < ORDFILE-MC
            AND  FAKTREC-MC < RESKOMA-MC
            AND  FAKTREC-MC < KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKTREC-PROCESS     TO TRUE
                   MOVE FAKTREC-MC         TO FAKTREC-MP
                   IF  FAKTREC-MC = KUNDEIN-MP
                     OR  FAKTREC-MC = ORDFILE-MP
                     OR  FAKTREC-MC = RESKOMA-MP
                     OR  FAKTREC-MC = KONTKU-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDFILE-MC < KUNDEIN-MC
            AND  ORDFILE-MC < FAKTREC-MC
            AND  ORDFILE-MC < RESKOMA-MC
            AND  ORDFILE-MC < KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDFILE-PROCESS     TO TRUE
                   MOVE ORDFILE-MC         TO ORDFILE-MP
                   IF  ORDFILE-MC = KUNDEIN-MP
                     OR  ORDFILE-MC = FAKTREC-MP
                     OR  ORDFILE-MC = RESKOMA-MP
                     OR  ORDFILE-MC = KONTKU-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESKOMA-MC < KUNDEIN-MC
            AND  RESKOMA-MC < FAKTREC-MC
            AND  RESKOMA-MC < ORDFILE-MC
            AND  RESKOMA-MC < KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKOMA-PROCESS     TO TRUE
                   MOVE RESKOMA-MC         TO RESKOMA-MP
                   IF  RESKOMA-MC = KUNDEIN-MP
                     OR  RESKOMA-MC = FAKTREC-MP
                     OR  RESKOMA-MC = ORDFILE-MP
                     OR  RESKOMA-MC = KONTKU-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KONTKU-MC < KUNDEIN-MC
            AND  KONTKU-MC < FAKTREC-MC
            AND  KONTKU-MC < ORDFILE-MC
            AND  KONTKU-MC < RESKOMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KONTKU-PROCESS      TO TRUE
                   MOVE KONTKU-MC          TO KONTKU-MP
                   IF  KONTKU-MC = KUNDEIN-MP
                     OR  KONTKU-MC = FAKTREC-MP
                     OR  KONTKU-MC = ORDFILE-MP
                     OR  KONTKU-MC = RESKOMA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEIN-MC = FAKTREC-MC
             OR  KUNDEIN-MC = ORDFILE-MC
             OR  KUNDEIN-MC = RESKOMA-MC
             OR  KUNDEIN-MC = KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEIN-PROCESS     TO TRUE
                   MOVE KUNDEIN-MC         TO KUNDEIN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  FAKTREC-MC = ORDFILE-MC
             OR  FAKTREC-MC = RESKOMA-MC
             OR  FAKTREC-MC = KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKTREC-PROCESS     TO TRUE
                   MOVE FAKTREC-MC         TO FAKTREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  ORDFILE-MC = RESKOMA-MC
             OR  ORDFILE-MC = KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDFILE-PROCESS     TO TRUE
                   MOVE ORDFILE-MC         TO ORDFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  RESKOMA-MC = KONTKU-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKOMA-PROCESS     TO TRUE
                   MOVE RESKOMA-MC         TO RESKOMA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO KUNDEMA-IO-AREA
               INITIALIZE KUNDEMA-IO-AREA
               MOVE KREC                   TO KUNDEMA-IO-AREA (1:200)
               IF  (NOT-I-12)
                   MOVE BLANK-X            TO KUNDEMA-IO-AREA (189:1)
               END-IF
               MOVE BLANK-X                TO KUNDEMA-IO-AREA (200:1)
               WRITE KUNDEMA-IO-AREA
           END-IF
           IF  (I-01 AND I-10 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KKNR                   TO LISTE-IO-AREA (20:6)
               MOVE KNAVN                  TO LISTE-IO-AREA (27:30)
               MOVE KADR                   TO LISTE-IO-AREA (59:30)
               MOVE KPNR                   TO LISTE-IO-AREA (91:4)
               MOVE KPADR                  TO LISTE-IO-AREA (96:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-01 AND I-84 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'SLETTEDE RECORDS FRA KUN' TO LISTE-IO-AREA (54:24)
               MOVE 'DEARKIVET'            TO LISTE-IO-AREA (78:9)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KNR'                  TO LISTE-IO-AREA (20:3)
               MOVE 'NAVN'                 TO LISTE-IO-AREA (27:4)
               MOVE 'ADRESSE'              TO LISTE-IO-AREA (59:7)
               MOVE 'POSTADRESSE'          TO LISTE-IO-AREA (91:11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-84 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'SLETTEDE RECORDS FRA KUN' TO LISTE-IO-AREA (54:24)
               MOVE 'DEARKIVET'            TO LISTE-IO-AREA (78:9)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KNR'                  TO LISTE-IO-AREA (20:3)
               MOVE 'NAVN'                 TO LISTE-IO-AREA (27:4)
               MOVE 'ADRESSE'              TO LISTE-IO-AREA (59:7)
               MOVE 'POSTADRESSE'          TO LISTE-IO-AREA (91:11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL REORGANISERTE REC' TO LISTE-IO-AREA (8:24)
               MOVE 'ORDS'                 TO LISTE-IO-AREA (32:4)
               MOVE ANT                    TO XO-60YY9R
               MOVE XO-60YY9R              TO LISTE-IO-AREA (37:8)
               MOVE 'ANTALL SLETTEDE RECORDS' TO LISTE-IO-AREA (49:23)
               MOVE SANT                   TO XO-60YY9R
               MOVE XO-60YY9R              TO LISTE-IO-AREA (73:8)
               MOVE 'DATO'                 TO LISTE-IO-AREA (85:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (90:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           SET KUNDEIN-LEVEL-INIT          TO TRUE
           INITIALIZE KUNDEIN-DATA-FIELDS
           SET KUNDEIN-EOF-OFF             TO TRUE
           SET KUNDEIN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEIN-MC
                                              KUNDEIN-MP
           OPEN INPUT KUNDEIN
           INITIALIZE FAKTREC-DATA-FIELDS
           SET FAKTREC-EOF-OFF             TO TRUE
           SET FAKTREC-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FAKTREC-MC
                                              FAKTREC-MP
           OPEN INPUT FAKTREC
           INITIALIZE ORDFILE-DATA-FIELDS
           SET ORDFILE-EOF-OFF             TO TRUE
           SET ORDFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO ORDFILE-MC
                                              ORDFILE-MP
           OPEN INPUT ORDFILE
           INITIALIZE RESKOMA-DATA-FIELDS
           SET RESKOMA-EOF-OFF             TO TRUE
           SET RESKOMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESKOMA-MC
                                              RESKOMA-MP
           OPEN INPUT RESKOMA
           INITIALIZE KONTKU-DATA-FIELDS
           SET KONTKU-EOF-OFF              TO TRUE
           SET KONTKU-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO KONTKU-MC
                                              KONTKU-MP
           OPEN INPUT KONTKU
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KUNDEIN
           CLOSE FAKTREC
           CLOSE ORDFILE
           CLOSE RESKOMA
           CLOSE KONTKU
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
