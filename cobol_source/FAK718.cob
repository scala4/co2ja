       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK718R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: FAK718    PROGRAMERT AV: ESPEN LARSEN               *
      *  DANNER FAKTURA.EDI.DATA TIL JERNIA (FRA FOMA).               *
      *  PROGRAMMERT: 04.04.2001                                      *
      *  ENDRET.....: 24.10.2001                                      *
      * 15.10.2001 VARENAVN LIGGER NÅ  I  INNFIL. VAREMAS FJERNET.    *
      * 15.10.2001 KUNDEREF UTVIDET TIL 15 KARAKTERER I INNFIL.       *
      * 24.10.2001 RETTET FEIL TEST PÅ NEGATIVT BELØP. ER NEGATIVT.   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK718.rpg
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
           SELECT FNRTAB
               ASSIGN TO UT-S-FNRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FNRTAB-STATUS.
           SELECT KNRTAB
               ASSIGN TO UT-S-KNRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KNRTAB-STATUS.
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
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
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT ORDNRM
               ASSIGN TO ORDNRM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDNRM-STATUS
               RECORD KEY IS ORDNRM-KEY1.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT EDIFIL
               ASSIGN TO EDIFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDIFIL-STATUS.
           SELECT EDIFIL2
               ASSIGN TO UT-S-EDIFIL2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDIFIL2-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FNRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FNRTAB-IO-AREA.
           05  FNRTAB-IO-AREA-X            PICTURE X(80).
       FD KNRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KNRTAB-IO-AREA.
           05  KNRTAB-IO-AREA-X            PICTURE X(80).
       FD INNFIL
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(150).
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
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD ORDNRM
               RECORD CONTAINS 100.
       01  ORDNRM-IO-AREA.
           05  ORDNRM-IO-AREA-X.
               10  ORDNRM-KEY1             PICTURE X(9).
               10  FILLER                  PICTURE X(91).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD EDIFIL
               RECORD CONTAINS 200.
       01  EDIFIL-IO-AREA.
           05  EDIFIL-IO-AREA-X            PICTURE X(200).
       FD EDIFIL2
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  EDIFIL2-IO-AREA.
           05  EDIFIL2-IO-AREA-X           PICTURE X(80).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABFNR-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       77  TABEAN-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       77  TABKNR-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFNR-TABLE.
               10  TABFNR-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY TABFNR-I
                                                      TABFNR-S
                                                      TABEAN-I
                                                      TABEAN-S.
                   15  TABFNR              PICTURE X(3).
                   15  TABEAN              PICTURE X(13).
           05  TABKNR-TABLE.
               10  TABKNR-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY TABKNR-I
                                                      TABKNR-S.
                   15  TABKNR              PICTURE X(9).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FNRTAB-STATUS               PICTURE 99 VALUE 0.
           10  KNRTAB-STATUS               PICTURE 99 VALUE 0.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  ORDNRM-STATUS               PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  EDIFIL-STATUS               PICTURE 99 VALUE 0.
           10  EDIFIL2-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  BMFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRTAB-EOF-OFF          VALUE '0'.
               88  FNRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KNRTAB-EOF-OFF          VALUE '0'.
               88  KNRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  INNFIL-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  ORDNRM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  BMFELT-XX-DATA-FIELDS.
               10  BETMAT                  PICTURE X(2).
               10  FILLER                  PICTURE X(24).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  BMTKST                  PICTURE X(24).
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L3.
                   15  INNFIL-01-L3-FIRMA  PICTURE X(3).
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-RESKNR PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-FAKTNR PICTURE X(6).
           05  INNFIL-LEVEL-02.
               10  INNFIL-02-L3.
                   15  INNFIL-02-L3-FIRMA  PICTURE X(3).
               10  INNFIL-02-L2.
                   15  INNFIL-02-L2-RESKNR PICTURE X(6).
               10  INNFIL-02-L1.
                   15  INNFIL-02-L1-FAKTNR PICTURE X(6).
           05  INNFIL-DATA-FIELDS.
               10  FNRRNR                  PICTURE X(9).
               10  FIRMA                   PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  FAKTNR                  PICTURE X(6).
               10  FAKTYP                  PICTURE X(2).
      *                                      21  21 ARTH
               10  FAKDTO                  PICTURE X(6).
               10  FFADTO                  PICTURE X(6).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(7)V9(2).
      *                                      78  84 FAKKID
               10  BETM                    PICTURE X(2).
               10  MVAFRI                  PICTURE X(1).
      *                                      21  21 ARTV
               10  ONR                     PICTURE X(6).
      *                                      28  47 ARTNR
               10  ARTN15                  PICTURE X(15).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  NTOBEL-IO.
                   15  NTOBEL              PICTURE S9(7)V9(2).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
      *                                      77  79 ALFKOD
               10  EDBNR                   PICTURE X(7).
               10  EDB2F                   PICTURE X(3).
               10  EDB3F                   PICTURE X(4).
               10  VARBET                  PICTURE X(30).
               10  KUREF                   PICTURE X(15).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FIADR                   PICTURE X(30).
               10  FIPNR                   PICTURE X(4).
               10  FIPOST                  PICTURE X(19).
           05  KUNDEMA-DATA-FIELDS.
               10  KMNAVN                  PICTURE X(30).
               10  KMADR1                  PICTURE X(30).
               10  KMADR2                  PICTURE X(30).
               10  KMSTED                  PICTURE X(15).
               10  KMPNR                   PICTURE X(4).
           05  KUNDEMX-DATA-FIELDS.
               10  KXEANL                  PICTURE X(13).
               10  KXEA4F                  PICTURE X(4).
           05  ORDNRM-DATA-FIELDS.
               10  ORDKNR                  PICTURE X(6).
           05  VARETIL-DATA-FIELDS.
               10  VTENH                   PICTURE X(3).
               10  VTEAN                   PICTURE X(13).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  VLNR-IO.
                   15  VLNR                PICTURE S9(3).
               10  ANTKRN-IO.
                   15  ANTKRN              PICTURE S9(5).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(5).
               10  FAKTOT-IO.
                   15  FAKTOT              PICTURE S9(9)V9(2).
               10  ONKEY                   PICTURE X(9).
               10  KMKEY                   PICTURE X(9).
               10  KXKEY                   PICTURE X(10).
               10  ANTEAF-IO.
                   15  ANTEAF              PICTURE S9(5).
               10  VMKEY                   PICTURE X(10).
               10  VTKEY                   PICTURE X(12).
               10  LEVANT-IO.
                   15  LEVANT              PICTURE S9(8)V9(2).
               10  BELNTO-IO.
                   15  BELNTO              PICTURE S9(8)V9(2).
               10  NTOXMV-IO.
                   15  NTOXMV              PICTURE S9(9)V9(2).
               10  RAB152-IO.
                   15  RAB152              PICTURE S9(3)V9(2).
               10  RAB252-IO.
                   15  RAB252              PICTURE S9(3)V9(2).
               10  TOTRPR-IO.
                   15  TOTRPR              PICTURE S9(3)V9(2).
               10  FELT74-IO.
                   15  FELT74              PICTURE S9(3)V9(4).
               10  FELT52-IO.
                   15  FELT52              PICTURE S9(3)V9(3).
               10  BELSTK-IO.
                   15  BELSTK              PICTURE S9(8)V9(2).
               10  FLT52-IO.
                   15  FLT52               PICTURE S9(3)V9(2).
               10  FAKTUP-IO.
                   15  FAKTUP              PICTURE S9(1)V9(6).
               10  BRTO-IO.
                   15  BRTO                PICTURE S9(8)V9(2).
               10  MVABEL-IO.
                   15  MVABEL              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  EDIT-LEVANT             PICTURE Z9999999,99.
               10  EDIT-BELSTK             PICTURE Z9999999,99.
               10  EDIT-RAB152             PICTURE Z99,99.
               10  EDIT-RAB252             PICTURE Z99,99.
               10  EDIT-BELNTO             PICTURE Z9999999,99.
               10  EDIT-NTOXMV             PICTURE Z99999999,99.
               10  EDIT-MVABEL             PICTURE Z9999999,99.
               10  EDIT-FAKTOT             PICTURE Z99999999,99.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   PERFORM INNFIL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET INNFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-CHK-LEVEL
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           IF  (I-L1)
               SET NOT-I-25                TO TRUE
               SET I-51                    TO TRUE
               SUBTRACT VLNR               FROM VLNR
               SUBTRACT NTOXMV             FROM NTOXMV
      *****************************************************************
      * HOVEDRUTINE: SELEKTERE DATA SOM SKAL OVERFØRES.               *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-19                TO TRUE
               SET TABFNR-S                TO TABFNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFNR-I FROM 1 BY 1
                         UNTIL TABFNR-I >= TABFNR-MAX
                            OR I-19
                   IF  FIRMA = TABFNR (TABFNR-I)
                       SET I-19            TO TRUE
                       SET TABFNR-S        TO TABFNR-I
                   END-IF
               END-PERFORM
               SET TABFNR-I                TO TABFNR-S
               IF  I-19
               AND TABFNR-I NOT > TABEAN-MAX
                   SET TABEAN-I            TO TABFNR-I
               END-IF
           END-IF
           IF  (NOT-I-19)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-80                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-80            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L2)
               SET NOT-I-20                TO TRUE
               SET TABKNR-S                TO TABKNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABKNR-I FROM 1 BY 1
                         UNTIL TABKNR-I >= TABKNR-MAX
                            OR I-20
                   IF  FNRRNR = TABKNR (TABKNR-I)
                       SET I-20            TO TRUE
                       SET TABKNR-S        TO TABKNR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (NOT-I-20)
               GO TO SLUTT-T
      *****************************************************************
      * OPPSLAG MOT KUNDE.MASTER FOR KUNDE SOM BETALER.               *
      *****************************************************************
      *  L2      FNRRNR    CHAINKUNDEMA              81
      *  L2                MOVE KMNAVN    KBNAVN 30         BET.NAVN
      *  L2                MOVE KMADR1    KBADR1 30         BET.ADR1.
      *  L2                MOVE KMADR2    KBADR2 30         BET.ADR2.
      *  L2                MOVE KMSTED    KBSTED 15         BET.P.STED
      *  L2                MOVE KMPNR     KBPNR   4         BET.P.NR.
      *****************************************************************
      * OPPSLAG MOT KUNDE.XTRA   FOR KUNDE SOM BETALER.               *
      *****************************************************************
      *  L2                MOVELFNRRNR    KXKEY  10         FNR/KNR.
      *  L2                MOVE "1"       KXKEY             REC.TYPE 1
      *  L2      KXKEY     CHAINKUNDEMX              82
      *  L2                MOVE KXEANL    KBEANL 13         BET.EAN.LOC.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBRUTINE FOR HENTING AV BETALINGSMÅTE-TEKST     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           IF  (I-L1)
               MOVE BETM                   TO BETMAT
               CALL 'BETBETN' USING BMFELT-XX-DATA-FIELDS
           END-IF
           IF  (I-L1)
               SET NOT-I-97                TO TRUE
               IF  BETMAT = '00'
                   SET I-97                TO TRUE
               END-IF
      *****************************************************************
      *          FAKTYP    COMP "22"                     22 RENTE/GEBYR
      *  22                GOTO SLUTT                       OVERF. IKKE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  FAKTNR NOT < '900000'
               SET I-26                    TO TRUE
           END-IF
           IF  (I-L1 AND I-26)
               ADD 1                       TO ANTKRN
           END-IF
           IF  (I-L1 AND NOT-I-26)
               ADD 1                       TO ANTFAK
           END-IF
           IF  (I-01)
               ADD FAKBEL TO ZERO      GIVING FAKTOT
               SET NOT-I-24                TO TRUE
               IF  MVAFRI = 'F'
                   SET I-24                TO TRUE
               END-IF
               GO TO SLUTT-T
      *****************************************************************
      * SJEKK OM DET ER FØRSTE FAKTURA VARELINJE.                     *
      *****************************************************************
           END-IF
           IF  (I-02 AND I-51)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-02)
               SET NOT-I-51                TO TRUE
               SET NOT-I-96                TO TRUE
      *  02      ONR       COMP "458829"             37
      *  02      ONR       COMP "465143"             37
      *  02      ONR       COMP "468020"             37
      *  02      ONR       COMP "468022"             37
      *  02      ONR       COMP "472569"             37
      *  02      ONR       COMP "473196"             37
      *  02      ONR       COMP "473197"             37
      *  02      ONR       COMP "473226"             37
      *  02      ONR       COMP "473230"             37
      *  02      ONR       COMP "475154"             37
      *  02      ONR       COMP "480301"             37
      *  02      ONR       COMP "458835"             38
      *  02      ONR       COMP "461602"             38
      *  02      ONR       COMP "461613"             38
      *  02      ONR       COMP "465159"             38
      *  02      ONR       COMP "465876"             38
      *  02      ONR       COMP "465913"             38
      *  02      ONR       COMP "466771"             38
      *  02      ONR       COMP "466800"             38
      *  02      ONR       COMP "469525"             38
      *  02      ONR       COMP "469531"             38
      *  02      ONR       COMP "470870"             38
      *  02      ONR       COMP "475997"             38
      *  02      ONR       COMP "478339"             38
      *****************************************************************
      * OPPSLAG MOT ORDRENR.MASTER FOR Å FINNE KUNDE SOM HAR FÅTT VARER
      *****************************************************************
           END-IF
           IF  (I-02 AND NOT-I-50)
               GO TO VLRUT2-T
           END-IF
           IF  (I-02)
               SET NOT-I-90                TO TRUE
               SET NOT-I-91                TO TRUE
      *  02                SETOF                     84
           END-IF
           IF  (I-02)
               MOVE FIRMA                  TO ONKEY (1:3)
               MOVE ONR                    TO ONKEY (4:6)
               MOVE ONKEY                  TO ORDNRM-KEY1
               READ ORDNRM RECORD KEY IS ORDNRM-KEY1
               INVALID KEY
                   SET I-83                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-83            TO TRUE
                   PERFORM ORDNRM-FLDSET
                   PERFORM ORDNRM-IDSET
               END-READ
           END-IF
           IF  (I-02 AND I-83)
               GO TO VLRUT2-T
      *  02      RESKNR    COMP ORDKNR                   84 SAMME KUNDE.
      *****************************************************************
      * OPPSLAG MOT KUNDE.MASTER FOR KUNDE SOM HAR FÅTT VARENE.       *
      *****************************************************************
           END-IF
           IF  (I-02)
               MOVE FIRMA                  TO KMKEY (1:3)
               MOVE ORDKNR                 TO KMKEY (4:6)
               MOVE KMKEY                  TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-85                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-85            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-02 AND NOT-I-85)
               SET I-90                    TO TRUE
      *****************************************************************
      * OPPSLAG MOT KUNDE.XTRA   FOR KUNDE SOM HAR FÅTT VARENE.       *
      *****************************************************************
           END-IF
           IF  (I-02)
               MOVE KMKEY                  TO KXKEY (1:9)
               MOVE '1'                    TO KXKEY (10:1)
               MOVE KXKEY                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-86                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-86            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-02 AND NOT-I-86)
               SET NOT-I-91                TO TRUE
               IF  KXEA4F = '7080'
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-86)
               SET I-92                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-86 AND NOT-I-91)
               SET I-92                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-86 AND I-91)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-25)
               ADD 1                       TO ANTEAF
      *****************************************************************
      * VARELINJE RUTINE.                                             *
      *****************************************************************
           END-IF
           .
 
       VLRUT2-T.
           IF  (I-02 AND NOT-I-25)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               ADD 1                       TO VLNR
      *****************************************************************
      * OPPSLAG MOT VARE.TILEGG FOR Å HENT VARENE"S EAN-NR.           *
      *****************************************************************
           END-IF
           IF  (I-02)
               MOVE FIRMA                  TO VMKEY (1:3)
               MOVE EDBNR                  TO VMKEY (4:7)
               MOVE '80'                   TO VTKEY (1:2)
               MOVE VMKEY                  TO VTKEY (3:10)
               MOVE VTKEY                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-88                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-88            TO TRUE
                   PERFORM VARETIL-FLDSET
                   PERFORM VARETIL-IDSET
               END-READ
      *****************************************************************
      * RUTINE FOR Å REDIGERE UTDATA.                                 *
      *****************************************************************
           END-IF
           IF  (I-02)
               ADD ANTLEV TO ZERO      GIVING LEVANT
               ADD NTOBEL TO ZERO      GIVING BELNTO
               SET NOT-I-94                TO TRUE
               IF  EDB2F = '94'
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-94)
               SET NOT-I-94                TO TRUE
               IF  EDB3F = '995'
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-94)
               SET I-96                    TO TRUE
      *  02N96   NTOXMV    ADD  BELNTO    NTOXMV 112        MVA.GRUNNLAG
           END-IF
           IF  (I-02)
               ADD BELNTO                  TO NTOXMV
      *  02 96   NTOXMV    SUB  BELNTO    NTOXMV            MVA.GRUNNLAG
      *****************************************************************
      * NY RUTINE FOR Å BEREGNE NETTO RABATTPROSENT. (RAB1+RAB2+RAB3) *
      *****************************************************************
           END-IF
           IF  (I-02)
               ADD RAB1 TO ZERO        GIVING RAB152
               ADD RAB2 TO ZERO        GIVING RAB252
               SUBTRACT RAB1 FROM 100  GIVING TOTRPR
      *
           END-IF
           IF  (I-02)
               DIVIDE RAB2 BY 100      GIVING FELT74 ROUNDED
               MULTIPLY FELT74 BY TOTRPR GIVING FELT52 ROUNDED
               SUBTRACT FELT52             FROM TOTRPR ROUNDED
      *
           END-IF
           IF  (I-02)
               DIVIDE RAB3 BY 100      GIVING FELT74 ROUNDED
               MULTIPLY FELT74 BY TOTRPR GIVING FELT52 ROUNDED
               SUBTRACT FELT52             FROM TOTRPR ROUNDED
      *
           END-IF
           IF  (I-02)
               SUBTRACT TOTRPR FROM 100 GIVING TOTRPR ROUNDED
               SET NOT-I-61                TO TRUE
               IF  TOTRPR = 0
                   SET I-61                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR OG BEREGNE BRUTTO SALGSPRIS PR. STK.               *
      *****************************************************************
           END-IF
           IF  (I-02 AND I-61)
               DIVIDE BELNTO BY LEVANT GIVING BELSTK ROUNDED
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SUBTRACT TOTRPR FROM 100 GIVING FLT52
               DIVIDE 100 BY FLT52     GIVING FAKTUP
               MULTIPLY FAKTUP BY BELNTO GIVING BRTO ROUNDED
               DIVIDE BRTO BY LEVANT   GIVING BELSTK ROUNDED
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-25)
               SET NOT-I-27                TO TRUE
               IF  NTOXMV < 0,00
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-25 AND I-26)
               DIVIDE FAKTOT BY -1     GIVING FAKTOT
           END-IF
           IF  (I-L1 AND I-25)
               SUBTRACT NTOXMV FROM FAKTOT GIVING MVABEL
           END-IF
           IF  (I-L1 AND I-25 AND I-24)
               MOVE 0,00                   TO MVABEL
           END-IF.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INNFIL-IO-AREA (21:1) = 'H' )
               MOVE INNFIL-IO-AREA (1:9)   TO FNRRNR (1:9)
               MOVE INNFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO RESKNR (1:6)
               MOVE INNFIL-IO-AREA (10:6)  TO FAKTNR (1:6)
               MOVE INNFIL-IO-AREA (16:2)  TO FAKTYP (1:2)
               MOVE INNFIL-IO-AREA (37:6)  TO FAKDTO (1:6)
               MOVE INNFIL-IO-AREA (43:6)  TO FFADTO (1:6)
               MOVE INNFIL-IO-AREA (51:9)  TO FAKBEL-IO
               INSPECT FAKBEL-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (85:2)  TO BETM (1:2)
               MOVE INNFIL-IO-AREA (100:1) TO MVAFRI (1:1)
           WHEN ( INNFIL-IO-AREA (21:1) = 'V' )
               MOVE INNFIL-IO-AREA (1:9)   TO FNRRNR (1:9)
               MOVE INNFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO RESKNR (1:6)
               MOVE INNFIL-IO-AREA (10:6)  TO FAKTNR (1:6)
               MOVE INNFIL-IO-AREA (16:2)  TO FAKTYP (1:2)
               MOVE INNFIL-IO-AREA (22:6)  TO ONR (1:6)
               MOVE INNFIL-IO-AREA (28:15) TO ARTN15 (1:15)
               MOVE INNFIL-IO-AREA (51:7)  TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (59:9)  TO NTOBEL-IO
               INSPECT NTOBEL-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (68:3)  TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (71:3)  TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (74:3)  TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (80:7)  TO EDBNR (1:7)
               MOVE INNFIL-IO-AREA (80:3)  TO EDB2F (1:3)
               MOVE INNFIL-IO-AREA (80:4)  TO EDB3F (1:4)
               MOVE INNFIL-IO-AREA (101:30) TO VARBET (1:30)
               MOVE INNFIL-IO-AREA (131:15) TO KUREF (1:15)
           END-EVALUATE.
 
       INNFIL-IDCHK SECTION.
       INNFIL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INNFIL-IO-AREA (21:1) = 'H' )
             OR ( INNFIL-IO-AREA (21:1) = 'V' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           EVALUATE TRUE
           WHEN ( INNFIL-IO-AREA (21:1) = 'H' )
               SET I-01                    TO TRUE
           WHEN ( INNFIL-IO-AREA (21:1) = 'V' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INNFIL-IO-AREA (21:1) = 'H' )
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (1:3)   TO INNFIL-01-L3-FIRMA
               MOVE INNFIL-IO-AREA (4:6)   TO INNFIL-01-L2-RESKNR
               MOVE INNFIL-IO-AREA (10:6)  TO INNFIL-01-L1-FAKTNR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L3          TO THE-PRIOR-L3
               MOVE  INNFIL-01-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           WHEN ( INNFIL-IO-AREA (21:1) = 'V' )
               MOVE LOW-VALUES             TO INNFIL-LEVEL-02
               MOVE INNFIL-IO-AREA (1:3)   TO INNFIL-02-L3-FIRMA
               MOVE INNFIL-IO-AREA (4:6)   TO INNFIL-02-L2-RESKNR
               MOVE INNFIL-IO-AREA (10:6)  TO INNFIL-02-L1-FAKTNR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-02-L3          TO THE-PRIOR-L3
               MOVE  INNFIL-02-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-02-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (504:30) TO FIADR (1:30)
               MOVE FIRMAF-IO-AREA (534:4) TO FIPNR (1:4)
               MOVE FIRMAF-IO-AREA (539:19) TO FIPOST (1:19)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KMNAVN (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KMADR1 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO KMADR2 (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO KMSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO KMPNR (1:4)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (167:13) TO KXEANL (1:13)
               MOVE KUNDEMX-IO-AREA (167:4) TO KXEA4F (1:4)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-05                        TO TRUE.
 
       ORDNRM-FLDSET SECTION.
       ORDNRM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRM-IO-AREA (25:6)  TO ORDKNR (1:6)
           END-EVALUATE.
 
       ORDNRM-IDSET SECTION.
       ORDNRM-IDSET-P.
           SET I-06                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (63:3) TO VTENH (1:3)
               MOVE VARETIL-IO-AREA (148:13) TO VTEAN (1:13)
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-08                        TO TRUE.
 
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
 
       FNRTAB-LOAD SECTION.
       FNRTAB-LOAD-P.
           OPEN INPUT FNRTAB
           SET TABFNR-I                    TO 1
           PERFORM UNTIL FNRTAB-EOF
               READ FNRTAB
               AT END
                   SET FNRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FNRTAB-IO-AREA (1:16) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FNRTAB.
 
       KNRTAB-LOAD SECTION.
       KNRTAB-LOAD-P.
           OPEN INPUT KNRTAB
           SET TABKNR-I                    TO 1
           PERFORM UNTIL KNRTAB-EOF
               READ KNRTAB
               AT END
                   SET KNRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE KNRTAB-IO-AREA (1:9) TO TABKNR-ENTRY (TABKNR-I)
                   SET TABKNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KNRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDKNR                 TO LISTE-IO-AREA (2:6)
               MOVE FAKTNR                 TO LISTE-IO-AREA (10:6)
               MOVE KMNAVN                 TO LISTE-IO-AREA (17:30)
               MOVE KMPNR                  TO LISTE-IO-AREA (48:4)
               MOVE KMSTED                 TO LISTE-IO-AREA (53:15)
               IF  (I-25)
                   MOVE KXEANL             TO LISTE-IO-AREA (76:13)
               END-IF
               IF  (NOT-I-25)
                   MOVE '** FEIL. UTEN EAN.LOC.NR' TO LISTE-IO-AREA
                                                               (76:24)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-25 AND I-50)
               MOVE SPACES TO EDIFIL2-IO-AREA
               INITIALIZE EDIFIL2-IO-AREA
               MOVE '913110000'            TO EDIFIL2-IO-AREA (1:9)
               MOVE FAKTNR                 TO EDIFIL2-IO-AREA (10:6)
               MOVE 'GLN'                  TO EDIFIL2-IO-AREA (16:3)
               MOVE KXEANL                 TO EDIFIL2-IO-AREA (19:13)
      *****************************************************************
      * HEADINGRECORD 1.  LEVERANDØR-DATA.                            *
      *****************************************************************
               WRITE EDIFIL2-IO-AREA
           END-IF
           IF  (I-02 AND I-25 AND I-50)
               MOVE SPACES TO EDIFIL-IO-AREA
               INITIALIZE EDIFIL-IO-AREA
               MOVE 'H01'                  TO EDIFIL-IO-AREA (1:3)
               IF  (NOT-I-26)
                   MOVE 'F'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               IF  (I-26)
                   MOVE 'K'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               MOVE FAKTNR                 TO EDIFIL-IO-AREA (5:6)
               MOVE '  '                   TO EDIFIL-IO-AREA (11:2)
               IF  (I-19)
                   MOVE TABEAN (TABEAN-I)  TO EDIFIL-IO-AREA (13:13)
               END-IF
               MOVE FINAVN                 TO EDIFIL-IO-AREA (26:30)
               MOVE FIADR                  TO EDIFIL-IO-AREA (61:30)
               MOVE FIPNR                  TO EDIFIL-IO-AREA (121:4)
               MOVE FIPOST                 TO EDIFIL-IO-AREA (133:19)
      *****************************************************************
      * HEADINGRECORD 2. KUNDE SOM HAR BESTILT VAREN.                 *
      *****************************************************************
               WRITE EDIFIL-IO-AREA
               MOVE SPACES TO EDIFIL-IO-AREA
               INITIALIZE EDIFIL-IO-AREA
               MOVE 'H02'                  TO EDIFIL-IO-AREA (1:3)
               IF  (NOT-I-26)
                   MOVE 'F'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               IF  (I-26)
                   MOVE 'K'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               MOVE FAKTNR                 TO EDIFIL-IO-AREA (5:6)
               MOVE '  '                   TO EDIFIL-IO-AREA (11:2)
      *                     N82KBEANL    25
               IF  (I-91)
                   MOVE KXEANL             TO EDIFIL-IO-AREA (13:13)
      *                        KBNAVN    55
      *                        KBADR1    90
      *                        KBADR2   120
      *                        KBPNR    124
      *                        KBSTED   147
               END-IF
               IF  (I-90)
                   MOVE KMNAVN             TO EDIFIL-IO-AREA (26:30)
               END-IF
               IF  (I-90)
                   MOVE KMADR1             TO EDIFIL-IO-AREA (61:30)
               END-IF
               IF  (I-90)
                   MOVE KMADR2             TO EDIFIL-IO-AREA (91:30)
               END-IF
               IF  (I-90)
                   MOVE KMPNR              TO EDIFIL-IO-AREA (121:4)
               END-IF
               IF  (I-90)
                   MOVE KMSTED             TO EDIFIL-IO-AREA (133:15)
      *****************************************************************
      * HEADINGRECORD 3.  VAREADRESSE DATA.                           *
      *****************************************************************
      *       D        02 25 50
      *                                   3 "H03"
      *                     N26           4 "F"
      *                      26           4 "K"
      *                        FAKTNR    10
      *                                  12 "  "
      *                      91KXEANL    25
      *                      90KMNAVN    55
      *                      90KMADR1    90
      *                      90KMADR2   120
      *                      90KMPNR    124
      *                      90KMSTED   147
      *****************************************************************
      * HEADINGRECORD 4. REFERANSE OG BETINGELSER                     *
      *****************************************************************
               END-IF
               WRITE EDIFIL-IO-AREA
               MOVE SPACES TO EDIFIL-IO-AREA
               INITIALIZE EDIFIL-IO-AREA
               MOVE 'H04'                  TO EDIFIL-IO-AREA (1:3)
               IF  (NOT-I-26)
                   MOVE 'F'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               IF  (I-26)
                   MOVE 'K'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               MOVE FAKTNR                 TO EDIFIL-IO-AREA (5:6)
               MOVE '  '                   TO EDIFIL-IO-AREA (11:2)
               MOVE '20'                   TO EDIFIL-IO-AREA (13:2)
               MOVE FAKDTO                 TO EDIFIL-IO-AREA (15:6)
               MOVE KUREF                  TO EDIFIL-IO-AREA (21:15)
      *                      37          35 "SERVICE        "
      *                      38          35 "PR.TLF         "
               IF  (NOT-I-97)
                   MOVE BMTKST             TO EDIFIL-IO-AREA (111:24)
               END-IF
               MOVE '20'                   TO EDIFIL-IO-AREA (141:2)
               MOVE FFADTO                 TO EDIFIL-IO-AREA (143:6)
      *****************************************************************
      * HEADINGRECORD 5.  TEKSTER                                     *
      *****************************************************************
      *       D        02 25 50
      *                                   3 "H05"
      *                     N26           4 "F"
      *                      26           4 "K"
      *                        FAKTNR    10
      *                                  12 "  "
      *****************************************************************
      * HEADINGRECORD 6.  FAKTURA RABATT/GEBYR                        *
      *****************************************************************
      *       D        02 25 50
      *                                   3 "H06"
      *                     N26           4 "F"
      *                      26           4 "K"
      *                        FAKTNR    10
      *                                  12 "  "
      *****************************************************************
      * VARELINJE RECORD 1.  FAKTURA VARELINJE.                       *
      *****************************************************************
               WRITE EDIFIL-IO-AREA
           END-IF
           IF  (I-02 AND I-25)
               MOVE SPACES TO EDIFIL-IO-AREA
               INITIALIZE EDIFIL-IO-AREA
               MOVE 'L01'                  TO EDIFIL-IO-AREA (1:3)
               IF  (NOT-I-26)
                   MOVE 'F'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               IF  (I-26)
                   MOVE 'K'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               MOVE FAKTNR                 TO EDIFIL-IO-AREA (5:6)
               MOVE '  '                   TO EDIFIL-IO-AREA (11:2)
               MOVE VLNR-IO                TO EDIFIL-IO-AREA (13:3)
               MOVE ARTN15                 TO EDIFIL-IO-AREA (16:15)
               IF  (NOT-I-88)
                   MOVE VTEAN              TO EDIFIL-IO-AREA (31:13)
               END-IF
               MOVE VARBET                 TO EDIFIL-IO-AREA (54:30)
               IF  (NOT-I-88)
                   MOVE VTENH              TO EDIFIL-IO-AREA (124:3)
               END-IF
               IF  (I-96)
                   MOVE '-'                TO EDIFIL-IO-AREA (127:1)
               END-IF
               IF  (NOT-I-96)
                   MOVE '+'                TO EDIFIL-IO-AREA (127:1)
               END-IF
               MOVE LEVANT                 TO EDIT-LEVANT
               MOVE EDIT-LEVANT            TO EDIFIL-IO-AREA (128:11)
               IF  (I-96)
                   MOVE '-'                TO EDIFIL-IO-AREA (139:1)
               END-IF
               IF  (NOT-I-96)
                   MOVE '+'                TO EDIFIL-IO-AREA (139:1)
               END-IF
               MOVE BELSTK                 TO EDIT-BELSTK
               MOVE EDIT-BELSTK            TO EDIFIL-IO-AREA (140:11)
               IF  (I-96)
                   MOVE '+'                TO EDIFIL-IO-AREA (151:1)
               END-IF
               IF  (NOT-I-96)
                   MOVE '-'                TO EDIFIL-IO-AREA (151:1)
               END-IF
               MOVE RAB152                 TO EDIT-RAB152
               MOVE EDIT-RAB152            TO EDIFIL-IO-AREA (152:6)
               IF  (I-96)
                   MOVE '+'                TO EDIFIL-IO-AREA (158:1)
               END-IF
               IF  (NOT-I-96)
                   MOVE '-'                TO EDIFIL-IO-AREA (158:1)
               END-IF
               MOVE RAB252                 TO EDIT-RAB252
               MOVE EDIT-RAB252            TO EDIFIL-IO-AREA (159:6)
               IF  (I-96)
                   MOVE '-'                TO EDIFIL-IO-AREA (165:1)
               END-IF
               IF  (NOT-I-96)
                   MOVE '+'                TO EDIFIL-IO-AREA (165:1)
               END-IF
               MOVE BELNTO                 TO EDIT-BELNTO
               MOVE EDIT-BELNTO            TO EDIFIL-IO-AREA (166:11)
               MOVE ONR                    TO EDIFIL-IO-AREA (177:6)
               MOVE ONR                    TO EDIFIL-IO-AREA (187:6)
      *****************************************************************
      * FAKTURA TOTAL LINJE.                                          *
      *****************************************************************
               WRITE EDIFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FOMA A/S            ' TO LISTE-IO-AREA (1:20)
               MOVE 'KONTROLLISTE EDI-FAK' TO LISTE-IO-AREA (36:20)
               MOVE 'TURA TIL JERNIA.'     TO LISTE-IO-AREA (56:16)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (76:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDENR'              TO LISTE-IO-AREA (1:7)
               MOVE 'FAKT.NR'              TO LISTE-IO-AREA (9:7)
               MOVE 'KUNDE NAVN'           TO LISTE-IO-AREA (17:10)
               MOVE 'POSTNR. OG STED'      TO LISTE-IO-AREA (48:15)
               MOVE 'EAN.LOC.NR./MERKNADER' TO LISTE-IO-AREA (75:21)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FOMA A/S            ' TO LISTE-IO-AREA (1:20)
               MOVE 'KONTROLLISTE EDI-FAK' TO LISTE-IO-AREA (36:20)
               MOVE 'TURA TIL JERNIA.'     TO LISTE-IO-AREA (56:16)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (76:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDENR'              TO LISTE-IO-AREA (1:7)
               MOVE 'FAKT.NR'              TO LISTE-IO-AREA (9:7)
               MOVE 'KUNDE NAVN'           TO LISTE-IO-AREA (17:10)
               MOVE 'POSTNR. OG STED'      TO LISTE-IO-AREA (48:15)
               MOVE 'EAN.LOC.NR./MERKNADER' TO LISTE-IO-AREA (75:21)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-25)
               MOVE SPACES TO EDIFIL-IO-AREA
               INITIALIZE EDIFIL-IO-AREA
               MOVE 'T01'                  TO EDIFIL-IO-AREA (1:3)
               IF  (NOT-I-26)
                   MOVE 'F'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               IF  (I-26)
                   MOVE 'K'                TO EDIFIL-IO-AREA (4:1)
               END-IF
               MOVE FAKTNR                 TO EDIFIL-IO-AREA (5:6)
               MOVE '  '                   TO EDIFIL-IO-AREA (11:2)
               IF  (I-27)
                   MOVE '-'                TO EDIFIL-IO-AREA (13:1)
               END-IF
               IF  (NOT-I-27)
                   MOVE '+'                TO EDIFIL-IO-AREA (13:1)
               END-IF
               MOVE NTOXMV                 TO EDIT-NTOXMV
               MOVE EDIT-NTOXMV            TO EDIFIL-IO-AREA (14:12)
               IF  (NOT-I-24)
                   MOVE '+025,00'          TO EDIFIL-IO-AREA (26:7)
               END-IF
               IF  (I-24)
                   MOVE '+000,00'          TO EDIFIL-IO-AREA (26:7)
               END-IF
               IF  (I-27)
                   MOVE '-'                TO EDIFIL-IO-AREA (33:1)
               END-IF
               IF  (NOT-I-27)
                   MOVE '+'                TO EDIFIL-IO-AREA (33:1)
               END-IF
               MOVE MVABEL                 TO EDIT-MVABEL
               MOVE EDIT-MVABEL            TO EDIFIL-IO-AREA (34:11)
               IF  (I-27)
                   MOVE '-'                TO EDIFIL-IO-AREA (45:1)
               END-IF
               IF  (NOT-I-27)
                   MOVE '+'                TO EDIFIL-IO-AREA (45:1)
               END-IF
               MOVE FAKTOT                 TO EDIT-FAKTOT
               MOVE EDIT-FAKTOT            TO EDIFIL-IO-AREA (46:12)
               MOVE 'NOK'                  TO EDIFIL-IO-AREA (58:3)
               WRITE EDIFIL-IO-AREA
           END-IF
           IF  (I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FAKTURA'       TO LISTE-IO-AREA (2:14)
               MOVE ANTFAK                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (21:6)
               INITIALIZE ANTFAK
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL KR.NOTA'       TO LISTE-IO-AREA (2:14)
               MOVE ANTKRN                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (21:6)
               INITIALIZE ANTKRN
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FEIL.  '       TO LISTE-IO-AREA (2:14)
               MOVE ANTEAF                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (21:6)
               INITIALIZE ANTEAF
               MOVE 'DISSE ER IKKE SENDT'  TO LISTE-IO-AREA (29:19)
               MOVE 'JERNIA'               TO LISTE-IO-AREA (49:6)
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
           PERFORM FNRTAB-LOAD
           PERFORM KNRTAB-LOAD
           SET INNFIL-LEVEL-INIT           TO TRUE
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE ORDNRM-DATA-FIELDS
           OPEN INPUT ORDNRM
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           OPEN OUTPUT EDIFIL
           OPEN OUTPUT EDIFIL2
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABFNR-I                    TO 1
           SET TABKNR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE ORDNRM
           CLOSE VARETIL
           CLOSE EDIFIL
           CLOSE EDIFIL2
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
