       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR210R.
      **********************************************  Z-WIN-RPG2   ****
      *  ROR210   PROGRAMMERT AV ESPEN LARSEN 19.10.95                *
      *  KOPIERER REST.ORDRE.MASTER OG FJERNER DE SOM SKAL FJERNES.   *
      *  LESER DAGENS REST.ORDRE.RECORDS FRA ORDRE.RUTINEN OG DANNER  *
      *  REST.ORDRE.MASTER RECORDS + REST.TEXT.FILE RECORDS.          *
      * 30/4-96 ALLTID ORDRENRBRUDD PÅ TILBUDSORDRE.                  *
      *  5/3-97 TILDELING AV AVDELING ETTER LAGERKODE, SCANGROSS.     *
      *  4/6-97 ALTERNATIVT LK SKAL IKKE BENYTTES VED LAGEROVERF.     *
      * 17/9-97 ORDRE MED REK.NR. FÅR ORDREBRUDD OM FIRMAPARM= J.     *
      * 18/9-97 ORDRE MED REK.NR. FÅR ORDREBRUDD OM FIRMAPARM= K.     *
      *         OG ALTID REKVNR. I KUNDE.MASTER                       *
      * 10/12-97 TILBUDSORDRE ELDERE ENN 3 MND. SLETTES.              *
      * 05/08-98 ÅR 2000 RUTINE LEGGER NED ÅRHUNDRE I POS 39-40.      *
      * 11/12-98 ORDREBEKREFTELSE HAR SAMME REGLER SOM TILBUDSORDRE.  *
      * OBS: RECORDART 1 OG 2 PÅ NYE RECORD KOMMER KUN OM DET ER      *
      *      VAREADRESSE ELLER REKV.NR.                               *
      * 11/ 3-98 REST/FORH.ORDRE ELDERE ENN 12 MND. SLETTES.          *
      * 03/02/00 KONSERNMODELL FOR KUNDE.MASTER.                      *
      * 31/07/01 FJERNER RESTORDRE OM: SKAL RESTORDRE NOTERES = N     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR210.rpg
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
           SELECT RESTMAS
               ASSIGN TO RESTMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESTMAS-STATUS
               RECORD KEY IS RESTMAS-KEY1.
           SELECT NYEREC
               ASSIGN TO UT-S-NYEREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYEREC-STATUS.
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
           SELECT RORUT
               ASSIGN TO UT-S-RORUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RORUT-STATUS.
           SELECT NYETEXT
               ASSIGN TO UT-S-NYETEXT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYETEXT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESTMAS
               RECORD CONTAINS 160.
       01  RESTMAS-IO-AREA.
           05  RESTMAS-IO-AREA-X.
               10  RESTMAS-KEY1.
                   15  RESTMAS-KEY1N       PICTURE S9(17).
               10  FILLER                  PICTURE X(143).
       FD NYEREC
               BLOCK CONTAINS 260
               RECORD CONTAINS 130.
       01  NYEREC-IO-AREA.
           05  NYEREC-IO-AREA-X            PICTURE X(130).
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
       FD RORUT
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  RORUT-IO-AREA.
           05  RORUT-IO-AREA-X             PICTURE X(160).
       FD NYETEXT
               BLOCK CONTAINS 260
               RECORD CONTAINS 130.
       01  NYETEXT-IO-AREA.
           05  NYETEXT-IO-AREA-X           PICTURE X(130).
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
           10  RESTMAS-STATUS              PICTURE 99 VALUE 0.
           10  NYEREC-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  RORUT-STATUS                PICTURE 99 VALUE 0.
           10  NYETEXT-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  RESTMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTMAS-EOF-OFF         VALUE '0'.
               88  RESTMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTMAS-READ-OFF        VALUE '0'.
               88  RESTMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTMAS-PROCESS-OFF     VALUE '0'.
               88  RESTMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESTMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  RESTMAS-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEREC-EOF-OFF          VALUE '0'.
               88  NYEREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEREC-READ-OFF         VALUE '0'.
               88  NYEREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYEREC-PROCESS-OFF      VALUE '0'.
               88  NYEREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  NYEREC-LEVEL-INIT-OFF   VALUE '0'.
               88  NYEREC-LEVEL-INIT       VALUE '1'.
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
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  RESTMAS-LEVEL-05.
               10  RESTMAS-05-L3.
                   15  RESTMAS-05-L3-FIRM  PICTURE X(3).
           05  RESTMAS-DATA-FIELDS.
               10  MSTKEY                  PICTURE X(17).
               10  FIRM                    PICTURE X(3).
               10  OTYPE                   PICTURE X(1).
               10  RKNR2F                  PICTURE X(2).
               10  RKNR2M                  PICTURE X(2).
               10  RKNR2S                  PICTURE X(2).
               10  RLK                     PICTURE X(2).
               10  RREKNR                  PICTURE X(15).
               10  OORDRE                  PICTURE X(6).
               10  ORDA-ELGR-IO.
                   15  ORDA-ELGR           PICTURE S9(2).
               10  ORDMND-IO.
                   15  ORDMND              PICTURE S9(2).
               10  MODATO                  PICTURE X(6).
               10  ANTIR-IO.
                   15  ANTIR               PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAVD                    PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  RORREC                  PICTURE X(160).
           05  RESTMAS-MP                  PICTURE X(3).
           05  RESTMAS-MC                  PICTURE X(3).
           05  RESTMAS-M-05            REDEFINES RESTMAS-MC.
               10  RESTMAS-M-05-M1.
                   15  RESTMAS-M-05-M1-FIRM-G.
                       20  RESTMAS-M-05-M1-FIRM PICTURE X(3).
           05  NYEREC-LEVEL-01.
               10  NYEREC-01-L3.
                   15  NYEREC-01-L3-FIRM   PICTURE X(3).
               10  NYEREC-01-L2.
                   15  NYEREC-01-L2-RKNR   PICTURE X(6).
               10  NYEREC-01-L1.
                   15  NYEREC-01-L1-RORDRE PICTURE X(6).
           05  NYEREC-LEVEL-02.
               10  NYEREC-02-L3.
                   15  NYEREC-02-L3-FIRM   PICTURE X(3).
               10  NYEREC-02-L2.
                   15  NYEREC-02-L2-RKNR   PICTURE X(6).
               10  NYEREC-02-L1.
                   15  NYEREC-02-L1-RORDRE PICTURE X(6).
           05  NYEREC-LEVEL-03.
               10  NYEREC-03-L3.
                   15  NYEREC-03-L3-FIRM   PICTURE X(3).
               10  NYEREC-03-L2.
                   15  NYEREC-03-L2-RKNR   PICTURE X(6).
               10  NYEREC-03-L1.
                   15  NYEREC-03-L1-RORDRE PICTURE X(6).
           05  NYEREC-DATA-FIELDS.
               10  RECKEY                  PICTURE X(19).
               10  RKNR                    PICTURE X(6).
               10  KNRKEY                  PICTURE X(9).
               10  RORDRE                  PICTURE X(6).
               10  AVD                     PICTURE X(1).
               10  REKVNR                  PICTURE X(15).
               10  VADR1                   PICTURE X(30).
               10  VADR4                   PICTURE X(20).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VADR2                   PICTURE X(30).
               10  VADR3                   PICTURE X(30).
               10  ROPOS                   PICTURE X(3).
               10  REDBNR                  PICTURE X(7).
               10  RALFA                   PICTURE X(3).
               10  RARTNR                  PICTURE X(20).
               10  ANTRST-IO.
                   15  ANTRST              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  LK                      PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  HND                     PICTURE X(3).
               10  OM                      PICTURE X(2).
               10  MVA                     PICTURE X(1).
               10  BM                      PICTURE X(2).
               10  VARBET                  PICTURE X(30).
           05  NYEREC-MP                   PICTURE X(3).
           05  NYEREC-MC                   PICTURE X(3).
           05  NYEREC-M-01             REDEFINES NYEREC-MC.
               10  NYEREC-M-01-M1.
                   15  NYEREC-M-01-M1-FIRM-G.
                       20  NYEREC-M-01-M1-FIRM PICTURE X(3).
           05  NYEREC-M-02             REDEFINES NYEREC-MC.
               10  NYEREC-M-02-M1.
                   15  NYEREC-M-02-M1-FIRM-G.
                       20  NYEREC-M-02-M1-FIRM PICTURE X(3).
           05  NYEREC-M-03             REDEFINES NYEREC-MC.
               10  NYEREC-M-03-M1.
                   15  NYEREC-M-03-M1-FIRM-G.
                       20  NYEREC-M-03-M1-FIRM PICTURE X(3).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FSLETT                  PICTURE X(1).
               10  AVDBRD                  PICTURE X(1).
               10  ROSTED                  PICTURE X(1).
               10  ROREKV                  PICTURE X(1).
               10  FIREST                  PICTURE X(1).
           05  KUNDEMA-DATA-FIELDS.
               10  ALTRNR                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTR5-IO.
                   15  ANTR5               PICTURE S9(5).
               10  ANTRS-IO.
                   15  ANTRS               PICTURE S9(5).
               10  ANTTS-IO.
                   15  ANTTS               PICTURE S9(5).
               10  ANTTB-IO.
                   15  ANTTB               PICTURE S9(5).
               10  ANTR4-IO.
                   15  ANTR4               PICTURE S9(5).
               10  KAVD                    PICTURE X(1).
               10  ANT3L1-IO.
                   15  ANT3L1              PICTURE S9(5).
               10  KKEY                    PICTURE X(9).
               10  KUNREF                  PICTURE X(15).
               10  VADR1T                  PICTURE X(30).
               10  VADR4T                  PICTURE X(20).
               10  ANTR1-IO.
                   15  ANTR1               PICTURE S9(5).
               10  VADR2T                  PICTURE X(30).
               10  VADR3T                  PICTURE X(30).
               10  ANTR2-IO.
                   15  ANTR2               PICTURE S9(5).
               10  ORDAT2-IO.
                   15  ORDAT2              PICTURE S9(6).
               10  ORDATO-N-IO.
                   15  ORDATO-N            PICTURE S9(7).
               10  ANTR3-IO.
                   15  ANTR3               PICTURE S9(5).
               10  TYPE-X                  PICTURE X(1).
               10  SAVD                    PICTURE X(1).
               10  ANTTXT-IO.
                   15  ANTTXT              PICTURE S9(5).
               10  A-ELGR100M              PICTURE X(2).
               10  REGMND-IO.
                   15  REGMND              PICTURE S9(5).
               10  DENMND-IO.
                   15  DENMND              PICTURE S9(5).
               10  DIFMND-IO.
                   15  DIFMND              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-05                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESTMAS-PROCESS
               SET RESTMAS-PROCESS-OFF     TO TRUE
               SET RESTMAS-READ            TO TRUE
           END-IF
 
           IF  RESTMAS-READ
               PERFORM RESTMAS-GET
               SET RESTMAS-READ-OFF        TO TRUE
               IF  NOT RESTMAS-EOF
                   PERFORM RESTMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  NYEREC-PROCESS
               SET NYEREC-PROCESS-OFF      TO TRUE
               SET NYEREC-READ             TO TRUE
           END-IF
 
           IF  NYEREC-READ
               PERFORM NYEREC-GET
               SET NYEREC-READ-OFF         TO TRUE
               IF  NOT NYEREC-EOF
                   PERFORM NYEREC-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM NYEREC-MATCH-SET
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
 
           IF  RESTMAS-PROCESS
               PERFORM RESTMAS-IDSET
           END-IF
 
           IF  NYEREC-PROCESS
               PERFORM NYEREC-IDSET
           END-IF
 
           IF  RESTMAS-PROCESS
               PERFORM RESTMAS-CHK-LEVEL
           END-IF
 
           IF  NYEREC-PROCESS
               PERFORM NYEREC-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  RESTMAS-PROCESS
               PERFORM RESTMAS-FLDOFF
               PERFORM RESTMAS-FLDSET
           END-IF
 
           IF  NYEREC-PROCESS
               PERFORM NYEREC-FLDOFF
               PERFORM NYEREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESTMAS-PROCESS
           OR  NYEREC-PROCESS
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
               SET NOT-I-50                TO TRUE
               SET NOT-I-99                TO TRUE
      *****************************************************************
      * RUTINE FOR Å SE OM AVDELING SKAL BENYTTES SOM SØKEBEGREP.     *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-96                TO TRUE
               IF  FIRM = '923'
                   SET I-96                TO TRUE
               END-IF
               MOVE FIRM                   TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-51                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-51            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-50                TO TRUE
               IF  AVDBRD = 'J'
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-99                TO TRUE
               IF  FSLETT = 'S'
                   SET I-99                TO TRUE
               END-IF
               SET NOT-I-98                TO TRUE
               IF  FIREST = 'N'
                   SET I-98                TO TRUE
               END-IF
               SET NOT-I-85                TO TRUE
               IF  ROSTED = 'H'
                   SET I-85                TO TRUE
               END-IF
               SET NOT-I-86                TO TRUE
               IF  ROSTED = 'A'
                   SET I-86                TO TRUE
               END-IF
               SET NOT-I-74                TO TRUE
               IF  ROREKV = 'J'
                   SET I-74                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  ROREKV = 'K'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-86)
               SET NOT-I-86                TO TRUE
               IF  ROSTED = '1'
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-85 AND NOT-I-86)
               SET I-85                    TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-50)
               SET I-85                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  RKNR2F = '59'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-05)
               SET NOT-I-18                TO TRUE
               IF  OTYPE = 'R'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  OTYPE = 'F'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-05)
               SET NOT-I-88                TO TRUE
               IF  OTYPE = 'F'
                   SET I-88                TO TRUE
               END-IF
               SET NOT-I-19                TO TRUE
               IF  OTYPE = 'T'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-19)
               SET NOT-I-19                TO TRUE
               IF  OTYPE = 'O'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-05)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-05 AND I-74 AND NOT-I-17)
               SET I-20                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å FJERNE RECORDS FRA REST.ORDRE.MASTER.            *
      *  1.    FIRMA SOM ER MERKET SLETTET.                           *
      *  2.    ANTALL I REST ER ENDRET TIL 0.                         *
      *  3.    STATUS = S (SLETTES).                                  *
      *  4.    STATUS = O (OVERFØRT TIL ORDREFILE)                    *
      *  5.    TILBUDSORDRE/ORDREBEKREFTELSE ELDERE ENN 3 MND.        *
      *  6.    Restordre/forhåndsordre eldre enn 12 MND.              *
      *****************************************************************
           END-IF
           IF  (I-05)
               SET NOT-I-60                TO TRUE
           END-IF
           IF  (I-05 AND I-99)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-05 AND I-98 AND NOT-I-88)
               AND (NOT-I-19)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-05)
               SET NOT-I-61                TO TRUE
               IF  ANTIR = 0,00
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  STATUS-X = 'S'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND I-61)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-05 AND I-19)
               PERFORM MNDRUT-S
           END-IF
           IF  (I-05 AND I-19 AND I-40)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-05 AND I-18)
               PERFORM MNDRUT-S
           END-IF
           IF  (I-05 AND I-18 AND I-40)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-05 AND NOT-I-60)
               MOVE MODATO                 TO DATO6
               PERFORM SR2000-S
           END-IF
           IF  (I-05)
               ADD 1                       TO ANTR5
           END-IF
           IF  (I-05 AND I-60)
               ADD 1                       TO ANTRS
           END-IF
           IF  (I-05 AND I-60 AND I-19)
               ADD 1                       TO ANTTS
           END-IF
           IF  (I-05 AND NOT-I-60 AND I-19)
               ADD 1                       TO ANTTB
           END-IF
           IF  (I-05 AND NOT-I-60)
               ADD 1                       TO ANTR4
      *****************************************************************
      * RUTINE FOR AVDELINGSTILDELING.                                *
      * FIRMA SOM BENYTTER AVDELING TILDELES REGISTRERT AVDELING.     *
      * SCANGROSS TILDELER AVDELING ETTER LAGERKODE.                  *
      * FIRMA SOM IKKE BENYTTER AVDELING TILDELES AVDELIN 1.          *
      *****************************************************************
           END-IF
           IF  (I-05 AND I-50)
               MOVE RAVD                   TO KAVD
           END-IF
           IF  (I-05)
               SET NOT-I-70                TO TRUE
               IF  RLK = '10'
                   SET I-70                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  RLK = '15'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND I-50 AND I-96)
               AND (I-70)
               MOVE '5'                    TO KAVD
           END-IF
           IF  (I-05 AND I-50 AND I-96)
               AND (I-75)
               MOVE '5'                    TO KAVD
           END-IF
           IF  (I-05 AND NOT-I-50)
               MOVE '1'                    TO KAVD
      *  05      FIRM      COMP "918"                    99 S OG B
      *  05 18             DEBUGLISTE     MSTKEY
           END-IF
           IF  (I-05)
               GO TO SLUTT-T
      *****************************************************************
      *                    DEBUGLISTE     RECKEY
           END-IF
           IF  (I-L1)
               SET NOT-I-15                TO TRUE
               SET NOT-I-21                TO TRUE
               SET NOT-I-22                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-26                TO TRUE
               SUBTRACT ANT3L1             FROM ANT3L1
      *****************************************************************
      *  RUTINE FOR OPPSLAG MOT KUNDEARKIV HVIST ORDRE MED REKV.NR    *
      *         SKAL PÅ EGENE ORDRE NÅR KUN PÅ KUNDER MED INNMELDT    *
      *         ALLTID REKV.NR.                                       *
      *  DENNE RUTINE ER KUN LAGET FOR NYE RESTORDRE.                 *
      *****************************************************************
           END-IF
           IF  (I-L2 AND I-77)
               MOVE KNRKEY                 TO KKEY
           END-IF
           IF  (I-L2 AND I-77 AND NOT-I-51)
               SET NOT-I-49                TO TRUE
               IF  KONFNR > '000'
                   SET I-49                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-77 AND NOT-I-51)
               AND (I-49)
               MOVE KONFNR                 TO KKEY (1:3)
           END-IF
           IF  (I-L2 AND I-77)
               MOVE KKEY                   TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-49                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-49            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
               SET NOT-I-48                TO TRUE
               IF  ALTRNR = '3'
                   SET I-48                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-77 AND NOT-I-48)
               SET NOT-I-48                TO TRUE
               IF  ALTRNR = '4'
                   SET I-48                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-77 AND NOT-I-48)
               SET NOT-I-48                TO TRUE
               IF  ALTRNR = '5'
                   SET I-48                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-77 AND I-48)
               AND (NOT-I-10)
               SET I-20                    TO TRUE
           END-IF
           IF  (I-01 AND I-74 AND NOT-I-10)
               SET I-20                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å DANNE REST.TEXT.FILE  (VAREADRESSER)             *
      *****************************************************************
           END-IF
           IF  (I-01 AND NOT-I-10)
               MOVE REKVNR                 TO KUNREF
               SET I-26                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-11)
               MOVE VADR1                  TO VADR1T
           END-IF
           IF  (I-01 AND NOT-I-14)
               MOVE VADR4                  TO VADR4T
           END-IF
           IF  (I-01 AND NOT-I-11)
               SET I-15                    TO TRUE
               SET I-21                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-14)
               SET I-15                    TO TRUE
               SET I-24                    TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTR1
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND NOT-I-12)
               MOVE VADR2                  TO VADR2T
           END-IF
           IF  (I-02 AND NOT-I-13)
               MOVE VADR3                  TO VADR3T
           END-IF
           IF  (I-02 AND NOT-I-12)
               SET I-15                    TO TRUE
               SET I-22                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-13)
               SET I-15                    TO TRUE
               SET I-23                    TO TRUE
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTR2
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR NY REST.ORDRE.MASTER                              *
      *****************************************************************
           END-IF
           MOVE ORDATO                     TO ORDATO-N
           MOVE ORDATO-N-IO (2:6)          TO ORDAT2-IO
           MOVE ORDAT2                     TO DATO6
      ** MLLzo
           MOVE '0'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE DATO6 (6:1)                TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO DATO6 (6:1)
           PERFORM SR2000-S
           ADD 1                           TO ANT3L1
           ADD 1                           TO ANTR3
           ADD 1                           TO ANTR4
           SET NOT-I-25                    TO TRUE
           IF  BK = 'F'
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  BK = 'T'
               SET I-27                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  BK = 'S'
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  BK = 'O'
               SET I-29                    TO TRUE
           END-IF
           MOVE 'R'                        TO TYPE-X
           IF  (I-25)
               MOVE 'F'                    TO TYPE-X
           END-IF
           IF  (I-27)
               MOVE 'T'                    TO TYPE-X
           END-IF
           IF  (I-28)
               MOVE 'S'                    TO TYPE-X
           END-IF
           IF  (I-29)
               MOVE 'O'                    TO TYPE-X
      *****************************************************************
      * RUTINE FOR Å SETTE INN AVDELING I SØKEBEGREP.                 *
      * FIRMA SOM IKKE BENYTTER AVDELING TILDELES AVDELIN 1.          *
      * FIRMA SOM BENYTTER AVDELING TILDELES REGISTRERT AVDELING.     *
      * SCANGROSS TILDELER AVDELING ETTER LAGERKODE.                  *
      *****************************************************************
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  (NOT-I-50)
               MOVE '1'                    TO SAVD
           END-IF
           IF  (I-50)
               MOVE AVD                    TO SAVD
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  LK = '10'
               SET I-70                    TO TRUE
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  LK = '15'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-50 AND I-96 AND I-70)
               MOVE '5'                    TO SAVD
           END-IF
           IF  (I-50 AND I-96 AND I-75)
               MOVE '5'                    TO SAVD
           END-IF
           SET NOT-I-53                    TO TRUE
           IF  SAVD = 'A'
               SET I-53                    TO TRUE
           END-IF
           IF  (NOT-I-53)
               SET NOT-I-53                TO TRUE
               IF  SAVD = 'B'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-50 AND NOT-I-53)
               SET NOT-I-52                TO TRUE
               IF  SAVD < '1'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-50 AND I-52)
               MOVE '1'                    TO SAVD
      *****************************************************************
      * SLUTT RUTINE                                                  *
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       SR2000-S SECTION.
       SR2000-S-P.
           MOVE '  '                       TO A-ELGR100M
           MOVE 'B'                        TO DATOK
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           SET NOT-I-42                    TO TRUE
           IF  DATOK = 'F'
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-42)
               MOVE AMD8 (1:2)             TO A-ELGR100M
           END-IF.
      *****************************************************************
      * SUBRUTE FOR TEST OM ORDREDATO ER ELDERE ENN 3 / 12 MND.       *
      * RESTORDRE/FORHÅNDSORDRE SLETTES NÅR DET ER ELDERE ENN 12 MND. *
      * TILBUD/ORDREBEKREFELSE  SLETTES NÅR DET ER ELDERE ENN  3 MND. *
      *****************************************************************
 
       MNDRUT-S SECTION.
       MNDRUT-S-P.
           SET NOT-I-40                    TO TRUE
           IF  ORDA-ELGR < 80
               SET I-40                    TO TRUE
           END-IF
           MULTIPLY 12 BY ORDA-ELGR    GIVING REGMND
           IF  (I-40)
               ADD 1200                    TO REGMND
           END-IF
           ADD ORDMND                      TO REGMND
           SET NOT-I-40                    TO TRUE
           IF  UYEAR < 80
               SET I-40                    TO TRUE
           END-IF
           MULTIPLY 12 BY UYEAR        GIVING DENMND
           IF  (I-40)
               ADD 1200                    TO DENMND
           END-IF
           ADD UMONTH                      TO DENMND
           SET NOT-I-40                    TO TRUE
           SUBTRACT REGMND FROM DENMND GIVING DIFMND
           IF  (I-19)
               SET NOT-I-40                TO TRUE
               IF  DIFMND > 3
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-18)
               SET NOT-I-40                TO TRUE
               IF  DIFMND > 12
                   SET I-40                TO TRUE
               END-IF
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-16                TO TRUE
               IF  ANT3L1 > 0
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-15 AND NOT-I-16)
               SET NOT-I-15                TO TRUE
           END-IF
           IF  (I-L1 AND I-15)
               ADD 1                       TO ANTTXT
      *****************************************************************
      * ÅR 2000 SUBRUTINE FOR Å HENTE ÅRHUNDRE.                       *
      *****************************************************************
           END-IF
           .
 
       RESTMAS-GET SECTION.
       RESTMAS-GET-P.
           IF  RESTMAS-EOF-OFF
               READ RESTMAS
               AT END
                   SET RESTMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESTMAS-FLDOFF SECTION.
       RESTMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-17                TO TRUE
           END-EVALUATE.
 
       RESTMAS-FLDSET SECTION.
       RESTMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESTMAS-IO-AREA (1:17) TO MSTKEY (1:17)
               MOVE RESTMAS-IO-AREA (1:3)  TO FIRM (1:3)
               MOVE RESTMAS-IO-AREA (5:1)  TO OTYPE (1:1)
               MOVE RESTMAS-IO-AREA (6:2)  TO RKNR2F (1:2)
               MOVE RESTMAS-IO-AREA (8:2)  TO RKNR2M (1:2)
               MOVE RESTMAS-IO-AREA (10:2) TO RKNR2S (1:2)
               MOVE RESTMAS-IO-AREA (33:2) TO RLK (1:2)
               MOVE RESTMAS-IO-AREA (48:15) TO RREKNR (1:15)
               IF  RREKNR = SPACES
                   SET I-17                TO TRUE
               END-IF
               MOVE RESTMAS-IO-AREA (64:6) TO OORDRE (1:6)
               MOVE RESTMAS-IO-AREA (73:2) TO ORDA-ELGR-IO
               INSPECT ORDA-ELGR-IO REPLACING ALL ' ' BY '0'
               MOVE RESTMAS-IO-AREA (75:2) TO ORDMND-IO
               INSPECT ORDMND-IO REPLACING ALL ' ' BY '0'
               MOVE RESTMAS-IO-AREA (73:6) TO MODATO (1:6)
               MOVE RESTMAS-IO-AREA (79:4) TO ANTIR-IO
               MOVE RESTMAS-IO-AREA (148:1) TO RAVD (1:1)
               MOVE RESTMAS-IO-AREA (156:1) TO STATUS-X (1:1)
               MOVE RESTMAS-IO-AREA (1:160) TO RORREC (1:160)
           END-EVALUATE.
 
       RESTMAS-IDSET SECTION.
       RESTMAS-IDSET-P.
           SET I-05                        TO TRUE.
 
       RESTMAS-CHK-LEVEL SECTION.
       RESTMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESTMAS-LEVEL-05
               MOVE RESTMAS-IO-AREA (1:3)  TO RESTMAS-05-L3-FIRM
               IF  RESTMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESTMAS-05-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   END-EVALUATE
               END-IF
               MOVE  RESTMAS-05-L3         TO THE-PRIOR-L3
               SET RESTMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESTMAS-MATCH-SET SECTION.
       RESTMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESTMAS-IO-AREA (1:3)  TO RESTMAS-M-05-M1-FIRM
           END-EVALUATE.
 
       NYEREC-GET SECTION.
       NYEREC-GET-P.
           IF  NYEREC-EOF-OFF
               READ NYEREC
               AT END
                   SET NYEREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYEREC-FLDOFF SECTION.
       NYEREC-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '1' )
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-14                TO TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '2' )
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
           END-EVALUATE.
 
       NYEREC-FLDSET SECTION.
       NYEREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '1' )
               MOVE NYEREC-IO-AREA (1:19)  TO RECKEY (1:19)
               MOVE NYEREC-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE NYEREC-IO-AREA (5:6)   TO RKNR (1:6)
               MOVE NYEREC-IO-AREA (2:9)   TO KNRKEY (1:9)
               MOVE NYEREC-IO-AREA (5:2)   TO RKNR2F (1:2)
               MOVE NYEREC-IO-AREA (7:2)   TO RKNR2M (1:2)
               MOVE NYEREC-IO-AREA (9:2)   TO RKNR2S (1:2)
               MOVE NYEREC-IO-AREA (14:6)  TO RORDRE (1:6)
               MOVE NYEREC-IO-AREA (20:1)  TO AVD (1:1)
               MOVE NYEREC-IO-AREA (21:15) TO REKVNR (1:15)
               IF  REKVNR = SPACES
                   SET I-10                TO TRUE
               END-IF
               MOVE NYEREC-IO-AREA (36:30) TO VADR1 (1:30)
               IF  VADR1 = SPACES
                   SET I-11                TO TRUE
               END-IF
               MOVE NYEREC-IO-AREA (66:20) TO VADR4 (1:20)
               IF  VADR4 = SPACES
                   SET I-14                TO TRUE
               END-IF
               MOVE NYEREC-IO-AREA (96:4)  TO ORDATO-IO
           WHEN ( NYEREC-IO-AREA (1:1) = '2' )
               MOVE NYEREC-IO-AREA (1:19)  TO RECKEY (1:19)
               MOVE NYEREC-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE NYEREC-IO-AREA (5:6)   TO RKNR (1:6)
               MOVE NYEREC-IO-AREA (5:2)   TO RKNR2F (1:2)
               MOVE NYEREC-IO-AREA (7:2)   TO RKNR2M (1:2)
               MOVE NYEREC-IO-AREA (9:2)   TO RKNR2S (1:2)
               MOVE NYEREC-IO-AREA (14:6)  TO RORDRE (1:6)
               MOVE NYEREC-IO-AREA (20:1)  TO AVD (1:1)
               MOVE NYEREC-IO-AREA (21:30) TO VADR2 (1:30)
               IF  VADR2 = SPACES
                   SET I-12                TO TRUE
               END-IF
               MOVE NYEREC-IO-AREA (51:30) TO VADR3 (1:30)
               IF  VADR3 = SPACES
                   SET I-13                TO TRUE
               END-IF
               MOVE NYEREC-IO-AREA (96:4)  TO ORDATO-IO
           WHEN ( NYEREC-IO-AREA (1:1) = '3' )
               MOVE NYEREC-IO-AREA (1:19)  TO RECKEY (1:19)
               MOVE NYEREC-IO-AREA (2:3)   TO FIRM (1:3)
               MOVE NYEREC-IO-AREA (5:6)   TO RKNR (1:6)
               MOVE NYEREC-IO-AREA (5:2)   TO RKNR2F (1:2)
               MOVE NYEREC-IO-AREA (7:2)   TO RKNR2M (1:2)
               MOVE NYEREC-IO-AREA (9:2)   TO RKNR2S (1:2)
               MOVE NYEREC-IO-AREA (11:3)  TO ROPOS (1:3)
               MOVE NYEREC-IO-AREA (14:6)  TO RORDRE (1:6)
               MOVE NYEREC-IO-AREA (20:1)  TO AVD (1:1)
               MOVE NYEREC-IO-AREA (21:7)  TO REDBNR (1:7)
               MOVE NYEREC-IO-AREA (28:3)  TO RALFA (1:3)
               MOVE NYEREC-IO-AREA (31:20) TO RARTNR (1:20)
               MOVE NYEREC-IO-AREA (51:4)  TO ANTRST-IO
               MOVE NYEREC-IO-AREA (55:5)  TO BEL-IO
               MOVE NYEREC-IO-AREA (60:2)  TO RAB1-IO
               MOVE NYEREC-IO-AREA (62:2)  TO RAB2-IO
               MOVE NYEREC-IO-AREA (64:2)  TO RAB3-IO
               MOVE NYEREC-IO-AREA (66:2)  TO LK (1:2)
               MOVE NYEREC-IO-AREA (68:1)  TO BK (1:1)
               MOVE NYEREC-IO-AREA (69:3)  TO HND (1:3)
               MOVE NYEREC-IO-AREA (73:2)  TO OM (1:2)
               MOVE NYEREC-IO-AREA (75:1)  TO MVA (1:1)
               MOVE NYEREC-IO-AREA (76:2)  TO BM (1:2)
               MOVE NYEREC-IO-AREA (96:4)  TO ORDATO-IO
               MOVE NYEREC-IO-AREA (101:30) TO VARBET (1:30)
           END-EVALUATE.
 
       NYEREC-IDCHK SECTION.
       NYEREC-IDCHK-P.
           EVALUATE TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '1' )
             OR ( NYEREC-IO-AREA (1:1) = '2' )
             OR ( NYEREC-IO-AREA (1:1) = '3' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       NYEREC-IDSET SECTION.
       NYEREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '3' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       NYEREC-CHK-LEVEL SECTION.
       NYEREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '1' )
               MOVE LOW-VALUES             TO NYEREC-LEVEL-01
               MOVE NYEREC-IO-AREA (2:3)   TO NYEREC-01-L3-FIRM
               MOVE NYEREC-IO-AREA (5:6)   TO NYEREC-01-L2-RKNR
               MOVE NYEREC-IO-AREA (14:6)  TO NYEREC-01-L1-RORDRE
               IF  NYEREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYEREC-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  NYEREC-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  NYEREC-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYEREC-01-L3          TO THE-PRIOR-L3
               MOVE  NYEREC-01-L2          TO THE-PRIOR-L2
               MOVE  NYEREC-01-L1          TO THE-PRIOR-L1
               SET NYEREC-LEVEL-INIT       TO TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '2' )
               MOVE LOW-VALUES             TO NYEREC-LEVEL-02
               MOVE NYEREC-IO-AREA (2:3)   TO NYEREC-02-L3-FIRM
               MOVE NYEREC-IO-AREA (5:6)   TO NYEREC-02-L2-RKNR
               MOVE NYEREC-IO-AREA (14:6)  TO NYEREC-02-L1-RORDRE
               IF  NYEREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYEREC-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  NYEREC-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  NYEREC-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYEREC-02-L3          TO THE-PRIOR-L3
               MOVE  NYEREC-02-L2          TO THE-PRIOR-L2
               MOVE  NYEREC-02-L1          TO THE-PRIOR-L1
               SET NYEREC-LEVEL-INIT       TO TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '3' )
               MOVE LOW-VALUES             TO NYEREC-LEVEL-03
               MOVE NYEREC-IO-AREA (2:3)   TO NYEREC-03-L3-FIRM
               MOVE NYEREC-IO-AREA (5:6)   TO NYEREC-03-L2-RKNR
               MOVE NYEREC-IO-AREA (14:6)  TO NYEREC-03-L1-RORDRE
               IF  NYEREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYEREC-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  NYEREC-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  NYEREC-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYEREC-03-L3          TO THE-PRIOR-L3
               MOVE  NYEREC-03-L2          TO THE-PRIOR-L2
               MOVE  NYEREC-03-L1          TO THE-PRIOR-L1
               SET NYEREC-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       NYEREC-MATCH-SET SECTION.
       NYEREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( NYEREC-IO-AREA (1:1) = '1' )
               MOVE NYEREC-IO-AREA (2:3)   TO NYEREC-M-01-M1-FIRM
           WHEN ( NYEREC-IO-AREA (1:1) = '2' )
               MOVE NYEREC-IO-AREA (2:3)   TO NYEREC-M-02-M1-FIRM
           WHEN ( NYEREC-IO-AREA (1:1) = '3' )
               MOVE NYEREC-IO-AREA (2:3)   TO NYEREC-M-03-M1-FIRM
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (123:1) TO FSLETT (1:1)
               MOVE FIRMAF-IO-AREA (858:1) TO AVDBRD (1:1)
               MOVE FIRMAF-IO-AREA (870:1) TO ROSTED (1:1)
               MOVE FIRMAF-IO-AREA (873:1) TO ROREKV (1:1)
               MOVE FIRMAF-IO-AREA (876:1) TO FIREST (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (178:1) TO ALTRNR (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-06                        TO TRUE.
 
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
           IF  RESTMAS-EOF
               MOVE HIGH-VALUES            TO RESTMAS-MC
                                              RESTMAS-MP
           END-IF
           IF  NYEREC-EOF
               MOVE HIGH-VALUES            TO NYEREC-MC
                                              NYEREC-MP
           END-IF
           IF  RESTMAS-MC < RESTMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  NYEREC-MC < NYEREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RESTMAS-MC < NYEREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESTMAS-PROCESS     TO TRUE
                   MOVE RESTMAS-MC         TO RESTMAS-MP
                   IF  RESTMAS-MC = NYEREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  NYEREC-MC < RESTMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET NYEREC-PROCESS      TO TRUE
                   MOVE NYEREC-MC          TO NYEREC-MP
                   IF  NYEREC-MC = RESTMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESTMAS-MC = NYEREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESTMAS-PROCESS     TO TRUE
                   MOVE RESTMAS-MC         TO RESTMAS-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-05 AND NOT-I-60)
               MOVE SPACES TO RORUT-IO-AREA
               INITIALIZE RORUT-IO-AREA
               MOVE RORREC                 TO RORUT-IO-AREA (1:160)
               MOVE KAVD                   TO RORUT-IO-AREA (4:1)
               IF  (I-19)
                   MOVE OORDRE             TO RORUT-IO-AREA (24:6)
               END-IF
               IF  (I-20)
                   MOVE OORDRE             TO RORUT-IO-AREA (24:6)
               END-IF
               IF  (I-59)
                   MOVE RKNR2S             TO RORUT-IO-AREA (33:2)
      *                96 75N59          34 "10"
               END-IF
               MOVE A-ELGR100M             TO RORUT-IO-AREA (39:2)
               MOVE ' '                    TO RORUT-IO-AREA (156:1)
               MOVE ' '                    TO RORUT-IO-AREA (157:1)
               IF  (I-85)
                   MOVE '10'               TO RORUT-IO-AREA (159:2)
               END-IF
               IF  (NOT-I-85)
                   MOVE RLK                TO RORUT-IO-AREA (159:2)
               END-IF
               IF  (I-59)
                   MOVE RKNR2M             TO RORUT-IO-AREA (159:2)
               END-IF
               WRITE RORUT-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO RORUT-IO-AREA
               INITIALIZE RORUT-IO-AREA
               MOVE FIRM                   TO RORUT-IO-AREA (1:3)
               MOVE SAVD                   TO RORUT-IO-AREA (4:1)
               MOVE TYPE-X                 TO RORUT-IO-AREA (5:1)
               MOVE RKNR                   TO RORUT-IO-AREA (6:6)
               IF  (I-15)
                   MOVE RORDRE             TO RORUT-IO-AREA (24:6)
               END-IF
               IF  (I-27)
                   MOVE RORDRE             TO RORUT-IO-AREA (24:6)
               END-IF
               IF  (I-29)
                   MOVE RORDRE             TO RORUT-IO-AREA (24:6)
               END-IF
               IF  (I-20)
                   MOVE RORDRE             TO RORUT-IO-AREA (24:6)
               END-IF
               MOVE BM                     TO RORUT-IO-AREA (30:2)
               MOVE MVA                    TO RORUT-IO-AREA (32:1)
               MOVE LK                     TO RORUT-IO-AREA (33:2)
               IF  (I-59)
                   MOVE RKNR2S             TO RORUT-IO-AREA (33:2)
               END-IF
               MOVE A-ELGR100M             TO RORUT-IO-AREA (39:2)
               MOVE REDBNR                 TO RORUT-IO-AREA (41:7)
               IF  (I-26)
                   MOVE KUNREF             TO RORUT-IO-AREA (48:15)
               END-IF
               MOVE RORDRE                 TO RORUT-IO-AREA (64:6)
               MOVE ROPOS                  TO RORUT-IO-AREA (70:3)
               MOVE ORDAT2-IO              TO RORUT-IO-AREA (73:6)
               MOVE ANTRST                 TO XO-52P
               MOVE XO-52P-EF              TO RORUT-IO-AREA (79:4)
               MOVE RALFA                  TO RORUT-IO-AREA (83:3)
               MOVE RARTNR                 TO RORUT-IO-AREA (86:20)
               MOVE VARBET                 TO RORUT-IO-AREA (106:30)
               MOVE BEL                    TO XO-72P
               MOVE XO-72P-EF              TO RORUT-IO-AREA (137:5)
               MOVE RAB1                   TO XO-21P
               MOVE XO-21P-EF              TO RORUT-IO-AREA (142:2)
               MOVE RAB2                   TO XO-21P
               MOVE XO-21P-EF              TO RORUT-IO-AREA (144:2)
               MOVE RAB3                   TO XO-21P
               MOVE XO-21P-EF              TO RORUT-IO-AREA (146:2)
               MOVE AVD                    TO RORUT-IO-AREA (148:1)
               MOVE OM                     TO RORUT-IO-AREA (149:2)
               MOVE HND                    TO RORUT-IO-AREA (151:3)
               IF  (I-15)
                   MOVE '*'                TO RORUT-IO-AREA (154:1)
               END-IF
               MOVE ' '                    TO RORUT-IO-AREA (156:1)
               MOVE ' '                    TO RORUT-IO-AREA (157:1)
               IF  (I-85)
                   MOVE '10'               TO RORUT-IO-AREA (159:2)
               END-IF
               IF  (NOT-I-85)
                   MOVE LK                 TO RORUT-IO-AREA (159:2)
               END-IF
               IF  (I-59)
                   MOVE RKNR2M             TO RORUT-IO-AREA (159:2)
               END-IF
               WRITE RORUT-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. ROR210 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ANTALL RECORDS' TO LISTE-IO-AREA (1:21)
               MOVE 'PÅ RESTORDREMASTER'   TO LISTE-IO-AREA (23:18)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (43:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-15)
               MOVE SPACES TO NYETEXT-IO-AREA
               INITIALIZE NYETEXT-IO-AREA
               MOVE FIRM                   TO NYETEXT-IO-AREA (1:3)
               IF  (NOT-I-25)
                   MOVE 'R'                TO NYETEXT-IO-AREA (4:1)
               END-IF
               IF  (I-25)
                   MOVE 'F'                TO NYETEXT-IO-AREA (4:1)
               END-IF
               MOVE RORDRE                 TO NYETEXT-IO-AREA (5:6)
               MOVE '000'                  TO NYETEXT-IO-AREA (11:3)
               IF  (I-21)
                   MOVE VADR1T             TO NYETEXT-IO-AREA (21:30)
               END-IF
               IF  (I-22)
                   MOVE VADR2T             TO NYETEXT-IO-AREA (51:30)
               END-IF
               IF  (I-23)
                   MOVE VADR3T             TO NYETEXT-IO-AREA (81:30)
               END-IF
               IF  (I-24)
                   MOVE VADR4T             TO NYETEXT-IO-AREA (111:20)
               END-IF
               WRITE NYETEXT-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. NYE VAREADR.REC. 1 ' TO LISTE-IO-AREA (3:24)
               MOVE ANTR1                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. NYE VAREADR.REC. 2 ' TO LISTE-IO-AREA (3:24)
               MOVE ANTR2                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. NYE TEXT. RECORDS  ' TO LISTE-IO-AREA (3:24)
               MOVE ANTTXT                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. GML REST. RECORDS  ' TO LISTE-IO-AREA (3:24)
               MOVE ANTR5                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. GML REST. SLETTET  ' TO LISTE-IO-AREA (3:24)
               MOVE ANTRS                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV TILB. SLETTET     ' TO LISTE-IO-AREA (3:24)
               MOVE ANTTS                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV TILB. BEHOLDT     ' TO LISTE-IO-AREA (3:24)
               MOVE ANTTB                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. NYE REST. RECORDS  ' TO LISTE-IO-AREA (3:24)
               MOVE ANTR3                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. REST. RECORDS NÅ.  ' TO LISTE-IO-AREA (3:24)
               MOVE ANTR4                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
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
           SET RESTMAS-LEVEL-INIT          TO TRUE
           INITIALIZE RESTMAS-DATA-FIELDS
           SET RESTMAS-EOF-OFF             TO TRUE
           SET RESTMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESTMAS-MC
                                              RESTMAS-MP
           OPEN INPUT RESTMAS
           SET NYEREC-LEVEL-INIT           TO TRUE
           INITIALIZE NYEREC-DATA-FIELDS
           SET NYEREC-EOF-OFF              TO TRUE
           SET NYEREC-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO NYEREC-MC
                                              NYEREC-MP
           OPEN INPUT NYEREC
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT RORUT
           OPEN OUTPUT NYETEXT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESTMAS
           CLOSE NYEREC
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE RORUT
           CLOSE NYETEXT
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
