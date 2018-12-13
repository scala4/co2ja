       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK033R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM FAK033                                                *
      * SEQ.NR. DANNES INNEN VÆRT ORDRENR. FOR Å BEHOLDE REKKEFØLGE.  *
      * KUNDER MED BET.MÅTE 07 OG 14 FÅR EGET FAKTURAKUNDENR.         *
      * KUNDER MED BET.MÅTE 47 RETTES TIL BET.KODE 07                 *
      *        DETTE ER KUNDER SOM SKAL HA FAKTURA PÅ SITT KUNDENR.   *
      *  29/4/93 KREDITNOTA OPPKRAV BLIR IKKE TILDELT FAKTURAKUNDENR. *
      *  15/7/94 BET.BET 22 RETTES TIL 07 FOR FIRMA 918.              *
      *  06/6/95 OPEL FORHANDLERE SKAL ENDRES TIL OPEL NORGE.         *
      *  23/1/96 OPEL FORHANDLERE = FIRMA 918 KUNDENR. 014XXX         *
      *   8/5/96 TABELL FOR ENDRING AV FAKTURA-KUNDENR.               *
      *  15/1/97 TABELL BLIR LEST FRA SEQ.DISK.FILE DANNET I FAK032   *
      *  23/1/97 FJERNER OG DANNER VAREADRESSEREC PÅ FAKTURA-KUNDENR. *
      *  24/7/97 FJERNEDE VAREADRESSE BLE IKKE MERKET SOM FAKTURERT   *
      *          OG DERVED OVERFØRT TIL NESTE FAKTURERING.            *
      *  25/7/97 ORDRE MED BET.KODE 07,14,47 SKAL IKKE OVERFØRES TIL  *
      *          NYTT FAKTURAKUNDENR. DA DISSE ORDRE ER BETALT.       *
      *          OG NY BETALINGSKODE VIL BLI TILDELT.                 *
      * 04.02.00 KONSERNMODELL FOR HENTEING AV KUNDE.MASTER DATA.     *
      *  4.07.00 LEGGE INN OPEL FORHANDLERNUMMER PÅ I KUNDEREF PÅ ALLE*
      *          KREDITORDRE TIL OPEL FOR SØRENSEN OG BALCHEN.        *
      *  5.09.00 ENDRING AV SUBARU OG ROVER FORHANDLERE TIL KUNDENR   *
      *          FOR AUTOINDUSTRI 104200.                             *
      *  7.12.01 ERSTATTET VAREADRESSERECORD TIL TEKSTRECORD MED      *
      *            VAREADRESSE VED OVERFØRING TIL FAKTURAKUNDENR.     *
      * 16.05.02 LAGT INN BRUDD PÅ KNR I TILLEGG TIL ORDRENR.         *
      *          DETTE P.G.A. RENTENOTA ALLE HAR SAMME ORDRENR.       *
      *   7/8/02 BET.BET 22 OG 47 TILDELES FAKT.MÅTE 2 (ORDREFAKT)    *
      *              DA DETTE LETTER FØRING AV INNBET. (NESJE)        *
      * 16.06.04 FIRMA MED BRUKERKODE "E" BLIR IKKE FAKTURERT.        *
      * 18.01.05 OVERFØRER IKKE FAKTURAKUNDENR"S BETBET VED ORDEFAKT. *
      * 22.02.05 SAAB KUNDER SKAL OGSÅ OMDØPE KUNDENR. TIL GM.NORGE.  *
      * 20.05.08 lagt inn linjer til avstemming fra fakpar         EN *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK033.rpg
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
           SELECT FKNRTAB
               ASSIGN TO UT-S-FKNRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKNRTAB-STATUS.
           SELECT CICSF
               ASSIGN TO UT-S-CICSF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS CICSF-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
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
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FKNRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  FKNRTAB-IO-AREA.
           05  FKNRTAB-IO-AREA-X           PICTURE X(40).
       FD CICSF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  CICSF-IO-AREA.
           05  CICSF-IO-AREA-X             PICTURE X(200).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
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
       FD OUTF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(200).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABKNO-MAX   VALUE 9900         PICTURE 9(4) USAGE BINARY.
       77  TABKNF-MAX   VALUE 9900         PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABKNO-TABLE.
               10  TABKNO-ENTRY
                                           OCCURS 9900 TIMES
                                           INDEXED BY TABKNO-I
                                                      TABKNO-S
                                                      TABKNF-I
                                                      TABKNF-S.
                   15  TABKNO              PICTURE X(9).
                   15  TABKNF              PICTURE X(6).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FKNRTAB-STATUS              PICTURE 99 VALUE 0.
           10  CICSF-STATUS                PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FKNRTAB-EOF-OFF         VALUE '0'.
               88  FKNRTAB-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  CICSF-EOF-OFF           VALUE '0'.
               88  CICSF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  CICSF-READ-OFF          VALUE '0'.
               88  CICSF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  CICSF-PROCESS-OFF       VALUE '0'.
               88  CICSF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  CICSF-LEVEL-INIT-OFF    VALUE '0'.
               88  CICSF-LEVEL-INIT        VALUE '1'.
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
           05  CICSF-LEVEL-03.
               10  CICSF-03-L2.
                   15  CICSF-03-L2-FIRMNR  PICTURE X(3).
               10  CICSF-03-L1.
                   15  CICSF-03-L1-KORDNR  PICTURE X(6).
                   15  CICSF-03-L1-KUNDNR  PICTURE X(6).
           05  CICSF-DATA-FIELDS.
               10  REC2                    PICTURE X(200).
               10  FIRMNR                  PICTURE X(3).
               10  FAKTYP                  PICTURE X(1).
               10  BETBET                  PICTURE X(2).
               10  KORDNR                  PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  VATEKS                  PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
               10  KNR3F                   PICTURE X(3).
               10  KNR4S                   PICTURE X(4).
           05  FAKPAR-DATA-FIELDS.
               10  PAR                     PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  PMNDO                   PICTURE X(1).
               10  PAARO                   PICTURE X(2).
               10  PSRDT                   PICTURE X(6).
               10  PREC1                   PICTURE X(100).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FOSRUT                  PICTURE X(1).
               10  FKSRUT                  PICTURE X(1).
               10  FIRTYP                  PICTURE X(1).
      *****************************************************************
      *  FAKTURAPARAMETER RUTINE.                                     *
      *****************************************************************
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KADR                    PICTURE X(30).
               10  KPSTED                  PICTURE X(15).
               10  KPNR                    PICTURE X(4).
               10  KBETM                   PICTURE X(2).
      *                                     170 170 KFAKFS
               10  KFAKMT                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(12).
           05  TEMPORARY-FIELDS.
               10  ANTIFF-IO.
                   15  ANTIFF              PICTURE S9(5).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  SEQVA1-IO.
                   15  SEQVA1              PICTURE S9(4).
               10  SEQVA2-IO.
                   15  SEQVA2              PICTURE S9(4).
               10  SEQVA3-IO.
                   15  SEQVA3              PICTURE S9(4).
               10  SEQVA4-IO.
                   15  SEQVA4              PICTURE S9(4).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(6).
               10  ANTOF-IO.
                   15  ANTOF               PICTURE S9(6).
               10  ANTONF-IO.
                   15  ANTONF              PICTURE S9(5).
               10  FAKKNR                  PICTURE X(6).
               10  KKEY                    PICTURE X(6).
               10  FAKKN2                  PICTURE X(6).
               10  ANTOKR-IO.
                   15  ANTOKR              PICTURE S9(6).
               10  KNRKEY                  PICTURE X(9).
               10  FBETM                   PICTURE X(2).
               10  FFAKMT                  PICTURE X(1).
               10  ANTFKN-IO.
                   15  ANTFKN              PICTURE S9(6).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(7)V9(2).
               10  NULANT-IO.
                   15  NULANT              PICTURE S9(5)V9(2).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  CICSF-PROCESS
               SET CICSF-PROCESS-OFF       TO TRUE
               SET CICSF-READ              TO TRUE
           END-IF
 
           IF  CICSF-READ
           AND RECORD-SELECTED-OFF
               PERFORM CICSF-GET
               SET CICSF-READ-OFF          TO TRUE
               IF  NOT CICSF-EOF
                   SET CICSF-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CICSF-PROCESS
               PERFORM CICSF-IDSET
           END-IF
 
           IF  CICSF-PROCESS
               PERFORM CICSF-CHK-LEVEL
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
 
           IF  CICSF-PROCESS
               PERFORM CICSF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  CICSF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-15)
               GO TO OFAKT-T
           END-IF
           READ FAKPAR
           AT END
               SET I-16                    TO TRUE
           NOT AT END
               SET NOT-I-16                TO TRUE
               PERFORM FAKPAR-FLDSET
               PERFORM FAKPAR-IDSET
           END-READ
           SET I-15                        TO TRUE.
 
       OFAKT-T.
      *****************************************************************
      *  HOVED RUTINE.                                                *
      *****************************************************************
           SET NOT-I-20                    TO TRUE
           SET NOT-I-27                    TO TRUE
           SET NOT-I-28                    TO TRUE
           IF  (I-L1)
               SET NOT-I-85                TO TRUE
               SET NOT-I-86                TO TRUE
               SET NOT-I-87                TO TRUE
               SET NOT-I-83                TO TRUE
           END-IF
           IF  (I-L2)
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  FIRTYP = 'E'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25)
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               ADD 1                       TO ANTIFF
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               SUBTRACT SEQNR              FROM SEQNR
               SET NOT-I-24                TO TRUE
               IF  FAKTYP = 'K'
                   SET I-24                TO TRUE
               END-IF
               PERFORM KNRRUT-S
           END-IF
           IF  (I-L1)
               PERFORM KNRRU2-S
      *****************************************************************
      *  OPPKRAVSALG PÅ 500165 OG KONTANTSALG PÅ 500190-99            *
      *   SKAL IKKE HA VAREADRESSE RECORDS.                           *
      *                                                               *
      *  SAMT FJERNE VAREADRESSE PÅ OVERFØRING TIL FAKTURA-KUNDENR.   *
      *  PÅ FAKTURAMÅTE 5,6,7,8 OG 9 (SAMLEFAKT OGSÅ NÅR VAREADR.)    *
      *       DISSE BLIR DANNET FRA KUNDEMASTER.                      *
      *****************************************************************
           END-IF
           IF  (I-86 AND NOT-I-87)
               SET I-27                    TO TRUE
           END-IF
           IF  (I-86 AND I-87)
               SET NOT-I-28                TO TRUE
               IF  RECART = 'L'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-28)
               SET NOT-I-86                TO TRUE
           END-IF
           IF  (I-86 AND NOT-I-87)
               SET NOT-I-86                TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  RECART = 'A'
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  VATEKS = 'A'
               SET I-26                    TO TRUE
           END-IF
           IF  (I-23 AND I-46)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-23 AND I-40 AND I-17)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-23 AND I-85)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-26 AND I-85 AND I-87)
               SET NOT-I-20                TO TRUE
      *****************************************************************
      * RUTINE FOR DANNE VAREADRESSE I TEKSTRECORD FOR FAKTURAKUNDENR *
      *    DANNE EGNE SEQ.NR. OG TELLIMG AV NYE RECORDS.              *
      *****************************************************************
           END-IF
           IF  (I-27)
               ADD 1                       TO SEQNR
               ADD SEQNR TO ZERO       GIVING SEQVA1
               ADD 1                       TO SEQNR
               ADD SEQNR TO ZERO       GIVING SEQVA2
           END-IF
           IF  (I-28)
               ADD 1                       TO SEQNR
               ADD SEQNR TO ZERO       GIVING SEQVA1
           END-IF
           IF  (I-28 AND NOT-I-72)
               ADD 1                       TO SEQNR
               ADD SEQNR TO ZERO       GIVING SEQVA2
           END-IF
           IF  (I-28 AND NOT-I-73)
               ADD 1                       TO SEQNR
               ADD SEQNR TO ZERO       GIVING SEQVA3
           END-IF
           IF  (I-28)
               ADD 1                       TO SEQNR
               ADD SEQNR TO ZERO       GIVING SEQVA4
           END-IF
           IF  (I-27)
               ADD 2                       TO ANTOF
           END-IF
           IF  (I-28)
               ADD 2                       TO ANTOF
           END-IF
           IF  (I-28 AND NOT-I-72)
               ADD 1                       TO ANTOF
           END-IF
           IF  (I-28 AND NOT-I-73)
               ADD 1                       TO ANTOF
           END-IF
           IF  (I-27)
               ADD 2                       TO ANTNYE
           END-IF
           IF  (I-28)
               ADD 2                       TO ANTNYE
           END-IF
           IF  (I-28 AND NOT-I-72)
               ADD 1                       TO ANTNYE
           END-IF
           IF  (I-28 AND NOT-I-73)
               ADD 1                       TO ANTNYE
      *****************************************************************
      *  RUTINE FOR Å DANNE SEQ.NR INNEN HVER ORDRE.                  *
      *  DETTE FOR Å BEHOLDE REKKEFØLGEN PÅ VARELINJER VED SORTERING. *
      *****************************************************************
           END-IF
           IF  (I-20)
               ADD 1                       TO SEQNR
           END-IF
           IF  (I-03 AND I-20)
               ADD 1                       TO ANTOF
           END-IF
           IF  (I-03 AND NOT-I-20)
               ADD 1                       TO ANTONF
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *  ENDRE BET.KODE PÅ ORDRE MED BET.KODE 47 TIL 07               *
      *  DETTE ER KUNDER SOM SKAL HA KONTANT FAKTURA                  *
      *****************************************************************
           SET NOT-I-47                    TO TRUE
           IF  BETBET = '47'
               SET I-47                    TO TRUE
           END-IF
           IF  (I-47)
               MOVE '07'                   TO BETBET
      *****************************************************************
      *  ENDRE BET.KODE PÅ ORDRE MED BET.KODE 22 TIL 07 PÅ FIRMA 918  *
      *  DETTE ER KUNDER SOM SKAL HA KONTANT FAKTURA, MEN SOM IKKE    *
      *  SKAL VÆRE MED PÅ DEN ORDINÆRE KONTANTSALGSRUTINEN.           *
      *****************************************************************
           END-IF
           SET NOT-I-48                    TO TRUE
           IF  FIRMNR = '918'
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-47                    TO TRUE
           IF  BETBET = '22'
               SET I-47                    TO TRUE
           END-IF
           IF  (I-48 AND I-47)
               MOVE '07'                   TO BETBET
      ****************************************************************
      *   SUBRUTINE FOR Å DANNE EGET FAKTURA KUNDENUMMER.            *
      *   DETTE DANNES PÅ BETALINGSMÅTE 07 (KONTANT) OG 14 (OPPKRAV) *
      *   OM FIRMA ØNSKER DET.                                       *
      *   KR.NOTA OPPKRAV SKAL IKKE HA EGET FAKTURA KUNDENR.         *
      *      5/5/95  DENNE TEST ER NÅ FJERNET.                       *
      ****************************************************************
           END-IF
           .
 
       KNRRUT-S SECTION.
       KNRRUT-S-P.
           SET NOT-I-40                    TO TRUE
           SET NOT-I-45                    TO TRUE
           SET NOT-I-46                    TO TRUE
           SET NOT-I-17                    TO TRUE
           IF  BETBET = '07'
               SET I-17                    TO TRUE
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  BETBET = '14'
               SET I-44                    TO TRUE
           END-IF
           IF  (NOT-I-17 AND NOT-I-44)
               GO TO KNREND-T
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  KUNDNR = '500165'
               SET I-45                    TO TRUE
           END-IF
           IF  (I-45)
               GO TO KNREND-T
      ***24 44             GOTO KNREND                      KR.NOTA OPPK
           END-IF
           SET NOT-I-46                    TO TRUE
           IF  KUNDNR = '500190'
               SET I-46                    TO TRUE
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500191'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500192'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500193'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500194'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500195'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500196'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500197'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500198'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  KUNDNR = '500199'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (I-46)
               GO TO KNREND-T
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  FOSRUT = '2'
               SET I-42                    TO TRUE
           END-IF
           IF  (I-42 AND I-44)
               GO TO KNREND-T
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  FKSRUT = '2'
               SET I-42                    TO TRUE
           END-IF
           IF  (I-42 AND I-17)
               GO TO KNREND-T
           END-IF
           IF  (I-44)
               MOVE '500165'               TO FAKKNR
           END-IF
           IF  (I-17)
               MOVE '500190'               TO FAKKNR
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  FIRMNR = '970'
               SET I-70                    TO TRUE
           END-IF
           IF  (I-17 AND I-70)
               MOVE '500199'               TO FAKKNR
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  FIRMNR = '977'
               SET I-70                    TO TRUE
           END-IF
           IF  (I-17 AND I-70)
               MOVE '500196'               TO FAKKNR
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  FIRMNR = '965'
               SET I-65                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  AVD = '3'
               SET I-33                    TO TRUE
           END-IF
           IF  (I-17 AND I-65 AND I-33)
               MOVE '500195'               TO FAKKNR
           END-IF
           SET I-40                        TO TRUE.
 
       KNREND-T.
           CONTINUE.
      *****************************************************************
      * SUBRUTINE FOR Å ERSTATTE ORDRE KUNDENR MED FAKTURA KUNDENR.   *
      * ORDRE MED BET.KODE: 07,14 OG 47 SKAL IKKE TILDELES NYTT KUNDENR
      *****************************************************************
 
       KNRRU2-S SECTION.
       KNRRU2-S-P.
           SET NOT-I-82                    TO TRUE
           SET NOT-I-83                    TO TRUE
           MOVE FIRMNR                     TO KKEY (1:3)
           MOVE KNR3F                      TO KKEY (4:3)
           SET NOT-I-83                    TO TRUE
           IF  KKEY = '918014'
               SET I-83                    TO TRUE
           END-IF
           IF  (NOT-I-83)
               SET NOT-I-83                TO TRUE
               IF  KKEY = '918026'
                   SET I-83                TO TRUE
               END-IF
           END-IF
           IF  (I-83)
               MOVE '105325'               TO FAKKN2
           END-IF
           IF  (I-83 AND I-24)
               SET I-82                    TO TRUE
           END-IF
           IF  (I-82)
               ADD 1                       TO ANTOKR
           END-IF
           IF  (I-83)
               GO TO ENDKR2-T
           END-IF
           SET NOT-I-83                    TO TRUE
           IF  KKEY = '918032'
               SET I-83                    TO TRUE
           END-IF
           IF  (NOT-I-83)
               SET NOT-I-83                TO TRUE
               IF  KKEY = '918033'
                   SET I-83                TO TRUE
               END-IF
           END-IF
           IF  (I-83)
               MOVE '104200'               TO FAKKN2
           END-IF
           IF  (I-83 AND I-24)
               SET I-82                    TO TRUE
           END-IF
           IF  (I-82)
               ADD 1                       TO ANTOKR
           END-IF
           IF  (I-83)
               GO TO ENDKR2-T
           END-IF
           SET NOT-I-83                    TO TRUE
           IF  KKEY = '918034'
               SET I-83                    TO TRUE
           END-IF
           IF  (I-83)
               MOVE '112500'               TO FAKKN2
           END-IF
           IF  (I-83 AND I-24)
               SET I-82                    TO TRUE
           END-IF
           IF  (I-82)
               ADD 1                       TO ANTOKR
           END-IF
           IF  (I-83)
               GO TO ENDKR2-T
           END-IF
           SET NOT-I-89                    TO TRUE
           IF  BETBET = '07'
               SET I-89                    TO TRUE
           END-IF
           IF  (NOT-I-89)
               SET NOT-I-89                TO TRUE
               IF  BETBET = '14'
                   SET I-89                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-89)
               SET NOT-I-89                TO TRUE
               IF  BETBET = '47'
                   SET I-89                TO TRUE
               END-IF
           END-IF
           IF  (I-89)
               GO TO ENDKR2-T
           END-IF
           MOVE FIRMNR                     TO KNRKEY (1:3)
           SET NOT-I-83                    TO TRUE
           IF  KONFNR > '000'
               SET I-83                    TO TRUE
           END-IF
           IF  (I-83)
               MOVE KONFNR                 TO KNRKEY (1:3)
           END-IF
           MOVE KUNDNR                     TO KNRKEY (4:6)
           SET NOT-I-83                    TO TRUE
           SET TABKNO-S                    TO TABKNO-I
           PERFORM WITH TEST AFTER
                   VARYING TABKNO-I FROM 1 BY 1
                     UNTIL TABKNO-I >= TABKNO-MAX
                        OR I-83
               IF  KNRKEY = TABKNO (TABKNO-I)
                   SET I-83                TO TRUE
                   SET TABKNO-S            TO TABKNO-I
               END-IF
           END-PERFORM
           SET TABKNO-I                    TO TABKNO-S
           IF  I-83
           AND TABKNO-I NOT > TABKNF-MAX
               SET TABKNF-I                TO TABKNO-I
           END-IF
           IF  (NOT-I-83)
               GO TO ENDKR2-T
           END-IF
           MOVE TABKNF(TABKNF-I)           TO FAKKN2
      *****************************************************************
      *    HENTE FAKTURADATA FRA FAKTURA-KUNDENR.                     *
      *    KORRIGERING AV FAKT.MÅTE FJERNET 22/4-97  EL               *
      *        REF. TLF. GJERT ROMNES.                                *
      * NÅR FAKTURAKUNDENR HAR ORDREFAKT OVERFØRES IKKE BET-KODE.     *
      *****************************************************************
           MOVE FAKKN2                     TO KNRKEY (4:6)
           MOVE KNRKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-84                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-84                TO TRUE
               PERFORM KUNDEMA-FLDOFF
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (I-84)
               GO TO ENDKR2-T
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  KFAKMT = '2'
               SET I-88                    TO TRUE
           END-IF
           IF  (NOT-I-88)
               MOVE KBETM                  TO FBETM
           END-IF
           IF  (I-88)
               MOVE BETBET                 TO FBETM
      *                    MOVE KFAKFS    FFAKFS  1         FAKT.FL.SIDE
           END-IF
           MOVE KFAKMT                     TO FFAKMT
           IF  (I-47)
               MOVE '*'                    TO FFAKMT
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  FFAKMT = '5'
               SET I-87                    TO TRUE
           END-IF
           IF  (NOT-I-87)
               SET NOT-I-87                TO TRUE
               IF  FFAKMT = '6'
                   SET I-87                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-87)
               SET NOT-I-87                TO TRUE
               IF  FFAKMT = '7'
                   SET I-87                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-87)
               SET NOT-I-87                TO TRUE
               IF  FFAKMT = '8'
                   SET I-87                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-87)
               SET NOT-I-87                TO TRUE
               IF  FFAKMT = '9'
                   SET I-87                TO TRUE
               END-IF
      ******************************************************
      *    HENTE VAREADRESSE FRA KUNDEMASTER.              *
      ******************************************************
           END-IF
           MOVE KUNDNR                     TO KNRKEY (4:6)
           MOVE KNRKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-84                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-84                TO TRUE
               PERFORM KUNDEMA-FLDOFF
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (I-84)
               GO TO ENDKR2-T
           END-IF
           SET I-85                        TO TRUE
           SET I-86                        TO TRUE
           ADD 1                           TO ANTFKN
           MOVE 0                          TO NULL-X
           MOVE 0,00                       TO NULANT.
 
       ENDKR2-T.
           CONTINUE.
      *****************************************************************
      * DANNE VAREADRESSE 1 VED OVERFØRING TIL FAKTURA-KUNDENR.       *
      *****************************************************************
 
       CICSF-GET SECTION.
       CICSF-GET-P.
           IF  CICSF-EOF-OFF
               READ CICSF
               AT END
                   SET CICSF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       CICSF-FLDSET SECTION.
       CICSF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE CICSF-IO-AREA (1:200)  TO REC2 (1:200)
               MOVE CICSF-IO-AREA (1:3)    TO FIRMNR (1:3)
               MOVE CICSF-IO-AREA (10:1)   TO FAKTYP (1:1)
               MOVE CICSF-IO-AREA (14:2)   TO BETBET (1:2)
               MOVE CICSF-IO-AREA (19:6)   TO KORDNR (1:6)
               MOVE CICSF-IO-AREA (25:1)   TO RECART (1:1)
               MOVE CICSF-IO-AREA (37:1)   TO VATEKS (1:1)
               MOVE CICSF-IO-AREA (166:1)  TO AVD (1:1)
               MOVE CICSF-IO-AREA (184:6)  TO KUNDNR (1:6)
               MOVE CICSF-IO-AREA (184:3)  TO KNR3F (1:3)
               MOVE CICSF-IO-AREA (186:4)  TO KNR4S (1:4)
           END-EVALUATE.
 
       CICSF-IDSET SECTION.
       CICSF-IDSET-P.
           SET I-03                        TO TRUE.
 
       CICSF-CHK-LEVEL SECTION.
       CICSF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO CICSF-LEVEL-03
               MOVE CICSF-IO-AREA (1:3)    TO CICSF-03-L2-FIRMNR
               MOVE CICSF-IO-AREA (19:6)   TO CICSF-03-L1-KORDNR
               MOVE CICSF-IO-AREA (184:6)  TO CICSF-03-L1-KUNDNR
               IF  CICSF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  CICSF-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  CICSF-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  CICSF-03-L2           TO THE-PRIOR-L2
               MOVE  CICSF-03-L1           TO THE-PRIOR-L1
               SET CICSF-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (6:2)   TO PAR (1:2)
               MOVE FAKPAR-IO-AREA (8:2)   TO PMND (1:2)
               MOVE FAKPAR-IO-AREA (3:1)   TO PMNDO (1:1)
               MOVE FAKPAR-IO-AREA (10:2)  TO PAARO (1:2)
               MOVE FAKPAR-IO-AREA (12:6)  TO PSRDT (1:6)
               MOVE FAKPAR-IO-AREA (1:100) TO PREC1 (1:100)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (746:1) TO FOSRUT (1:1)
               MOVE FIRMAF-IO-AREA (786:1) TO FKSRUT (1:1)
               MOVE FIRMAF-IO-AREA (956:1) TO FIRTYP (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-09                        TO TRUE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-72                TO TRUE
               SET NOT-I-73                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KNAVN2 (1:30)
               IF  KNAVN2 = SPACES
                   SET I-72                TO TRUE
               END-IF
               MOVE KUNDEMA-IO-AREA (76:30) TO KADR (1:30)
               IF  KADR = SPACES
                   SET I-73                TO TRUE
               END-IF
               MOVE KUNDEMA-IO-AREA (106:15) TO KPSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO KPNR (1:4)
               MOVE KUNDEMA-IO-AREA (127:2) TO KBETM (1:2)
               MOVE KUNDEMA-IO-AREA (171:1) TO KFAKMT (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
 
       FKNRTAB-LOAD SECTION.
       FKNRTAB-LOAD-P.
           OPEN INPUT FKNRTAB
           SET TABKNO-I                    TO 1
           PERFORM UNTIL FKNRTAB-EOF
               READ FKNRTAB
               AT END
                   SET FKNRTAB-EOF         TO TRUE
               NOT AT END
                   MOVE FKNRTAB-IO-AREA (1:15) TO TABKNO-ENTRY
                                                            (TABKNO-I)
                   SET TABKNO-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FKNRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-27)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               IF  (I-83)
                   MOVE FAKKN2             TO OUTF-IO-AREA (4:6)
               END-IF
               MOVE 'F'                    TO OUTF-IO-AREA (11:1)
               IF  (I-85)
                   MOVE FFAKMT             TO OUTF-IO-AREA (12:1)
               END-IF
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               IF  (I-85)
                   MOVE FBETM              TO OUTF-IO-AREA (14:2)
               END-IF
               MOVE 'A'                    TO OUTF-IO-AREA (25:1)
               MOVE SEQVA1-IO              TO OUTF-IO-AREA (26:4)
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               MOVE KNAVN1                 TO OUTF-IO-AREA (41:30)
               MOVE KNAVN2                 TO OUTF-IO-AREA (77:30)
               MOVE '                    ' TO OUTF-IO-AREA (107:20)
               MOVE '     '                TO OUTF-IO-AREA (127:5)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE PAR                    TO OUTF-IO-AREA (167:2)
               MOVE PMND                   TO OUTF-IO-AREA (169:2)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
               MOVE 'VVV'                  TO OUTF-IO-AREA (190:3)
      *****************************************************************
      * DANNE VAREADRESSE 2 VED OVERFØRING TIL FAKTURA-KUNDENR.       *
      *****************************************************************
               WRITE OUTF-IO-AREA
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               IF  (I-83)
                   MOVE FAKKN2             TO OUTF-IO-AREA (4:6)
               END-IF
               MOVE 'F'                    TO OUTF-IO-AREA (11:1)
               IF  (I-85)
                   MOVE FFAKMT             TO OUTF-IO-AREA (12:1)
               END-IF
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               IF  (I-85)
                   MOVE FBETM              TO OUTF-IO-AREA (14:2)
               END-IF
               MOVE 'A'                    TO OUTF-IO-AREA (25:1)
               MOVE SEQVA2-IO              TO OUTF-IO-AREA (26:4)
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               MOVE KADR                   TO OUTF-IO-AREA (41:30)
               MOVE KPNR                   TO OUTF-IO-AREA (77:4)
               MOVE ' '                    TO OUTF-IO-AREA (81:1)
               MOVE KPSTED                 TO OUTF-IO-AREA (82:15)
               MOVE '          '           TO OUTF-IO-AREA (97:10)
               MOVE '                    ' TO OUTF-IO-AREA (107:20)
               MOVE '     '                TO OUTF-IO-AREA (127:5)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE PAR                    TO OUTF-IO-AREA (167:2)
               MOVE PMND                   TO OUTF-IO-AREA (169:2)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
               MOVE 'VVV'                  TO OUTF-IO-AREA (190:3)
      *****************************************************************
      * DANNE VAREADRESSE 1 VED OVERFØRING TIL FAKT-KUNDENR OG SAMLEF *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03 AND I-28)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               IF  (I-83)
                   MOVE FAKKN2             TO OUTF-IO-AREA (4:6)
               END-IF
               MOVE 'F'                    TO OUTF-IO-AREA (11:1)
               IF  (I-85)
                   MOVE FFAKMT             TO OUTF-IO-AREA (12:1)
               END-IF
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               IF  (I-85)
                   MOVE FBETM              TO OUTF-IO-AREA (14:2)
               END-IF
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE SEQVA1-IO              TO OUTF-IO-AREA (26:4)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               MOVE KNAVN1                 TO OUTF-IO-AREA (82:30)
               MOVE '                    ' TO OUTF-IO-AREA (112:20)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE PAR                    TO OUTF-IO-AREA (167:2)
               MOVE PMND                   TO OUTF-IO-AREA (169:2)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
      *                                 192 "VVV"
      *****************************************************************
      * DANNE VAREADRESSE 2 VED OVERFØRING TIL FAKT-KUNDENR OG SAMLEF *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03 AND I-28 AND NOT-I-72)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               IF  (I-83)
                   MOVE FAKKN2             TO OUTF-IO-AREA (4:6)
               END-IF
               MOVE 'F'                    TO OUTF-IO-AREA (11:1)
               IF  (I-85)
                   MOVE FFAKMT             TO OUTF-IO-AREA (12:1)
               END-IF
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               IF  (I-85)
                   MOVE FBETM              TO OUTF-IO-AREA (14:2)
               END-IF
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE SEQVA2-IO              TO OUTF-IO-AREA (26:4)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               MOVE KNAVN2                 TO OUTF-IO-AREA (82:30)
               MOVE '                    ' TO OUTF-IO-AREA (112:20)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE PAR                    TO OUTF-IO-AREA (167:2)
               MOVE PMND                   TO OUTF-IO-AREA (169:2)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
      *                                 192 "VVV"
      *****************************************************************
      * DANNE VAREADRESSE 3 VED OVERFØRING TIL FAKT-KUNDENR OG SAMLEF *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03 AND I-28 AND NOT-I-73)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               IF  (I-83)
                   MOVE FAKKN2             TO OUTF-IO-AREA (4:6)
               END-IF
               MOVE 'F'                    TO OUTF-IO-AREA (11:1)
               IF  (I-85)
                   MOVE FFAKMT             TO OUTF-IO-AREA (12:1)
               END-IF
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               IF  (I-85)
                   MOVE FBETM              TO OUTF-IO-AREA (14:2)
               END-IF
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE SEQVA3-IO              TO OUTF-IO-AREA (26:4)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               MOVE KADR                   TO OUTF-IO-AREA (82:30)
               MOVE '                    ' TO OUTF-IO-AREA (112:20)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE PAR                    TO OUTF-IO-AREA (167:2)
               MOVE PMND                   TO OUTF-IO-AREA (169:2)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
      *                                 192 "VVV"
      *****************************************************************
      * DANNE VAREADRESSE 4 VED OVERFØRING TIL FAKT-KUNDENR OG SAMLEF *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03 AND I-28)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               IF  (I-83)
                   MOVE FAKKN2             TO OUTF-IO-AREA (4:6)
               END-IF
               MOVE 'F'                    TO OUTF-IO-AREA (11:1)
               IF  (I-85)
                   MOVE FFAKMT             TO OUTF-IO-AREA (12:1)
               END-IF
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               IF  (I-85)
                   MOVE FBETM              TO OUTF-IO-AREA (14:2)
               END-IF
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE SEQVA4-IO              TO OUTF-IO-AREA (26:4)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               MOVE KPNR                   TO OUTF-IO-AREA (82:4)
               MOVE ' '                    TO OUTF-IO-AREA (86:1)
               MOVE KPSTED                 TO OUTF-IO-AREA (87:15)
               MOVE '                    ' TO OUTF-IO-AREA (102:20)
               MOVE '          '           TO OUTF-IO-AREA (122:10)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE PAR                    TO OUTF-IO-AREA (167:2)
               MOVE PMND                   TO OUTF-IO-AREA (169:2)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
      *                                 192 "VVV"
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03 AND I-20)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               IF  (I-40)
                   MOVE FAKKNR             TO OUTF-IO-AREA (4:6)
               END-IF
               IF  (I-83)
                   MOVE FAKKN2             TO OUTF-IO-AREA (4:6)
               END-IF
               MOVE 'F'                    TO OUTF-IO-AREA (11:1)
               IF  (I-85)
                   MOVE FFAKMT             TO OUTF-IO-AREA (12:1)
               END-IF
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               IF  (I-85)
                   MOVE FBETM              TO OUTF-IO-AREA (14:2)
               END-IF
               MOVE SEQNR-IO               TO OUTF-IO-AREA (26:4)
               IF  (I-82)
                   MOVE 'F.HANDLER      '  TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-82)
                   MOVE KNR4S              TO OUTF-IO-AREA (51:4)
               END-IF
               MOVE PAR                    TO OUTF-IO-AREA (167:2)
               MOVE PMND                   TO OUTF-IO-AREA (169:2)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03)
               MOVE 'F'                    TO CICSF-IO-AREA (183:1)
               IF  (I-25)
                   MOVE ' '                TO CICSF-IO-AREA (183:1)
               END-IF
               REWRITE CICSF-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PREC1                  TO LISTE-IO-AREA (1:100)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
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
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '   --- FAK033 ---    ***' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTIFF                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE 'RECORDS TIL NESTE FAKT.' TO LISTE-IO-AREA (12:23)
               MOVE 'IKKE I FIRMA.FILE'    TO LISTE-IO-AREA (36:17)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTONF                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               MOVE 'RECORDS SLETTET.       ' TO LISTE-IO-AREA (12:23)
               MOVE 'VAREADRESSER     '    TO LISTE-IO-AREA (36:17)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTNYE                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'NYE ADRESSE REC. DANNET' TO LISTE-IO-AREA (12:23)
               MOVE ' FAKTURA-KUNDENR.'    TO LISTE-IO-AREA (36:17)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFKN                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ORDRE TILDELT NYTT     ' TO LISTE-IO-AREA (12:23)
               MOVE 'FAKTURA KUNDENR. '    TO LISTE-IO-AREA (36:17)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTOKR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'KR.ORDRE MED ENDRET    ' TO LISTE-IO-AREA (12:23)
               MOVE 'KUNDEREFERANSE.  '    TO LISTE-IO-AREA (36:17)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTOF                  TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'RECORDS TIL FAKT. FRA' TO LISTE-IO-AREA (12:21)
               MOVE 'ORDREREG.'            TO LISTE-IO-AREA (34:9)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PMNDO                  TO LISTE-IO-AREA (8:1)
               MOVE '= FAKTURAOMGANGSNUMMER I' TO LISTE-IO-AREA (10:24)
               MOVE ' MND (1/4)              ' TO LISTE-IO-AREA (34:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PAARO                  TO LISTE-IO-AREA (7:2)
               MOVE '= FAKTURAOMGANGSNUMMER I' TO LISTE-IO-AREA (10:24)
               MOVE ' AAR, DENNE KJØRING     ' TO LISTE-IO-AREA (34:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSRDT                  TO LISTE-IO-AREA (3:6)
               MOVE '= SISTE REG.DATO SOM SKA' TO LISTE-IO-AREA (10:24)
               MOVE 'L FAKTURERES DDMMAA     ' TO LISTE-IO-AREA (34:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           PERFORM FKNRTAB-LOAD
           SET CICSF-LEVEL-INIT            TO TRUE
           INITIALIZE CICSF-DATA-FIELDS
           SET CICSF-EOF-OFF               TO TRUE
           SET CICSF-PROCESS               TO TRUE
           OPEN I-O CICSF
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT OUTF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABKNO-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE CICSF
           CLOSE FAKPAR
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE OUTF
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
