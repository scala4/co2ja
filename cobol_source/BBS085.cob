       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBS085R.
      **********************************************  Z-WIN-RPG2   ****
      * OBS! BBS-DATO BRUKES DA BANKENES BOKFØRINGSSDATO IKKE ER MED  *
      *      I SENDINGEN. BANK-DATO ER BEHANDLINGSDATO I BANKEN.      *
      * PROGRAMNAVN: BBS085.                                          *
      * KONVERTERING OG KONTROLLISTER INNBETALING FRA                 *
      * BBS.  PARAMETERKORT MED BILAGSNUMMER MÅ MATCHES               *
      * MED DATO. FIRMANR. HENTER BANKKONTONUMMER I                   *
      * TABELL FOR Å MATCHE DENNE MOT BANKKONTONUMMER I               *
      * INNBETALINGSRECORDEN.                                         *
      *E 04.06.96: TAKLER 7-SIFRET KID                                *
      *E 08.04.97: FJERNER TRANSER SOM IKKE ER OCR-TRANSER.           *
      *            SKRIVER LISTE MED FJERNEDE TRANSER.                *
      *            LAGET ETIKETT FOR LISTENE.                         *
      *            ENDRET TEKSTER/RETTET FEIL.                        *
      *E 03.07.01: LISTER UT KUNDENR/REFNR OG BELØP VED FEIL KONTO.   *
      *E 02.08.01: SLÅTT SAMMEN TO LISTER TIL EN. SKRIVER IKKE GOD-   *
      *            KJENTE OPPDRAG LENGER, KUN VED FEIL I OPPDRAG.     *
      *E 22.08.01: LISTET IKKE KUNDENR VED FEIL GIROKID.              *
      *E 02.04.02: LAGET FEILLINJE NÅR DATO ER FEIL.                  *
      *E 26.04.02: SENDER KVITTERING KUN NÅR DET ER FEIL.             *
      *E 15.10.02: TAR MED RECORD 31 M/BANKKONTONR.                   *
      *E 14.11.03: RETTET JCL MED MAIL.                               *
      *E 14.04.05: SENDER FEILMELDING TIL OPERATØR.                   *
      *E 03.08.05: ENDRET DEST I LISTKORT.                            *
      *E 18.04.06: BRUKER RBS TIL FEILLISTE. FIRMA 399 OPPGAVE BBS01. *
      *            SKRIVER ALLE RECORDS PÅ LISTEN. LAGRER PÅ RWEB.    *
      *E 18.06.06: ENDRET JCL TIL PARTISJON 5.                        *
      *E 12.10.06: SJEKKER DATASENTRALNR 510 I TILLEGG TIL 117.       *
      *            SENDER MAIL TIL GURBINDER OG MORTEN NÅR U6 ER PÅ.  *
      *            HOPPER OVER DATOKONTROLL NÅR U7 ER PÅ (CREMUL)     *
      *E 03.04.08: LAGT INN FIRMANR I 88-REC                          *
      *E 25.03.09: TAKLER PREFIKS 962/963 FRA NY PURRERUTINE.         *
      *E 31.03.09: ENDRET CLIENT TIL EMAIL.                           *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BBS085.rpg
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
           SELECT BBSTAB
               ASSIGN TO UT-S-BBSTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSTAB-STATUS.
           SELECT BBSTRAI
               ASSIGN TO UT-S-BBSTRAI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSTRAI-STATUS.
           SELECT BANKGF
               ASSIGN TO BANKGF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS BANKGF-STATUS
               RECORD KEY IS BANKGF-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT BBSREC
               ASSIGN TO UT-S-BBSREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSREC-STATUS.
           SELECT LISTE1
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE1-STATUS.
           SELECT BBSFEIL
               ASSIGN TO UT-S-BBSFEIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSFEIL-STATUS.
           SELECT BBSUTT
               ASSIGN TO UT-S-BBSUTT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSUTT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BBSTAB
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  BBSTAB-IO-AREA.
           05  BBSTAB-IO-AREA-X            PICTURE X(120).
       FD BBSTRAI
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  BBSTRAI-IO-AREA.
           05  BBSTRAI-IO-AREA-X           PICTURE X(80).
       FD BANKGF
               RECORD CONTAINS 80.
       01  BANKGF-IO-AREA.
           05  BANKGF-IO-AREA-X.
               10  BANKGF-KEY1             PICTURE X(11).
               10  FILLER                  PICTURE X(69).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD BBSREC
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  BBSREC-IO-AREA.
           05  BBSREC-IO-AREA-X            PICTURE X(80).
       FD LISTE1
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE1-IO-PRINT.
           05  LISTE1-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE1-IO-AREA.
           05  LISTE1-IO-AREA-X            PICTURE X(132).
      *FLISTEO O   F  80  80            PRINTERSYSLST
       FD BBSFEIL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  BBSFEIL-IO-AREA.
           05  BBSFEIL-IO-AREA-X           PICTURE X(80).
       FD BBSUTT
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  BBSUTT-IO-AREA.
           05  BBSUTT-IO-AREA-X            PICTURE X(120).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(6).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BBSTAB-STATUS               PICTURE 99 VALUE 0.
           10  BBSTRAI-STATUS              PICTURE 99 VALUE 0.
           10  BANKGF-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  BBSREC-STATUS               PICTURE 99 VALUE 0.
           10  LISTE1-STATUS               PICTURE 99 VALUE 0.
           10  BBSFEIL-STATUS              PICTURE 99 VALUE 0.
           10  BBSUTT-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSTAB-EOF-OFF          VALUE '0'.
               88  BBSTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSTRAI-EOF-OFF         VALUE '0'.
               88  BBSTRAI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSTRAI-READ-OFF        VALUE '0'.
               88  BBSTRAI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSTRAI-PROCESS-OFF     VALUE '0'.
               88  BBSTRAI-PROCESS         VALUE '1'.
           05  BANKGF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  LISTE1-DATA-FIELDS.
               10  LISTE1-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-CLR-IO           PICTURE X VALUE 'Y'.
           05  CONSOLE-IO-AREA.
               10  CONSOLE-IO-AREA-X       PICTURE X(800).
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
           05  BBSTRAI-DATA-FIELDS.
               10  REC10                   PICTURE X(80).
               10  FORTJE                  PICTURE X(4).
               10  BBSEDB-IO.
                   15  BBSEDB              PICTURE S9(3).
               10  REC20                   PICTURE X(80).
               10  BBSKTO                  PICTURE X(11).
               10  BBSDAT                  PICTURE X(6).
               10  REC30                   PICTURE X(80).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  INKPUR                  PICTURE X(2).
               10  KIDF18                  PICTURE X(18).
               10  KIDKNR                  PICTURE X(6).
               10  KIDS07                  PICTURE X(7).
               10  KIDREF                  PICTURE X(6).
               10  REC31                   PICTURE X(80).
               10  REC88                   PICTURE X(80).
               10  REC88A-IO.
                   15  REC88A              PICTURE S9(8).
               10  REC89                   PICTURE X(80).
               10  REC89A-IO.
                   15  REC89A              PICTURE S9(8).
               10  REC89R-IO.
                   15  REC89R              PICTURE S9(8).
               10  RECDIV                  PICTURE X(80).
           05  BANKGF-DATA-FIELDS.
               10  TABFNR                  PICTURE X(3).
           05  FIRMAF-DATA-FIELDS.
      * **  SJEKKER AT FORMAT/TJENESTEKODE ER RETT FOR OCR-INNBET
      * **  - TJENESTE = 00 I START/SLUTT RECORD FOR FORSENDELSE
      * **  - TJENESTE = 09 FOR OCR-INNBETALINGER.
               10  FILLER                  PICTURE X.
           05  TEMPORARY-FIELDS.
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(5).
               10  ANTFEI-IO.
                   15  ANTFEI              PICTURE S9(5).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-80YN9R               PICTURE ZZZZZZZ9-.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BBSTRAI-PROCESS
               SET BBSTRAI-PROCESS-OFF     TO TRUE
               SET BBSTRAI-READ            TO TRUE
           END-IF
 
           IF  BBSTRAI-READ
           AND RECORD-SELECTED-OFF
               PERFORM BBSTRAI-GET
               SET BBSTRAI-READ-OFF        TO TRUE
               IF  NOT BBSTRAI-EOF
                   SET BBSTRAI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BBSTRAI-PROCESS
               PERFORM BBSTRAI-IDSET
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
 
           IF  BBSTRAI-PROCESS
               PERFORM BBSTRAI-FLDOFF
               PERFORM BBSTRAI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-19                    TO TRUE
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE
      *  06                MOVE "FORTJE  "BUGFL1  8        DISPLAY FIELD
      *  06      BUGFL1    DEBUGFLISTEO   FORTJE           VIS INDIKATOR
           SET NOT-I-40                    TO TRUE
           IF  FORTJE = 'NY00'
               SET I-40                    TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  FORTJE = 'NY09'
               SET I-41                    TO TRUE
           END-IF
           IF  (I-40)
               SET I-19                    TO TRUE
           END-IF
           IF  (I-41)
               SET I-19                    TO TRUE
           END-IF
           IF  (NOT-I-19 AND NOT-I-44)
               SET I-44                    TO TRUE
      * START TESTKODE **********************
           END-IF
           IF  (I-U1)
               SET NOT-I-70                TO TRUE
               IF  ANTFEI = 4
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND I-70)
               SET I-LR                    TO TRUE
               SET I-15                    TO TRUE
           END-IF
           IF  (I-U1 AND I-15)
               GO TO SLUTT-T
      * SLUTT TESTKODE **********************
           END-IF
           IF  (NOT-I-20 AND NOT-I-19)
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-19)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-15                TO TRUE
               SET NOT-I-16                TO TRUE
      * **  BBS-RECORDS.   ** *
           END-IF
           IF  (I-02)
               SET NOT-I-10                TO TRUE
               IF  BBSEDB = 117
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  BBSEDB = 510
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-02 AND NOT-I-44 AND NOT-I-10)
               SET I-44                    TO TRUE
      *
           END-IF
           MOVE 0                          TO X.
 
       BBSRUT-T.
           IF  (I-04)
               ADD 1                       TO X
           END-IF
           IF  (I-04 AND NOT-I-U7)
               SET NOT-I-17                TO TRUE
               IF  BBSDAT = ARA (X)
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-17)
               GO TO BBSEND-T
           END-IF
           IF  (I-04)
               SET NOT-I-18                TO TRUE
               IF  X = 20
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-18)
               GO TO BBSEND-T
           END-IF
           IF  (I-04)
               GO TO BBSRUT-T
           END-IF.
 
       BBSEND-T.
      *  04 17             SETON                     LR15
           IF  (I-04 AND I-17)
               SET I-15                    TO TRUE
           END-IF
           IF  (I-04 AND NOT-I-44 AND I-17)
               SET I-44                    TO TRUE
      *
           END-IF
           IF  (I-04 AND I-15)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               MOVE BBSKTO                 TO BANKGF-KEY1
               READ BANKGF RECORD KEY IS BANKGF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM BANKGF-FLDSET
                   PERFORM BANKGF-IDSET
               END-READ
           END-IF
           IF  (I-03 AND I-11)
               SET I-15                    TO TRUE
               SET I-16                    TO TRUE
           END-IF
           IF  (I-03 AND NOT-I-44 AND I-11)
               SET I-44                    TO TRUE
           END-IF
           IF  (I-03)
               MOVE TABFNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-31                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-31            TO TRUE
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-04)
               SET NOT-I-30                TO TRUE
               IF  INKPUR = '60'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  INKPUR = '61'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  INKPUR = '62'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  INKPUR = '63'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-04)
               SET NOT-I-51                TO TRUE
               IF  KIDS07 > '0000000'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-51)
               SET NOT-I-51                TO TRUE
               IF  KIDS07 NOT > '9999999'
                   SET I-51                TO TRUE
               END-IF
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           MOVE ' '                        TO BBEST
           MOVE 'BBS01'                    TO LONR
           MOVE '399'                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'BBS085  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0)
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-L0 AND NOT-I-19)
               ADD 1                       TO ANTFEI
           END-IF
           IF  (I-L0 AND I-15)
               ADD 1                       TO ANTFEI
           END-IF
           IF  (I-L0 AND I-19 AND NOT-I-15)
               ADD 1                       TO ANTUT
      *R                   MOVE "FORTJE  "BUGFL1  8        DISPLAY FIELD
      *R         BUGFL1    DEBUGFLISTEO   FORTJE           VIS INDIKATOR
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       BBSTRAI-GET SECTION.
       BBSTRAI-GET-P.
           IF  BBSTRAI-EOF-OFF
               READ BBSTRAI
               AT END
                   SET BBSTRAI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BBSTRAI-FLDOFF SECTION.
       BBSTRAI-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '3'
            AND   BBSTRAI-IO-AREA (8:1) = '0' )
               SET NOT-I-52                TO TRUE
           END-EVALUATE.
 
       BBSTRAI-FLDSET SECTION.
       BBSTRAI-FLDSET-P.
           EVALUATE TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '1' )
               MOVE BBSTRAI-IO-AREA (1:80) TO REC10 (1:80)
               MOVE BBSTRAI-IO-AREA (1:4)  TO FORTJE (1:4)
               MOVE BBSTRAI-IO-AREA (29:3) TO BBSEDB-IO
               INSPECT BBSEDB-IO REPLACING ALL ' ' BY '0'
           WHEN ( BBSTRAI-IO-AREA (7:1) = '2'
            AND   BBSTRAI-IO-AREA (8:1) = '0' )
               MOVE BBSTRAI-IO-AREA (1:80) TO REC20 (1:80)
               MOVE BBSTRAI-IO-AREA (1:4)  TO FORTJE (1:4)
               MOVE BBSTRAI-IO-AREA (25:11) TO BBSKTO (1:11)
           WHEN ( BBSTRAI-IO-AREA (7:1) = '3'
            AND   BBSTRAI-IO-AREA (8:1) = '0' )
               MOVE BBSTRAI-IO-AREA (16:6) TO BBSDAT (1:6)
               MOVE BBSTRAI-IO-AREA (1:80) TO REC30 (1:80)
               MOVE BBSTRAI-IO-AREA (1:4)  TO FORTJE (1:4)
               MOVE BBSTRAI-IO-AREA (41:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE BBSTRAI-IO-AREA (62:2) TO INKPUR (1:2)
               MOVE BBSTRAI-IO-AREA (50:18) TO KIDF18 (1:18)
               IF  KIDF18 = SPACES
                   SET I-52                TO TRUE
               END-IF
               MOVE BBSTRAI-IO-AREA (56:6) TO KIDKNR (1:6)
               MOVE BBSTRAI-IO-AREA (68:7) TO KIDS07 (1:7)
               MOVE BBSTRAI-IO-AREA (68:6) TO KIDREF (1:6)
           WHEN ( BBSTRAI-IO-AREA (7:1) = '3'
            AND   BBSTRAI-IO-AREA (8:1) = '1' )
               MOVE BBSTRAI-IO-AREA (1:80) TO REC31 (1:80)
           WHEN ( BBSTRAI-IO-AREA (7:1) = '8'
            AND   BBSTRAI-IO-AREA (8:1) = '8' )
               MOVE BBSTRAI-IO-AREA (1:80) TO REC88 (1:80)
               MOVE BBSTRAI-IO-AREA (1:4)  TO FORTJE (1:4)
               MOVE BBSTRAI-IO-AREA (9:8)  TO REC88A-IO
               INSPECT REC88A-IO REPLACING ALL ' ' BY '0'
           WHEN ( BBSTRAI-IO-AREA (7:1) = '8'
            AND   BBSTRAI-IO-AREA (8:1) = '9' )
               MOVE BBSTRAI-IO-AREA (1:80) TO REC89 (1:80)
               MOVE BBSTRAI-IO-AREA (1:4)  TO FORTJE (1:4)
               MOVE BBSTRAI-IO-AREA (9:8)  TO REC89A-IO
               INSPECT REC89A-IO REPLACING ALL ' ' BY '0'
               MOVE BBSTRAI-IO-AREA (17:8) TO REC89R-IO
               INSPECT REC89R-IO REPLACING ALL ' ' BY '0'
           WHEN OTHER
               MOVE BBSTRAI-IO-AREA (1:80) TO RECDIV (1:80)
               MOVE BBSTRAI-IO-AREA (1:4)  TO FORTJE (1:4)
           END-EVALUATE.
 
       BBSTRAI-IDSET SECTION.
       BBSTRAI-IDSET-P.
           EVALUATE TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '1' )
               SET I-02                    TO TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '2'
            AND   BBSTRAI-IO-AREA (8:1) = '0' )
               SET I-03                    TO TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '3'
            AND   BBSTRAI-IO-AREA (8:1) = '0' )
               SET I-04                    TO TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '3'
            AND   BBSTRAI-IO-AREA (8:1) = '1' )
               SET I-09                    TO TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '8'
            AND   BBSTRAI-IO-AREA (8:1) = '8' )
               SET I-05                    TO TRUE
           WHEN ( BBSTRAI-IO-AREA (7:1) = '8'
            AND   BBSTRAI-IO-AREA (8:1) = '9' )
               SET I-06                    TO TRUE
           WHEN  OTHER
               SET I-07                    TO TRUE
           END-EVALUATE.
 
       BANKGF-FLDSET SECTION.
       BANKGF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BANKGF-IO-AREA (12:3)  TO TABFNR (1:3)
           END-EVALUATE.
 
       BANKGF-IDSET SECTION.
       BANKGF-IDSET-P.
           SET I-08                        TO TRUE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
       LISTE1-PRINT-LINE SECTION.
       LISTE1-PRINT-LINE-P.
           IF  LISTE1-BEFORE-SKIP > 0
               PERFORM LISTE1-SKIP-BEFORE
           END-IF
           IF  LISTE1-BEFORE-SPACE > 0
               PERFORM LISTE1-SPACE-BEFORE
               IF  LISTE1-AFTER-SKIP > 0
                   PERFORM LISTE1-SKIP-AFTER
               END-IF
               IF  LISTE1-AFTER-SPACE > 0
                   PERFORM LISTE1-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE1-AFTER-SKIP > 0
                   PERFORM LISTE1-SKIP-AFTER
               END-IF
               PERFORM LISTE1-SPACE-AFTER
           END-IF
           IF  LISTE1-LINE-COUNT NOT < LISTE1-MAX-LINES
               MOVE 7                      TO LISTE1-AFTER-SKIP
           END-IF.
 
       LISTE1-SKIP-BEFORE SECTION.
       LISTE1-SKIP-BEFORE-P.
           WRITE LISTE1-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE1-LINE-COUNT
           MOVE 0                          TO LISTE1-BEFORE-SKIP
           INITIALIZE LISTE1-IO-AREA.
 
       LISTE1-SPACE-BEFORE SECTION.
       LISTE1-SPACE-BEFORE-P.
           WRITE LISTE1-IO-PRINT        AFTER LISTE1-BEFORE-SPACE LINES
           ADD LISTE1-BEFORE-SPACE         TO LISTE1-LINE-COUNT
           MOVE SPACES TO LISTE1-IO-AREA
           INITIALIZE LISTE1-IO-AREA
           MOVE 0                          TO LISTE1-BEFORE-SPACE.
 
       LISTE1-SKIP-AFTER SECTION.
       LISTE1-SKIP-AFTER-P.
           WRITE LISTE1-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE1-LINE-COUNT
           MOVE 0                          TO LISTE1-AFTER-SKIP
           INITIALIZE LISTE1-IO-AREA.
 
       LISTE1-SPACE-AFTER SECTION.
       LISTE1-SPACE-AFTER-P.
           WRITE LISTE1-IO-PRINT       BEFORE LISTE1-AFTER-SPACE LINES
           ADD LISTE1-AFTER-SPACE          TO LISTE1-LINE-COUNT
           INITIALIZE LISTE1-IO-AREA
           MOVE 0                          TO LISTE1-AFTER-SPACE.
 
       BBSTAB-LOAD SECTION.
       BBSTAB-LOAD-P.
           OPEN INPUT BBSTAB
           SET ARA-I                       TO 1
           PERFORM UNTIL BBSTAB-EOF
               READ BBSTAB
               AT END
                   SET BBSTAB-EOF          TO TRUE
               NOT AT END
                   MOVE BBSTAB-IO-AREA (1:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (7:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (13:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (19:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (25:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (31:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (37:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (43:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (49:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (55:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (61:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (67:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (73:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (79:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (85:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (91:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (97:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (103:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (109:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
                   MOVE BBSTAB-IO-AREA (115:6) TO ARA-ENTRY (ARA-I)
                   SET ARA-I               UP BY 1
               END-READ
           END-PERFORM
           CLOSE BBSTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND NOT-I-15 AND I-19)
               MOVE SPACES TO BBSREC-IO-AREA
               INITIALIZE BBSREC-IO-AREA
               MOVE REC20                  TO BBSREC-IO-AREA (1:80)
               MOVE TABFNR                 TO BBSREC-IO-AREA (57:3)
               MOVE BBSKTO                 TO BBSREC-IO-AREA (70:11)
               WRITE BBSREC-IO-AREA
           END-IF
           IF  (I-04 AND NOT-I-15 AND I-19)
               MOVE SPACES TO BBSREC-IO-AREA
               INITIALIZE BBSREC-IO-AREA
               MOVE REC30                  TO BBSREC-IO-AREA (1:80)
               IF  (NOT-I-30 AND NOT-I-31)
                   MOVE '60'               TO BBSREC-IO-AREA (62:2)
               END-IF
               IF  (I-51 AND I-52)
                   MOVE '  '               TO BBSREC-IO-AREA (62:2)
               END-IF
               IF  (I-51 AND I-52)
                   MOVE 'F'                TO BBSREC-IO-AREA (67:1)
               END-IF
               MOVE TABFNR                 TO BBSREC-IO-AREA (75:3)
               WRITE BBSREC-IO-AREA
           END-IF
           IF  (I-09 AND NOT-I-15 AND I-19)
               MOVE SPACES TO BBSREC-IO-AREA
               INITIALIZE BBSREC-IO-AREA
               MOVE REC31                  TO BBSREC-IO-AREA (1:80)
               MOVE TABFNR                 TO BBSREC-IO-AREA (75:3)
               WRITE BBSREC-IO-AREA
           END-IF
           IF  (I-05 AND NOT-I-15 AND I-19)
               MOVE SPACES TO BBSREC-IO-AREA
               INITIALIZE BBSREC-IO-AREA
               MOVE REC88                  TO BBSREC-IO-AREA (1:80)
               MOVE TABFNR                 TO BBSREC-IO-AREA (78:3)
               WRITE BBSREC-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE REC10                  TO LISTE1-IO-AREA (1:80)
               MOVE 'START FORSENDELSE.'   TO LISTE1-IO-AREA (83:18)
               IF  (I-10)
                   MOVE 'RIKTIG'           TO LISTE1-IO-AREA (102:6)
               END-IF
               IF  (NOT-I-10)
                   MOVE 'FEIL'             TO LISTE1-IO-AREA (104:4)
               END-IF
               MOVE 'DATASENTRALNR:'       TO LISTE1-IO-AREA (109:14)
               MOVE BBSEDB-IO              TO LISTE1-IO-AREA (124:3)
               MOVE '.'                    TO LISTE1-IO-AREA (127:1)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-03)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE REC20                  TO LISTE1-IO-AREA (1:80)
               MOVE 'START OPPDRAG.'       TO LISTE1-IO-AREA (83:14)
               MOVE 'KTO/FNR:'             TO LISTE1-IO-AREA (98:8)
               MOVE BBSKTO                 TO LISTE1-IO-AREA (107:11)
               MOVE '/'                    TO LISTE1-IO-AREA (118:1)
               IF  (NOT-I-16)
                   MOVE TABFNR             TO LISTE1-IO-AREA (119:3)
               END-IF
               IF  (I-16)
                   MOVE 'UKJENT.'          TO LISTE1-IO-AREA (123:7)
               END-IF
               IF  (NOT-I-16)
                   MOVE 'O.K.   '          TO LISTE1-IO-AREA (123:7)
               END-IF
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-04 AND I-15 AND I-16)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'RECORD ART 30:>'      TO LISTE1-IO-AREA (1:15)
               MOVE REC30                  TO LISTE1-IO-AREA (16:80)
               MOVE '<'                    TO LISTE1-IO-AREA (96:1)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-04 AND I-17)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'RECORD ART 30:>'      TO LISTE1-IO-AREA (1:15)
               MOVE REC30                  TO LISTE1-IO-AREA (16:80)
               MOVE '<'                    TO LISTE1-IO-AREA (96:1)
               MOVE '* FEIL I DATO/KJØRT FØR ' TO LISTE1-IO-AREA
                                                               (97:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (NOT-I-19 AND I-44)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               IF  (I-02)
                   MOVE REC10              TO LISTE1-IO-AREA (3:80)
               END-IF
               IF  (I-03)
                   MOVE REC20              TO LISTE1-IO-AREA (3:80)
               END-IF
               IF  (I-04)
                   MOVE REC30              TO LISTE1-IO-AREA (3:80)
               END-IF
               IF  (I-09)
                   MOVE REC31              TO LISTE1-IO-AREA (3:80)
               END-IF
               IF  (I-05)
                   MOVE REC88              TO LISTE1-IO-AREA (3:80)
               END-IF
               IF  (I-06)
                   MOVE REC89              TO LISTE1-IO-AREA (3:80)
               END-IF
               IF  (I-07)
                   MOVE RECDIV             TO LISTE1-IO-AREA (3:80)
               END-IF
               MOVE '* FEIL FORMAT/TJEN KODE ' TO LISTE1-IO-AREA
                                                               (97:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-04 AND I-15 AND I-16)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               IF  (I-51 AND I-52)
                   MOVE 'FAKTURANR: '      TO LISTE1-IO-AREA (1:11)
               END-IF
               IF  (I-51 AND I-52)
                   MOVE KIDREF             TO LISTE1-IO-AREA (12:6)
               END-IF
               IF  (I-51 AND I-52)
                   MOVE ' (FAKTURAKID).'   TO LISTE1-IO-AREA (18:14)
               END-IF
               IF  (NOT-I-52)
                   MOVE 'KUNDENR: '        TO LISTE1-IO-AREA (1:9)
               END-IF
               IF  (NOT-I-52)
                   MOVE KIDKNR             TO LISTE1-IO-AREA (10:6)
               END-IF
               IF  (NOT-I-52)
                   MOVE ' (GIROKID).'      TO LISTE1-IO-AREA (16:11)
               END-IF
               MOVE ' BELØP:'              TO LISTE1-IO-AREA (32:7)
               MOVE BEL                    TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE1-IO-AREA (40:13)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-05)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE REC88                  TO LISTE1-IO-AREA (1:80)
               MOVE 'SLUTT OPPDRAG.'       TO LISTE1-IO-AREA (83:14)
               MOVE 'ANT. TRANSER:'        TO LISTE1-IO-AREA (98:13)
               MOVE REC88A                 TO XO-80YN9R
               MOVE XO-80YN9R              TO LISTE1-IO-AREA (112:9)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-06)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE REC89                  TO LISTE1-IO-AREA (1:80)
               MOVE 'SLUTT FORSENDELSE.'   TO LISTE1-IO-AREA (83:18)
               MOVE 'TRANS/RECS:'          TO LISTE1-IO-AREA (102:11)
               MOVE REC89A                 TO XO-80YN9R
               MOVE XO-80YN9R              TO LISTE1-IO-AREA (112:9)
               MOVE '/'                    TO LISTE1-IO-AREA (121:1)
               MOVE REC89R                 TO XO-80YN9R
               MOVE XO-80YN9R              TO LISTE1-IO-AREA (121:9)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               IF  (NOT-I-15)
                   MOVE '** FEILFRI OVERFØRING **' TO LISTE1-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-15)
                   MOVE 'FEIL. RETT OPP - KJØR OM' TO LISTE1-IO-AREA
                                                                (1:24)
               END-IF
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-44)
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '* ÅÅ JOB JNM=BBS02M,CLAS' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE 'S=5,DISP=D,PRI=9        ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '* ÅÅ LST LST=SYSLST,LST=' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE '00E,CLASS=Z,DISP=H      ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '// JOB BBS02M  SENDER MA' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE 'IL M/BBS OCR TIL OPERATØ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               MOVE 'R                       ' TO BBSFEIL-IO-AREA
                                                               (49:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '/*'                   TO BBSFEIL-IO-AREA (1:2)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '*  STEP 01 TELNET-CLIENT' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE ' MAIL FRA HOST.         ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '* ÅÅ LST CLASS=Z,DISP=H ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE '                        ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '// EXEC EMAIL,SIZE=E' TO BBSFEIL-IO-AREA (1:20)
               MOVE 'MAIL,PARM='           TO BBSFEIL-IO-AREA (21:10)
               MOVE '''ID=02'''              TO BBSFEIL-IO-AREA (31:7)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET FROM=REGNSKAP@AUTODA' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE 'TA.NO                   ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
           END-IF
           IF  (I-LR AND I-44 AND NOT-I-U6)
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET TO=OPERA@AUTODATA.NO' TO BBSFEIL-IO-AREA
                                                                (1:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET COPY=SVERRE@AUTODATA' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE '.NO                     ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET COPY=KNUT@AUTODATA.N' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE 'O                       ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
           END-IF
           IF  (I-LR AND I-44 AND I-U6)
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET TO=ELIN@AUTODATA.NO ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET COPY=MORTEN@AUTODATA' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE '.NO                     ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
           END-IF
           IF  (I-LR AND I-44)
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET SUBJECT=BBS02 - OCR ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE 'INNBETALINGER           ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SET DISP=HOLD           ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE '                        ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'TEXT                    ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'FEIL I KJØRINGEN - SJEKK' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE ' OPPGAVE BBS01 PÅ REPORT' TO BBSFEIL-IO-AREA
                                                               (25:24)
               MOVE ' WEB                    ' TO BBSFEIL-IO-AREA
                                                               (49:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '/+                      ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE '                        ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'SEND                    ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE 'QUIT                    ' TO BBSFEIL-IO-AREA
                                                                (1:24)
               MOVE '                        ' TO BBSFEIL-IO-AREA
                                                               (25:24)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '/*'                   TO BBSFEIL-IO-AREA (1:2)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '/&'                   TO BBSFEIL-IO-AREA (1:2)
               WRITE BBSFEIL-IO-AREA
               MOVE SPACES TO BBSFEIL-IO-AREA
               INITIALIZE BBSFEIL-IO-AREA
               MOVE '* ÅÅ EOJ'             TO BBSFEIL-IO-AREA (1:8)
               WRITE BBSFEIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'KONTROLL OG FEILLISTE FO' TO LISTE1-IO-AREA (1:24)
               MOVE 'R OCR INNBETALINGER FRA ' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE 'BBS. FRAMSTILT:'      TO LISTE1-IO-AREA (49:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (68:8)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'BBS-RECORDS:'         TO LISTE1-IO-AREA (1:12)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-15)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '*** KJØRING AVBRUDT  ***' TO LISTE1-IO-AREA (1:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '*** SJEKK FEILLISTER ***' TO LISTE1-IO-AREA (1:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'ANTALL INN-RECORDS:'  TO LISTE1-IO-AREA (1:19)
               MOVE ANTINN                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE1-IO-AREA (25:6)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'ANTALL FEILRECORDS:'  TO LISTE1-IO-AREA (1:19)
               MOVE ANTFEI                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE1-IO-AREA (25:6)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'ANTALL UT-RECORDS :'  TO LISTE1-IO-AREA (1:19)
               MOVE ANTUT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE1-IO-AREA (25:6)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-98 AND I-U8)
           AND (I-08 AND I-06 AND I-10)
           AND (I-09)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE PSDS                   TO LISTE1-IO-AREA (41:80)
               MOVE R                      TO LISTE1-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE1-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE1-IO-AREA (116:5)
               MOVE LONR                   TO LISTE1-IO-AREA (116:5)
               MOVE LFIRMA                 TO LISTE1-IO-AREA (118:3)
               MOVE LUNDGR                 TO LISTE1-IO-AREA (118:3)
               MOVE LPROG                  TO LISTE1-IO-AREA (113:8)
               MOVE LOPNVN                 TO LISTE1-IO-AREA (86:35)
               MOVE LANTX-IO               TO LISTE1-IO-AREA (118:3)
               MOVE LPRIID                 TO LISTE1-IO-AREA (117:4)
               MOVE FINAVN                 TO LISTE1-IO-AREA (91:30)
               MOVE BJOBN                  TO LISTE1-IO-AREA (113:8)
               MOVE BBEST                  TO LISTE1-IO-AREA (120:1)
               MOVE BPERS                  TO LISTE1-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE1-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE1-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE1-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE1-IO-AREA (118:3)
               MOVE BPCLAS                 TO LISTE1-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE1-IO-AREA (118:3)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND NOT-I-U7)
               MOVE SPACES TO BBSUTT-IO-AREA
               INITIALIZE BBSUTT-IO-AREA
               MOVE ARA (2)                TO BBSUTT-IO-AREA (1:6)
               MOVE ARA (3)                TO BBSUTT-IO-AREA (7:6)
               MOVE ARA (4)                TO BBSUTT-IO-AREA (13:6)
               MOVE ARA (5)                TO BBSUTT-IO-AREA (19:6)
               MOVE ARA (6)                TO BBSUTT-IO-AREA (25:6)
               MOVE ARA (7)                TO BBSUTT-IO-AREA (31:6)
               MOVE ARA (8)                TO BBSUTT-IO-AREA (37:6)
               MOVE ARA (9)                TO BBSUTT-IO-AREA (43:6)
               MOVE ARA (10)               TO BBSUTT-IO-AREA (49:6)
               MOVE ARA (11)               TO BBSUTT-IO-AREA (55:6)
               MOVE ARA (12)               TO BBSUTT-IO-AREA (61:6)
               MOVE ARA (13)               TO BBSUTT-IO-AREA (67:6)
               MOVE ARA (14)               TO BBSUTT-IO-AREA (73:6)
               MOVE ARA (15)               TO BBSUTT-IO-AREA (79:6)
               MOVE ARA (16)               TO BBSUTT-IO-AREA (85:6)
               MOVE ARA (17)               TO BBSUTT-IO-AREA (91:6)
               MOVE ARA (18)               TO BBSUTT-IO-AREA (97:6)
               MOVE ARA (19)               TO BBSUTT-IO-AREA (103:6)
               MOVE ARA (20)               TO BBSUTT-IO-AREA (109:6)
               MOVE BBSDAT                 TO BBSUTT-IO-AREA (115:6)
               WRITE BBSUTT-IO-AREA
           END-IF
           IF  (I-LR AND I-15)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE 'CANCEL '              TO CONSOLE-IO-AREA (1:7)
               MOVE 'FEIL DATO, SJEKK S'   TO CONSOLE-IO-AREA (9:18)
               MOVE 'ISTE SENDING.  '      TO CONSOLE-IO-AREA (27:15)
               MOVE 'KONTO:'               TO CONSOLE-IO-AREA (42:6)
               MOVE BBSKTO                 TO CONSOLE-IO-AREA (49:11)
               IF  (I-17)
                   MOVE BBSDAT             TO CONSOLE-IO-AREA (62:6)
               END-IF
               DISPLAY CONSOLE-IO-AREA
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
           PERFORM BBSTAB-LOAD
           INITIALIZE BBSTRAI-DATA-FIELDS
           SET BBSTRAI-EOF-OFF             TO TRUE
           SET BBSTRAI-PROCESS             TO TRUE
           OPEN INPUT BBSTRAI
           INITIALIZE BANKGF-DATA-FIELDS
           OPEN INPUT BANKGF
           OPEN INPUT FIRMAF
           OPEN OUTPUT BBSREC
           OPEN OUTPUT LISTE1
           INITIALIZE LISTE1-IO-AREA
           INITIALIZE LISTE1-DATA-FIELDS
           MOVE 57                         TO LISTE1-MAX-LINES
           OPEN OUTPUT BBSFEIL
           OPEN OUTPUT BBSUTT.
           SET ARA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BBSTRAI
           CLOSE BANKGF
           CLOSE FIRMAF
           CLOSE BBSREC
           IF LISTE1-IO-AREA NOT = SPACES
             WRITE LISTE1-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE1-IO-AREA
           END-IF
           CLOSE LISTE1
           CLOSE BBSFEIL
           CLOSE BBSUTT.
 
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
