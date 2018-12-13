       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS010R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: VOS010, PLUKKER UT VERKSTEDORDRE TIL SANERING*
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VSAMUKE                                      *
      *  LAGET DATO....: 27.06.95                                     *
      *  ENDRET........: 27.12.99 SKRIVER KVITT TOT NÅR U1 ER PÅ.     *
      *                           SKRIVER KVITT DET NÅR U2 ER PÅ.     *
      *                  06.04.01 SANERER SLETTEDE FIRMA.             *
      *                  28.05.03 SANERER VERSTEDORDRE NÅR SLETTEKODE *
      *                           I FIRM ER SATT TIL V                *
      *  RETTET........:                                              *
      *  INPUT.........: SEKVENSIELL VERKSTED ORDRE FILE (VERK030).   *
      *  BEHANDLING....: LESER VERK030 (SEKV.) MED RECORDART=H4, DEN  *
      *                  ARTEN SOM INNEHOLDER ORDREDATO.              *
      *                  FINNER SANERINGSDATO HJA ORDREDATO+LAGRINGS- *
      *                  TID. SKRIVER FILE MED KEY FOR Å SLETTE +     *
      *                  SANERINGSDATA FOR UTSKRIFT I VOS020.         *
      *  OUTPUT........: KEYFILE FOR SANERING AV VERKSTED ORDRE FILE. *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS010.rpg
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
           SELECT VERK030
               ASSIGN TO UT-S-VERK030
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERK030-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT VERKSAN
               ASSIGN TO UT-S-VERKSAN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKSAN-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VERK030
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  VERK030-IO-AREA.
           05  VERK030-IO-AREA-X           PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VERKSAN
               BLOCK CONTAINS 6000
               RECORD CONTAINS 60.
       01  VERKSAN-IO-AREA.
           05  VERKSAN-IO-AREA-X           PICTURE X(60).
      *FLISTEO O   F  80  80            PRINTERSYSLST
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VERK030-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  VERKSAN-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VERK030-EOF-OFF         VALUE '0'.
               88  VERK030-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERK030-READ-OFF        VALUE '0'.
               88  VERK030-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERK030-PROCESS-OFF     VALUE '0'.
               88  VERK030-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VERK030-LEVEL-INIT-OFF  VALUE '0'.
               88  VERK030-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KLISTEO-DATA-FIELDS.
               10  KLISTEO-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-CLR-IO          PICTURE X VALUE 'Y'.
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
               10  LFIRNR                  PICTURE X(3).
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
               10  LFIRNA                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BFILL                   PICTURE X(157).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
           05  VERK030-LEVEL-01.
               10  VERK030-01-L1.
                   15  VERK030-01-L1-VOFNR PICTURE X(3).
           05  VERK030-DATA-FIELDS.
               10  VOKEY                   PICTURE X(9).
               10  VOFNR                   PICTURE X(3).
               10  VOONR                   PICTURE X(6).
               10  LAGTID                  PICTURE X(2).
               10  ONR                     PICTURE X(6).
               10  OAAR                    PICTURE X(4).
               10  OMND                    PICTURE X(2).
               10  ODAG                    PICTURE X(2).
               10  ODATO                   PICTURE X(8).
               10  OSTAT                   PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
      *****************************************************************
      * HOUSEKEEPING.                                                 *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  F6-IO.
                   15  F6                  PICTURE S9(6).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DDATO-IO.
                   15  DDATO               PICTURE S9(6).
               10  DDATO6                  PICTURE X(6).
               10  DDX8                    PICTURE X(8).
               10  DDN8-IO.
                   15  DDN8                PICTURE S9(8).
               10  DDX4                    PICTURE X(4).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  ODX4                    PICTURE X(4).
               10  ODN8-IO.
                   15  ODN8                PICTURE S9(8).
               10  SDN8-IO.
                   15  SDN8                PICTURE S9(8).
               10  LAGMND-IO.
                   15  LAGMND              PICTURE S9(2).
               10  AARNUM-IO.
                   15  AARNUM              PICTURE S9(4).
               10  ANTMND-IO.
                   15  ANTMND              PICTURE S9(7).
               10  MNDNUM-IO.
                   15  MNDNUM              PICTURE S9(2).
               10  SANAAR                  PICTURE X(4).
               10  ANTMN2-IO.
                   15  ANTMN2              PICTURE S9(7).
               10  MNDNR-IO.
                   15  MNDNR               PICTURE S9(2).
               10  SANMND                  PICTURE X(2).
               10  SDX4                    PICTURE X(4).
               10  SDX8                    PICTURE X(8).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  EDIT-ODN8               PICTURE ZZ/ZZ/ZZZZ.
               10  EDIT-SDN8               PICTURE ZZ/ZZ/ZZZZ.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VERK030-PROCESS
               SET VERK030-PROCESS-OFF     TO TRUE
               SET VERK030-READ            TO TRUE
           END-IF
 
           IF  VERK030-READ
           AND RECORD-SELECTED-OFF
               PERFORM VERK030-GET
               SET VERK030-READ-OFF        TO TRUE
               IF  NOT VERK030-EOF
                   SET VERK030-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VERK030-PROCESS
               PERFORM VERK030-IDSET
           END-IF
 
           IF  VERK030-PROCESS
               PERFORM VERK030-CHK-LEVEL
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
 
           IF  VERK030-PROCESS
               PERFORM VERK030-FLDOFF
               PERFORM VERK030-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VERK030-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-99)
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET I-98                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE UDATE                  TO F6-IO
               MOVE F6 (1:2)               TO DD
               MOVE F6 (5:2)               TO AA-IO
      ** MLLzo
               IF AA < 0
                   MULTIPLY -1 BY AA
               END-IF
               MOVE F6                     TO DDATO-IO
               MOVE AA                     TO DDATO (1:2)
               MOVE DD                     TO DDATO-IO (5:2)
      ** MLLzo
               IF DD < 0
                   MULTIPLY -1 BY DD
               END-IF
               MOVE DDATO                  TO DDATO6
               MOVE DDATO6                 TO DDX8 (3:6)
               SET NOT-I-97                TO TRUE
               IF  AA < 94
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-99 AND NOT-I-97)
               MOVE '19'                   TO DDX8 (1:2)
           END-IF
           IF  (I-99 AND I-97)
               MOVE '20'                   TO DDX8 (1:2)
           END-IF
           IF  (I-99)
               MOVE F6                     TO DDN8 (1:6)
               MOVE AA                     TO DDX4 (3:2)
           END-IF
           IF  (I-99 AND NOT-I-97)
               MOVE '19'                   TO DDX4 (1:2)
           END-IF
           IF  (I-99 AND I-97)
               MOVE '20'                   TO DDX4 (1:2)
           END-IF
           IF  (I-99)
               MOVE DDX4                   TO DDN8-IO (5:4)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      *****************************************************************
      * NYTT FIRMA, NY SIDE.                                          *
      *****************************************************************
           END-IF
           IF  (I-L1 AND I-U2)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L1)
               PERFORM FISLET-S
      *****************************************************************
      * RECORDART ANNET ENN H4 OG ORDRESTATUS 5 (FERDIG PÅ VERKSTEDET)*
      * SKAL IKKE SANERES.                                            *
      *****************************************************************
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  (I-95)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-50)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  OSTAT = '2'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  OSTAT = '3'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  OSTAT = '4'
               SET I-14                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  OSTAT = '5'
               SET I-15                    TO TRUE
           END-IF
           MOVE ODAG                       TO ODX4 (1:2)
           MOVE OMND                       TO ODX4 (3:2)
           MOVE ODX4                       TO ODN8 (1:4)
           MOVE OAAR                       TO ODN8-IO (5:4)
      *****************************************************************
      * BEREGNER SANERINGSDATO                                        *
      *****************************************************************
      * HVIS LAGRINGSTID IKKE ER REGISTRERT, SANERES ORDRE.           *
           SET NOT-I-50                    TO TRUE
           IF  LAGTID < '01'
               SET I-50                    TO TRUE
           END-IF
           IF  (NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  LAGTID > '60'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-50)
               MOVE DDN8                   TO SDN8-IO
               GO TO SLUTT-T
      * LAGRINGSTID BEREGNES VED Å MULTIPLISERE ANTALL ÅR MED 12, FOR *
      * DERETTER Å LEGGE TIL ANTALL LAGRINGSMÅNEDER.                  *
           END-IF
           MOVE LAGTID                     TO LAGMND-IO
           MOVE OAAR                       TO AARNUM-IO
           MULTIPLY 12 BY AARNUM       GIVING ANTMND
           ADD LAGMND                      TO ANTMND
           MOVE OMND                       TO MNDNUM-IO
           ADD MNDNUM                      TO ANTMND
      * ÅRSTALLET FOR SANERING FINNES VED Å DIVIDERE ANTALL MÅNEDER  *
      * PÅ 12 UTEN AVRUNDING. REST ANTALL MÅNEDER FINNES VED Å SUB-  *
      * TRAHERE ANTALL MÅNEDER FOR SANERINGSDATO FRA ANTALL MÅNEDER  *
      * FOR ORDREDATO.                                               *
           DIVIDE ANTMND BY 12         GIVING AARNUM
           MOVE AARNUM                     TO SANAAR
           MULTIPLY 12 BY AARNUM       GIVING ANTMN2
           SUBTRACT ANTMN2 FROM ANTMND GIVING MNDNR
           MOVE MNDNR                      TO SANMND
           MOVE SANMND                     TO SDX4 (1:2)
           MOVE ODAG                       TO SDX4 (3:2)
           MOVE SANAAR                     TO SDX8 (1:4)
           MOVE SDX4                       TO SDX8 (5:4)
      * SJEKKER OM LAGRINGSTID ER UTLØPT FOR VERKSTEDFERDIGE OBJEKTER *
           IF  (I-15)
               SET NOT-I-50                TO TRUE
               IF  DDX8 > SDX8
                   SET I-50                TO TRUE
               END-IF
      * REDIGERER SANERINGSDATO NUMERISK.                             *
           END-IF
           MOVE ODAG                       TO SDX4 (1:2)
           MOVE SANMND                     TO SDX4 (3:2)
           MOVE SDX4                       TO SDN8 (1:4)
           MOVE SANAAR                     TO SDN8-IO (5:4)
      *
      *                    MOVE "SDX8    "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGFLISTEO   SDX8             VIS INDIKATOR
      *
           .
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-80                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'VOS01'                    TO LONR
           MOVE VOFNR                      TO LFIRNR
           MOVE '000'                      TO LUNDGR
           MOVE 'VOS010  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-80                    TO TRUE
           IF  LANTX = 0
               SET I-80                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-95                    TO TRUE
           SET NOT-I-95                    TO TRUE
           IF  VOFNR = '   '
               SET I-95                    TO TRUE
           END-IF
           IF  (I-95)
               GO TO FIEND-T
           END-IF
           MOVE VOFNR                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-96)
               SET NOT-I-95                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-96 AND NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMSL = 'V'
                   SET I-95                TO TRUE
               END-IF
           END-IF.
 
       FIEND-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-50)
               ADD 1                       TO ANTORD
      ******************************************************************
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       VERK030-GET SECTION.
       VERK030-GET-P.
           IF  VERK030-EOF-OFF
               READ VERK030
               AT END
                   SET VERK030-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VERK030-FLDOFF SECTION.
       VERK030-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( VERK030-IO-AREA (10:1) = 'H'
            AND   VERK030-IO-AREA (11:1) = '4' )
               SET NOT-I-62                TO TRUE
               SET NOT-I-61                TO TRUE
           END-EVALUATE.
 
       VERK030-FLDSET SECTION.
       VERK030-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VERK030-IO-AREA (10:1) = 'H'
            AND   VERK030-IO-AREA (11:1) = '4' )
               MOVE VERK030-IO-AREA (1:9)  TO VOKEY (1:9)
               MOVE VERK030-IO-AREA (1:3)  TO VOFNR (1:3)
               MOVE VERK030-IO-AREA (4:6)  TO VOONR (1:6)
               MOVE VERK030-IO-AREA (91:2) TO LAGTID (1:2)
               IF  LAGTID = SPACES
                   SET I-62                TO TRUE
               END-IF
               MOVE VERK030-IO-AREA (119:6) TO ONR (1:6)
               MOVE VERK030-IO-AREA (129:4) TO OAAR (1:4)
               MOVE VERK030-IO-AREA (133:2) TO OMND (1:2)
               MOVE VERK030-IO-AREA (135:2) TO ODAG (1:2)
               MOVE VERK030-IO-AREA (129:8) TO ODATO (1:8)
               IF  ODATO = SPACES
                   SET I-61                TO TRUE
               END-IF
               MOVE VERK030-IO-AREA (159:1) TO OSTAT (1:1)
           END-EVALUATE.
 
       VERK030-IDSET SECTION.
       VERK030-IDSET-P.
           EVALUATE TRUE
           WHEN ( VERK030-IO-AREA (10:1) = 'H'
            AND   VERK030-IO-AREA (11:1) = '4' )
               SET I-01                    TO TRUE
           WHEN  OTHER
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       VERK030-CHK-LEVEL SECTION.
       VERK030-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( VERK030-IO-AREA (10:1) = 'H'
            AND   VERK030-IO-AREA (11:1) = '4' )
               MOVE LOW-VALUES             TO VERK030-LEVEL-01
               MOVE VERK030-IO-AREA (1:3)  TO VERK030-01-L1-VOFNR
               IF  VERK030-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERK030-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VERK030-01-L1         TO THE-PRIOR-L1
               SET VERK030-LEVEL-INIT      TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
       KLISTEO-PRINT-LINE SECTION.
       KLISTEO-PRINT-LINE-P.
           IF  KLISTEO-BEFORE-SKIP > 0
               PERFORM KLISTEO-SKIP-BEFORE
           END-IF
           IF  KLISTEO-BEFORE-SPACE > 0
               PERFORM KLISTEO-SPACE-BEFORE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               IF  KLISTEO-AFTER-SPACE > 0
                   PERFORM KLISTEO-SPACE-AFTER
               END-IF
           ELSE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               PERFORM KLISTEO-SPACE-AFTER
           END-IF
           IF  KLISTEO-LINE-COUNT NOT < KLISTEO-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       KLISTEO-SKIP-BEFORE SECTION.
       KLISTEO-SKIP-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-BEFORE-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-BEFORE SECTION.
       KLISTEO-SPACE-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER KLISTEO-BEFORE-SPACE
                                                                 LINES
           ADD KLISTEO-BEFORE-SPACE        TO KLISTEO-LINE-COUNT
           MOVE SPACES TO KLISTEO-IO-AREA
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-BEFORE-SPACE.
 
       KLISTEO-SKIP-AFTER SECTION.
       KLISTEO-SKIP-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-AFTER-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-AFTER SECTION.
       KLISTEO-SPACE-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE KLISTEO-AFTER-SPACE LINES
           ADD KLISTEO-AFTER-SPACE         TO KLISTEO-LINE-COUNT
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO VERKSAN-IO-AREA
               INITIALIZE VERKSAN-IO-AREA
               MOVE VOKEY                  TO VERKSAN-IO-AREA (1:9)
               MOVE ODATO                  TO VERKSAN-IO-AREA (10:8)
               MOVE LAGTID                 TO VERKSAN-IO-AREA (18:2)
               MOVE SDX8                   TO VERKSAN-IO-AREA (20:8)
               MOVE DDX8                   TO VERKSAN-IO-AREA (28:8)
               MOVE ONR                    TO VERKSAN-IO-AREA (36:6)
               WRITE VERKSAN-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-80 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE VOONR                  TO KLISTEO-IO-AREA (1:6)
               IF  (NOT-I-61)
                   MOVE ODN8               TO EDIT-ODN8
                   MOVE EDIT-ODN8          TO KLISTEO-IO-AREA (11:10)
               END-IF
               MOVE '     '                TO KLISTEO-IO-AREA (26:5)
               IF  (NOT-I-62)
                   MOVE LAGTID             TO KLISTEO-IO-AREA (25:2)
               END-IF
               IF  (NOT-I-62)
                   MOVE 'MND'              TO KLISTEO-IO-AREA (28:3)
               END-IF
               MOVE '          '           TO KLISTEO-IO-AREA (33:10)
               IF  (NOT-I-62)
                   MOVE SDN8               TO EDIT-SDN8
                   MOVE EDIT-SDN8          TO KLISTEO-IO-AREA (33:10)
      *                  N62   SDX8      42
               END-IF
               MOVE '       '              TO KLISTEO-IO-AREA (49:7)
               IF  (I-15)
                   MOVE ONR                TO KLISTEO-IO-AREA (50:6)
      *                        DDX8      55
               END-IF
               MOVE 'VERKSTEDJOBB IKKE FERDIG' TO KLISTEO-IO-AREA
                                                               (58:24)
               IF  (I-15)
                   MOVE 'DATO IKKE UTLØPT        ' TO KLISTEO-IO-AREA
                                                               (58:24)
               END-IF
               IF  (I-50)
                   MOVE '******* SLETTES ******* ' TO KLISTEO-IO-AREA
                                                               (58:24)
               END-IF
               MOVE '               '      TO KLISTEO-IO-AREA (84:15)
               IF  (I-12)
                   MOVE 'ORDRENR TILDELT'  TO KLISTEO-IO-AREA (84:15)
               END-IF
               IF  (I-13)
                   MOVE 'ARB ORD SKREVET'  TO KLISTEO-IO-AREA (84:15)
               END-IF
               IF  (I-14)
                   MOVE 'JOBB STARTET   '  TO KLISTEO-IO-AREA (84:15)
               END-IF
               IF  (I-15)
                   MOVE 'JOBB FERDIG    '  TO KLISTEO-IO-AREA (84:15)
               END-IF
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-01 AND I-02 AND I-80)
           AND (I-81 AND I-99 AND I-03)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE LONR                   TO KLISTEO-IO-AREA (1:5)
               MOVE LFIRNR                 TO KLISTEO-IO-AREA (6:3)
               MOVE LUNDGR                 TO KLISTEO-IO-AREA (9:3)
               MOVE LPROG                  TO KLISTEO-IO-AREA (12:8)
               MOVE LPRIID                 TO KLISTEO-IO-AREA (47:4)
               MOVE BBEST                  TO KLISTEO-IO-AREA (51:1)
               MOVE PSDS                   TO KLISTEO-IO-AREA (1:60)
               MOVE R                      TO KLISTEO-IO-AREA (63:8)
               MOVE P-IO                   TO KLISTEO-IO-AREA (78:3)
               MOVE S-IO                   TO KLISTEO-IO-AREA (86:5)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1 AND NOT-I-80 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV VERKSTED-ORDRE.      ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (67:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (77:8)
               MOVE 'JOB=VERK030'          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=VOS010'       TO KLISTEO-IO-AREA (107:14)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE LFIRNR                 TO KLISTEO-IO-AREA (1:3)
               MOVE LFIRNA                 TO KLISTEO-IO-AREA (5:30)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTED'             TO KLISTEO-IO-AREA (1:8)
               MOVE 'ORDRE       '         TO KLISTEO-IO-AREA (11:12)
               MOVE 'ORDRE '               TO KLISTEO-IO-AREA (25:6)
               MOVE 'ORDRE '               TO KLISTEO-IO-AREA (50:6)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ORDRENR '             TO KLISTEO-IO-AREA (1:8)
               MOVE 'DATO        '         TO KLISTEO-IO-AREA (11:12)
               MOVE 'LAGRES'               TO KLISTEO-IO-AREA (25:6)
               MOVE 'SANERINGSDATO'        TO KLISTEO-IO-AREA (33:13)
               MOVE 'NR    '               TO KLISTEO-IO-AREA (50:6)
               MOVE 'MERKNAD                 ' TO KLISTEO-IO-AREA
                                                               (58:24)
               MOVE 'ORDRE-STATUS   '      TO KLISTEO-IO-AREA (84:15)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-80 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV VERKSTED-ORDRE.      ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (67:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (77:8)
               MOVE 'JOB=VERK030'          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=VOS010'       TO KLISTEO-IO-AREA (107:14)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE LFIRNR                 TO KLISTEO-IO-AREA (1:3)
               MOVE LFIRNA                 TO KLISTEO-IO-AREA (5:30)
               IF  I-U1
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTED'             TO KLISTEO-IO-AREA (1:8)
               MOVE 'ORDRE       '         TO KLISTEO-IO-AREA (11:12)
               MOVE 'ORDRE '               TO KLISTEO-IO-AREA (25:6)
               MOVE 'ORDRE '               TO KLISTEO-IO-AREA (50:6)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ORDRENR '             TO KLISTEO-IO-AREA (1:8)
               MOVE 'DATO        '         TO KLISTEO-IO-AREA (11:12)
               MOVE 'LAGRES'               TO KLISTEO-IO-AREA (25:6)
               MOVE 'SANERINGSDATO'        TO KLISTEO-IO-AREA (33:13)
               MOVE 'NR    '               TO KLISTEO-IO-AREA (50:6)
               MOVE 'MERKNAD                 ' TO KLISTEO-IO-AREA
                                                               (58:24)
               MOVE 'ORDRE-STATUS   '      TO KLISTEO-IO-AREA (84:15)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U1)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV VERKSTED-ORDRE.      ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (67:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (77:8)
               MOVE 'JOB=VERK030'          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=VOS010'       TO KLISTEO-IO-AREA (107:14)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-BEFORE-SPACE
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL ORDRE SANERT    :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTORD                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
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
           SET VERK030-LEVEL-INIT          TO TRUE
           INITIALIZE VERK030-DATA-FIELDS
           SET VERK030-EOF-OFF             TO TRUE
           SET VERK030-PROCESS             TO TRUE
           OPEN INPUT VERK030
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT VERKSAN
           IF I-U1
               OPEN OUTPUT KLISTEO
           END-IF
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VERK030
           CLOSE FIRMAF
           CLOSE VERKSAN
           IF I-U1
               CLOSE KLISTEO
           END-IF.
 
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
