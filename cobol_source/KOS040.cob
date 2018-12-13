       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOS040R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: KOS040, BOKFØRINGSBILAG KASSEOPPGJØR         *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: FAK50A1                                      *
      *  LAGET DATO....: 14.12.11 I PROD 11.04.12                     *
      *  ENDRET........: 10.12.12 Skriver om data fra bankterminal er *
      *                           gjort opp manuelt.                  *
      *                  08.06.12 Fjernet merge-indikator på oppgj-nr *
      *                  09.07.12 Tar ikke med nullposter.            *
      *                  09.07.13 Viser innkastnr på kvitteringen.    *
      *                  26.06.14 Utvidet teller fra 3-6 pos.         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KOS040.rpg
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
           SELECT KASSOPP
               ASSIGN TO UT-S-KASSOPP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KASSOPP-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
           SELECT KONTOMA
               ASSIGN TO KONTOMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTOMA-STATUS
               RECORD KEY IS KONTOMA-KEY1.
           SELECT REGNFIL
               ASSIGN TO UT-S-REGNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGNFIL-STATUS.
           SELECT MERKFIL
               ASSIGN TO UT-S-MERKFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MERKFIL-STATUS.
           SELECT LISTE1
               ASSIGN TO UT-S-LISTE1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE1-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KASSOPP
               BLOCK CONTAINS 400
               RECORD CONTAINS 400.
       01  KASSOPP-IO-AREA.
           05  KASSOPP-IO-AREA-X           PICTURE X(400).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
       FD KONTOMA
               RECORD CONTAINS 60.
       01  KONTOMA-IO-AREA.
           05  KONTOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KONTOMA-KEY1            PICTURE X(7).
               10  FILLER                  PICTURE X(52).
       FD REGNFIL
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  REGNFIL-IO-AREA.
           05  REGNFIL-IO-AREA-X           PICTURE X(240).
       FD MERKFIL
               BLOCK CONTAINS 160
               RECORD CONTAINS 160.
       01  MERKFIL-IO-AREA.
           05  MERKFIL-IO-AREA-X           PICTURE X(160).
      *ISTE2  O   F 120 120            PRINTERSYS021
      *UGFILO O   F  80  80            PRINTERSYS020
       FD LISTE1
               BLOCK CONTAINS 121
               RECORD CONTAINS 121.
       01  LISTE1-IO-PRINT.
           05  LISTE1-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE1-IO-AREA.
           05  LISTE1-IO-AREA-X            PICTURE X(120).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KASSOPP-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  REGNFIL-STATUS              PICTURE 99 VALUE 0.
           10  MERKFIL-STATUS              PICTURE 99 VALUE 0.
           10  LISTE1-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPP-EOF-OFF         VALUE '0'.
               88  KASSOPP-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPP-READ-OFF        VALUE '0'.
               88  KASSOPP-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPP-PROCESS-OFF     VALUE '0'.
               88  KASSOPP-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KASSOPP-LEVEL-INIT-OFF  VALUE '0'.
               88  KASSOPP-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  KASSOPP-LEVEL-03.
               10  KASSOPP-03-L3.
                   15  KASSOPP-03-L3-FIRMA PICTURE X(3).
               10  KASSOPP-03-L2.
                   15  KASSOPP-03-L2-KASSE PICTURE X(2).
               10  KASSOPP-03-L1.
                   15  KASSOPP-03-L1-OPPGNR PICTURE S9(6).
           05  KASSOPP-LEVEL-04.
               10  KASSOPP-04-L3.
                   15  KASSOPP-04-L3-FIRMA PICTURE X(3).
               10  KASSOPP-04-L2.
                   15  KASSOPP-04-L2-KASSE PICTURE X(2).
               10  KASSOPP-04-L1.
                   15  KASSOPP-04-L1-OPPGNR PICTURE S9(6).
           05  KASSOPP-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KASSE                   PICTURE X(2).
               10  KOAAR                   PICTURE X(4).
               10  KOMND                   PICTURE X(2).
               10  KODAG                   PICTURE X(2).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KOTIM                   PICTURE X(2).
               10  KOMIN                   PICTURE X(2).
               10  KOSEK                   PICTURE X(2).
               10  FADAG                   PICTURE X(2).
               10  FAMND                   PICTURE X(2).
               10  FAAAR                   PICTURE X(4).
               10  OPPGNR-IO.
                   15  OPPGNR              PICTURE S9(6).
               10  BTAAR                   PICTURE X(4).
               10  BTMND                   PICTURE X(2).
               10  BTDAG                   PICTURE X(2).
               10  BTTIM                   PICTURE X(2).
               10  BTMIN                   PICTURE X(2).
               10  BTSEK                   PICTURE X(2).
               10  USERID                  PICTURE X(8).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  INNKNR                  PICTURE X(6).
               10  REC160                  PICTURE X(160).
               10  REC100                  PICTURE X(100).
               10  REC240                  PICTURE X(240).
               10  KFIRMA                  PICTURE X(3).
               10  BILART                  PICTURE X(1).
               10  BILNR                   PICTURE X(6).
               10  BILDTO                  PICTURE X(6).
               10  MOTKTO                  PICTURE X(4).
               10  BILTOT-IO.
                   15  BILTOT              PICTURE S9(8)V9(2).
               10  HDK                     PICTURE X(1).
               10  HTEKST                  PICTURE X(25).
               10  SIGN-X                  PICTURE X(2).
               10  LINTYP                  PICTURE X(1).
      *                                     162 169 VKONTO
               10  VKONT4                  PICTURE X(4).
               10  VKONT6                  PICTURE X(6).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  DDK                     PICTURE X(1).
               10  REFNR                   PICTURE X(6).
               10  AVD4                    PICTURE X(4).
               10  FFDATO                  PICTURE X(6).
               10  AK                      PICTURE X(1).
               10  DTEKST                  PICTURE X(24).
      *
           05  FIRMAF-DATA-FIELDS.
               10  FNAVN                   PICTURE X(30).
           05  SYSPARM-DATA-FIELDS.
               10  SYSALT                  PICTURE X(1).
           05  KONTOMA-DATA-FIELDS.
               10  ALTKTO                  PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KLOKKE-IO.
                   15  KLOKKE              PICTURE S9(6).
               10  SYSKEY                  PICTURE X(10).
               10  ANTREC-IO.
                   15  ANTREC              PICTURE S9(6).
               10  SEQNRH-IO.
                   15  SEQNRH              PICTURE S9(7).
               10  SEQNR-N-IO.
                   15  SEQNR-N             PICTURE S9(7).
               10  OPPGDG                  PICTURE X(1).
               10  KONTO                   PICTURE X(4).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(8)V9(2).
               10  KTOKEY                  PICTURE X(7).
           05  EDITTING-FIELDS.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KASSOPP-PROCESS
               SET KASSOPP-PROCESS-OFF     TO TRUE
               SET KASSOPP-READ            TO TRUE
           END-IF
 
           IF  KASSOPP-READ
           AND RECORD-SELECTED-OFF
               PERFORM KASSOPP-GET
               SET KASSOPP-READ-OFF        TO TRUE
               IF  NOT KASSOPP-EOF
                   SET KASSOPP-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KASSOPP-PROCESS
               PERFORM KASSOPP-IDSET
           END-IF
 
           IF  KASSOPP-PROCESS
               PERFORM KASSOPP-CHK-LEVEL
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
 
           IF  KASSOPP-PROCESS
               PERFORM KASSOPP-FLDOFF
               PERFORM KASSOPP-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KASSOPP-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-10)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO KLOKKE (1:6)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-05)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-18                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-18            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3)
               SET NOT-I-21                TO TRUE
               SET NOT-I-22                TO TRUE
               MOVE KFIRMA                 TO SYSKEY (1:3)
               MOVE 'REGA011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-21 AND I-01)
               SET NOT-I-22                TO TRUE
               IF  SYSALT = 'J'
                   SET I-22                TO TRUE
               END-IF
      *  L1                SETOF                     50
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTREC
               SET NOT-I-50                TO TRUE
               IF  ANTREC > 0
                   SET I-50                TO TRUE
               END-IF
               MOVE SEQNR                  TO SEQNR-N
               MOVE SEQNR-N-IO             TO SEQNRH-IO
               MOVE SEQNRH (1:1)           TO OPPGDG
           END-IF
           IF  (I-03 AND I-22)
               MOVE MOTKTO                 TO KONTO
               PERFORM KTORUT-S
      *                    MOVE "REC160  "BUGFLD  8        DISPLAY FIELD
      *          BUGFLD    DEBUGBUGFILO   REC160           VIS INDIKATOR
      *                    MOVE "REC240  "BUGFLD  8        DISPLAY FIELD
      *          BUGFLD    DEBUGBUGFILO   REC240           VIS INDIKATOR
           END-IF
           IF  (I-03)
               SET NOT-I-12                TO TRUE
               IF  BTTIM NOT > '00'
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-13                TO TRUE
               IF  BTTIM = '99'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-04)
               ADD 1                       TO ANTREC
               SET NOT-I-50                TO TRUE
               IF  ANTREC > 0
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  DDK = 'D'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-53)
               ADD BEL                     TO TOTBEL
           END-IF
           IF  (I-04 AND NOT-I-53)
               SUBTRACT BEL                FROM TOTBEL
           END-IF
           IF  (I-04)
               SET NOT-I-11                TO TRUE
               IF  LINTYP = 'B'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-22 AND I-11)
               MOVE VKONT4                 TO KONTO
               PERFORM KTORUT-S
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'REG58'                    TO LONR
           MOVE KFIRMA                     TO LFIRMA
           IF  (I-LR)
               MOVE '399'                  TO LFIRMA
           END-IF
           MOVE '000'                      TO LUNDGR
           MOVE 'KOS040  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
      *R         RBS21     BEGSR
      *R                   MOVE " "       BBEST             NO OVERSTYRI
      *R                   MOVE "REG58"   LONR              OPPGAVENR.
      *R                   MOVE FIRMA     LFIRMA            FIRMANR.
      *R                   MOVE "000"     LUNDGR            UNDERGRUPPE.
      *R                   MOVEL"KOS040  "LPROG             PROGRAMNAVN.
      *R                   MOVE "PRT1"    LPRIID            PRINTERIDENT
      *R                   CALL "ILBDSET0"             88   CALL COBOL -
      *R                   CALL "RBS021"               88   SUBPROGRAM.
      *R                   PARM           LDATA             LINKDATA
      *R                   ENDSR
      *****************************************************************
      * SUBRUTINE FOR Å HENTE ALTERNATIVT KONTONR FRA KONTOMA.        *
      *****************************************************************
 
       KTORUT-S SECTION.
       KTORUT-S-P.
           MOVE KFIRMA                     TO KTOKEY (1:3)
           MOVE KONTO                      TO KTOKEY (4:4)
           MOVE KTOKEY                     TO KONTOMA-KEY1
           READ KONTOMA RECORD KEY IS KONTOMA-KEY1
           INVALID KEY
               SET I-23                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-23                TO TRUE
               PERFORM KONTOMA-FLDSET
               PERFORM KONTOMA-IDSET
           END-READ.
      ******************************************************
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           IF  (NOT-I-50)
               PERFORM RBSRUT-S
      *RN50                EXSR RBS21
      *R                   MOVE "ANTREC  "BUGFLD  8        DISPLAY FIELD
      *R         BUGFLD    DEBUGBUGFILO   ANTREC           VIS INDIKATOR
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       KASSOPP-GET SECTION.
       KASSOPP-GET-P.
           IF  KASSOPP-EOF-OFF
               READ KASSOPP
               AT END
                   SET KASSOPP-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KASSOPP-FLDOFF SECTION.
       KASSOPP-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'B' )
             OR ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'R' )
               SET NOT-I-14                TO TRUE
           END-EVALUATE.
 
       KASSOPP-FLDSET SECTION.
       KASSOPP-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KASSOPP-IO-AREA (161:1) = '1'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'B' )
               MOVE KASSOPP-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KASSOPP-IO-AREA (5:2)  TO KASSE (1:2)
               MOVE KASSOPP-IO-AREA (7:4)  TO KOAAR (1:4)
               MOVE KASSOPP-IO-AREA (11:2) TO KOMND (1:2)
               MOVE KASSOPP-IO-AREA (13:2) TO KODAG (1:2)
               MOVE KASSOPP-IO-AREA (15:4) TO SEQNR-IO
               MOVE KASSOPP-IO-AREA (19:2) TO KOTIM (1:2)
               MOVE KASSOPP-IO-AREA (21:2) TO KOMIN (1:2)
               MOVE KASSOPP-IO-AREA (23:2) TO KOSEK (1:2)
               MOVE KASSOPP-IO-AREA (25:2) TO FADAG (1:2)
               MOVE KASSOPP-IO-AREA (27:2) TO FAMND (1:2)
               MOVE KASSOPP-IO-AREA (29:4) TO FAAAR (1:4)
               MOVE KASSOPP-IO-AREA (33:6) TO OPPGNR-IO
               INSPECT OPPGNR-IO REPLACING ALL ' ' BY '0'
               MOVE KASSOPP-IO-AREA (39:4) TO BTAAR (1:4)
               MOVE KASSOPP-IO-AREA (43:2) TO BTMND (1:2)
               MOVE KASSOPP-IO-AREA (45:2) TO BTDAG (1:2)
               MOVE KASSOPP-IO-AREA (47:2) TO BTTIM (1:2)
               MOVE KASSOPP-IO-AREA (49:2) TO BTMIN (1:2)
               MOVE KASSOPP-IO-AREA (51:2) TO BTSEK (1:2)
               MOVE KASSOPP-IO-AREA (53:8) TO USERID (1:8)
               MOVE KASSOPP-IO-AREA (119:3) TO ANTORD-IO
               MOVE KASSOPP-IO-AREA (124:6) TO INNKNR (1:6)
               MOVE KASSOPP-IO-AREA (1:160) TO REC160 (1:160)
               MOVE KASSOPP-IO-AREA (161:100) TO REC100 (1:100)
               MOVE KASSOPP-IO-AREA (161:240) TO REC240 (1:240)
               MOVE KASSOPP-IO-AREA (162:3) TO KFIRMA (1:3)
               MOVE KASSOPP-IO-AREA (165:1) TO BILART (1:1)
               MOVE KASSOPP-IO-AREA (166:6) TO BILNR (1:6)
               MOVE KASSOPP-IO-AREA (172:6) TO BILDTO (1:6)
               MOVE KASSOPP-IO-AREA (178:4) TO MOTKTO (1:4)
               MOVE KASSOPP-IO-AREA (182:10) TO BILTOT-IO
               INSPECT BILTOT-IO REPLACING ALL ' ' BY '0'
               MOVE KASSOPP-IO-AREA (192:1) TO HDK (1:1)
               MOVE KASSOPP-IO-AREA (193:25) TO HTEKST (1:25)
               MOVE KASSOPP-IO-AREA (239:2) TO SIGN-X (1:2)
           WHEN ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'B' )
             OR ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'R' )
               MOVE KASSOPP-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KASSOPP-IO-AREA (5:2)  TO KASSE (1:2)
               MOVE KASSOPP-IO-AREA (33:6) TO OPPGNR-IO
               INSPECT OPPGNR-IO REPLACING ALL ' ' BY '0'
               MOVE KASSOPP-IO-AREA (122:1) TO LINTYP (1:1)
               MOVE KASSOPP-IO-AREA (1:160) TO REC160 (1:160)
               MOVE KASSOPP-IO-AREA (161:100) TO REC100 (1:100)
               MOVE KASSOPP-IO-AREA (161:240) TO REC240 (1:240)
               MOVE KASSOPP-IO-AREA (162:4) TO VKONT4 (1:4)
               MOVE KASSOPP-IO-AREA (162:6) TO VKONT6 (1:6)
               MOVE KASSOPP-IO-AREA (170:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               IF  BEL = ZERO
                   SET I-14                TO TRUE
               END-IF
               MOVE KASSOPP-IO-AREA (179:1) TO DDK (1:1)
               MOVE KASSOPP-IO-AREA (184:6) TO REFNR (1:6)
               MOVE KASSOPP-IO-AREA (190:4) TO AVD4 (1:4)
               MOVE KASSOPP-IO-AREA (202:6) TO FFDATO (1:6)
               MOVE KASSOPP-IO-AREA (208:1) TO AK (1:1)
               MOVE KASSOPP-IO-AREA (237:24) TO DTEKST (1:24)
           END-EVALUATE.
 
       KASSOPP-IDSET SECTION.
       KASSOPP-IDSET-P.
           EVALUATE TRUE
           WHEN ( KASSOPP-IO-AREA (161:1) = '1'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'B' )
               SET I-03                    TO TRUE
           WHEN ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'B' )
             OR ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'R' )
               SET I-04                    TO TRUE
           WHEN  OTHER
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       KASSOPP-CHK-LEVEL SECTION.
       KASSOPP-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KASSOPP-IO-AREA (161:1) = '1'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'B' )
               MOVE LOW-VALUES             TO KASSOPP-LEVEL-03
               MOVE KASSOPP-IO-AREA (1:3)  TO KASSOPP-03-L3-FIRMA
               MOVE KASSOPP-IO-AREA (5:2)  TO KASSOPP-03-L2-KASSE
               MOVE KASSOPP-IO-AREA (33:6) TO KASSOPP-03-L1-OPPGNR
               IF  KASSOPP-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KASSOPP-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  KASSOPP-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KASSOPP-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KASSOPP-03-L3         TO THE-PRIOR-L3
               MOVE  KASSOPP-03-L2         TO THE-PRIOR-L2
               MOVE  KASSOPP-03-L1         TO THE-PRIOR-L1
               SET KASSOPP-LEVEL-INIT      TO TRUE
           WHEN ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'B' )
             OR ( KASSOPP-IO-AREA (161:1) = '2'
            AND   KASSOPP-IO-AREA (123:1) NOT = 'J'
            AND   KASSOPP-IO-AREA (122:1) = 'R' )
               MOVE LOW-VALUES             TO KASSOPP-LEVEL-04
               MOVE KASSOPP-IO-AREA (1:3)  TO KASSOPP-04-L3-FIRMA
               MOVE KASSOPP-IO-AREA (5:2)  TO KASSOPP-04-L2-KASSE
               MOVE KASSOPP-IO-AREA (33:6) TO KASSOPP-04-L1-OPPGNR
               IF  KASSOPP-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KASSOPP-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  KASSOPP-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KASSOPP-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KASSOPP-04-L3         TO THE-PRIOR-L3
               MOVE  KASSOPP-04-L2         TO THE-PRIOR-L2
               MOVE  KASSOPP-04-L1         TO THE-PRIOR-L1
               SET KASSOPP-LEVEL-INIT      TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FNAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-06                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (026:1) TO SYSALT (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-01                        TO TRUE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (45:4) TO ALTKTO (1:4)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-02                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03)
           OR  (I-04 AND NOT-I-14)
               MOVE SPACES TO REGNFIL-IO-AREA
               INITIALIZE REGNFIL-IO-AREA
               MOVE REC240                 TO REGNFIL-IO-AREA (1:240)
               WRITE REGNFIL-IO-AREA
           END-IF
           IF  (I-03 AND NOT-I-86 AND I-50)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FINAVN                 TO LISTE1-IO-AREA (1:30)
               MOVE 'Program:'             TO LISTE1-IO-AREA (63:8)
               MOVE '    KOS040'           TO LISTE1-IO-AREA (71:10)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Dato   :'             TO LISTE1-IO-AREA (63:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Automatisk poster'    TO LISTE1-IO-AREA (22:17)
               MOVE 'ing av kasseoppgjør'  TO LISTE1-IO-AREA (39:19)
               MOVE 'Klokke :'             TO LISTE1-IO-AREA (63:8)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (73:8)
      *       D 22     03N86 50
      *                                  47 "R e g n s k a p s r u t "
      *                                  54 "i n e n"
               MOVE 2                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Bilagsart:'           TO LISTE1-IO-AREA (1:10)
               MOVE BILART                 TO LISTE1-IO-AREA (12:1)
               MOVE 2                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Bilags-'              TO LISTE1-IO-AREA (1:7)
               MOVE 'Bilags-'              TO LISTE1-IO-AREA (10:7)
               MOVE 'Mot  '                TO LISTE1-IO-AREA (19:5)
               MOVE 'D'                    TO LISTE1-IO-AREA (42:1)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'nr.    '              TO LISTE1-IO-AREA (1:7)
               MOVE 'dato   '              TO LISTE1-IO-AREA (10:7)
               MOVE 'konto'                TO LISTE1-IO-AREA (19:5)
               MOVE '   Bilagstotal'       TO LISTE1-IO-AREA (26:14)
               MOVE 'K'                    TO LISTE1-IO-AREA (42:1)
               MOVE 'Sign'                 TO LISTE1-IO-AREA (45:4)
               MOVE 'Tekst                   ' TO LISTE1-IO-AREA
                                                               (51:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '------------------------' TO LISTE1-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '--------'             TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE BILNR                  TO LISTE1-IO-AREA (1:6)
               MOVE BILDTO                 TO LISTE1-IO-AREA (10:6)
               MOVE MOTKTO                 TO LISTE1-IO-AREA (20:4)
               IF  (I-02 AND I-22 AND NOT-I-23)
                   MOVE ALTKTO             TO LISTE1-IO-AREA (20:4)
               END-IF
               MOVE BILTOT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE1-IO-AREA (26:14)
               MOVE HDK                    TO LISTE1-IO-AREA (42:1)
               MOVE SIGN-X                 TO LISTE1-IO-AREA (46:2)
               MOVE HTEKST                 TO LISTE1-IO-AREA (51:25)
               MOVE 2                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'A'                    TO LISTE1-IO-AREA (15:1)
               MOVE 'D'                    TO LISTE1-IO-AREA (31:1)
               MOVE 'Forf. '               TO LISTE1-IO-AREA (42:6)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Konto'                TO LISTE1-IO-AREA (1:5)
               MOVE 'Avd '                 TO LISTE1-IO-AREA (9:4)
               MOVE 'K'                    TO LISTE1-IO-AREA (15:1)
               MOVE 'Beløp'                TO LISTE1-IO-AREA (24:5)
               MOVE 'K'                    TO LISTE1-IO-AREA (31:1)
               MOVE 'Refnr '               TO LISTE1-IO-AREA (34:6)
               MOVE 'dato  '               TO LISTE1-IO-AREA (42:6)
               MOVE 'Tekst                   ' TO LISTE1-IO-AREA
                                                               (50:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '------------------------' TO LISTE1-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '--------'             TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-04 AND NOT-I-86 AND I-50)
           AND (NOT-I-14)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               IF  (I-11)
                   MOVE VKONT4             TO LISTE1-IO-AREA (1:4)
               END-IF
               IF  (I-11 AND I-22 AND NOT-I-23)
                   MOVE ALTKTO             TO LISTE1-IO-AREA (1:4)
               END-IF
               IF  (I-11 AND I-22 AND I-23)
                   MOVE '????'             TO LISTE1-IO-AREA (1:4)
               END-IF
               IF  (NOT-I-11)
                   MOVE VKONT6             TO LISTE1-IO-AREA (1:6)
               END-IF
               MOVE AVD4                   TO LISTE1-IO-AREA (9:4)
               MOVE AK                     TO LISTE1-IO-AREA (15:1)
               MOVE BEL                    TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE1-IO-AREA (17:13)
               MOVE DDK                    TO LISTE1-IO-AREA (31:1)
               MOVE REFNR                  TO LISTE1-IO-AREA (34:6)
               MOVE FFDATO                 TO LISTE1-IO-AREA (42:6)
               MOVE DTEKST                 TO LISTE1-IO-AREA (50:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-98 AND I-U8)
           AND (I-06)
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
               MOVE BJOBN                  TO LISTE1-IO-AREA (113:8)
               MOVE BBEST                  TO LISTE1-IO-AREA (120:1)
               MOVE BPERS                  TO LISTE1-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE1-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE1-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE1-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE1-IO-AREA (118:3)
               MOVE BPCLAS                 TO LISTE1-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE1-IO-AREA (118:3)
               MOVE REC100                 TO LISTE1-IO-AREA (21:100)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-50)
               MOVE SPACES TO MERKFIL-IO-AREA
               INITIALIZE MERKFIL-IO-AREA
               MOVE REC160                 TO MERKFIL-IO-AREA (1:160)
               WRITE MERKFIL-IO-AREA
           END-IF
           IF  (I-L1 AND NOT-I-86 AND I-50)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Total'                TO LISTE1-IO-AREA (1:5)
               MOVE TOTBEL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE1-IO-AREA (16:14)
               INITIALIZE TOTBEL
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Kasse fra firma/avd.   :' TO LISTE1-IO-AREA (1:24)
               MOVE FIRMA                  TO LISTE1-IO-AREA (26:3)
               IF  (I-18)
                   MOVE ' >>> UKJENT FIRMA <<<' TO LISTE1-IO-AREA
                                                               (29:21)
               END-IF
               IF  (NOT-I-18)
                   MOVE FNAVN              TO LISTE1-IO-AREA (30:30)
               END-IF
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Kasse gjort opp        :' TO LISTE1-IO-AREA (1:24)
               MOVE KODAG                  TO LISTE1-IO-AREA (26:2)
               MOVE '.'                    TO LISTE1-IO-AREA (28:1)
               MOVE KOMND                  TO LISTE1-IO-AREA (29:2)
               MOVE '.'                    TO LISTE1-IO-AREA (31:1)
               MOVE KOAAR                  TO LISTE1-IO-AREA (32:4)
               MOVE KOTIM                  TO LISTE1-IO-AREA (37:2)
               MOVE '.'                    TO LISTE1-IO-AREA (39:1)
               MOVE KOMIN                  TO LISTE1-IO-AREA (40:2)
               MOVE '.'                    TO LISTE1-IO-AREA (42:1)
               MOVE KOSEK                  TO LISTE1-IO-AREA (43:2)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Oppgjør nr             :' TO LISTE1-IO-AREA (1:24)
               MOVE OPPGNR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE1-IO-AREA (25:7)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Oppgjør nr idag        :' TO LISTE1-IO-AREA (1:24)
               MOVE OPPGDG                 TO LISTE1-IO-AREA (31:1)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Innkast nr             :' TO LISTE1-IO-AREA (1:24)
               MOVE INNKNR                 TO LISTE1-IO-AREA (26:6)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Antall ordre           :' TO LISTE1-IO-AREA (1:24)
               MOVE ANTORD                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE1-IO-AREA (26:6)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Bankterminal avsluttet :' TO LISTE1-IO-AREA (1:24)
               MOVE BTDAG                  TO LISTE1-IO-AREA (26:2)
               MOVE '.'                    TO LISTE1-IO-AREA (28:1)
               MOVE BTMND                  TO LISTE1-IO-AREA (29:2)
               MOVE '.'                    TO LISTE1-IO-AREA (31:1)
               MOVE BTAAR                  TO LISTE1-IO-AREA (32:4)
               MOVE BTTIM                  TO LISTE1-IO-AREA (37:2)
               MOVE '.'                    TO LISTE1-IO-AREA (39:1)
               MOVE BTMIN                  TO LISTE1-IO-AREA (40:2)
               MOVE '.'                    TO LISTE1-IO-AREA (42:1)
               MOVE BTSEK                  TO LISTE1-IO-AREA (43:2)
               IF  (I-12)
                   MOVE '*** Ingen data fra bank-' TO LISTE1-IO-AREA
                                                               (25:24)
               END-IF
               IF  (I-12)
                   MOVE 'terminal ***            ' TO LISTE1-IO-AREA
                                                               (49:24)
               END-IF
               IF  (I-13)
                   MOVE '*** Bankterminal er gjor' TO LISTE1-IO-AREA
                                                               (25:24)
               END-IF
               IF  (I-13)
                   MOVE 't opp manuelt ***       ' TO LISTE1-IO-AREA
                                                               (49:24)
               END-IF
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Foretatt av bruker-id  :' TO LISTE1-IO-AREA (1:24)
               MOVE USERID                 TO LISTE1-IO-AREA (26:8)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Kontant faktura dato   :' TO LISTE1-IO-AREA (1:24)
               MOVE FADAG                  TO LISTE1-IO-AREA (26:2)
               MOVE '.'                    TO LISTE1-IO-AREA (28:1)
               MOVE FAMND                  TO LISTE1-IO-AREA (29:2)
               MOVE '.'                    TO LISTE1-IO-AREA (31:1)
               MOVE FAAAR                  TO LISTE1-IO-AREA (32:4)
               MOVE ' jfr FAK08'           TO LISTE1-IO-AREA (36:10)
      *ISTE2  T 3101   LRN50
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND NOT-I-50)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'AutoData Norge AS'    TO LISTE1-IO-AREA (1:17)
               MOVE 'Program:'             TO LISTE1-IO-AREA (63:8)
               MOVE '    KOS040'           TO LISTE1-IO-AREA (71:10)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 3                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Dato   :'             TO LISTE1-IO-AREA (63:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Automatisk poster'    TO LISTE1-IO-AREA (22:17)
               MOVE 'ing av kasseoppgjør'  TO LISTE1-IO-AREA (39:19)
               MOVE 2                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '>>>>>>>>>>>>>>>>>>>>>>> ' TO LISTE1-IO-AREA (1:24)
               MOVE 'Ingen kasseoppgjør idag.' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE ' <<<<<<<<<<<<<<<<<<<<<<<' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'Kvittering når det ikke ' TO LISTE1-IO-AREA (1:24)
               MOVE 'er kasseoppgjør, lagres ' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE 'på 399 på rweb inntil...' TO LISTE1-IO-AREA
                                                               (49:24)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
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
           SET KASSOPP-LEVEL-INIT          TO TRUE
           INITIALIZE KASSOPP-DATA-FIELDS
           SET KASSOPP-EOF-OFF             TO TRUE
           SET KASSOPP-PROCESS             TO TRUE
           OPEN INPUT KASSOPP
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           OPEN OUTPUT REGNFIL
           OPEN OUTPUT MERKFIL
           OPEN OUTPUT LISTE1
           INITIALIZE LISTE1-IO-AREA
           INITIALIZE LISTE1-DATA-FIELDS
           MOVE 57                         TO LISTE1-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KASSOPP
           CLOSE FIRMAF
           CLOSE SYSPARM
           CLOSE KONTOMA
           CLOSE REGNFIL
           CLOSE MERKFIL
           IF LISTE1-IO-AREA NOT = SPACES
             WRITE LISTE1-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE1-IO-AREA
           END-IF
           CLOSE LISTE1.
 
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
