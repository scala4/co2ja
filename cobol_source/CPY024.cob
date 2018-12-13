       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPY024R.
      **********************************************  Z-WIN-RPG2   ****
      * OBS!!!!!! IKKE TESTET MED KUNDENR-SKIFTE (FORDI INGEN SKULLE  *
      * ENDRE NR FØRSTE GANG DETTE PROGRAMMET BLE BRUKT).             *
      * PROGRAM.......: CPY024, FLYTTER VERK030 FRA ET FIRMA TIL ET   *
      *                         ANNET.                                *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: CPY08A                                       *
      *  LAGET DATO....: 14.12.01                                     *
      *  ENDRET DATO...: 28.08.02 TAKLER RECART S1 - SLETTELOGG       *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: CPY024.rpg
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
           SELECT FIRTAB
               ASSIGN TO UT-S-FIRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRTAB-STATUS.
           SELECT AVDTAB
               ASSIGN TO UT-S-AVDTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVDTAB-STATUS.
           SELECT VERKINN
               ASSIGN TO UT-S-VERKINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKINN-STATUS.
           SELECT ORDREIN
               ASSIGN TO UT-S-ORDREIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREIN-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT VERKUT
               ASSIGN TO UT-S-VERKUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRTAB
               BLOCK CONTAINS 10
               RECORD CONTAINS 10.
       01  FIRTAB-IO-AREA.
           05  FIRTAB-IO-AREA-X            PICTURE X(10).
       FD AVDTAB
               BLOCK CONTAINS 10
               RECORD CONTAINS 10.
       01  AVDTAB-IO-AREA.
           05  AVDTAB-IO-AREA-X            PICTURE X(10).
       FD VERKINN
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  VERKINN-IO-AREA.
           05  VERKINN-IO-AREA-X           PICTURE X(200).
       FD ORDREIN
               BLOCK CONTAINS 90
               RECORD CONTAINS 9.
       01  ORDREIN-IO-AREA.
           05  ORDREIN-IO-AREA-X           PICTURE X(9).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD VERKUT
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  VERKUT-IO-AREA.
           05  VERKUT-IO-AREA-X            PICTURE X(200).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABFRA-MAX   VALUE 99           PICTURE 9(4) USAGE BINARY.
       77  TABTIL-MAX   VALUE 99           PICTURE 9(4) USAGE BINARY.
       77  TABAVD-MAX   VALUE 99           PICTURE 9(4) USAGE BINARY.
       77  TABFIA-MAX   VALUE 99           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFRA-TABLE.
               10  TABFRA-ENTRY
                                           OCCURS 99 TIMES
                                           INDEXED BY TABFRA-I
                                                      TABFRA-S
                                                      TABTIL-I
                                                      TABTIL-S.
                   15  TABFRA              PICTURE X(3).
                   15  TABTIL              PICTURE X(4).
           05  TABAVD-TABLE.
               10  TABAVD-ENTRY
                                           OCCURS 99 TIMES
                                           INDEXED BY TABAVD-I
                                                      TABAVD-S
                                                      TABFIA-I
                                                      TABFIA-S.
                   15  TABAVD              PICTURE X(4).
                   15  TABFIA              PICTURE X(4).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FIRTAB-STATUS               PICTURE 99 VALUE 0.
           10  AVDTAB-STATUS               PICTURE 99 VALUE 0.
           10  VERKINN-STATUS              PICTURE 99 VALUE 0.
           10  ORDREIN-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  VERKUT-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRTAB-EOF-OFF          VALUE '0'.
               88  FIRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AVDTAB-EOF-OFF          VALUE '0'.
               88  AVDTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKINN-EOF-OFF         VALUE '0'.
               88  VERKINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKINN-READ-OFF        VALUE '0'.
               88  VERKINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKINN-PROCESS-OFF     VALUE '0'.
               88  VERKINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VERKINN-LEVEL-INIT-OFF  VALUE '0'.
               88  VERKINN-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREIN-EOF-OFF         VALUE '0'.
               88  ORDREIN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREIN-READ-OFF        VALUE '0'.
               88  ORDREIN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREIN-PROCESS-OFF     VALUE '0'.
               88  ORDREIN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDREIN-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDREIN-LEVEL-INIT      VALUE '1'.
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
           05  VERKINN-LEVEL-01.
               10  VERKINN-01-L2.
                   15  VERKINN-01-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-02.
               10  VERKINN-02-L2.
                   15  VERKINN-02-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-03.
               10  VERKINN-03-L2.
                   15  VERKINN-03-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-04.
               10  VERKINN-04-L2.
                   15  VERKINN-04-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-05.
               10  VERKINN-05-L2.
                   15  VERKINN-05-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-06.
               10  VERKINN-06-L2.
                   15  VERKINN-06-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-07.
               10  VERKINN-07-L2.
                   15  VERKINN-07-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-08.
               10  VERKINN-08-L2.
                   15  VERKINN-08-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-09.
               10  VERKINN-09-L2.
                   15  VERKINN-09-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-10.
               10  VERKINN-10-L2.
                   15  VERKINN-10-L2-VFIR  PICTURE X(3).
           05  VERKINN-LEVEL-11.
               10  VERKINN-11-L2.
                   15  VERKINN-11-L2-VFIR  PICTURE X(3).
           05  VERKINN-DATA-FIELDS.
               10  VREC01                  PICTURE X(200).
               10  VFIR                    PICTURE X(3).
               10  VORD                    PICTURE X(6).
               10  VKNR01                  PICTURE X(6).
               10  VKS011                  PICTURE X(1).
               10  VREC02                  PICTURE X(200).
               10  VKNR02                  PICTURE X(6).
               10  VKS021                  PICTURE X(1).
               10  VREC03                  PICTURE X(200).
               10  VKNR03                  PICTURE X(6).
               10  VKS031                  PICTURE X(1).
               10  VREC04                  PICTURE X(200).
               10  VRKNR                   PICTURE X(6).
               10  VRKNR1                  PICTURE X(1).
               10  VAVD                    PICTURE X(1).
               10  VREC05                  PICTURE X(200).
               10  VREC06                  PICTURE X(200).
               10  VREC07                  PICTURE X(200).
               10  VREC08                  PICTURE X(200).
               10  VREC09                  PICTURE X(200).
               10  VREC10                  PICTURE X(200).
               10  VREC11                  PICTURE X(200).
           05  VERKINN-MP                  PICTURE X(9).
           05  VERKINN-MC                  PICTURE X(9).
           05  VERKINN-M-01            REDEFINES VERKINN-MC.
               10  VERKINN-M-01-M2.
                   15  VERKINN-M-01-M2-VFIR-G.
                       20  VERKINN-M-01-M2-VFIR PICTURE X(3).
               10  VERKINN-M-01-M1.
                   15  VERKINN-M-01-M1-VORD-G.
                       20  VERKINN-M-01-M1-VORD PICTURE X(6).
           05  VERKINN-M-02            REDEFINES VERKINN-MC.
               10  VERKINN-M-02-M2.
                   15  VERKINN-M-02-M2-VFIR-G.
                       20  VERKINN-M-02-M2-VFIR PICTURE X(3).
               10  VERKINN-M-02-M1.
                   15  VERKINN-M-02-M1-VORD-G.
                       20  VERKINN-M-02-M1-VORD PICTURE X(6).
           05  VERKINN-M-03            REDEFINES VERKINN-MC.
               10  VERKINN-M-03-M2.
                   15  VERKINN-M-03-M2-VFIR-G.
                       20  VERKINN-M-03-M2-VFIR PICTURE X(3).
               10  VERKINN-M-03-M1.
                   15  VERKINN-M-03-M1-VORD-G.
                       20  VERKINN-M-03-M1-VORD PICTURE X(6).
           05  VERKINN-M-04            REDEFINES VERKINN-MC.
               10  VERKINN-M-04-M2.
                   15  VERKINN-M-04-M2-VFIR-G.
                       20  VERKINN-M-04-M2-VFIR PICTURE X(3).
               10  VERKINN-M-04-M1.
                   15  VERKINN-M-04-M1-VORD-G.
                       20  VERKINN-M-04-M1-VORD PICTURE X(6).
           05  VERKINN-M-05            REDEFINES VERKINN-MC.
               10  VERKINN-M-05-M2.
                   15  VERKINN-M-05-M2-VFIR-G.
                       20  VERKINN-M-05-M2-VFIR PICTURE X(3).
               10  VERKINN-M-05-M1.
                   15  VERKINN-M-05-M1-VORD-G.
                       20  VERKINN-M-05-M1-VORD PICTURE X(6).
           05  VERKINN-M-06            REDEFINES VERKINN-MC.
               10  VERKINN-M-06-M2.
                   15  VERKINN-M-06-M2-VFIR-G.
                       20  VERKINN-M-06-M2-VFIR PICTURE X(3).
               10  VERKINN-M-06-M1.
                   15  VERKINN-M-06-M1-VORD-G.
                       20  VERKINN-M-06-M1-VORD PICTURE X(6).
           05  VERKINN-M-07            REDEFINES VERKINN-MC.
               10  VERKINN-M-07-M2.
                   15  VERKINN-M-07-M2-VFIR-G.
                       20  VERKINN-M-07-M2-VFIR PICTURE X(3).
               10  VERKINN-M-07-M1.
                   15  VERKINN-M-07-M1-VORD-G.
                       20  VERKINN-M-07-M1-VORD PICTURE X(6).
           05  VERKINN-M-08            REDEFINES VERKINN-MC.
               10  VERKINN-M-08-M2.
                   15  VERKINN-M-08-M2-VFIR-G.
                       20  VERKINN-M-08-M2-VFIR PICTURE X(3).
               10  VERKINN-M-08-M1.
                   15  VERKINN-M-08-M1-VORD-G.
                       20  VERKINN-M-08-M1-VORD PICTURE X(6).
           05  VERKINN-M-09            REDEFINES VERKINN-MC.
               10  VERKINN-M-09-M2.
                   15  VERKINN-M-09-M2-VFIR-G.
                       20  VERKINN-M-09-M2-VFIR PICTURE X(3).
               10  VERKINN-M-09-M1.
                   15  VERKINN-M-09-M1-VORD-G.
                       20  VERKINN-M-09-M1-VORD PICTURE X(6).
           05  VERKINN-M-10            REDEFINES VERKINN-MC.
               10  VERKINN-M-10-M2.
                   15  VERKINN-M-10-M2-VFIR-G.
                       20  VERKINN-M-10-M2-VFIR PICTURE X(3).
               10  VERKINN-M-10-M1.
                   15  VERKINN-M-10-M1-VORD-G.
                       20  VERKINN-M-10-M1-VORD PICTURE X(6).
           05  VERKINN-M-11            REDEFINES VERKINN-MC.
               10  VERKINN-M-11-M2.
                   15  VERKINN-M-11-M2-VFIR-G.
                       20  VERKINN-M-11-M2-VFIR PICTURE X(3).
               10  VERKINN-M-11-M1.
                   15  VERKINN-M-11-M1-VORD-G.
                       20  VERKINN-M-11-M1-VORD PICTURE X(6).
           05  ORDREIN-LEVEL-12.
               10  ORDREIN-12-L2.
                   15  ORDREIN-12-L2-OFIR  PICTURE X(3).
           05  ORDREIN-DATA-FIELDS.
               10  OFIR                    PICTURE X(3).
               10  OORD                    PICTURE X(6).
           05  ORDREIN-MP                  PICTURE X(9).
           05  ORDREIN-MC                  PICTURE X(9).
           05  ORDREIN-M-12            REDEFINES ORDREIN-MC.
               10  ORDREIN-M-12-M2.
                   15  ORDREIN-M-12-M2-OFIR-G.
                       20  ORDREIN-M-12-M2-OFIR PICTURE X(3).
               10  ORDREIN-M-12-M1.
                   15  ORDREIN-M-12-M1-OORD-G.
                       20  ORDREIN-M-12-M1-OORD PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  KNDALT                  PICTURE X(6).
      ******************** CALC **************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  TILFIR                  PICTURE X(3).
               10  KODE                    PICTURE X(1).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(7).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANT12-IO.
                   15  ANT12               PICTURE S9(7).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7).
               10  ANTKOP-IO.
                   15  ANTKOP              PICTURE S9(7).
               10  ANTFLY-IO.
                   15  ANTFLY              PICTURE S9(7).
               10  AKEY                    PICTURE X(4).
               10  TILAVD                  PICTURE X(1).
               10  KKEY                    PICTURE X(6).
               10  ALTKEY                  PICTURE X(9).
               10  TILKNR                  PICTURE X(6).
               10  ALTNUL-IO.
                   15  ALTNUL              PICTURE S9(7).
               10  NYTTNR-IO.
                   15  NYTTNR              PICTURE S9(7).
               10  TILRAB                  PICTURE X(6).
               10  NYTRAB-IO.
                   15  NYTRAB              PICTURE S9(7).
           05  EDITTING-FIELDS.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           SET NOT-I-15                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VERKINN-PROCESS
               SET VERKINN-PROCESS-OFF     TO TRUE
               SET VERKINN-READ            TO TRUE
           END-IF
 
           IF  VERKINN-READ
               PERFORM VERKINN-GET
               SET VERKINN-READ-OFF        TO TRUE
               IF  NOT VERKINN-EOF
                   PERFORM VERKINN-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM VERKINN-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDREIN-PROCESS
               SET ORDREIN-PROCESS-OFF     TO TRUE
               SET ORDREIN-READ            TO TRUE
           END-IF
 
           IF  ORDREIN-READ
               PERFORM ORDREIN-GET
               SET ORDREIN-READ-OFF        TO TRUE
               IF  NOT ORDREIN-EOF
                   PERFORM ORDREIN-MATCH-SET
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
 
           IF  VERKINN-PROCESS
               PERFORM VERKINN-IDSET
           END-IF
 
           IF  ORDREIN-PROCESS
               PERFORM ORDREIN-IDSET
           END-IF
 
           IF  VERKINN-PROCESS
               PERFORM VERKINN-CHK-LEVEL
           END-IF
 
           IF  ORDREIN-PROCESS
               PERFORM ORDREIN-CHK-LEVEL
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
 
           IF  VERKINN-PROCESS
               PERFORM VERKINN-FLDOFF
               PERFORM VERKINN-FLDSET
           END-IF
 
           IF  ORDREIN-PROCESS
               PERFORM ORDREIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VERKINN-PROCESS
           OR  ORDREIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-21                    TO TRUE
           IF  (I-L2)
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
               SET NOT-I-30                TO TRUE
               SET TABFRA-S                TO TABFRA-I
               PERFORM WITH TEST AFTER
                       VARYING TABFRA-I FROM 1 BY 1
                         UNTIL TABFRA-I >= TABFRA-MAX
                            OR I-30
                   IF  VFIR = TABFRA (TABFRA-I)
                       SET I-30            TO TRUE
                       SET TABFRA-S        TO TABFRA-I
                   END-IF
               END-PERFORM
               SET TABFRA-I                TO TABFRA-S
               IF  I-30
               AND TABFRA-I NOT > TABTIL-MAX
                   SET TABTIL-I            TO TABFRA-I
               END-IF
      *  L2                MOVE "VFIR    "BUGFL2  8        LEDETXT DEBUG
      *  L2      BUGFL2    DEBUGBUGFILO   VFIR             VIS FELT/IND
           END-IF
           IF  (I-L2)
               MOVE TABTIL(TABTIL-I) (1:3) TO TILFIR
               MOVE TABTIL(TABTIL-I) (4:1)  TO KODE
               SET NOT-I-31                TO TRUE
               IF  KODE = 'K'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-MR AND NOT-I-12)
               ADD 1                       TO ANTMR
           END-IF
           IF  (NOT-I-12)
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-12)
               ADD 1                       TO ANT12
           END-IF
           IF  (NOT-I-MR AND NOT-I-12)
               ADD 1                       TO ANTUT
           END-IF
           IF  (NOT-I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-12)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-30)
               ADD 1                       TO ANTUT
               GO TO SLUTT-T
           END-IF
           IF  (I-31)
               ADD 1                       TO ANTKOP
           END-IF
           IF  (NOT-I-31)
               ADD 1                       TO ANTFLY
           END-IF
           IF  (I-31)
               ADD 2                       TO ANTUT
           END-IF
           IF  (NOT-I-31)
               ADD 1                       TO ANTUT
           END-IF
           IF  (I-01)
               SET NOT-I-25                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  VKS011 = 'V'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-20)
               MOVE VKNR01                 TO KKEY
               PERFORM LESKUN-S
           END-IF
           IF  (I-02)
               SET NOT-I-25                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  VKS021 = 'V'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-20)
               MOVE VKNR02                 TO KKEY
               PERFORM LESKUN-S
           END-IF
           IF  (I-03)
               SET NOT-I-25                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  VKS031 = 'V'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-20)
               MOVE VKNR03                 TO KKEY
               PERFORM LESKUN-S
           END-IF
           IF  (I-04)
               SET NOT-I-26                TO TRUE
           END-IF
           IF  (I-03)
               SET NOT-I-20                TO TRUE
               IF  VRKNR1 = 'V'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-20 AND NOT-I-21)
               PERFORM RABKUN-S
           END-IF
           IF  (I-04)
               SET NOT-I-27                TO TRUE
               MOVE VFIR                   TO AKEY (1:3)
               MOVE VAVD                   TO AKEY (4:1)
               SET NOT-I-27                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-27
                   IF  AKEY = TABAVD (TABAVD-I)
                       SET I-27            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-27
               AND TABAVD-I NOT > TABFIA-MAX
                   SET TABFIA-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-04 AND I-27)
               MOVE TABFIA(TABFIA-I) (4:1)  TO TILAVD
               SET NOT-I-27                TO TRUE
               IF  VAVD NOT = TILAVD
                   SET I-27                TO TRUE
               END-IF
           END-IF.
 
       SLUTT-T.
      ******************** SR ****************************************
           CONTINUE.
 
       LESKUN-S SECTION.
       LESKUN-S-P.
           SET I-22                        TO TRUE
           SET I-23                        TO TRUE
           SET I-24                        TO TRUE
           MOVE VFIR                       TO KKEY (1:3)
           MOVE KKEY                       TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-22                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-22                TO TRUE
               PERFORM KUNDEMA-FLDOFF
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (NOT-I-22 AND NOT-I-24)
               MOVE TILFIR                 TO ALTKEY (1:3)
               MOVE KNDALT                 TO ALTKEY (4:6)
               MOVE KNDALT                 TO TILKNR
               MOVE ALTKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-23                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-23            TO TRUE
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (NOT-I-23 AND NOT-I-24)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-24)
               ADD 1                       TO ALTNUL
           END-IF
           IF  (I-25)
               ADD 1                       TO NYTTNR
           END-IF.
      *
 
       RABKUN-S SECTION.
       RABKUN-S-P.
           SET I-22                        TO TRUE
           SET I-23                        TO TRUE
           SET I-24                        TO TRUE
           MOVE VFIR                       TO KKEY (1:3)
           MOVE VRKNR                      TO KKEY
           MOVE KKEY                       TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-22                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-22                TO TRUE
               PERFORM KUNDEMA-FLDOFF
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (NOT-I-22 AND NOT-I-24)
               MOVE TILFIR                 TO ALTKEY (1:3)
               MOVE KNDALT                 TO ALTKEY (4:6)
               MOVE KNDALT                 TO TILRAB
               MOVE ALTKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-23                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-23            TO TRUE
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (NOT-I-23 AND NOT-I-24)
               SET I-26                    TO TRUE
           END-IF
           IF  (I-26)
               ADD 1                       TO NYTRAB
           END-IF.
      ******************** OUT ***************************************
      * FIRMA SOM IKKE ER BERØRT ELLER DET SKAL KOPIERES FRA.
 
       VERKINN-GET SECTION.
       VERKINN-GET-P.
           IF  VERKINN-EOF-OFF
               READ VERKINN
               AT END
                   SET VERKINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VERKINN-FLDOFF SECTION.
       VERKINN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               SET NOT-I-21                TO TRUE
           END-EVALUATE.
 
       VERKINN-FLDSET SECTION.
       VERKINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC01 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
               MOVE VERKINN-IO-AREA (37:6) TO VKNR01 (1:6)
               MOVE VERKINN-IO-AREA (37:1) TO VKS011 (1:1)
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC02 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
               MOVE VERKINN-IO-AREA (31:6) TO VKNR02 (1:6)
               MOVE VERKINN-IO-AREA (31:1) TO VKS021 (1:1)
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC03 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
               MOVE VERKINN-IO-AREA (31:6) TO VKNR03 (1:6)
               MOVE VERKINN-IO-AREA (31:1) TO VKS031 (1:1)
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC04 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
               MOVE VERKINN-IO-AREA (37:6) TO VRKNR (1:6)
               IF  VRKNR = SPACES
                   SET I-21                TO TRUE
               END-IF
               MOVE VERKINN-IO-AREA (37:1) TO VRKNR1 (1:1)
               MOVE VERKINN-IO-AREA (95:1) TO VAVD (1:1)
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '5' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC05 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC06 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC07 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC08 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC09 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
           WHEN ( VERKINN-IO-AREA (10:1) = 'V'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC10 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
           WHEN ( VERKINN-IO-AREA (10:1) = 'S'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:200) TO VREC11 (1:200)
               MOVE VERKINN-IO-AREA (1:3)  TO VFIR (1:3)
               MOVE VERKINN-IO-AREA (4:6)  TO VORD (1:6)
           END-EVALUATE.
 
       VERKINN-IDCHK SECTION.
       VERKINN-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '1' )
             OR ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '2' )
             OR ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '3' )
             OR ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '4' )
             OR ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '5' )
             OR ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '1' )
             OR ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '2' )
             OR ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '3' )
             OR ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '4' )
             OR ( VERKINN-IO-AREA (10:1) = 'V'
            AND   VERKINN-IO-AREA (11:1) = '1' )
             OR ( VERKINN-IO-AREA (10:1) = 'S'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VERKINN-IDSET SECTION.
       VERKINN-IDSET-P.
           EVALUATE TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               SET I-04                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '5' )
               SET I-05                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               SET I-06                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               SET I-07                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               SET I-08                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               SET I-09                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'V'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               SET I-10                    TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'S'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               SET I-11                    TO TRUE
           END-EVALUATE.
 
       VERKINN-CHK-LEVEL SECTION.
       VERKINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-01
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-01-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-01-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-02
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-02-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-02-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-03
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-03-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-03-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-04
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-04-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-04-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '5' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-05
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-05-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-05-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-05-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-06
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-06-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-06-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-06-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-07
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-07-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-07-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-07-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-08
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-08-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-08-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-08-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-09
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-09-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-09-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-09-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'V'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-10
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-10-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-10-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-10-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'S'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE LOW-VALUES             TO VERKINN-LEVEL-11
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-11-L2-VFIR
               IF  VERKINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKINN-11-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKINN-11-L2         TO THE-PRIOR-L2
               SET VERKINN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VERKINN-MATCH-SET SECTION.
       VERKINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-01-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-01-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-02-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-02-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-03-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-03-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-04-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-04-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'H'
            AND   VERKINN-IO-AREA (11:1) = '5' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-05-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-05-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-06-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-06-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '2' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-07-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-07-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '3' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-08-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-08-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'T'
            AND   VERKINN-IO-AREA (11:1) = '4' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-09-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-09-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'V'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-10-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-10-M1-VORD
           WHEN ( VERKINN-IO-AREA (10:1) = 'S'
            AND   VERKINN-IO-AREA (11:1) = '1' )
               MOVE VERKINN-IO-AREA (1:3)  TO VERKINN-M-11-M2-VFIR
               MOVE VERKINN-IO-AREA (4:6)  TO VERKINN-M-11-M1-VORD
           END-EVALUATE.
 
       ORDREIN-GET SECTION.
       ORDREIN-GET-P.
           IF  ORDREIN-EOF-OFF
               READ ORDREIN
               AT END
                   SET ORDREIN-EOF         TO TRUE
               END-READ
           END-IF.
 
       ORDREIN-FLDSET SECTION.
       ORDREIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDREIN-IO-AREA (1:3)  TO OFIR (1:3)
               MOVE ORDREIN-IO-AREA (4:6)  TO OORD (1:6)
           END-EVALUATE.
 
       ORDREIN-IDSET SECTION.
       ORDREIN-IDSET-P.
           SET I-12                        TO TRUE.
 
       ORDREIN-CHK-LEVEL SECTION.
       ORDREIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDREIN-LEVEL-12
               MOVE ORDREIN-IO-AREA (1:3)  TO ORDREIN-12-L2-OFIR
               IF  ORDREIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREIN-12-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  ORDREIN-12-L2         TO THE-PRIOR-L2
               SET ORDREIN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       ORDREIN-MATCH-SET SECTION.
       ORDREIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDREIN-IO-AREA (1:3)  TO ORDREIN-M-12-M2-OFIR
               MOVE ORDREIN-IO-AREA (4:6)  TO ORDREIN-M-12-M1-OORD
           END-EVALUATE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-24                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (172:6) TO KNDALT (1:6)
               IF  KNDALT = SPACES
                   SET I-24                TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-15                        TO TRUE.
 
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
           IF  VERKINN-EOF
               MOVE HIGH-VALUES            TO VERKINN-MC
                                              VERKINN-MP
           END-IF
           IF  ORDREIN-EOF
               MOVE HIGH-VALUES            TO ORDREIN-MC
                                              ORDREIN-MP
           END-IF
           IF  VERKINN-MC < VERKINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDREIN-MC < ORDREIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VERKINN-MC < ORDREIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKINN-PROCESS     TO TRUE
                   MOVE VERKINN-MC         TO VERKINN-MP
                   IF  VERKINN-MC = ORDREIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDREIN-MC < VERKINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDREIN-PROCESS     TO TRUE
                   MOVE ORDREIN-MC         TO ORDREIN-MP
                   IF  ORDREIN-MC = VERKINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VERKINN-MC = ORDREIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKINN-PROCESS     TO TRUE
                   MOVE VERKINN-MC         TO VERKINN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       FIRTAB-LOAD SECTION.
       FIRTAB-LOAD-P.
           OPEN INPUT FIRTAB
           SET TABFRA-I                    TO 1
           PERFORM UNTIL FIRTAB-EOF
               READ FIRTAB
               AT END
                   SET FIRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FIRTAB-IO-AREA (1:7) TO TABFRA-ENTRY (TABFRA-I)
                   SET TABFRA-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FIRTAB.
 
       AVDTAB-LOAD SECTION.
       AVDTAB-LOAD-P.
           OPEN INPUT AVDTAB
           SET TABAVD-I                    TO 1
           PERFORM UNTIL AVDTAB-EOF
               READ AVDTAB
               AT END
                   SET AVDTAB-EOF          TO TRUE
               NOT AT END
                   MOVE AVDTAB-IO-AREA (1:8) TO TABAVD-ENTRY (TABAVD-I)
                   SET TABAVD-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE AVDTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-30)
           OR  (I-01 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-01 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC01                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-30)
           OR  (I-02 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-02 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC02                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-03 AND NOT-I-30)
           OR  (I-03 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-03 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC03                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-04 AND NOT-I-30)
           OR  (I-04 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-04 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC04                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-05 AND NOT-I-30)
           OR  (I-05 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-05 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC05                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-06 AND NOT-I-30)
           OR  (I-06 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-06 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC06                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-07 AND NOT-I-30)
           OR  (I-07 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-07 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC07                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-08 AND NOT-I-30)
           OR  (I-08 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-08 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC08                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-09 AND NOT-I-30)
           OR  (I-09 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-09 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC09                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-10 AND NOT-I-30)
           OR  (I-10 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-10 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC10                 TO VERKUT-IO-AREA (1:200)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-11 AND NOT-I-30)
           OR  (I-11 AND I-30 AND I-31)
           AND (I-MR)
           OR  (I-11 AND I-30 AND NOT-I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC11                 TO VERKUT-IO-AREA (1:200)
      * FIRMA SOM SKAL FLYTTES ELLER KOPIERES.
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-01 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC01                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               IF  (I-25)
                   MOVE TILKNR             TO VERKUT-IO-AREA (37:6)
               END-IF
               IF  (I-25)
                   MOVE TILKNR             TO VERKUT-IO-AREA (43:6)
               END-IF
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-02 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC02                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               IF  (I-25)
                   MOVE TILKNR             TO VERKUT-IO-AREA (31:6)
               END-IF
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-03 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC03                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               IF  (I-25)
                   MOVE TILKNR             TO VERKUT-IO-AREA (31:6)
               END-IF
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-04 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC04                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               IF  (I-26)
                   MOVE TILRAB             TO VERKUT-IO-AREA (37:6)
               END-IF
               IF  (I-27)
                   MOVE TILAVD             TO VERKUT-IO-AREA (95:1)
               END-IF
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-05 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC05                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-06 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC06                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-07 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC07                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-08 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC08                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-09 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC09                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-10 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC10                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-11 AND I-30 AND I-MR)
               MOVE SPACES TO VERKUT-IO-AREA
               INITIALIZE VERKUT-IO-AREA
               MOVE VREC11                 TO VERKUT-IO-AREA (1:200)
               MOVE TILFIR                 TO VERKUT-IO-AREA (1:3)
               MOVE TILFIR                 TO VERKUT-IO-AREA (16:3)
      *
               WRITE VERKUT-IO-AREA
           END-IF
           IF  (I-L2 AND I-30)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA FRA:'           TO LISTE-IO-AREA (1:10)
               MOVE VFIR                   TO LISTE-IO-AREA (13:3)
               MOVE 'FIRMA TIL:'           TO LISTE-IO-AREA (21:10)
               MOVE TILFIR                 TO LISTE-IO-AREA (33:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INN      :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FLYTTET  :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTFLY                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KOPIERT  :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTKOP                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'UT       :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NYE KNR  :'           TO LISTE-IO-AREA (1:10)
               MOVE NYTTNR                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BLANKE NR:'           TO LISTE-IO-AREA (1:10)
               MOVE ALTNUL                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NYE RABNR:'           TO LISTE-IO-AREA (1:10)
               MOVE NYTRAB                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT ORDRE:'           TO LISTE-IO-AREA (1:10)
               MOVE ANT12                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT MR   :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTMR                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U8 AND I-15)
           AND (I-12)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (10:1)
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
           PERFORM FIRTAB-LOAD
           PERFORM AVDTAB-LOAD
           SET VERKINN-LEVEL-INIT          TO TRUE
           INITIALIZE VERKINN-DATA-FIELDS
           SET VERKINN-EOF-OFF             TO TRUE
           SET VERKINN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VERKINN-MC
                                              VERKINN-MP
           OPEN INPUT VERKINN
           SET ORDREIN-LEVEL-INIT          TO TRUE
           INITIALIZE ORDREIN-DATA-FIELDS
           SET ORDREIN-EOF-OFF             TO TRUE
           SET ORDREIN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO ORDREIN-MC
                                              ORDREIN-MP
           OPEN INPUT ORDREIN
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT VERKUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABFRA-I                    TO 1
           SET TABAVD-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VERKINN
           CLOSE ORDREIN
           CLOSE KUNDEMA
           CLOSE VERKUT
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
