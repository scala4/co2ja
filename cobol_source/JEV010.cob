       IDENTIFICATION DIVISION.
       PROGRAM-ID. JEV010R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: AUD010
      *  PROGRAMERER: STEIN SANDVOLD                                  *
      *  PROGRAMERT.: 10.10.2002                                      *
      *                                                               *
      *  PROGRAMMET DANNER AUTOMATISK FAKTURA FOR AUTODATA            *
      *  GJELDER ALLE KUNDER PÅ HND 110                               *
      *  FØLGENDE FELTER LIGGER I INNDATA MEN IKKE HENSYNTATT         *
      *  FORELØPIG:                                                   *
      *       35 - 37 RABATT 99,9.                                    *
      *       38 - 45 PRIS   999999,99.                               *
      *       46 - 75 VAREBET                                         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: JEV010.rpg
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
           SELECT INNDATA
               ASSIGN TO UT-S-INNDATA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNDATA-STATUS.
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS OPPSMAS-STATUS
               RECORD KEY IS OPPSMAS-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT UTDATA
               ASSIGN TO UT-S-UTDATA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTDATA-STATUS.
           SELECT LISTE2
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNDATA
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  INNDATA-IO-AREA.
           05  INNDATA-IO-AREA-X           PICTURE X(80).
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X.
               10  OPPSMAS-KEY1            PICTURE X(21).
               10  FILLER                  PICTURE X(9).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD UTDATA
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  UTDATA-IO-AREA.
           05  UTDATA-IO-AREA-X            PICTURE X(80).
       FD LISTE2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE2-IO-PRINT.
           05  LISTE2-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE2-IO-AREA.
           05  LISTE2-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNDATA-STATUS              PICTURE 99 VALUE 0.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  UTDATA-STATUS               PICTURE 99 VALUE 0.
           10  LISTE2-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNDATA-EOF-OFF         VALUE '0'.
               88  INNDATA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNDATA-READ-OFF        VALUE '0'.
               88  INNDATA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNDATA-PROCESS-OFF     VALUE '0'.
               88  INNDATA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNDATA-LEVEL-INIT-OFF  VALUE '0'.
               88  INNDATA-LEVEL-INIT      VALUE '1'.
           05  OPPSMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LISTE2-DATA-FIELDS.
               10  LISTE2-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-CLR-IO           PICTURE X VALUE 'Y'.
           05  INNDATA-LEVEL-01.
               10  INNDATA-01-L2.
                   15  INNDATA-01-L2-FIRMA PICTURE X(3).
               10  INNDATA-01-L1.
                   15  INNDATA-01-L1-AVSKNR PICTURE X(6).
           05  INNDATA-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  SNR                     PICTURE X(3).
               10  AVSKNR                  PICTURE X(6).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(14).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(5).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(6)V9(2).
               10  NAVN                    PICTURE X(10).
               10  SIGN-X                  PICTURE X(2).
           05  OPPSMAS-DATA-FIELDS.
               10  EDBNR                   PICTURE X(7).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN                   PICTURE X(30).
               10  KPSTED                  PICTURE X(15).
               10  KPNR                    PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KNRKEY                  PICTURE X(9).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  ANTLES-IO.
                   15  ANTLES              PICTURE S9(6).
               10  KUNBEL-IO.
                   15  KUNBEL              PICTURE S9(6)V9(2).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(6)V9(2).
               10  VARKEY                  PICTURE X(21).
               10  FELT4                   PICTURE X(4).
               10  FELT18                  PICTURE X(18).
               10  ANTBST-IO.
                   15  ANTBST              PICTURE S9(5)V9(2).
               10  NYEDBN                  PICTURE X(7).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-62YY9                PICTURE ZZZ.ZZZ,99.
               10  EDIT-BEL                PICTURE Z999999,99.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNDATA-PROCESS
               SET INNDATA-PROCESS-OFF     TO TRUE
               SET INNDATA-READ            TO TRUE
           END-IF
 
           IF  INNDATA-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNDATA-GET
               SET INNDATA-READ-OFF        TO TRUE
               IF  NOT INNDATA-EOF
                   PERFORM INNDATA-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET INNDATA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNDATA-PROCESS
               PERFORM INNDATA-IDSET
           END-IF
 
           IF  INNDATA-PROCESS
               PERFORM INNDATA-CHK-LEVEL
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
 
           IF  INNDATA-PROCESS
               PERFORM INNDATA-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNDATA-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '399'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-11)
               GO TO SLUTT-T
      *****************************************************************
      *  SJEKKE ATT KUNDENR. FINNES I KUNDEMASTER.                    *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO KNRKEY (1:3)
               MOVE AVSKNR                 TO KNRKEY (4:6)
               MOVE KNRKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               SET NOT-I-52                TO TRUE
               SET NOT-I-53                TO TRUE
               SET NOT-I-55                TO TRUE
      *****************************************************************
      * RUTINE FOR FØRSTE RECORD PR. SENDING PR. MOTTAGER.            *
      *****************************************************************
           END-IF
           IF  (I-01)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
           END-IF
           IF  (I-L1)
               SUBTRACT ANTLES             FROM ANTLES
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTLES
               ADD BEL                     TO KUNBEL
               ADD BEL                     TO TOTBEL
      *****************************************************************
      *  RUTINE FOR VARELINJERECORDS.                                 *
      *  OM DETTE EAN-NR IKKE ER INNMELDT SETTES EDB-NR TIL 9999981   *
      *     PASS PÅ OG MELDE DETTE INN I VARE.MASTER PÅ NYE FIRMA.    *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE FIRMA                  TO VARKEY (1:3)
               MOVE ALFA                   TO FELT4 (1:3)
               MOVE ' '                    TO FELT4 (4:1)
               MOVE ARTNR                  TO FELT18 (5:14)
               MOVE FELT4                  TO FELT18 (1:4)
               MOVE FELT18                 TO VARKEY (4:18)
               MOVE VARKEY                 TO OPPSMAS-KEY1
               READ OPPSMAS RECORD KEY IS OPPSMAS-KEY1
               INVALID KEY
                   SET I-13                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-13            TO TRUE
                   PERFORM OPPSMAS-FLDSET
                   PERFORM OPPSMAS-IDSET
               END-READ
               ADD ANTALL TO ZERO      GIVING ANTBST
           END-IF
           IF  (I-01 AND NOT-I-13)
               MOVE EDBNR                  TO NYEDBN
               GO TO SLUTT-T
      *****************************************************************
      * SLUTT RUTINE.                                                 *
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           IF  (I-01 AND NOT-I-50)
               ADD 1                       TO SEQNR
           END-IF
           SET NOT-I-52                    TO TRUE
           SET NOT-I-54                    TO TRUE
           IF  (I-01 AND NOT-I-11)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND I-12)
               SET I-50                    TO TRUE
               SET I-51                    TO TRUE
           END-IF
           IF  (I-01 AND I-13)
               SET I-51                    TO TRUE
           END-IF
           IF  (I-01 AND I-13 AND NOT-I-12)
               AND (NOT-I-55)
               SET I-54                    TO TRUE
               SET I-55                    TO TRUE
           END-IF
           IF  (I-51 AND NOT-I-53)
               SET I-52                    TO TRUE
               SET I-53                    TO TRUE
      *****************************************************************
      * LISTE2 = DETTE ER EN KVITTERINGSLISTE FOR ALLE ORDRE SOM      *
      *          KOMMER INN VIA HÅNDTERMINALER.                       *
      *****************************************************************
           END-IF
           .
 
       INNDATA-GET SECTION.
       INNDATA-GET-P.
           IF  INNDATA-EOF-OFF
               READ INNDATA
               AT END
                   SET INNDATA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNDATA-FLDSET SECTION.
       INNDATA-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INNDATA-IO-AREA (1:1) NOT = ' ' )
               MOVE INNDATA-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE INNDATA-IO-AREA (4:3)  TO SNR (1:3)
               MOVE INNDATA-IO-AREA (7:6)  TO AVSKNR (1:6)
               MOVE INNDATA-IO-AREA (13:3) TO ALFA (1:3)
               MOVE INNDATA-IO-AREA (16:14) TO ARTNR (1:14)
               MOVE INNDATA-IO-AREA (30:5) TO ANTALL-IO
               INSPECT ANTALL-IO REPLACING ALL ' ' BY '0'
               MOVE INNDATA-IO-AREA (38:8) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE INNDATA-IO-AREA (46:10) TO NAVN (1:10)
               MOVE INNDATA-IO-AREA (57:2) TO SIGN-X (1:2)
           END-EVALUATE.
 
       INNDATA-IDCHK SECTION.
       INNDATA-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INNDATA-IO-AREA (1:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INNDATA-IDSET SECTION.
       INNDATA-IDSET-P.
           EVALUATE TRUE
           WHEN ( INNDATA-IO-AREA (1:1) NOT = ' ' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       INNDATA-CHK-LEVEL SECTION.
       INNDATA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INNDATA-IO-AREA (1:1) NOT = ' ' )
               MOVE LOW-VALUES             TO INNDATA-LEVEL-01
               MOVE INNDATA-IO-AREA (1:3)  TO INNDATA-01-L2-FIRMA
               MOVE INNDATA-IO-AREA (7:6)  TO INNDATA-01-L1-AVSKNR
               IF  INNDATA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNDATA-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNDATA-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNDATA-01-L2         TO THE-PRIOR-L2
               MOVE  INNDATA-01-L1         TO THE-PRIOR-L1
               SET INNDATA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       OPPSMAS-FLDSET SECTION.
       OPPSMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (23:7) TO EDBNR (1:7)
           END-EVALUATE.
 
       OPPSMAS-IDSET SECTION.
       OPPSMAS-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO KPSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO KPNR (1:4)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-06                        TO TRUE.
 
       LISTE2-PRINT-LINE SECTION.
       LISTE2-PRINT-LINE-P.
           IF  LISTE2-BEFORE-SKIP > 0
               PERFORM LISTE2-SKIP-BEFORE
           END-IF
           IF  LISTE2-BEFORE-SPACE > 0
               PERFORM LISTE2-SPACE-BEFORE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               IF  LISTE2-AFTER-SPACE > 0
                   PERFORM LISTE2-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               PERFORM LISTE2-SPACE-AFTER
           END-IF
           IF  LISTE2-LINE-COUNT NOT < LISTE2-MAX-LINES
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
           END-IF.
 
       LISTE2-SKIP-BEFORE SECTION.
       LISTE2-SKIP-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-BEFORE-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-BEFORE SECTION.
       LISTE2-SPACE-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER LISTE2-BEFORE-SPACE LINES
           ADD LISTE2-BEFORE-SPACE         TO LISTE2-LINE-COUNT
           MOVE SPACES TO LISTE2-IO-AREA
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-BEFORE-SPACE.
 
       LISTE2-SKIP-AFTER SECTION.
       LISTE2-SKIP-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-AFTER-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-AFTER SECTION.
       LISTE2-SPACE-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE LISTE2-AFTER-SPACE LINES
           ADD LISTE2-AFTER-SPACE          TO LISTE2-LINE-COUNT
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE FIRMA                  TO LISTE2-IO-AREA (1:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (17:8)
               MOVE AVSKNR                 TO LISTE2-IO-AREA (38:6)
               MOVE SNR                    TO LISTE2-IO-AREA (60:3)
               MOVE 'RECORD GODKJENT '     TO LISTE2-IO-AREA (68:16)
               IF  (I-12)
                   MOVE 'FEIL KUNDENR    ' TO LISTE2-IO-AREA (68:16)
               END-IF
               IF  (I-13)
                   MOVE 'FEIL ARTNR      ' TO LISTE2-IO-AREA (68:16)
               END-IF
               MOVE NAVN                   TO LISTE2-IO-AREA (123:10)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-50)
               MOVE SPACES TO UTDATA-IO-AREA
               INITIALIZE UTDATA-IO-AREA
               MOVE '2;4;'                 TO UTDATA-IO-AREA (1:4)
               MOVE AVSKNR                 TO UTDATA-IO-AREA (5:6)
               MOVE ';'                    TO UTDATA-IO-AREA (11:1)
               MOVE 'BST.OPPG.'            TO UTDATA-IO-AREA (12:9)
               MOVE ';'                    TO UTDATA-IO-AREA (21:1)
               MOVE '000001'               TO UTDATA-IO-AREA (22:6)
               MOVE ';'                    TO UTDATA-IO-AREA (28:1)
               MOVE BEL                    TO EDIT-BEL
               MOVE EDIT-BEL               TO UTDATA-IO-AREA (42:10)
               MOVE ';'                    TO UTDATA-IO-AREA (52:1)
               MOVE BEL                    TO EDIT-BEL
               MOVE EDIT-BEL               TO UTDATA-IO-AREA (54:10)
               WRITE UTDATA-IO-AREA
               MOVE SPACES TO UTDATA-IO-AREA
               INITIALIZE UTDATA-IO-AREA
               MOVE '1;4;'                 TO UTDATA-IO-AREA (1:4)
               MOVE AVSKNR                 TO UTDATA-IO-AREA (5:6)
               MOVE ';'                    TO UTDATA-IO-AREA (11:1)
               MOVE NAVN                   TO UTDATA-IO-AREA (12:10)
               MOVE ';'                    TO UTDATA-IO-AREA (22:1)
               WRITE UTDATA-IO-AREA
               MOVE SPACES TO UTDATA-IO-AREA
               INITIALIZE UTDATA-IO-AREA
               MOVE '1;4;'                 TO UTDATA-IO-AREA (1:4)
               MOVE AVSKNR                 TO UTDATA-IO-AREA (5:6)
               MOVE ';'                    TO UTDATA-IO-AREA (11:1)
               MOVE SIGN-X                 TO UTDATA-IO-AREA (12:2)
               MOVE ';'                    TO UTDATA-IO-AREA (14:1)
               WRITE UTDATA-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-01 AND I-L2)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'LISTE ORDRE TIL AUTO' TO LISTE2-IO-AREA (6:20)
               MOVE 'DATA.  FAK399D'       TO LISTE2-IO-AREA (26:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (46:8)
               MOVE 'KLOKKEN'              TO LISTE2-IO-AREA (56:7)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (64:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'FNR'                  TO LISTE2-IO-AREA (1:3)
               MOVE 'AVS.DATO'             TO LISTE2-IO-AREA (17:8)
               MOVE 'AVS.TIDSP'            TO LISTE2-IO-AREA (26:9)
               MOVE 'KUNDENR'              TO LISTE2-IO-AREA (37:7)
               MOVE 'AVSENDERS NAVN'       TO LISTE2-IO-AREA (45:14)
               MOVE 'SNR'                  TO LISTE2-IO-AREA (60:3)
               MOVE 'VAREBETEGNELSE  '     TO LISTE2-IO-AREA (100:16)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE 2                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'LISTE ORDRE TIL AUTO' TO LISTE2-IO-AREA (6:20)
               MOVE 'DATA.  FAK399D'       TO LISTE2-IO-AREA (26:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (46:8)
               MOVE 'KLOKKEN'              TO LISTE2-IO-AREA (56:7)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (64:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'FNR'                  TO LISTE2-IO-AREA (1:3)
               MOVE 'AVS.DATO'             TO LISTE2-IO-AREA (17:8)
               MOVE 'AVS.TIDSP'            TO LISTE2-IO-AREA (26:9)
               MOVE 'KUNDENR'              TO LISTE2-IO-AREA (37:7)
               MOVE 'AVSENDERS NAVN'       TO LISTE2-IO-AREA (45:14)
               MOVE 'SNR'                  TO LISTE2-IO-AREA (60:3)
               MOVE 'VAREBETEGNELSE  '     TO LISTE2-IO-AREA (100:16)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE 2                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TOTALT BEL. KUNDE'    TO LISTE2-IO-AREA (2:17)
               MOVE KUNBEL                 TO XO-62YY9
               MOVE XO-62YY9               TO LISTE2-IO-AREA (19:10)
               INITIALIZE KUNBEL
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 3                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-L2)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TOTALT BEL. FIRMA'    TO LISTE2-IO-AREA (2:17)
               MOVE TOTBEL                 TO XO-62YY9
               MOVE XO-62YY9               TO LISTE2-IO-AREA (19:10)
               INITIALIZE TOTBEL
               MOVE 'TOTALT ANT. RECORDS'  TO LISTE2-IO-AREA (37:19)
      *                        ANTREC1   66
               IF  (I-50)
                   MOVE 'INNEHOLDER FEIL ' TO LISTE2-IO-AREA (68:16)
               END-IF
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
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
           SET INNDATA-LEVEL-INIT          TO TRUE
           INITIALIZE INNDATA-DATA-FIELDS
           SET INNDATA-EOF-OFF             TO TRUE
           SET INNDATA-PROCESS             TO TRUE
           OPEN INPUT INNDATA
           INITIALIZE OPPSMAS-DATA-FIELDS
           OPEN INPUT OPPSMAS
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT UTDATA
           OPEN OUTPUT LISTE2
           INITIALIZE LISTE2-IO-AREA
           INITIALIZE LISTE2-DATA-FIELDS
           MOVE 57                         TO LISTE2-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNDATA
           CLOSE OPPSMAS
           CLOSE KUNDEMA
           CLOSE UTDATA
           IF LISTE2-IO-AREA NOT = SPACES
             WRITE LISTE2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE2-IO-AREA
           END-IF
           CLOSE LISTE2.
 
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
