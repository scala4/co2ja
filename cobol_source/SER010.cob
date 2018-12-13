       IDENTIFICATION DIVISION.
       PROGRAM-ID. SER010R.
      **********************************************  Z-WIN-RPG2   ****
      ** LIGGER PÅ RWEB I EXCEL                                   ******
      *   SERVICEPROSENTRAPPORT.                                     *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: SER010.rpg
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
           SELECT PFILE
               ASSIGN TO UT-S-PFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PFILE-STATUS.
           SELECT FIRMAK
               ASSIGN TO FIRMAK
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAK-STATUS
               RECORD KEY IS FIRMAK-KEY1.
           SELECT SFILE
               ASSIGN TO UT-S-SFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PFILE
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  PFILE-IO-AREA.
           05  PFILE-IO-AREA-X             PICTURE X(100).
       FD FIRMAK
               RECORD CONTAINS 1000.
       01  FIRMAK-IO-AREA.
           05  FIRMAK-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAK-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD SFILE
               BLOCK CONTAINS 1950
               RECORD CONTAINS 30.
       01  SFILE-IO-AREA.
           05  SFILE-IO-AREA-X             PICTURE X(30).
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
           10  PFILE-STATUS                PICTURE 99 VALUE 0.
           10  FIRMAK-STATUS               PICTURE 99 VALUE 0.
           10  SFILE-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-EOF-OFF           VALUE '0'.
               88  PFILE-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-READ-OFF          VALUE '0'.
               88  PFILE-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-PROCESS-OFF       VALUE '0'.
               88  PFILE-PROCESS           VALUE '1'.
           05  FIRMAK-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  SFILE-EOF-OFF           VALUE '0'.
               88  SFILE-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SFILE-READ-OFF          VALUE '0'.
               88  SFILE-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SFILE-PROCESS-OFF       VALUE '0'.
               88  SFILE-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SFILE-LEVEL-INIT-OFF    VALUE '0'.
               88  SFILE-LEVEL-INIT        VALUE '1'.
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
           05  PFILE-DATA-FIELDS.
               10  A-ELGR                  PICTURE X(4).
               10  MNDNAV                  PICTURE X(9).
           05  FIRMAK-DATA-FIELDS.
               10  AVDN1                   PICTURE X(10).
               10  AVDN2                   PICTURE X(10).
               10  AVDN3                   PICTURE X(10).
               10  AVDN4                   PICTURE X(10).
               10  AVDN5                   PICTURE X(10).
               10  AVDN6                   PICTURE X(10).
               10  AVDN7                   PICTURE X(10).
               10  AVDN8                   PICTURE X(10).
               10  AVDN9                   PICTURE X(10).
               10  AVDNC                   PICTURE X(10).
               10  AVDNA                   PICTURE X(10).
               10  AVDNB                   PICTURE X(10).
           05  SFILE-LEVEL-02.
               10  SFILE-02-L2.
                   15  SFILE-02-L2-FIRM    PICTURE X(3).
               10  SFILE-02-L1.
                   15  SFILE-02-L1-AVD     PICTURE X(1).
           05  SFILE-DATA-FIELDS.
               10  AVD                     PICTURE X(1).
               10  TANT-IO.
                   15  TANT                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  RANT-IO.
                   15  RANT                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TBEL-IO.
                   15  TBEL                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RBEL-IO.
                   15  RBEL                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FIRM                    PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  VTOT-IO.
                   15  VTOT                PICTURE S9(8).
               10  VRTOT-IO.
                   15  VRTOT               PICTURE S9(8).
               10  BTOT-IO.
                   15  BTOT                PICTURE S9(8)V9(2).
               10  BRTOT-IO.
                   15  BRTOT               PICTURE S9(8)V9(2).
               10  VTOTA-IO.
                   15  VTOTA               PICTURE S9(8).
               10  VRTOTA-IO.
                   15  VRTOTA              PICTURE S9(8).
               10  BTOTA-IO.
                   15  BTOTA               PICTURE S9(8)V9(2).
               10  BRTOTA-IO.
                   15  BRTOTA              PICTURE S9(8)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(8).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(10).
               10  SPRO-IO.
                   15  SPRO                PICTURE S9(2)V9(1).
               10  SUM3-IO.
                   15  SUM3                PICTURE S9(11).
               10  RPRO-IO.
                   15  RPRO                PICTURE S9(2)V9(1).
               10  TSPRO-IO.
                   15  TSPRO               PICTURE S9(2)V9(1).
               10  TRPRO-IO.
                   15  TRPRO               PICTURE S9(2)V9(1).
               10  AVDNAV                  PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
               10  XO-21YY9                PICTURE ZZ,9.
               10  XO-82YY9                PICTURE ZZ.ZZZ.ZZZ,99.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PFILE-PROCESS
               SET PFILE-PROCESS-OFF       TO TRUE
               SET PFILE-READ              TO TRUE
           END-IF
 
           IF  PFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM PFILE-GET
               SET PFILE-READ-OFF          TO TRUE
               IF  NOT PFILE-EOF
                   PERFORM PFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PFILE-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  SFILE-PROCESS
               SET SFILE-PROCESS-OFF       TO TRUE
               SET SFILE-READ              TO TRUE
           END-IF
 
           IF  SFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM SFILE-GET
               SET SFILE-READ-OFF          TO TRUE
               IF  NOT SFILE-EOF
                   PERFORM SFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET SFILE-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PFILE-PROCESS
               PERFORM PFILE-IDSET
           END-IF
 
           IF  SFILE-PROCESS
               PERFORM SFILE-IDSET
           END-IF
 
           IF  SFILE-PROCESS
               PERFORM SFILE-CHK-LEVEL
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
 
           IF  PFILE-PROCESS
               PERFORM PFILE-FLDSET
           END-IF
 
           IF  SFILE-PROCESS
               PERFORM SFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SFILE-PROCESS
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
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               MOVE FIRM                   TO FIRMAK-KEY1
               READ FIRMAK RECORD KEY IS FIRMAK-KEY1
               INVALID KEY
                   SET I-08                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-08            TO TRUE
                   PERFORM FIRMAK-FLDSET
                   PERFORM FIRMAK-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-08)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2)
               SET NOT-I-95                TO TRUE
               IF  FIRM = '950'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               PERFORM AVDRUT-S
      *
      *  FIRMATOTALSUMMER.
           END-IF
           ADD TANT                        TO VTOT
           SET NOT-I-21                    TO TRUE
           IF  VTOT = 0
               SET I-21                    TO TRUE
           END-IF
           ADD RANT                        TO VRTOT
           ADD TBEL                        TO BTOT
           SET NOT-I-22                    TO TRUE
           IF  BTOT = 0
               SET I-22                    TO TRUE
           END-IF
           ADD RBEL                        TO BRTOT
      *  AVD. TOTALSUMMER.
      *
           ADD TANT                        TO VTOTA
           SET NOT-I-23                    TO TRUE
           IF  VTOTA = 0
               SET I-23                    TO TRUE
           END-IF
           ADD RANT                        TO VRTOTA
           ADD TBEL                        TO BTOTA
           SET NOT-I-24                    TO TRUE
           IF  BTOTA = 0
               SET I-24                    TO TRUE
           END-IF
           ADD RBEL                        TO BRTOTA.
 
       SLUTT-T.
      *  UTREGNING AV SERVICEPROSENT PR AVD.
      *
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'SER01'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'SER010  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-U2)
               SET NOT-I-86                TO TRUE
               IF  LANTX < 2
                   SET I-86                TO TRUE
               END-IF
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR HENTING AV AVDELINGSNAVN.         *
      ******************************************************
 
       AVDRUT-S SECTION.
       AVDRUT-S-P.
           SET NOT-I-99                    TO TRUE
           IF  AVD = '0'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99 AND I-95)
               MOVE 'Ø.ASKEDA'             TO AVDNAV (1:8)
               MOVE 'L '                   TO AVDNAV (9:2)
           END-IF
           IF  (I-99 AND NOT-I-95)
               MOVE 'AVD. 0  '             TO AVDNAV (3:8)
           END-IF
           IF  (I-99)
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '1'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN1                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '2'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN2                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '3'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN3                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '4'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN4                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '5'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN5                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '6'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN6                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '7'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN7                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '8'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN8                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = '9'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDN9                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = 'A'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDNA                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = 'B'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDNB                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  AVD = 'C'
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE AVDNC                  TO AVDNAV
               GO TO ENDAVD-T
           END-IF.
 
       ENDAVD-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-86)
               GO TO L1END-T
           END-IF
           IF  (I-L1)
               SUBTRACT VRTOTA FROM VTOTA GIVING SUM1
               MULTIPLY 100 BY SUM1    GIVING SUM2
           END-IF
           IF  (I-L1 AND NOT-I-23)
               DIVIDE SUM2 BY VTOTA    GIVING SPRO ROUNDED
           END-IF
           IF  (I-L1)
               MULTIPLY 100 BY BRTOTA  GIVING SUM3
           END-IF
           IF  (I-L1 AND NOT-I-24)
               DIVIDE SUM3 BY BTOTA    GIVING RPRO ROUNDED
      *  UTREGNING AV SERVICEPROSENT TOTALT.
      *
           END-IF
           IF  (I-L2)
               SUBTRACT VRTOT FROM VTOT GIVING SUM1
               MULTIPLY 100 BY SUM1    GIVING SUM2
           END-IF
           IF  (I-L2 AND NOT-I-21)
               DIVIDE SUM2 BY VTOT     GIVING TSPRO ROUNDED
           END-IF
           IF  (I-L2)
               MULTIPLY 100 BY BRTOT   GIVING SUM3
           END-IF
           IF  (I-L2 AND NOT-I-22)
               DIVIDE SUM3 BY BTOT     GIVING TRPRO ROUNDED
           END-IF.
 
       L1END-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       PFILE-GET SECTION.
       PFILE-GET-P.
           IF  PFILE-EOF-OFF
               READ PFILE
               AT END
                   SET PFILE-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PFILE-FLDSET SECTION.
       PFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PFILE-IO-AREA (1:1) = '9'
            AND   PFILE-IO-AREA (2:1) = '0' )
               MOVE PFILE-IO-AREA (3:4)    TO A-ELGR (1:4)
               MOVE PFILE-IO-AREA (9:9)    TO MNDNAV (1:9)
           END-EVALUATE.
 
       PFILE-IDCHK SECTION.
       PFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PFILE-IO-AREA (1:1) = '9'
            AND   PFILE-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PFILE-IDSET SECTION.
       PFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( PFILE-IO-AREA (1:1) = '9'
            AND   PFILE-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       FIRMAK-FLDSET SECTION.
       FIRMAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAK-IO-AREA (405:10) TO AVDN1 (1:10)
               MOVE FIRMAK-IO-AREA (416:10) TO AVDN2 (1:10)
               MOVE FIRMAK-IO-AREA (427:10) TO AVDN3 (1:10)
               MOVE FIRMAK-IO-AREA (438:10) TO AVDN4 (1:10)
               MOVE FIRMAK-IO-AREA (449:10) TO AVDN5 (1:10)
               MOVE FIRMAK-IO-AREA (460:10) TO AVDN6 (1:10)
               MOVE FIRMAK-IO-AREA (471:10) TO AVDN7 (1:10)
               MOVE FIRMAK-IO-AREA (482:10) TO AVDN8 (1:10)
               MOVE FIRMAK-IO-AREA (493:10) TO AVDN9 (1:10)
               MOVE FIRMAK-IO-AREA (886:10) TO AVDNC (1:10)
               MOVE FIRMAK-IO-AREA (976:10) TO AVDNA (1:10)
               MOVE FIRMAK-IO-AREA (987:10) TO AVDNB (1:10)
           END-EVALUATE.
 
       FIRMAK-IDSET SECTION.
       FIRMAK-IDSET-P.
           SET I-06                        TO TRUE.
 
       SFILE-GET SECTION.
       SFILE-GET-P.
           IF  SFILE-EOF-OFF
               READ SFILE
               AT END
                   SET SFILE-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SFILE-FLDSET SECTION.
       SFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = '7'
            AND   SFILE-IO-AREA (2:1) = '1' )
               MOVE SFILE-IO-AREA (3:1)    TO AVD (1:1)
               MOVE SFILE-IO-AREA (10:3)   TO TANT-IO
               MOVE SFILE-IO-AREA (13:3)   TO RANT-IO
               MOVE SFILE-IO-AREA (16:5)   TO TBEL-IO
               MOVE SFILE-IO-AREA (21:5)   TO RBEL-IO
               MOVE SFILE-IO-AREA (28:3)   TO FIRM (1:3)
           END-EVALUATE.
 
       SFILE-IDCHK SECTION.
       SFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = '7'
            AND   SFILE-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       SFILE-IDSET SECTION.
       SFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = '7'
            AND   SFILE-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       SFILE-CHK-LEVEL SECTION.
       SFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( SFILE-IO-AREA (1:1) = '7'
            AND   SFILE-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO SFILE-LEVEL-02
               MOVE SFILE-IO-AREA (28:3)   TO SFILE-02-L2-FIRM
               MOVE SFILE-IO-AREA (3:1)    TO SFILE-02-L1-AVD
               IF  SFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SFILE-02-L2           TO THE-PRIOR-L2
               MOVE  SFILE-02-L1           TO THE-PRIOR-L1
               SET SFILE-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'S E R V I C E P R O S E' TO LISTE-IO-AREA (37:23)
               MOVE 'N T R A P P O R T'    TO LISTE-IO-AREA (61:17)
               MOVE 'FOR'                  TO LISTE-IO-AREA (81:3)
               MOVE MNDNAV                 TO LISTE-IO-AREA (86:9)
               MOVE A-ELGR                 TO LISTE-IO-AREA (97:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVDELINGSNAVN'        TO LISTE-IO-AREA (5:13)
               MOVE 'ANT.VARELINJER'       TO LISTE-IO-AREA (37:14)
               MOVE 'ANT.VARELINJER'       TO LISTE-IO-AREA (54:14)
               MOVE 'SERVICE'              TO LISTE-IO-AREA (71:7)
               MOVE 'ORDREBELØP'           TO LISTE-IO-AREA (82:10)
               MOVE 'RESTORDRE'            TO LISTE-IO-AREA (98:9)
               MOVE 'RESTORDRE'            TO LISTE-IO-AREA (110:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT'               TO LISTE-IO-AREA (45:6)
               MOVE 'MED RESTORDRE'        TO LISTE-IO-AREA (55:13)
               MOVE 'PROSENT'              TO LISTE-IO-AREA (71:7)
               MOVE 'TOTALT'               TO LISTE-IO-AREA (86:6)
               MOVE 'I BELØP'              TO LISTE-IO-AREA (100:7)
               MOVE 'I PROSENT'            TO LISTE-IO-AREA (110:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE AVD                    TO LISTE-IO-AREA (2:1)
               MOVE AVDNAV                 TO LISTE-IO-AREA (25:10)
               INITIALIZE AVDNAV
               MOVE VTOTA                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (41:10)
               INITIALIZE VTOTA
               MOVE VRTOTA                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (58:10)
               INITIALIZE VRTOTA
               MOVE SPRO                   TO XO-21YY9
               MOVE XO-21YY9               TO LISTE-IO-AREA (73:4)
               INITIALIZE SPRO
               MOVE BTOTA                  TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (79:13)
               INITIALIZE BTOTA
               MOVE BRTOTA                 TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (94:13)
               INITIALIZE BRTOTA
               MOVE RPRO                   TO XO-21YY9
               MOVE XO-21YY9               TO LISTE-IO-AREA (115:4)
               INITIALIZE RPRO
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'F I R M A T O T A L'  TO LISTE-IO-AREA (5:19)
               MOVE VTOT                   TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (41:10)
               INITIALIZE VTOT
               MOVE VRTOT                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (58:10)
               INITIALIZE VRTOT
               MOVE TSPRO                  TO XO-21YY9
               MOVE XO-21YY9               TO LISTE-IO-AREA (73:4)
               INITIALIZE TSPRO
               MOVE BTOT                   TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (79:13)
               INITIALIZE BTOT
               MOVE BRTOT                  TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (94:13)
               INITIALIZE BRTOT
               MOVE TRPRO                  TO XO-21YY9
               MOVE XO-21YY9               TO LISTE-IO-AREA (115:4)
               INITIALIZE TRPRO
      *******************************************
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
           INITIALIZE PFILE-DATA-FIELDS
           SET PFILE-EOF-OFF               TO TRUE
           SET PFILE-PROCESS               TO TRUE
           OPEN INPUT PFILE
           INITIALIZE FIRMAK-DATA-FIELDS
           OPEN INPUT FIRMAK
           SET SFILE-LEVEL-INIT            TO TRUE
           INITIALIZE SFILE-DATA-FIELDS
           SET SFILE-EOF-OFF               TO TRUE
           SET SFILE-PROCESS               TO TRUE
           OPEN INPUT SFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PFILE
           CLOSE FIRMAK
           CLOSE SFILE
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
