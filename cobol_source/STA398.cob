       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA398R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring excel på Report Web *****************
      *  STATISTIKK OVER ANTALL ORDRE PR KUNDE AV AUTODATA.  ****
      *  KORR. 12/2-1991 AV ESPEN LARSEN                     ****
      *                  NY GRAND TOTALER.                   ****
      *  KORR. 10/2-1994 AV ESPEN LARSEN                     ****
      *                  DANNE OUTPUT STAT.FILE.             ****
      *  KORR. 23/4-2007 AV ELIN. LAGT INN HEADING RBSRUT.   ****
      *                  DANNE OUTPUT STAT.FILE.             ****
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA398.rpg
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
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT ORDSTAT
               ASSIGN TO UT-S-ORDSTAT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSTAT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1.
                   15  FIRMAF-KEY1N        PICTURE S9(3).
               10  FILLER                  PICTURE X(994).
       FD ORDSTAT
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  ORDSTAT-IO-AREA.
           05  ORDSTAT-IO-AREA-X           PICTURE X(120).
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
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  ORDSTAT-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-EOF-OFF          VALUE '0'.
               88  FIRMAF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-READ-OFF         VALUE '0'.
               88  FIRMAF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-PROCESS-OFF      VALUE '0'.
               88  FIRMAF-PROCESS          VALUE '1'.
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
           05  FIRMAF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  NAVN                    PICTURE X(30).
               10  VANT-IO.
                   15  VANT                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  OANT-IO.
                   15  OANT                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VAKK-IO.
                   15  VAKK                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  OAKK-IO.
                   15  OAKK                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  DOANT-IO.
                   15  DOANT               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  DOAKK-IO.
                   15  DOAKK               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  TEMPORARY-FIELDS.
               10  REC-IO.
                   15  REC                 PICTURE S9(9).
               10  NYVAKK-IO.
                   15  NYVAKK              PICTURE S9(9).
               10  NYOAKK-IO.
                   15  NYOAKK              PICTURE S9(7).
               10  NYDAKK-IO.
                   15  NYDAKK              PICTURE S9(7).
               10  VOANT-IO.
                   15  VOANT               PICTURE S9(5)V9(2).
               10  VOAKK-IO.
                   15  VOAKK               PICTURE S9(5)V9(2).
               10  TVANT-IO.
                   15  TVANT               PICTURE S9(9).
               10  TOANT-IO.
                   15  TOANT               PICTURE S9(9).
               10  TDOANT-IO.
                   15  TDOANT              PICTURE S9(9).
               10  TVAKK-IO.
                   15  TVAKK               PICTURE S9(9).
               10  TOAKK-IO.
                   15  TOAKK               PICTURE S9(9).
               10  TDOAKK-IO.
                   15  TDOAKK              PICTURE S9(9).
               10  GTVAKK-IO.
                   15  GTVAKK              PICTURE S9(9).
               10  GTOAKK-IO.
                   15  GTOAKK              PICTURE S9(9).
               10  GTDAKK-IO.
                   15  GTDAKK              PICTURE S9(9).
               10  GVOANT-IO.
                   15  GVOANT              PICTURE S9(5)V9(2).
               10  GVOAKK-IO.
                   15  GVOAKK              PICTURE S9(5)V9(2).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
               10  XO-52YY9                PICTURE ZZ.ZZZ,99.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FIRMAF-PROCESS
               SET FIRMAF-PROCESS-OFF      TO TRUE
               SET FIRMAF-READ             TO TRUE
           END-IF
 
           IF  FIRMAF-READ
           AND RECORD-SELECTED-OFF
               PERFORM FIRMAF-GET
               SET FIRMAF-READ-OFF         TO TRUE
               IF  NOT FIRMAF-EOF
                   SET FIRMAF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-IDSET
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
           PERFORM HEADING-OVERFLOW
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-FLDOFF
               PERFORM FIRMAF-FLDSET
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
           IF  (I-01)
               SET NOT-I-90                TO TRUE
               ADD 1                       TO REC
               SET NOT-I-90                TO TRUE
               IF  REC = 1
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-90)
               PERFORM RBSRUT-S
      ************************************************************
      * SUMMERING TIL TOTALER PR. FIRMA.                         *
      ************************************************************
           END-IF
           ADD VANT TO VAKK            GIVING NYVAKK
           ADD OANT TO OAKK            GIVING NYOAKK
           SET NOT-I-10                    TO TRUE
           IF  NYOAKK = 0
               SET I-10                    TO TRUE
           END-IF
           ADD DOANT TO DOAKK          GIVING NYDAKK
           IF  (NOT-I-15)
               DIVIDE VANT BY OANT     GIVING VOANT ROUNDED
           END-IF
           IF  (NOT-I-10)
               DIVIDE NYVAKK BY NYOAKK GIVING VOAKK ROUNDED
      ************************************************************
      * SUMMERING TIL GRAND TOTALER ALLE FIRMA.                  *
      ************************************************************
           END-IF
           ADD VANT                        TO TVANT
           ADD OANT                        TO TOANT
           ADD DOANT                       TO TDOANT
           ADD VAKK                        TO TVAKK
           ADD OAKK                        TO TOAKK
           ADD DOAKK                       TO TDOAKK
      ************************************************************
      * UTREGNING AV GRAND TOTALER ALLE FIRMA.                   *
      ************************************************************
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'AD001'                    TO LONR
           MOVE 399                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'STA398  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *****************************************************************
      * STATESTIKKFILE                                                *
      *****************************************************************
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD TVANT TO TVAKK          GIVING GTVAKK
           ADD TOANT TO TOAKK          GIVING GTOAKK
           ADD TDOANT TO TDOAKK        GIVING GTDAKK
           DIVIDE TVANT BY TOANT       GIVING GVOANT ROUNDED
           DIVIDE GTVAKK BY GTOAKK     GIVING GVOAKK ROUNDED
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           .
 
       FIRMAF-GET SECTION.
       FIRMAF-GET-P.
           IF  FIRMAF-EOF-OFF
               READ FIRMAF
               AT END
                   SET FIRMAF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FIRMAF-FLDOFF SECTION.
       FIRMAF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (5:3)   TO FIRMA (1:3)
               MOVE FIRMAF-IO-AREA (8:30)  TO NAVN (1:30)
               MOVE FIRMAF-IO-AREA (787:4) TO VANT-IO
               MOVE FIRMAF-IO-AREA (791:3) TO OANT-IO
               IF  OANT = ZERO
                   SET I-15                TO TRUE
               END-IF
               MOVE FIRMAF-IO-AREA (794:5) TO VAKK-IO
               MOVE FIRMAF-IO-AREA (799:4) TO OAKK-IO
               MOVE FIRMAF-IO-AREA (901:3) TO DOANT-IO
               MOVE FIRMAF-IO-AREA (904:4) TO DOAKK-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-01                        TO TRUE.
 
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO ORDSTAT-IO-AREA
               INITIALIZE ORDSTAT-IO-AREA
               MOVE FIRMA                  TO ORDSTAT-IO-AREA (1:3)
               MOVE UYEAR                  TO ORDSTAT-IO-AREA (5:2)
               MOVE UMONTH                 TO ORDSTAT-IO-AREA (7:2)
               MOVE OANT                   TO XO-50P
               MOVE XO-50P-EF              TO ORDSTAT-IO-AREA (11:3)
               MOVE DOANT                  TO XO-50P
               MOVE XO-50P-EF              TO ORDSTAT-IO-AREA (14:3)
               MOVE NYOAKK                 TO XO-70P
               MOVE XO-70P-EF              TO ORDSTAT-IO-AREA (17:4)
               MOVE NYDAKK                 TO XO-70P
               MOVE XO-70P-EF              TO ORDSTAT-IO-AREA (21:4)
               MOVE VANT                   TO XO-70P
               MOVE XO-70P-EF              TO ORDSTAT-IO-AREA (25:4)
               MOVE NYVAKK                 TO XO-90P
               MOVE XO-90P-EF              TO ORDSTAT-IO-AREA (29:5)
      *****************************************************************
      * STATISTIKK                                                    *
      *****************************************************************
               WRITE ORDSTAT-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE NAVN                   TO LISTE-IO-AREA (1:30)
               MOVE FIRMA                  TO LISTE-IO-AREA (33:3)
               MOVE OANT                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (37:6)
               MOVE DOANT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (47:6)
               MOVE VANT                   TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (55:9)
               MOVE VOANT                  TO XO-52YY9
               MOVE XO-52YY9               TO LISTE-IO-AREA (70:9)
               INITIALIZE VOANT
               MOVE NYOAKK                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (82:9)
               MOVE NYDAKK                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (91:9)
               MOVE NYVAKK                 TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (105:11)
               MOVE VOAKK                  TO XO-52YY9
               MOVE XO-52YY9               TO LISTE-IO-AREA (122:9)
               INITIALIZE VOAKK
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-90)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  STATESTIKK  OVER' TO LISTE-IO-AREA (4:21)
               MOVE 'ANTALL  ORDRE  FOR  AUTO' TO LISTE-IO-AREA (27:24)
               MOVE 'DATAS  KUNDER   ***'  TO LISTE-IO-AREA (51:19)
               MOVE 'DATO='                TO LISTE-IO-AREA (80:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (85:8)
               MOVE '* LEVERES ADM.DIR. *' TO LISTE-IO-AREA (104:20)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '----------'           TO LISTE-IO-AREA (38:10)
               MOVE '-------    DENNE MÅNED' TO LISTE-IO-AREA (46:22)
               MOVE '----------'           TO LISTE-IO-AREA (72:10)
               MOVE '------------'         TO LISTE-IO-AREA (86:12)
               MOVE '-------   HITTIL I ÅR' TO LISTE-IO-AREA (95:21)
               MOVE '-------------'        TO LISTE-IO-AREA (120:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV    '            TO LISTE-IO-AREA (44:9)
               MOVE 'HERAV    '            TO LISTE-IO-AREA (93:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (1:9)
               MOVE 'FNR  ORDRE'           TO LISTE-IO-AREA (33:10)
               MOVE 'DIR.ORDRE'            TO LISTE-IO-AREA (44:9)
               MOVE 'ANT VLINJ   VLINJ PR' TO LISTE-IO-AREA (55:20)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (76:5)
               MOVE 'ORDRE  DIR.ORDRE'     TO LISTE-IO-AREA (86:16)
               MOVE 'ANT VLINJ'            TO LISTE-IO-AREA (107:9)
               MOVE 'VLINJ PR ORDRE'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  STATESTIKK  OVER' TO LISTE-IO-AREA (4:21)
               MOVE 'ANTALL  ORDRE  FOR  AUTO' TO LISTE-IO-AREA (27:24)
               MOVE 'DATAS  KUNDER   ***'  TO LISTE-IO-AREA (51:19)
               MOVE 'DATO='                TO LISTE-IO-AREA (80:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (85:8)
               MOVE '* LEVERES ADM.DIR. *' TO LISTE-IO-AREA (104:20)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '----------'           TO LISTE-IO-AREA (38:10)
               MOVE '-------    DENNE MÅNED' TO LISTE-IO-AREA (46:22)
               MOVE '----------'           TO LISTE-IO-AREA (72:10)
               MOVE '------------'         TO LISTE-IO-AREA (86:12)
               MOVE '-------   HITTIL I ÅR' TO LISTE-IO-AREA (95:21)
               MOVE '-------------'        TO LISTE-IO-AREA (120:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV    '            TO LISTE-IO-AREA (44:9)
               MOVE 'HERAV    '            TO LISTE-IO-AREA (93:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (1:9)
               MOVE 'FNR  ORDRE'           TO LISTE-IO-AREA (33:10)
               MOVE 'DIR.ORDRE'            TO LISTE-IO-AREA (44:9)
               MOVE 'ANT VLINJ   VLINJ PR' TO LISTE-IO-AREA (55:20)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (76:5)
               MOVE 'ORDRE  DIR.ORDRE'     TO LISTE-IO-AREA (86:16)
               MOVE 'ANT VLINJ'            TO LISTE-IO-AREA (107:9)
               MOVE 'VLINJ PR ORDRE'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRAND TOTALER.'       TO LISTE-IO-AREA (5:14)
               MOVE TOANT                  TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (32:11)
               MOVE TVANT                  TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (53:11)
               MOVE GVOANT                 TO XO-52YY9
               MOVE XO-52YY9               TO LISTE-IO-AREA (70:9)
               MOVE GTOAKK                 TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (80:11)
               MOVE GTVAKK                 TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (105:11)
               MOVE GVOAKK                 TO XO-52YY9
               MOVE XO-52YY9               TO LISTE-IO-AREA (122:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALER. DIR ORDRE'   TO LISTE-IO-AREA (5:18)
               MOVE TDOANT                 TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (42:11)
               MOVE GTDAKK                 TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (89:11)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE FIRMAF-DATA-FIELDS
           SET FIRMAF-EOF-OFF              TO TRUE
           SET FIRMAF-PROCESS              TO TRUE
           OPEN INPUT FIRMAF
           OPEN OUTPUT ORDSTAT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FIRMAF
           CLOSE ORDSTAT
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
