       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS035R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: VOS035, GRUNNLAG MEKANIKER STATISTIKK.       *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VOS02A                                       *
      *  LAGET DATO....: 10.10.96                                     *
      *  ENDRET........: 26.02.97 LEGGER UT DAGENS ORDRE VED MR FOR   *
      *                           OPPDATERING I VOS050.               *
      *  RETTET........:                                              *
      *  INPUT.........: VAREGRUPPETABELL MED GRUPPE FOR ARBEID,      *
      *                  DAGENS VERKDTEDORDRE (VERKSTA),              *
      *                  SEKVENSIELL VERKSTED ORDRE FILE (ORDREM ).   *
      *  BEHANDLING....: MERGER DAGENS ORDRE MED VERKSTED ORDRE FILE  *
      *                  (KUN VARELINJER) OG AKKUMULERER BELØP PR     *
      *                  ARBEID OG DELER.                             *
      *  OUTPUT........: DAGENS VERKSTEDORDRE PR ARBEID OG DELER.     *
      *                  KONTROLL-LISTE NÅR UPSI 1 ER PÅ.             *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS035.rpg
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
           SELECT VGRTAB
               ASSIGN TO UT-S-VGRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VGRTAB-STATUS.
           SELECT VERKSTA
               ASSIGN TO UT-S-VERKSTA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKSTA-STATUS.
           SELECT VORDLIN
               ASSIGN TO UT-S-VORDLIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VORDLIN-STATUS.
           SELECT VERKSTO
               ASSIGN TO UT-S-VERKSTO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKSTO-STATUS.
           SELECT MEKSTAO
               ASSIGN TO UT-S-MEKSTAO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKSTAO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VGRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  VGRTAB-IO-AREA.
           05  VGRTAB-IO-AREA-X            PICTURE X(80).
       FD VERKSTA
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VERKSTA-IO-AREA.
           05  VERKSTA-IO-AREA-X           PICTURE X(60).
       FD VORDLIN
               BLOCK CONTAINS 1640
               RECORD CONTAINS 164.
       01  VORDLIN-IO-AREA.
           05  VORDLIN-IO-AREA-X           PICTURE X(164).
       FD VERKSTO
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VERKSTO-IO-AREA.
           05  VERKSTO-IO-AREA-X           PICTURE X(60).
       FD MEKSTAO
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  MEKSTAO-IO-AREA.
           05  MEKSTAO-IO-AREA-X           PICTURE X(120).
      *FLISTEO O   F  80  80            PRINTERSYSLST
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABVGR-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABVGR-TABLE.
               10  TABVGR-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY TABVGR-I
                                                      TABVGR-S.
                   15  TABVGR              PICTURE X(8).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VGRTAB-STATUS               PICTURE 99 VALUE 0.
           10  VERKSTA-STATUS              PICTURE 99 VALUE 0.
           10  VORDLIN-STATUS              PICTURE 99 VALUE 0.
           10  VERKSTO-STATUS              PICTURE 99 VALUE 0.
           10  MEKSTAO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VGRTAB-EOF-OFF          VALUE '0'.
               88  VGRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKSTA-EOF-OFF         VALUE '0'.
               88  VERKSTA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKSTA-READ-OFF        VALUE '0'.
               88  VERKSTA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKSTA-PROCESS-OFF     VALUE '0'.
               88  VERKSTA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VERKSTA-LEVEL-INIT-OFF  VALUE '0'.
               88  VERKSTA-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VORDLIN-EOF-OFF         VALUE '0'.
               88  VORDLIN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VORDLIN-READ-OFF        VALUE '0'.
               88  VORDLIN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VORDLIN-PROCESS-OFF     VALUE '0'.
               88  VORDLIN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VORDLIN-LEVEL-INIT-OFF  VALUE '0'.
               88  VORDLIN-LEVEL-INIT      VALUE '1'.
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
           05  VERKSTA-LEVEL-01.
               10  VERKSTA-01-L3.
                   15  VERKSTA-01-L3-VSFNR PICTURE X(3).
               10  VERKSTA-01-L2.
                   15  VERKSTA-01-L2-VSONR PICTURE X(6).
           05  VERKSTA-DATA-FIELDS.
               10  VSREC                   PICTURE X(60).
               10  VSFNR                   PICTURE X(3).
               10  VSVONR                  PICTURE X(6).
               10  VSONR                   PICTURE X(6).
               10  VSMEK                   PICTURE X(3).
               10  VSDTO                   PICTURE X(6).
               10  VSDAG                   PICTURE X(2).
               10  VSMND                   PICTURE X(2).
               10  VSAAR                   PICTURE X(2).
               10  VSSTA                   PICTURE X(1).
      *                                      32  422VSSUM
           05  VERKSTA-MP                  PICTURE X(9).
           05  VERKSTA-MC                  PICTURE X(9).
           05  VERKSTA-M-01            REDEFINES VERKSTA-MC.
               10  VERKSTA-M-01-M3.
                   15  VERKSTA-M-01-M3-VSFNR-G.
                       20  VERKSTA-M-01-M3-VSFNR PICTURE X(3).
               10  VERKSTA-M-01-M2.
                   15  VERKSTA-M-01-M2-VSONR-G.
                       20  VERKSTA-M-01-M2-VSONR PICTURE X(6).
           05  VORDLIN-LEVEL-02.
               10  VORDLIN-02-L3.
                   15  VORDLIN-02-L3-VOFNR PICTURE X(3).
               10  VORDLIN-02-L2.
                   15  VORDLIN-02-L2-VOONR PICTURE X(6).
               10  VORDLIN-02-L1.
                   15  VORDLIN-02-L1-VOVGR PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  VORDLIN-DATA-FIELDS.
               10  VOFNR                   PICTURE X(3).
               10  VOONR                   PICTURE X(6).
               10  VOLEVA-IO.
                   15  VOLEVA              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VOVGR-IO.
                   15  VOVGR               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VOPRIS-IO.
                   15  VOPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VORAB1-IO.
                   15  VORAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VORAB2-IO.
                   15  VORAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VORAB3-IO.
                   15  VORAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VOPTIL-IO.
                   15  VOPTIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *****************************************************************
      * HOUSEKEEPING.                                                 *
      *****************************************************************
           05  VORDLIN-MP                  PICTURE X(9).
           05  VORDLIN-MC                  PICTURE X(9).
           05  VORDLIN-M-02            REDEFINES VORDLIN-MC.
               10  VORDLIN-M-02-M3.
                   15  VORDLIN-M-02-M3-VOFNR-G.
                       20  VORDLIN-M-02-M3-VOFNR PICTURE X(3).
               10  VORDLIN-M-02-M2.
                   15  VORDLIN-M-02-M2-VOONR-G.
                       20  VORDLIN-M-02-M2-VOONR PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  ANTT02-IO.
                   15  ANTT02              PICTURE S9(6).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(6).
               10  OB02A-IO.
                   15  OB02A               PICTURE S9(9)V9(2).
               10  OB02D-IO.
                   15  OB02D               PICTURE S9(9)V9(2).
               10  KB02A-IO.
                   15  KB02A               PICTURE S9(9)V9(2).
               10  KB02D-IO.
                   15  KB02D               PICTURE S9(9)V9(2).
               10  NBAL2-IO.
                   15  NBAL2               PICTURE S9(9)V9(2).
               10  NBDL2-IO.
                   15  NBDL2               PICTURE S9(9)V9(2).
               10  BEL02T-IO.
                   15  BEL02T              PICTURE S9(9)V9(2).
               10  TOTML2-IO.
                   15  TOTML2              PICTURE S9(11).
               10  PTILL2-IO.
                   15  PTILL2              PICTURE S9(11).
               10  TOTOL2-IO.
                   15  TOTOL2              PICTURE S9(11).
               10  ANTTIM-IO.
                   15  ANTTIM              PICTURE S9(7)V9(2).
               10  VGRN-IO.
                   15  VGRN                PICTURE S9(5).
               10  VOVGR-N-IO.
                   15  VOVGR-N             PICTURE S9(5).
               10  VGRX                    PICTURE X(5).
               10  TABKEY                  PICTURE X(8).
               10  ANTI01-IO.
                   15  ANTI01              PICTURE S9(6).
               10  AARHUN                  PICTURE X(2).
               10  ANTI02-IO.
                   15  ANTI02              PICTURE S9(6).
               10  BEL02-IO.
                   15  BEL02               PICTURE S9(7)V9(2).
               10  FAK1-IO.
                   15  FAK1                PICTURE S9(1)V9(3).
               10  FAK2-IO.
                   15  FAK2                PICTURE S9(1)V9(3).
               10  FAK3-IO.
                   15  FAK3                PICTURE S9(1)V9(3).
               10  ANTA02-IO.
                   15  ANTA02              PICTURE S9(6).
               10  ANTD02-IO.
                   15  ANTD02              PICTURE S9(6).
               10  PTIL-IO.
                   15  PTIL                PICTURE S9(7)V9(2).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(6).
               10  ANTO-IO.
                   15  ANTO                PICTURE S9(3).
           05  EDITTING-FIELDS.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-110YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZ9-.
               10  XO-72YNZR               PICTURE ZZZZZZZ,ZZ-.
               10  XO-60YY9R               PICTURE ZZZ.ZZ9-.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VERKSTA-PROCESS
               SET VERKSTA-PROCESS-OFF     TO TRUE
               SET VERKSTA-READ            TO TRUE
           END-IF
 
           IF  VERKSTA-READ
               PERFORM VERKSTA-GET
               SET VERKSTA-READ-OFF        TO TRUE
               IF  NOT VERKSTA-EOF
                   PERFORM VERKSTA-MATCH-SET
               END-IF
           END-IF
 
           IF  VORDLIN-PROCESS
               SET VORDLIN-PROCESS-OFF     TO TRUE
               SET VORDLIN-READ            TO TRUE
           END-IF
 
           IF  VORDLIN-READ
               PERFORM VORDLIN-GET
               SET VORDLIN-READ-OFF        TO TRUE
               IF  NOT VORDLIN-EOF
                   PERFORM VORDLIN-MATCH-SET
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
 
           IF  VERKSTA-PROCESS
               PERFORM VERKSTA-IDSET
           END-IF
 
           IF  VORDLIN-PROCESS
               PERFORM VORDLIN-IDSET
           END-IF
 
           IF  VERKSTA-PROCESS
               PERFORM VERKSTA-CHK-LEVEL
           END-IF
 
           IF  VORDLIN-PROCESS
               PERFORM VORDLIN-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VERKSTA-PROCESS
               PERFORM VERKSTA-FLDSET
           END-IF
 
           IF  VORDLIN-PROCESS
               PERFORM VORDLIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VERKSTA-PROCESS
           OR  VORDLIN-PROCESS
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
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      *****************************************************************
      * RECORDART ANNET ENN V1 LESES OVER.                            *
      * BELØP SPLITTES PÅ DELER OG ARBEID (I HHT VAREGRUPPETABELL).   *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTT02
           END-IF
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNMR
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               SUBTRACT OB02A              FROM OB02A
               SUBTRACT OB02D              FROM OB02D
               SUBTRACT KB02A              FROM KB02A
               SUBTRACT KB02D              FROM KB02D
               SUBTRACT NBAL2              FROM NBAL2
               SUBTRACT NBDL2              FROM NBDL2
               SUBTRACT BEL02T             FROM BEL02T
               SUBTRACT TOTML2             FROM TOTML2
               SUBTRACT PTILL2             FROM PTILL2
               SUBTRACT TOTOL2             FROM TOTOL2
               SUBTRACT ANTTIM             FROM ANTTIM
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
               IF  VSONR NOT > '899999'
                   SET I-31                TO TRUE
               END-IF
               IF  VSONR > '899999'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  VSSTA = 'U'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               MOVE VOVGR                  TO VOVGR-N
               MOVE VOVGR-N-IO             TO VGRN-IO
               MOVE VGRN                   TO VGRX
               MOVE VOFNR                  TO TABKEY (1:3)
               MOVE VGRX                   TO TABKEY (4:5)
               SET NOT-I-10                TO TRUE
               SET TABVGR-S                TO TABVGR-I
               PERFORM WITH TEST AFTER
                       VARYING TABVGR-I FROM 1 BY 1
                         UNTIL TABVGR-I >= TABVGR-MAX
                            OR I-10
                   IF  TABKEY = TABVGR (TABVGR-I)
                       SET I-10            TO TRUE
                       SET TABVGR-S        TO TABVGR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTI01
               SET NOT-I-20                TO TRUE
               SET NOT-I-21                TO TRUE
               IF  VSAAR NOT < '90'
                   SET I-20                TO TRUE
               END-IF
               IF  VSAAR < '90'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-20)
               MOVE '19'                   TO AARHUN
           END-IF
           IF  (I-01 AND I-21)
               MOVE '20'                   TO AARHUN
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTI02
           END-IF
           MULTIPLY VOLEVA BY VOPRIS   GIVING BEL02
           DIVIDE VORAB1 BY 100        GIVING FAK1
           DIVIDE VORAB2 BY 100        GIVING FAK2
           DIVIDE VORAB3 BY 100        GIVING FAK3
           SUBTRACT FAK1 FROM 1        GIVING FAK1
           SUBTRACT FAK2 FROM 1        GIVING FAK2
           SUBTRACT FAK3 FROM 1        GIVING FAK3
           MULTIPLY FAK1 BY BEL02      GIVING BEL02 ROUNDED
           MULTIPLY FAK2 BY BEL02      GIVING BEL02 ROUNDED
           MULTIPLY FAK3 BY BEL02      GIVING BEL02 ROUNDED
           IF  (I-10 AND I-30 AND NOT-I-32)
               SUBTRACT BEL02              FROM KB02A
           END-IF
           IF  (NOT-I-10 AND I-30 AND NOT-I-32)
               SUBTRACT BEL02              FROM KB02D
           END-IF
           IF  (I-10 AND I-30 AND I-32)
               ADD BEL02                   TO KB02A
           END-IF
           IF  (NOT-I-10 AND I-30 AND I-32)
               ADD BEL02                   TO KB02D
           END-IF
           IF  (I-10 AND I-31 AND NOT-I-32)
               ADD BEL02                   TO OB02A
           END-IF
           IF  (NOT-I-10 AND I-31 AND NOT-I-32)
               ADD BEL02                   TO OB02D
           END-IF
           IF  (I-10 AND I-31 AND I-32)
               SUBTRACT BEL02              FROM OB02A
           END-IF
           IF  (NOT-I-10 AND I-31 AND I-32)
               SUBTRACT BEL02              FROM OB02D
           END-IF
           IF  (I-10)
               ADD 1                       TO ANTA02
           END-IF
           IF  (I-10 AND I-31 AND NOT-I-32)
               ADD VOLEVA                  TO ANTTIM
           END-IF
           IF  (I-10 AND I-31 AND I-32)
               SUBTRACT VOLEVA             FROM ANTTIM
           END-IF
           IF  (I-10 AND I-30 AND NOT-I-32)
               SUBTRACT VOLEVA             FROM ANTTIM
           END-IF
           IF  (I-10 AND I-30 AND I-32)
               ADD VOLEVA                  TO ANTTIM
           END-IF
           IF  (NOT-I-10)
               ADD 1                       TO ANTD02
           END-IF
           MULTIPLY VOLEVA BY VOPTIL   GIVING PTIL ROUNDED
           IF  (I-10 AND I-31 AND NOT-I-32)
               ADD PTIL                    TO BEL02T
           END-IF
           IF  (I-10 AND I-31 AND I-32)
               SUBTRACT PTIL               FROM BEL02T
           END-IF
           IF  (I-10 AND I-30)
               SUBTRACT PTIL               FROM BEL02T
           END-IF
           SET I-50                        TO TRUE
      *                    MOVE "FAK1    "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGFLISTEO   FAK1             VIS INDIKATOR
           .
 
       SLUTT-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L2 AND I-50)
               ADD 1                       TO ANTUT
               ADD KB02A TO OB02A      GIVING NBAL2 ROUNDED
               ADD KB02D TO OB02D      GIVING NBDL2 ROUNDED
               ADD NBDL2 TO NBAL2      GIVING TOTML2 ROUNDED
               ADD BEL02T TO ZERO      GIVING PTILL2 ROUNDED
               ADD TOTML2 TO PTILL2    GIVING TOTOL2 ROUNDED
               MOVE 1                      TO ANTO
      *****************************************************************
      * DAGENS VERKSTEDORDRE                                          *
      *****************************************************************
           END-IF
           .
 
       VERKSTA-GET SECTION.
       VERKSTA-GET-P.
           IF  VERKSTA-EOF-OFF
               READ VERKSTA
               AT END
                   SET VERKSTA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VERKSTA-FLDSET SECTION.
       VERKSTA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKSTA-IO-AREA (1:60) TO VSREC (1:60)
               MOVE VERKSTA-IO-AREA (1:3)  TO VSFNR (1:3)
               MOVE VERKSTA-IO-AREA (4:6)  TO VSVONR (1:6)
               MOVE VERKSTA-IO-AREA (16:6) TO VSONR (1:6)
               MOVE VERKSTA-IO-AREA (22:3) TO VSMEK (1:3)
               MOVE VERKSTA-IO-AREA (25:6) TO VSDTO (1:6)
               MOVE VERKSTA-IO-AREA (25:2) TO VSDAG (1:2)
               MOVE VERKSTA-IO-AREA (27:2) TO VSMND (1:2)
               MOVE VERKSTA-IO-AREA (29:2) TO VSAAR (1:2)
               MOVE VERKSTA-IO-AREA (31:1) TO VSSTA (1:1)
           END-EVALUATE.
 
       VERKSTA-IDSET SECTION.
       VERKSTA-IDSET-P.
           SET I-01                        TO TRUE.
 
       VERKSTA-CHK-LEVEL SECTION.
       VERKSTA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VERKSTA-LEVEL-01
               MOVE VERKSTA-IO-AREA (1:3)  TO VERKSTA-01-L3-VSFNR
               MOVE VERKSTA-IO-AREA (16:6) TO VERKSTA-01-L2-VSONR
               IF  VERKSTA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKSTA-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  VERKSTA-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKSTA-01-L3         TO THE-PRIOR-L3
               MOVE  VERKSTA-01-L2         TO THE-PRIOR-L2
               SET VERKSTA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VERKSTA-MATCH-SET SECTION.
       VERKSTA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKSTA-IO-AREA (1:3)  TO VERKSTA-M-01-M3-VSFNR
               MOVE VERKSTA-IO-AREA (16:6) TO VERKSTA-M-01-M2-VSONR
           END-EVALUATE.
 
       VORDLIN-GET SECTION.
       VORDLIN-GET-P.
           IF  VORDLIN-EOF-OFF
               READ VORDLIN
               AT END
                   SET VORDLIN-EOF         TO TRUE
               END-READ
           END-IF.
 
       VORDLIN-FLDSET SECTION.
       VORDLIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VORDLIN-IO-AREA (2:3)  TO VOFNR (1:3)
               MOVE VORDLIN-IO-AREA (5:6)  TO VOONR (1:6)
               MOVE VORDLIN-IO-AREA (29:4) TO VOLEVA-IO
               MOVE VORDLIN-IO-AREA (91:3) TO VOVGR-IO
               MOVE VORDLIN-IO-AREA (94:5) TO VOPRIS-IO
               MOVE VORDLIN-IO-AREA (99:2) TO VORAB1-IO
               MOVE VORDLIN-IO-AREA (101:2) TO VORAB2-IO
               MOVE VORDLIN-IO-AREA (103:2) TO VORAB3-IO
               MOVE VORDLIN-IO-AREA (126:4) TO VOPTIL-IO
           END-EVALUATE.
 
       VORDLIN-IDSET SECTION.
       VORDLIN-IDSET-P.
           SET I-02                        TO TRUE.
 
       VORDLIN-CHK-LEVEL SECTION.
       VORDLIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VORDLIN-LEVEL-02
               MOVE VORDLIN-IO-AREA (2:3)  TO VORDLIN-02-L3-VOFNR
               MOVE VORDLIN-IO-AREA (5:6)  TO VORDLIN-02-L2-VOONR
               MOVE VORDLIN-IO-AREA (91:3) TO VORDLIN-02-L1-VOVGR
               IF  VORDLIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VORDLIN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  VORDLIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VORDLIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VORDLIN-02-L3         TO THE-PRIOR-L3
               MOVE  VORDLIN-02-L2         TO THE-PRIOR-L2
               MOVE  VORDLIN-02-L1         TO THE-PRIOR-L1
               SET VORDLIN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VORDLIN-MATCH-SET SECTION.
       VORDLIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VORDLIN-IO-AREA (2:3)  TO VORDLIN-M-02-M3-VOFNR
               MOVE VORDLIN-IO-AREA (5:6)  TO VORDLIN-M-02-M2-VOONR
           END-EVALUATE.
 
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VERKSTA-EOF
               MOVE HIGH-VALUES            TO VERKSTA-MC
                                              VERKSTA-MP
           END-IF
           IF  VORDLIN-EOF
               MOVE HIGH-VALUES            TO VORDLIN-MC
                                              VORDLIN-MP
           END-IF
           IF  VERKSTA-MC < VERKSTA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VORDLIN-MC < VORDLIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VERKSTA-MC < VORDLIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKSTA-PROCESS     TO TRUE
                   MOVE VERKSTA-MC         TO VERKSTA-MP
                   IF  VERKSTA-MC = VORDLIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VORDLIN-MC < VERKSTA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VORDLIN-PROCESS     TO TRUE
                   MOVE VORDLIN-MC         TO VORDLIN-MP
                   IF  VORDLIN-MC = VERKSTA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VERKSTA-MC = VORDLIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKSTA-PROCESS     TO TRUE
                   MOVE VERKSTA-MC         TO VERKSTA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       VGRTAB-LOAD SECTION.
       VGRTAB-LOAD-P.
           OPEN INPUT VGRTAB
           SET TABVGR-I                    TO 1
           PERFORM UNTIL VGRTAB-EOF
               READ VGRTAB
               AT END
                   SET VGRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE VGRTAB-IO-AREA (1:8) TO TABVGR-ENTRY (TABVGR-I)
                   SET TABVGR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE VGRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE SPACES TO VERKSTO-IO-AREA
               INITIALIZE VERKSTO-IO-AREA
               MOVE VSREC                  TO VERKSTO-IO-AREA (1:60)
      *****************************************************************
      * DAGENS VERKSTEDORDRE PR MEK                                   *
      *****************************************************************
               WRITE VERKSTO-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-U1 AND I-L3)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTEDORDRE. GRUNNLAG ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'MEKANIKER-STATISTIKK.   ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (67:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (77:8)
               MOVE 'JOB=VOS02A'           TO KLISTEO-IO-AREA (86:10)
               MOVE 'PROGRAM=VOS035'       TO KLISTEO-IO-AREA (97:14)
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'DAGENS VERKSTED-ORDRE' TO KLISTEO-IO-AREA (57:21)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'FIR'                  TO KLISTEO-IO-AREA (1:3)
               MOVE 'VORDNR'               TO KLISTEO-IO-AREA (6:6)
               MOVE 'ORDRNR'               TO KLISTEO-IO-AREA (14:6)
               MOVE 'MEK'                  TO KLISTEO-IO-AREA (22:3)
               MOVE ' DATO '               TO KLISTEO-IO-AREA (27:6)
               MOVE '    BELØP ARB  '      TO KLISTEO-IO-AREA (35:15)
               MOVE ' +  BELØP DEL  '      TO KLISTEO-IO-AREA (52:15)
               MOVE ' =  BELØP TOT  '      TO KLISTEO-IO-AREA (69:15)
               MOVE ' + PRISTILLEGG '      TO KLISTEO-IO-AREA (86:15)
               MOVE ' =    ORDRESUM '      TO KLISTEO-IO-AREA (103:15)
               MOVE 'ANT TIMER'            TO KLISTEO-IO-AREA (122:9)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-U1 AND I-OF)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'VERKSTEDORDRE. GRUNNLAG ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'MEKANIKER-STATISTIKK.   ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'TIDSPUNKT:'           TO KLISTEO-IO-AREA (67:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (77:8)
               MOVE 'JOB=VOS02A'           TO KLISTEO-IO-AREA (86:10)
               MOVE 'PROGRAM=VOS035'       TO KLISTEO-IO-AREA (97:14)
               MOVE 01                     TO KLISTEO-BEFORE-SKIP
               MOVE 2                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'DAGENS VERKSTED-ORDRE' TO KLISTEO-IO-AREA (57:21)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'FIR'                  TO KLISTEO-IO-AREA (1:3)
               MOVE 'VORDNR'               TO KLISTEO-IO-AREA (6:6)
               MOVE 'ORDRNR'               TO KLISTEO-IO-AREA (14:6)
               MOVE 'MEK'                  TO KLISTEO-IO-AREA (22:3)
               MOVE ' DATO '               TO KLISTEO-IO-AREA (27:6)
               MOVE '    BELØP ARB  '      TO KLISTEO-IO-AREA (35:15)
               MOVE ' +  BELØP DEL  '      TO KLISTEO-IO-AREA (52:15)
               MOVE ' =  BELØP TOT  '      TO KLISTEO-IO-AREA (69:15)
               MOVE ' + PRISTILLEGG '      TO KLISTEO-IO-AREA (86:15)
               MOVE ' =    ORDRESUM '      TO KLISTEO-IO-AREA (103:15)
               MOVE 'ANT TIMER'            TO KLISTEO-IO-AREA (122:9)
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND I-50)
               MOVE SPACES TO MEKSTAO-IO-AREA
               INITIALIZE MEKSTAO-IO-AREA
               MOVE VSFNR                  TO MEKSTAO-IO-AREA (1:3)
               MOVE VSMEK                  TO MEKSTAO-IO-AREA (4:3)
               MOVE AARHUN                 TO MEKSTAO-IO-AREA (7:2)
               MOVE VSAAR                  TO MEKSTAO-IO-AREA (9:2)
               MOVE VSMND                  TO MEKSTAO-IO-AREA (11:2)
               MOVE VSDAG                  TO MEKSTAO-IO-AREA (13:2)
               MOVE OB02A-IO               TO MEKSTAO-IO-AREA (15:11)
               MOVE OB02D-IO               TO MEKSTAO-IO-AREA (26:11)
               MOVE KB02A-IO               TO MEKSTAO-IO-AREA (37:11)
               MOVE KB02D-IO               TO MEKSTAO-IO-AREA (48:11)
               MOVE ANTO-IO                TO MEKSTAO-IO-AREA (59:3)
               MOVE ANTTIM-IO              TO MEKSTAO-IO-AREA (62:9)
      * OPPDATERINGSDATO                114
      * OPPDATERINGSTIDSPUNKT           120
               WRITE MEKSTAO-IO-AREA
           END-IF
           IF  (I-U1 AND I-L2 AND I-50)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE VSFNR                  TO KLISTEO-IO-AREA (1:3)
               MOVE VSVONR                 TO KLISTEO-IO-AREA (6:6)
               MOVE VSONR                  TO KLISTEO-IO-AREA (14:6)
               MOVE VSSTA                  TO KLISTEO-IO-AREA (20:1)
               MOVE VSMEK                  TO KLISTEO-IO-AREA (22:3)
               MOVE VSDTO                  TO KLISTEO-IO-AREA (27:6)
               MOVE NBAL2                  TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (35:15)
               MOVE NBDL2                  TO XO-92YY9R
               MOVE XO-92YY9R              TO KLISTEO-IO-AREA (52:15)
               MOVE TOTML2                 TO XO-110YY9R
               MOVE XO-110YY9R             TO KLISTEO-IO-AREA (69:15)
               MOVE PTILL2                 TO XO-110YY9R
               MOVE XO-110YY9R             TO KLISTEO-IO-AREA (86:15)
               MOVE TOTOL2                 TO XO-110YY9R
               MOVE XO-110YY9R             TO KLISTEO-IO-AREA (103:15)
               MOVE ANTTIM                 TO XO-72YNZR
               MOVE XO-72YNZR              TO KLISTEO-IO-AREA (120:11)
      *       D  1     U1 01 MR
      *                        VOKEY     15
      *                        VOLEVAL   40
      *                        VOVGR L   50
      *                        VOPRISJ   60
      *                        VORAB1J   70
      *                        VORAB2J   80
      *                        VORAB3J   90
      *                        FAK1  J  100
      *                        FAK2  J  108
      *                        FAK3  J  115
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-L3)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL ORDRE        :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTI01                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTI01
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL VARELIN. INN :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTT02                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTT02
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL VARELIN. NMR :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTNMR                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTNMR
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL VARELIN. OK  :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTI02                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTI02
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL V.LIN ARBEID :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTA02                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTA02
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL V.LIN DELER  :' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTD02                 TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTD02
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL ORDRE SKREVET:' TO KLISTEO-IO-AREA (5:21)
               MOVE ANTUT                  TO XO-60YY9R
               MOVE XO-60YY9R              TO KLISTEO-IO-AREA (28:8)
               INITIALIZE ANTUT
               MOVE 1                      TO KLISTEO-AFTER-SPACE
               PERFORM KLISTEO-PRINT-LINE
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
           PERFORM VGRTAB-LOAD
           SET VERKSTA-LEVEL-INIT          TO TRUE
           INITIALIZE VERKSTA-DATA-FIELDS
           SET VERKSTA-EOF-OFF             TO TRUE
           SET VERKSTA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VERKSTA-MC
                                              VERKSTA-MP
           OPEN INPUT VERKSTA
           SET VORDLIN-LEVEL-INIT          TO TRUE
           INITIALIZE VORDLIN-DATA-FIELDS
           SET VORDLIN-EOF-OFF             TO TRUE
           SET VORDLIN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VORDLIN-MC
                                              VORDLIN-MP
           OPEN INPUT VORDLIN
           OPEN OUTPUT VERKSTO
           OPEN OUTPUT MEKSTAO
           OPEN OUTPUT KLISTEO
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           SET TABVGR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VERKSTA
           CLOSE VORDLIN
           CLOSE VERKSTO
           CLOSE MEKSTAO
           IF KLISTEO-IO-AREA NOT = SPACES
             WRITE KLISTEO-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO KLISTEO-IO-AREA
           END-IF
           CLOSE KLISTEO.
 
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
