       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAK054R.
      **********************************************  Z-WIN-RPG2      *
      * PROGRAM.....: TAK054                               *
      * PROGRAMERE..: ESPEN LARSEN                         *
      * PROGRAMERT..:  2.05.97                             *
      * SISTE KORR..: 15.05.11                             *
      * LESER VARE.KONTO.KURANT OG FJERNER GAMLE RECORDS   *
      * SAMT DANNER HJELPEFILE.                            *
      * følgende records blir fjernet:                     *
      *    firma som er slettet.                           *
      *    varer som ikke finnes i varemaster.             *
      *    RECORDS SOM ER ELDERE EN 4 ÅR.                  *
      * LAGT INN 4 SIFFERET ÅR I SORTFELT + FJERNERUTINE.  *
      * UTVIDET VAREKON TIL 100 POS                        *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAK054.rpg
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
           SELECT VAREMIN
               ASSIGN TO UT-S-VAREMIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMIN-STATUS.
           SELECT VAREKON
               ASSIGN TO VAREKON
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VAREKON-STATUS
               RECORD KEY IS VAREKON-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KOPIUT
               ASSIGN TO UT-S-KOPIUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPIUT-STATUS.
           SELECT HJFILE
               ASSIGN TO UT-S-HJFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS HJFILE-STATUS.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREMIN
               BLOCK CONTAINS 100
               RECORD CONTAINS 10.
       01  VAREMIN-IO-AREA.
           05  VAREMIN-IO-AREA-X           PICTURE X(10).
       FD VAREKON
               RECORD CONTAINS 100.
       01  VAREKON-IO-AREA.
           05  VAREKON-IO-AREA-X.
               10  VAREKON-KEY1.
                   15  VAREKON-KEY1N       PICTURE S9(15).
               10  FILLER                  PICTURE X(85).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KOPIUT
               BLOCK CONTAINS 228
               RECORD CONTAINS 114.
       01  KOPIUT-IO-AREA.
           05  KOPIUT-IO-AREA-X            PICTURE X(114).
       FD HJFILE
               BLOCK CONTAINS 48
               RECORD CONTAINS 24.
       01  HJFILE-IO-AREA.
           05  HJFILE-IO-AREA-X            PICTURE X(24).
       FD TOTALER
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREMIN-STATUS              PICTURE 99 VALUE 0.
           10  VAREKON-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KOPIUT-STATUS               PICTURE 99 VALUE 0.
           10  HJFILE-STATUS               PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMIN-EOF-OFF         VALUE '0'.
               88  VAREMIN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMIN-READ-OFF        VALUE '0'.
               88  VAREMIN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMIN-PROCESS-OFF     VALUE '0'.
               88  VAREMIN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREMIN-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREMIN-LEVEL-INIT      VALUE '1'.
           05  VAREKON-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREKON-EOF-OFF         VALUE '0'.
               88  VAREKON-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREKON-READ-OFF        VALUE '0'.
               88  VAREKON-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREKON-PROCESS-OFF     VALUE '0'.
               88  VAREKON-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREKON-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREKON-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  TOTALER-DATA-FIELDS.
               10  TOTALER-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-CLR-IO          PICTURE X VALUE 'Y'.
           05  VAREMIN-LEVEL-01.
               10  VAREMIN-01-L2.
                   15  VAREMIN-01-L2-FIRM  PICTURE S9(3).
               10  VAREMIN-01-L1.
                   15  VAREMIN-01-L1-EDBN  PICTURE S9(7).
           05  VAREMIN-DATA-FIELDS.
               10  REC1                    PICTURE X(10).
               10  FIRM-IO.
                   15  FIRM                PICTURE S9(3).
               10  EDBN-IO.
                   15  EDBN                PICTURE S9(7).
           05  VAREMIN-MP                  PICTURE X(10).
           05  VAREMIN-MC                  PICTURE X(10).
           05  VAREMIN-M-01            REDEFINES VAREMIN-MC.
               10  VAREMIN-M-01-M2.
                   15  VAREMIN-M-01-M2-FIRM-G.
                       20  VAREMIN-M-01-M2-FIRM PICTURE S9(3).
               10  VAREMIN-M-01-M1.
                   15  VAREMIN-M-01-M1-EDBN-G.
                       20  VAREMIN-M-01-M1-EDBN PICTURE S9(7).
           05  VAREKON-LEVEL-02.
               10  VAREKON-02-L2.
                   15  VAREKON-02-L2-FIRM  PICTURE S9(3).
               10  VAREKON-02-L1.
                   15  VAREKON-02-L1-EDBN  PICTURE S9(7).
           05  VAREKON-DATA-FIELDS.
               10  KONREC                  PICTURE X(100).
               10  UPDATO-IO.
                   15  UPDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  UPTID-IO.
                   15  UPTID               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RECTYP                  PICTURE X(2).
           05  VAREKON-MP                  PICTURE X(10).
           05  VAREKON-MC                  PICTURE X(10).
           05  VAREKON-M-02            REDEFINES VAREKON-MC.
               10  VAREKON-M-02-M2.
                   15  VAREKON-M-02-M2-FIRM-G.
                       20  VAREKON-M-02-M2-FIRM PICTURE S9(3).
               10  VAREKON-M-02-M1.
                   15  VAREKON-M-02-M1-EDBN-G.
                       20  VAREKON-M-02-M1-EDBN PICTURE S9(7).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  YEAR2N-IO.
                   15  YEAR2N              PICTURE S9(2).
               10  YEAR2                   PICTURE X(2).
               10  GMLA-ELGR-IO.
                   15  GMLA-ELGR           PICTURE S9(4).
               10  GMLMND-IO.
                   15  GMLMND              PICTURE S9(2).
               10  GMLMA-ELG-IO.
                   15  GMLMA-ELG           PICTURE S9(6).
               10  FIRNUM-IO.
                   15  FIRNUM              PICTURE S9(3).
               10  FIRMA                   PICTURE X(3).
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(8).
               10  ANT01T-IO.
                   15  ANT01T              PICTURE S9(8).
               10  FN6-IO.
                   15  FN6                 PICTURE S9(6).
               10  UPDATO-N-IO.
                   15  UPDATO-N            PICTURE S9(7).
               10  DATO6                   PICTURE X(6).
               10  UPAAR                   PICTURE X(4).
               10  DATO                    PICTURE X(6).
               10  UPDAG                   PICTURE X(2).
               10  DATOUP                  PICTURE X(8).
               10  UPTID-N-IO.
                   15  UPTID-N             PICTURE S9(7).
               10  TIDUP                   PICTURE X(6).
               10  ANTI-IO.
                   15  ANTI                PICTURE S9(6).
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(8).
               10  ANTINT-IO.
                   15  ANTINT              PICTURE S9(8).
               10  ANTNM-IO.
                   15  ANTNM               PICTURE S9(8).
               10  ANTNMT-IO.
                   15  ANTNMT              PICTURE S9(8).
               10  DATOUX-IO.
                   15  DATOUX              PICTURE S9(8).
               10  UPMNA-ELGR-IO.
                   15  UPMNA-ELGR          PICTURE S9(6).
               10  ANTGM-IO.
                   15  ANTGM               PICTURE S9(8).
               10  ANTGMT-IO.
                   15  ANTGMT              PICTURE S9(8).
               10  ANTN-IO.
                   15  ANTN                PICTURE S9(6).
               10  ANTNY-IO.
                   15  ANTNY               PICTURE S9(8).
               10  ANTNYT-IO.
                   15  ANTNYT              PICTURE S9(8).
               10  ANTBUP-IO.
                   15  ANTBUP              PICTURE S9(8).
               10  YEAR4                   PICTURE X(4).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
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
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREMIN-PROCESS
               SET VAREMIN-PROCESS-OFF     TO TRUE
               SET VAREMIN-READ            TO TRUE
           END-IF
 
           IF  VAREMIN-READ
               PERFORM VAREMIN-GET
               SET VAREMIN-READ-OFF        TO TRUE
               IF  NOT VAREMIN-EOF
                   PERFORM VAREMIN-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREKON-PROCESS
               SET VAREKON-PROCESS-OFF     TO TRUE
               SET VAREKON-READ            TO TRUE
           END-IF
 
           IF  VAREKON-READ
               PERFORM VAREKON-GET
               SET VAREKON-READ-OFF        TO TRUE
               IF  NOT VAREKON-EOF
                   PERFORM VAREKON-MATCH-SET
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
 
           IF  VAREMIN-PROCESS
               PERFORM VAREMIN-IDSET
           END-IF
 
           IF  VAREKON-PROCESS
               PERFORM VAREKON-IDSET
           END-IF
 
           IF  VAREMIN-PROCESS
               PERFORM VAREMIN-CHK-LEVEL
           END-IF
 
           IF  VAREKON-PROCESS
               PERFORM VAREKON-CHK-LEVEL
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VAREMIN-PROCESS
               PERFORM VAREMIN-FLDSET
           END-IF
 
           IF  VAREKON-PROCESS
               PERFORM VAREKON-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREMIN-PROCESS
           OR  VAREKON-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-51                TO TRUE
               SET NOT-I-52                TO TRUE
           END-IF
           SET NOT-I-50                    TO TRUE
           SET NOT-I-55                    TO TRUE
           IF  (I-02)
               SET NOT-I-55                TO TRUE
               IF  RECTYP = 'S1'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-55)
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR Å BEREGNE ÅR/MND SOM SKAL FJERNES.                 *
      *****************************************************************
           END-IF
           IF  (NOT-I-99)
               MOVE UYEAR                  TO YEAR2N-IO
               MOVE YEAR2N                 TO YEAR2
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE YEAR2 (2:1)            TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO YEAR2 (2:1)
               PERFORM AARRUT-S
           END-IF
           IF  (NOT-I-99)
               MOVE YEAR4                  TO GMLA-ELGR
               MOVE UMONTH                 TO GMLMND-IO
               SUBTRACT 5                  FROM GMLA-ELGR
               MULTIPLY 100 BY GMLA-ELGR GIVING GMLMA-ELG
               ADD GMLMND                  TO GMLMA-ELG
               SET I-99                    TO TRUE
      *****************************************************************
      * RUTINE VED FIRMABRUDD.                                        *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-10                TO TRUE
               ADD FIRM TO ZERO        GIVING FIRNUM
               MOVE FIRNUM                 TO FIRMA
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE FIRMA (3:1)            TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO FIRMA (3:1)
               PERFORM FISLET-S
           END-IF
           IF  (I-L2 AND I-98)
               SET I-10                    TO TRUE
      *****************************************************************
      * RUTINE VED NYTT EDB-NR.                                       *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET I-51                    TO TRUE
               ADD 1                       TO ANT01
               ADD 1                       TO ANT01T
               GO TO SLUTT-T
      *****************************************************************
      * LAG SORTFELT BESTÅENDE AV ÅÅÅÅMMDD OG MMTTSS                  *
      *****************************************************************
           END-IF
           IF  (I-02)
               MOVE UPDATO                 TO UPDATO-N
               MOVE UPDATO-N-IO (2:6)      TO FN6-IO
               MOVE FN6                    TO DATO6
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE DATO6 (6:1)            TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO DATO6 (6:1)
               MOVE DATO6 (5:2)            TO YEAR2
               PERFORM AARRUT-S
           END-IF
           IF  (I-02)
               MOVE YEAR4                  TO UPAAR
               MOVE DATO6                  TO DATO
               MOVE DATO6 (1:2)            TO UPDAG
               MOVE UPDAG                  TO DATO (5:2)
               MOVE DATO                   TO DATOUP (3:6)
               MOVE UPAAR                  TO DATOUP (1:4)
      *
           END-IF
           IF  (I-02)
               MOVE UPTID                  TO UPTID-N
               MOVE UPTID-N-IO (2:6)       TO FN6-IO
               MOVE FN6                    TO TIDUP
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE TIDUP (6:1)            TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO TIDUP (6:1)
      *****************************************************************
      * RUTINE FOR Å FJERNE RECORD SOM IKKE ER I VAREMASTER OG        *
      *        RECORD SOM ER BLITT FOR GAMLE.                         *
      *****************************************************************
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTI
               ADD 1                       TO ANTIN
               ADD 1                       TO ANTINT
      *
           END-IF
           IF  (I-02 AND I-10)
               OR  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNM
               ADD 1                       TO ANTNMT
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               MOVE DATOUP                 TO DATOUX-IO
               DIVIDE DATOUX BY 100    GIVING UPMNA-ELGR
               SET NOT-I-92                TO TRUE
               IF  UPMNA-ELGR < GMLMA-ELG
                   SET I-92                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-92)
               ADD 1                       TO ANTGM
               ADD 1                       TO ANTGMT
               GO TO SLUTT-T
      *****************************************************************
      *  KONTOKURANT RECORDS SOM SKAL BEHOLDES.                       *
      *****************************************************************
           END-IF
           IF  (I-02)
               SET I-50                    TO TRUE
               SET I-52                    TO TRUE
               ADD 1                       TO ANTN
               ADD 1                       TO ANTNY
               ADD 1                       TO ANTNYT
           END-IF.
 
       SLUTT-T.
           IF  (I-02 AND NOT-I-50)
               ADD 1                       TO ANTBUP
      ******************************************************
      *    SUBRUTINE FOR 4 SIFFER ÅRSTALL IBM SUBRUT.      *
      ******************************************************
           END-IF
           .
 
       AARRUT-S SECTION.
       AARRUT-S-P.
      *R                   MOVE "20"      YEARW   2         YEAR WINDOW
      *R                   CALL "ILNY224"              95   4 SIFFER ÅR.
      *R                   PARM           YEAR2             LINKDATA
      *R                   PARM           YEARW             LINKDATA
      *R                   PARM           YEAR4   4         LINKDATA
           MOVE '20'                       TO YEAR4 (1:2)
           MOVE YEAR2                      TO YEAR4 (3:2).
      ******************************************************
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-96 AND NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'R'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
      *****************************************************************
 
       VAREMIN-GET SECTION.
       VAREMIN-GET-P.
           IF  VAREMIN-EOF-OFF
               READ VAREMIN
               AT END
                   SET VAREMIN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMIN-FLDSET SECTION.
       VAREMIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMIN-IO-AREA (1:10) TO REC1 (1:10)
               MOVE VAREMIN-IO-AREA (1:3)  TO FIRM-IO
               INSPECT FIRM-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMIN-IO-AREA (4:7)  TO EDBN-IO
               INSPECT EDBN-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       VAREMIN-IDSET SECTION.
       VAREMIN-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMIN-CHK-LEVEL SECTION.
       VAREMIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREMIN-LEVEL-01
               MOVE VAREMIN-IO-AREA (1:3)  TO VAREMIN-01-L2-FIRM
               MOVE VAREMIN-IO-AREA (4:7)  TO VAREMIN-01-L1-EDBN
               IF  VAREMIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREMIN-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREMIN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREMIN-01-L2         TO THE-PRIOR-L2
               MOVE  VAREMIN-01-L1         TO THE-PRIOR-L1
               SET VAREMIN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMIN-MATCH-SET SECTION.
       VAREMIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMIN-IO-AREA (1:3)  TO VAREMIN-M-01-M2-FIRM
               MOVE VAREMIN-IO-AREA (4:7)  TO VAREMIN-M-01-M1-EDBN
           END-EVALUATE.
 
       VAREKON-GET SECTION.
       VAREKON-GET-P.
           IF  VAREKON-EOF-OFF
               READ VAREKON
               AT END
                   SET VAREKON-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREKON-FLDSET SECTION.
       VAREKON-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREKON-IO-AREA (1:100) TO KONREC (1:100)
               MOVE VAREKON-IO-AREA (1:3)  TO FIRM-IO
               INSPECT FIRM-IO REPLACING ALL ' ' BY '0'
               MOVE VAREKON-IO-AREA (4:7)  TO EDBN-IO
               INSPECT EDBN-IO REPLACING ALL ' ' BY '0'
               MOVE VAREKON-IO-AREA (19:4) TO UPDATO-IO
               MOVE VAREKON-IO-AREA (23:4) TO UPTID-IO
               MOVE VAREKON-IO-AREA (27:2) TO RECTYP (1:2)
           END-EVALUATE.
 
       VAREKON-IDSET SECTION.
       VAREKON-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREKON-CHK-LEVEL SECTION.
       VAREKON-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREKON-LEVEL-02
               MOVE VAREKON-IO-AREA (1:3)  TO VAREKON-02-L2-FIRM
               MOVE VAREKON-IO-AREA (4:7)  TO VAREKON-02-L1-EDBN
               IF  VAREKON-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREKON-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREKON-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREKON-02-L2         TO THE-PRIOR-L2
               MOVE  VAREKON-02-L1         TO THE-PRIOR-L1
               SET VAREKON-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREKON-MATCH-SET SECTION.
       VAREKON-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREKON-IO-AREA (1:3)  TO VAREKON-M-02-M2-FIRM
               MOVE VAREKON-IO-AREA (4:7)  TO VAREKON-M-02-M1-EDBN
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
       TOTALER-PRINT-LINE SECTION.
       TOTALER-PRINT-LINE-P.
           IF  TOTALER-BEFORE-SKIP > 0
               PERFORM TOTALER-SKIP-BEFORE
           END-IF
           IF  TOTALER-BEFORE-SPACE > 0
               PERFORM TOTALER-SPACE-BEFORE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               IF  TOTALER-AFTER-SPACE > 0
                   PERFORM TOTALER-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               PERFORM TOTALER-SPACE-AFTER
           END-IF
           IF  TOTALER-LINE-COUNT NOT < TOTALER-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       TOTALER-SKIP-BEFORE SECTION.
       TOTALER-SKIP-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-BEFORE-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-BEFORE SECTION.
       TOTALER-SPACE-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER TOTALER-BEFORE-SPACE
                                                                 LINES
           ADD TOTALER-BEFORE-SPACE        TO TOTALER-LINE-COUNT
           MOVE SPACES TO TOTALER-IO-AREA
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-BEFORE-SPACE.
 
       TOTALER-SKIP-AFTER SECTION.
       TOTALER-SKIP-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-AFTER-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-AFTER SECTION.
       TOTALER-SPACE-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE TOTALER-AFTER-SPACE LINES
           ADD TOTALER-AFTER-SPACE         TO TOTALER-LINE-COUNT
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VAREMIN-EOF
               MOVE HIGH-VALUES            TO VAREMIN-MC
                                              VAREMIN-MP
           END-IF
           IF  VAREKON-EOF
               MOVE HIGH-VALUES            TO VAREKON-MC
                                              VAREKON-MP
           END-IF
           IF  VAREMIN-MC < VAREMIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREKON-MC < VAREKON-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREMIN-MC < VAREKON-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMIN-PROCESS     TO TRUE
                   MOVE VAREMIN-MC         TO VAREMIN-MP
                   IF  VAREMIN-MC = VAREKON-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREKON-MC < VAREMIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREKON-PROCESS     TO TRUE
                   MOVE VAREKON-MC         TO VAREKON-MP
                   IF  VAREKON-MC = VAREMIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMIN-MC = VAREKON-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMIN-PROCESS     TO TRUE
                   MOVE VAREMIN-MC         TO VAREMIN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-50)
               MOVE SPACES TO KOPIUT-IO-AREA
               INITIALIZE KOPIUT-IO-AREA
               MOVE KONREC                 TO KOPIUT-IO-AREA (1:100)
               MOVE DATOUP                 TO KOPIUT-IO-AREA (101:8)
               MOVE TIDUP                  TO KOPIUT-IO-AREA (109:6)
               WRITE KOPIUT-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '      REORG.'         TO TOTALER-IO-AREA (4:12)
               MOVE 'AV VARE.KONTO.KURANT' TO TOTALER-IO-AREA (17:20)
               MOVE 'PR.'                  TO TOTALER-IO-AREA (38:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (43:8)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FIRMA'                TO TOTALER-IO-AREA (1:5)
               MOVE 'ANT INN'              TO TOTALER-IO-AREA (14:7)
               MOVE 'ANT NMR'              TO TOTALER-IO-AREA (29:7)
               MOVE 'ANT GML'              TO TOTALER-IO-AREA (44:7)
               MOVE 'ANT UT'               TO TOTALER-IO-AREA (60:6)
               MOVE 'ANT EDBNR'            TO TOTALER-IO-AREA (72:9)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '      REORG.'         TO TOTALER-IO-AREA (4:12)
               MOVE 'AV VARE.KONTO.KURANT' TO TOTALER-IO-AREA (17:20)
               MOVE 'PR.'                  TO TOTALER-IO-AREA (38:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (43:8)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FIRMA'                TO TOTALER-IO-AREA (1:5)
               MOVE 'ANT INN'              TO TOTALER-IO-AREA (14:7)
               MOVE 'ANT NMR'              TO TOTALER-IO-AREA (29:7)
               MOVE 'ANT GML'              TO TOTALER-IO-AREA (44:7)
               MOVE 'ANT UT'               TO TOTALER-IO-AREA (60:6)
               MOVE 'ANT EDBNR'            TO TOTALER-IO-AREA (72:9)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-51)
               MOVE SPACES TO HJFILE-IO-AREA
               INITIALIZE HJFILE-IO-AREA
               MOVE REC1                   TO HJFILE-IO-AREA (1:10)
               MOVE ANTI                   TO XO-60P
               MOVE XO-60P-EF              TO HJFILE-IO-AREA (15:4)
               INITIALIZE ANTI
               MOVE ANTN                   TO XO-60P
               MOVE XO-60P-EF              TO HJFILE-IO-AREA (18:4)
               INITIALIZE ANTN
               IF  (I-52)
                   MOVE '*'                TO HJFILE-IO-AREA (24:1)
               END-IF
               WRITE HJFILE-IO-AREA
           END-IF
           IF  (I-L2)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMA                  TO TOTALER-IO-AREA (3:3)
               MOVE ANTIN                  TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (11:10)
               INITIALIZE ANTIN
               MOVE ANTNM                  TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (26:10)
               INITIALIZE ANTNM
               MOVE ANTGM                  TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (41:10)
               INITIALIZE ANTGM
               MOVE ANTNY                  TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (56:10)
               INITIALIZE ANTNY
               MOVE ANT01                  TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (71:10)
               INITIALIZE ANT01
               IF  (I-10)
                   MOVE 'SLETTET.'         TO TOTALER-IO-AREA (93:8)
               END-IF
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'TOTAL'                TO TOTALER-IO-AREA (1:5)
               MOVE ANTINT                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (11:10)
               MOVE ANTNMT                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (26:10)
               MOVE ANTGMT                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (41:10)
               MOVE ANTNYT                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (56:10)
               MOVE ANT01T                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (71:10)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'RECORDS ELDRE ENN'    TO TOTALER-IO-AREA (3:17)
               MOVE GMLMA-ELG-IO           TO TOTALER-IO-AREA (21:6)
               MOVE 'ER FJERNET. TOTALT'   TO TOTALER-IO-AREA (29:18)
               MOVE ANTBUP                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (48:10)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               PERFORM TOTALER-PRINT-LINE
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
           SET VAREMIN-LEVEL-INIT          TO TRUE
           INITIALIZE VAREMIN-DATA-FIELDS
           SET VAREMIN-EOF-OFF             TO TRUE
           SET VAREMIN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMIN-MC
                                              VAREMIN-MP
           OPEN INPUT VAREMIN
           SET VAREKON-LEVEL-INIT          TO TRUE
           INITIALIZE VAREKON-DATA-FIELDS
           SET VAREKON-EOF-OFF             TO TRUE
           SET VAREKON-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREKON-MC
                                              VAREKON-MP
           OPEN INPUT VAREKON
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT KOPIUT
           OPEN OUTPUT HJFILE
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREMIN
           CLOSE VAREKON
           CLOSE FIRMAF
           CLOSE KOPIUT
           CLOSE HJFILE
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
