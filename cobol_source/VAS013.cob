       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS013R.
      **********************************************  Z-WIN-RPG2   ****
      * DANNER VARESTAT.RECORD FRA FASTE VARESETT.         *
      * PROGRAMMER: ESPEN LARSEN  28.10.2005               *
      *    KODE V = FASTE VARESETT.                        *
      *    KODE W = LØSE STRUKTURER.                       *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS013.rpg
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
           SELECT VARSTVS
               ASSIGN TO UT-S-VARSTVS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTVS-STATUS.
           SELECT VARESET
               ASSIGN TO VARESET
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARESET-STATUS
               RECORD KEY IS VARESET-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT VARSTAA
               ASSIGN TO UT-S-VARSTAA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTAA-STATUS.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARSTVS
               BLOCK CONTAINS 100
               RECORD CONTAINS 50.
       01  VARSTVS-IO-AREA.
           05  VARSTVS-IO-AREA-X           PICTURE X(50).
       FD VARESET
               RECORD CONTAINS 100.
       01  VARESET-IO-AREA.
           05  VARESET-IO-AREA-X.
               10  VARESET-KEY1            PICTURE X(14).
               10  FILLER                  PICTURE X(86).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD VARSTAA
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  VARSTAA-IO-AREA.
           05  VARSTAA-IO-AREA-X           PICTURE X(40).
       FD TOTALER
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VARSTVS-STATUS              PICTURE 99 VALUE 0.
           10  VARESET-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARSTAA-STATUS              PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTVS-EOF-OFF         VALUE '0'.
               88  VARSTVS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTVS-READ-OFF        VALUE '0'.
               88  VARSTVS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTVS-PROCESS-OFF     VALUE '0'.
               88  VARSTVS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARSTVS-LEVEL-INIT-OFF  VALUE '0'.
               88  VARSTVS-LEVEL-INIT      VALUE '1'.
           05  VARESET-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  VARSTVS-LEVEL-01.
               10  VARSTVS-01-L2.
                   15  VARSTVS-01-L2-FIRMA PICTURE X(3).
               10  VARSTVS-01-L1.
                   15  VARSTVS-01-L1-EDBSET PICTURE X(7).
           05  VARSTVS-DATA-FIELDS.
      *                                       1   1 TYPE
               10  FIRMA                   PICTURE X(3).
               10  EDBSET                  PICTURE X(7).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  SETTYP                  PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *                                   P  36  402BEL
               10  REC01                   PICTURE X(40).
               10  ORDNR                   PICTURE X(6).
               10  POSNR                   PICTURE X(3).
           05  VARESET-DATA-FIELDS.
               10  SEQNR                   PICTURE X(3).
               10  EDBVAR                  PICTURE X(7).
               10  ANTSTK-IO.
                   15  ANTSTK              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SSEQNR                  PICTURE X(3).
               10  FAKTOR-IO.
                   15  FAKTOR              PICTURE S9(3)V9(4) USAGE
                                                       PACKED-DECIMAL.
           05  VAREMAS-DATA-FIELDS.
               10  VMALFA                  PICTURE X(3).
               10  VMARTN                  PICTURE X(20).
               10  VUTPRI-IO.
                   15  VUTPRI              PICTURE S9(7)V9(2).
               10  VMVGR                   PICTURE X(5).
      *****************************************************************
      * RUTINE FOR OPPDATERINGSRECORDS. 01                            *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  ANT1-IO.
                   15  ANT1                PICTURE S9(5).
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(6).
               10  ARTFNR                  PICTURE X(4).
               10  EDBSEQ                  PICTURE X(10).
               10  SETKEY                  PICTURE X(14).
               10  ANT1MR-IO.
                   15  ANT1MR              PICTURE S9(5).
               10  ANT2-IO.
                   15  ANT2                PICTURE S9(5).
               10  ANT1NM-IO.
                   15  ANT1NM              PICTURE S9(5).
               10  ENDSEQ                  PICTURE X(3).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(7)V9(2).
               10  ANT2MR-IO.
                   15  ANT2MR              PICTURE S9(5).
               10  SEQNR2-IO.
                   15  SEQNR2              PICTURE S9(3).
               10  SEQNR3                  PICTURE X(3).
               10  EDBKEY                  PICTURE X(10).
               10  SUMLEV-IO.
                   15  SUMLEV              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
 
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
           IF  VARSTVS-PROCESS
               SET VARSTVS-PROCESS-OFF     TO TRUE
               SET VARSTVS-READ            TO TRUE
           END-IF
 
           IF  VARSTVS-READ
           AND RECORD-SELECTED-OFF
               PERFORM VARSTVS-GET
               SET VARSTVS-READ-OFF        TO TRUE
               IF  NOT VARSTVS-EOF
                   SET VARSTVS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VARSTVS-PROCESS
               PERFORM VARSTVS-IDSET
           END-IF
 
           IF  VARSTVS-PROCESS
               PERFORM VARSTVS-CHK-LEVEL
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
 
           IF  VARSTVS-PROCESS
               PERFORM VARSTVS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARSTVS-PROCESS
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
               ADD 1                       TO ANT1
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-61                TO TRUE
               ADD ORDATO TO ZERO      GIVING ODATO
      *****************************************************************
      * RUTINE FOR Å HENTE VARSETT RECORDS.                           *
      * UTREGNE ANTALL LEVERT OG BELØP.                               *
      * HENTE VAREGRUPPE OG ALFAKODE FRA VAREMASTER.                  *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE 'A'                    TO ARTFNR (1:1)
               MOVE FIRMA                  TO ARTFNR (2:3)
               MOVE EDBSET                 TO EDBSEQ (1:7)
               MOVE '001'                  TO EDBSEQ (8:3)
               MOVE ARTFNR                 TO SETKEY (1:4)
               MOVE EDBSEQ                 TO SETKEY (5:10)
               MOVE SETKEY                 TO VARESET-KEY1
               READ VARESET RECORD KEY IS VARESET-KEY1
               INVALID KEY
                   SET I-30                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-30            TO TRUE
                   PERFORM VARESET-IDCHK
                   PERFORM VARESET-FLDSET
                   PERFORM VARESET-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-30)
               ADD 1                       TO ANT1MR
               ADD 1                       TO ANT2
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01)
               SET I-61                    TO TRUE
           END-IF
           IF  (I-01 AND I-61)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-01)
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-01 AND I-30)
               ADD 1                       TO ANT1NM
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE SSEQNR                 TO ENDSEQ
               MULTIPLY ANT BY ANTSTK  GIVING ANTLEV
               MOVE 0,00                   TO SUMLEV
               PERFORM VARRUT-S
           END-IF
           IF  (I-01)
               SET I-51                    TO TRUE
           END-IF
           IF  (I-01 AND I-51)
               PERFORM EXCEPTION-OUTPUT
               ADD 1                       TO ANT2MR
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               SET NOT-I-59                TO TRUE
               IF  ENDSEQ NOT > '001'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-59)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE 1                      TO SEQNR2
      *****************************************************************
      *  LOOP FOR Å HENTE VARERECORDS ETTER SEQ. 001.                 *
      *****************************************************************
           END-IF
           .
 
       LOOP1-T.
           IF  (I-01)
               ADD 1                       TO SEQNR2
               MOVE SEQNR2                 TO SEQNR3
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE SEQNR3 (3:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO SEQNR3 (3:1)
               SET NOT-I-59                TO TRUE
               IF  SEQNR3 > ENDSEQ
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-59)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE SEQNR3                 TO EDBSEQ (8:3)
               MOVE ARTFNR                 TO SETKEY (1:4)
               MOVE EDBSEQ                 TO SETKEY (5:10)
               MOVE SETKEY                 TO VARESET-KEY1
               READ VARESET RECORD KEY IS VARESET-KEY1
               INVALID KEY
                   SET I-30                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-30            TO TRUE
                   PERFORM VARESET-IDCHK
                   PERFORM VARESET-FLDSET
                   PERFORM VARESET-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-30)
               GO TO LOOP1-T
           END-IF
           IF  (I-01 AND NOT-I-30)
               ADD 1                       TO ANT2
           END-IF
           IF  (I-01)
               MULTIPLY ANT BY ANTSTK  GIVING ANTLEV
               MOVE 0,00                   TO SUMLEV
               PERFORM VARRUT-S
           END-IF
           IF  (I-01)
               SET I-51                    TO TRUE
           END-IF
           IF  (I-01 AND I-51)
               PERFORM EXCEPTION-OUTPUT
               ADD 1                       TO ANT2MR
           END-IF
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               GO TO LOOP1-T
      *****************************************************************
      *   TOTAL RUTINE.                                               *
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
      *****************************************************************
      * RUTINE FOR Å HENTE DATA FRA VAREMASTER.                       *
      *****************************************************************
           CONTINUE.
 
       VARRUT-S SECTION.
       VARRUT-S-P.
           MOVE FIRMA                      TO EDBKEY (1:3)
           MOVE EDBVAR                     TO EDBKEY (4:7)
           MOVE EDBKEY                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-31                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-31                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (NOT-I-31)
               MULTIPLY ANTLEV BY VUTPRI GIVING SUMLEV
           END-IF.
      ******************************************************
 
       VARSTVS-GET SECTION.
       VARSTVS-GET-P.
           IF  VARSTVS-EOF-OFF
               READ VARSTVS
               AT END
                   SET VARSTVS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARSTVS-FLDSET SECTION.
       VARSTVS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARSTVS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE VARSTVS-IO-AREA (5:7)  TO EDBSET (1:7)
               MOVE VARSTVS-IO-AREA (22:4) TO ORDATO-IO
               MOVE VARSTVS-IO-AREA (30:1) TO SETTYP (1:1)
               MOVE VARSTVS-IO-AREA (31:5) TO ANT-IO
               MOVE VARSTVS-IO-AREA (1:40) TO REC01 (1:40)
               MOVE VARSTVS-IO-AREA (41:6) TO ORDNR (1:6)
               MOVE VARSTVS-IO-AREA (47:3) TO POSNR (1:3)
           END-EVALUATE.
 
       VARSTVS-IDSET SECTION.
       VARSTVS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARSTVS-CHK-LEVEL SECTION.
       VARSTVS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARSTVS-LEVEL-01
               MOVE VARSTVS-IO-AREA (2:3)  TO VARSTVS-01-L2-FIRMA
               MOVE VARSTVS-IO-AREA (5:7)  TO VARSTVS-01-L1-EDBSET
               IF  VARSTVS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARSTVS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARSTVS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARSTVS-01-L2         TO THE-PRIOR-L2
               MOVE  VARSTVS-01-L1         TO THE-PRIOR-L1
               SET VARSTVS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VARESET-FLDSET SECTION.
       VARESET-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VARESET-IO-AREA (1:1) = 'A' )
               MOVE VARESET-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE VARESET-IO-AREA (5:7)  TO EDBSET (1:7)
               MOVE VARESET-IO-AREA (12:3) TO SEQNR (1:3)
               MOVE VARESET-IO-AREA (15:7) TO EDBVAR (1:7)
               MOVE VARESET-IO-AREA (22:5) TO ANTSTK-IO
               MOVE VARESET-IO-AREA (27:3) TO SSEQNR (1:3)
               MOVE VARESET-IO-AREA (31:4) TO FAKTOR-IO
           END-EVALUATE.
 
       VARESET-IDCHK SECTION.
       VARESET-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VARESET-IO-AREA (1:1) = 'A' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VARESET-IDSET SECTION.
       VARESET-IDSET-P.
           EVALUATE TRUE
           WHEN ( VARESET-IO-AREA (1:1) = 'A' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO VMALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO VMARTN (1:20)
               MOVE VAREMAS-IO-AREA (75:9) TO VUTPRI-IO
               INSPECT VUTPRI-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (118:5) TO VMVGR (1:5)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
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
               MOVE 7                      TO TOTALER-AFTER-SKIP
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
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-01 AND I-51)
               MOVE SPACES TO VARSTAA-IO-AREA
               INITIALIZE VARSTAA-IO-AREA
               MOVE REC01                  TO VARSTAA-IO-AREA (1:40)
               MOVE EDBVAR                 TO VARSTAA-IO-AREA (5:7)
               IF  (NOT-I-31)
                   MOVE VMALFA             TO VARSTAA-IO-AREA (14:3)
               END-IF
               IF  (NOT-I-31)
                   MOVE VMVGR              TO VARSTAA-IO-AREA (17:5)
               END-IF
               MOVE ANTLEV                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTAA-IO-AREA (31:5)
               MOVE SUMLEV                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTAA-IO-AREA (36:5)
               WRITE VARSTAA-IO-AREA
           END-IF
           IF  (I-01 AND I-61 AND I-U1)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMA                  TO TOTALER-IO-AREA (1:3)
               MOVE ORDNR                  TO TOTALER-IO-AREA (5:6)
               MOVE EDBSET                 TO TOTALER-IO-AREA (12:7)
               MOVE SETTYP                 TO TOTALER-IO-AREA (20:1)
               MOVE POSNR                  TO TOTALER-IO-AREA (22:3)
               MOVE ANT                    TO XO-72YY9
               MOVE XO-72YY9               TO TOTALER-IO-AREA (26:12)
               IF  (I-50)
                   MOVE 'FUNNET VARESETT'  TO TOTALER-IO-AREA (39:15)
               END-IF
               IF  (NOT-I-50)
                   MOVE 'UKJENT VARESETT'  TO TOTALER-IO-AREA (39:15)
               END-IF
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (55:8)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-01 AND I-51 AND I-U1)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE EDBVAR                 TO TOTALER-IO-AREA (12:7)
               MOVE SEQNR                  TO TOTALER-IO-AREA (22:3)
               MOVE ANTLEV                 TO XO-72YY9
               MOVE XO-72YY9               TO TOTALER-IO-AREA (26:12)
               MOVE SUMLEV                 TO XO-72YY9
               MOVE XO-72YY9               TO TOTALER-IO-AREA (39:12)
               IF  (NOT-I-31)
                   MOVE VMALFA             TO TOTALER-IO-AREA (52:3)
               END-IF
               IF  (NOT-I-31)
                   MOVE VMARTN             TO TOTALER-IO-AREA (56:20)
               END-IF
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P AND I-U1)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (3:8)
               MOVE 'DANNE VARSTAT.REC. FRA ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 'VARESETT'             TO TOTALER-IO-AREA (35:8)
               MOVE 'FASTE VARESETT.   '   TO TOTALER-IO-AREA (45:18)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U1)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANT1                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'VARESETT LEST INN.     ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANT1MR                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'VARESETT FUNNET.       ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANT1NM                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'VARESETT IKKE FUNNET   ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANT2                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'VARESETT DET.REC. LEST ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANT2MR                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'VARE.STAT.REC DANNET.  ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 2                      TO TOTALER-AFTER-SPACE
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
           MOVE 1                          TO LR-CHECK
           SET VARSTVS-LEVEL-INIT          TO TRUE
           INITIALIZE VARSTVS-DATA-FIELDS
           SET VARSTVS-EOF-OFF             TO TRUE
           SET VARSTVS-PROCESS             TO TRUE
           OPEN INPUT VARSTVS
           INITIALIZE VARESET-DATA-FIELDS
           OPEN INPUT VARESET
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT VARSTAA
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARSTVS
           CLOSE VARESET
           CLOSE VAREMAS
           CLOSE VARSTAA
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
