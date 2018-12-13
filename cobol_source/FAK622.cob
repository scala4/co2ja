       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK622R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK622  DANNE EFAKTURA FILE TIL POSTEN PÅ PUNCH.     *
      *          UPSI 1 = LEGGER UT FAKTURAKOPI.                      *
      *          UPSI 2 = BENYTTES IKKE.                              *
      *          UPSI 3 = FAKTURA KOPI (UTEN GIRO OG BETALINGSDOK)    *
      *          UPSI 4 = PRINTER IKKE TOTALSUMMER.                   *
      *          UPSI 5 = LEGGER UT FAKTURA MED A-POST                *
      *          UPSI 6 = LEGGER UT FAKTURA MED B-POST                *
      *          UPSI 8 = LEGGER UT sluttrecord (EPLX) kun Capella.   *
      *  27/11-01 PROGRAMMERT AV ESPEN LARSEN                         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK622.rpg
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
           SELECT INNFILE
               ASSIGN TO UT-S-INNFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFILE-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT TEKSTF
               ASSIGN TO TEKSTF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS TEKSTF-STATUS
               RECORD KEY IS TEKSTF-KEY1.
           SELECT EFAKT
               ASSIGN TO UT-S-EFAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EFAKT-STATUS.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
           SELECT PRF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFILE
               BLOCK CONTAINS 256
               RECORD CONTAINS 128.
       01  INNFILE-IO-AREA.
           05  INNFILE-IO-AREA-X           PICTURE X(128).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD TEKSTF
               RECORD CONTAINS 800.
       01  TEKSTF-IO-AREA.
           05  TEKSTF-IO-AREA-X.
               10  TEKSTF-KEY1             PICTURE X(4).
               10  FILLER                  PICTURE X(796).
       FD EFAKT
               BLOCK CONTAINS 128
               RECORD CONTAINS 128.
       01  EFAKT-IO-AREA.
           05  EFAKT-IO-AREA-X             PICTURE X(128).
       FD UTFILE
               BLOCK CONTAINS 256
               RECORD CONTAINS 128.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(128).
       FD PRF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRF-IO-PRINT.
           05  PRF-IO-AREA-CONTROL         PICTURE X VALUE ' '.
        02 PRF-IO-AREA.
           05  PRF-IO-AREA-X               PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFILE-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  TEKSTF-STATUS               PICTURE 99 VALUE 0.
           10  EFAKT-STATUS                PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-EOF-OFF         VALUE '0'.
               88  INNFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-READ-OFF        VALUE '0'.
               88  INNFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-PROCESS-OFF     VALUE '0'.
               88  INNFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  INNFILE-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  TEKSTF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  PRF-DATA-FIELDS.
               10  PRF-AFTER-SPACE         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-AFTER-SKIP          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SPACE        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SKIP         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-MAX-LINES           PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-LINE-COUNT          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-CLR-IO              PICTURE X VALUE 'Y'.
           05  INNFILE-LEVEL-02.
               10  INNFILE-02-L2.
                   15  INNFILE-02-L2-FNR   PICTURE X(3).
               10  INNFILE-02-L1.
                   15  INNFILE-02-L1-FAKNR PICTURE X(6).
           05  INNFILE-LEVEL-03.
               10  INNFILE-03-L2.
                   15  INNFILE-03-L2-FNR   PICTURE X(3).
               10  INNFILE-03-L1.
                   15  INNFILE-03-L1-FAKNR PICTURE X(6).
           05  INNFILE-LEVEL-04.
               10  INNFILE-04-L2.
                   15  INNFILE-04-L2-FNR   PICTURE X(3).
               10  INNFILE-04-L1.
                   15  INNFILE-04-L1-FAKNR PICTURE X(6).
           05  INNFILE-DATA-FIELDS.
               10  HEDREC                  PICTURE X(128).
               10  RECART                  PICTURE X(2).
               10  ANTKOP-IO.
                   15  ANTKOP              PICTURE S9(1).
               10  FPRTYP                  PICTURE X(1).
               10  FNR                     PICTURE X(3).
               10  FAKNR                   PICTURE X(6).
               10  RECTYP                  PICTURE X(1).
               10  EPLK                    PICTURE X(128).
               10  FORMNR                  PICTURE X(5).
               10  REC3                    PICTURE X(128).
               10  REC4                    PICTURE X(128).
           05  FIRMAF-DATA-FIELDS.
               10  FAKTPT                  PICTURE X(1).
               10  FAKTBK                  PICTURE X(1).
           05  TEKSTF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FIADR                   PICTURE X(30).
               10  FIPNR                   PICTURE X(4).
               10  FIPST                   PICTURE X(30).
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  TEKKEY                  PICTURE X(4).
               10  RECSNR-IO.
                   15  RECSNR              PICTURE S9(6).
               10  ANTSKA-IO.
                   15  ANTSKA              PICTURE S9(6).
               10  ANTSSA-IO.
                   15  ANTSSA              PICTURE S9(6).
               10  ANTSKB-IO.
                   15  ANTSKB              PICTURE S9(6).
               10  ANTSSB-IO.
                   15  ANTSSB              PICTURE S9(6).
               10  ANTSAT-IO.
                   15  ANTSAT              PICTURE S9(6).
               10  ANTSTT-IO.
                   15  ANTSTT              PICTURE S9(6).
               10  ANTS8-IO.
                   15  ANTS8               PICTURE S9(5).
               10  ANTS8A-IO.
                   15  ANTS8A              PICTURE S9(4).
               10  ANTS8B-IO.
                   15  ANTS8B              PICTURE S9(4).
               10  ANTS8T-IO.
                   15  ANTS8T              PICTURE S9(4).
           05  EDITTING-FIELDS.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-40YY9                PICTURE Z.ZZ9.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFILE-PROCESS
               SET INNFILE-PROCESS-OFF     TO TRUE
               SET INNFILE-READ            TO TRUE
           END-IF
 
           IF  INNFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFILE-GET
               SET INNFILE-READ-OFF        TO TRUE
               IF  NOT INNFILE-EOF
                   SET INNFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-IDSET
           END-IF
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-CHK-LEVEL
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
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFILE-PROCESS
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
               SET NOT-I-45                TO TRUE
               SET NOT-I-46                TO TRUE
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-09                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-09            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               MOVE FNR                    TO TEKKEY (1:3)
               MOVE 'C'                    TO TEKKEY (4:1)
               MOVE TEKKEY                 TO TEKSTF-KEY1
               READ TEKSTF RECORD KEY IS TEKSTF-KEY1
               INVALID KEY
                   SET I-08                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-08            TO TRUE
                   PERFORM TEKSTF-FLDSET
                   PERFORM TEKSTF-IDSET
               END-READ
               SET NOT-I-45                TO TRUE
               IF  FAKTPT = 'A'
                   SET I-45                TO TRUE
               END-IF
               SET NOT-I-46                TO TRUE
               IF  FAKTBK = 'J'
                   SET I-46                TO TRUE
               END-IF
      *****************************************************************
           END-IF
           SET NOT-I-50                    TO TRUE
           SET NOT-I-51                    TO TRUE
           IF  (I-02)
               SET NOT-I-52                TO TRUE
           END-IF
           IF  (I-L2)
               SUBTRACT RECSNR             FROM RECSNR
           END-IF
           IF  (I-02)
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'M'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'E'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'A'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'B'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-59)
               MOVE 'U'                    TO FPRTYP
           END-IF
           IF  (I-02 AND I-U3)
               SET NOT-I-53                TO TRUE
               IF  FPRTYP = 'U'
                   SET I-53                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  FPRTYP = 'X'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-52 AND I-U3)
               SET NOT-I-52                TO TRUE
               IF  FPRTYP = 'Y'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-52 AND I-U3)
               SET NOT-I-52                TO TRUE
               IF  FPRTYP = 'Z'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-52 AND NOT-I-53)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-52 AND I-U3)
               ADD 1                       TO RECSNR
      *****************************************************************
      * ER DETTE NY SIDE ELLER NYTT BREV ?                            *
      *****************************************************************
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  RECART = '01'
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  RECART = '02'
               SET I-32                    TO TRUE
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  RECART = '27'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-50 AND I-31 AND I-45)
               ADD 1                       TO ANTSKA
           END-IF
           IF  (I-50 AND I-32 AND I-45)
               ADD ANTKOP                  TO ANTSSA
           END-IF
           IF  (I-50 AND I-31 AND NOT-I-45)
               ADD 1                       TO ANTSKB
           END-IF
           IF  (I-50 AND I-32 AND NOT-I-45)
               ADD ANTKOP                  TO ANTSSB
           END-IF
           IF  (I-52 AND I-31 AND I-U3)
               ADD 1                       TO ANTSAT
           END-IF
           IF  (I-52 AND I-32 AND I-U3)
               ADD ANTKOP                  TO ANTSTT
      *****************************************************************
      * TELL ANTALL BREV OVER 8 SIDER.                                *
      *****************************************************************
           END-IF
           SET NOT-I-89                    TO TRUE
           IF  (I-31)
               SUBTRACT ANTS8              FROM ANTS8
           END-IF
           IF  (I-32)
               ADD 1                       TO ANTS8
               SET NOT-I-89                TO TRUE
               IF  ANTS8 = 9
                   SET I-89                TO TRUE
               END-IF
           END-IF
           IF  (I-50 AND I-32 AND I-45)
               AND (I-89)
               ADD 1                       TO ANTS8A
           END-IF
           IF  (I-50 AND I-32 AND NOT-I-45)
               AND (I-89)
               ADD 1                       TO ANTS8B
           END-IF
           IF  (I-52 AND I-32 AND I-U3)
               AND (I-89)
               ADD 1                       TO ANTS8T
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  RECTYP = 'A'
               SET I-12                    TO TRUE
           END-IF
           IF  (I-U3 AND NOT-I-12)
               GO TO SLUTT-T
           END-IF
           IF  (I-46)
               SET I-51                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  FORMNR = '50000'
               SET I-15                    TO TRUE
           END-IF
           IF  (I-51 AND I-31 AND I-U3)
               ADD 1                       TO ANTSAT
           END-IF
           IF  (I-51 AND I-04 AND I-U3)
               AND (I-L1)
               ADD 1                       TO ANTSAT
           END-IF
           IF  (I-51 AND I-32 AND I-U3)
               ADD 1                       TO ANTSTT
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * DATA SOM SKAL LEGGES  UT TIL POSTEN.                          *
      * UPSI 1 = FAKTURAKOPI, UPSI 5 = A-POST, UPSI 6  = B-POST.      *
      *****************************************************************
           CONTINUE.
 
       INNFILE-GET SECTION.
       INNFILE-GET-P.
           IF  INNFILE-EOF-OFF
               READ INNFILE
               AT END
                   SET INNFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFILE-FLDSET SECTION.
       INNFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '0' )
               MOVE INNFILE-IO-AREA (1:128) TO HEDREC (1:128)
               MOVE INNFILE-IO-AREA (126:2) TO RECART (1:2)
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '1' )
               MOVE INNFILE-IO-AREA (15:1) TO ANTKOP-IO
               INSPECT ANTKOP-IO REPLACING ALL ' ' BY '0'
               MOVE INNFILE-IO-AREA (80:1) TO FPRTYP (1:1)
               MOVE INNFILE-IO-AREA (113:3) TO FNR (1:3)
               MOVE INNFILE-IO-AREA (116:6) TO FAKNR (1:6)
               MOVE INNFILE-IO-AREA (126:2) TO RECART (1:2)
               MOVE INNFILE-IO-AREA (128:1) TO RECTYP (1:1)
               MOVE INNFILE-IO-AREA (1:128) TO EPLK (1:128)
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '3' )
               MOVE INNFILE-IO-AREA (113:3) TO FNR (1:3)
               MOVE INNFILE-IO-AREA (116:6) TO FAKNR (1:6)
               MOVE INNFILE-IO-AREA (4:5)  TO FORMNR (1:5)
               MOVE INNFILE-IO-AREA (126:2) TO RECART (1:2)
               MOVE INNFILE-IO-AREA (128:1) TO RECTYP (1:1)
               MOVE INNFILE-IO-AREA (1:128) TO REC3 (1:128)
           WHEN OTHER
               MOVE INNFILE-IO-AREA (113:3) TO FNR (1:3)
               MOVE INNFILE-IO-AREA (116:6) TO FAKNR (1:6)
               MOVE INNFILE-IO-AREA (126:2) TO RECART (1:2)
               MOVE INNFILE-IO-AREA (128:1) TO RECTYP (1:1)
               MOVE INNFILE-IO-AREA (1:128) TO REC4 (1:128)
           END-EVALUATE.
 
       INNFILE-IDSET SECTION.
       INNFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '1' )
               SET I-02                    TO TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '3' )
               SET I-03                    TO TRUE
           WHEN  OTHER
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       INNFILE-CHK-LEVEL SECTION.
       INNFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '0' )
               CONTINUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '1' )
               MOVE LOW-VALUES             TO INNFILE-LEVEL-02
               MOVE INNFILE-IO-AREA (113:3) TO INNFILE-02-L2-FNR
               MOVE INNFILE-IO-AREA (116:6) TO INNFILE-02-L1-FAKNR
               IF  INNFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFILE-02-L2         TO THE-PRIOR-L2
               MOVE  INNFILE-02-L1         TO THE-PRIOR-L1
               SET INNFILE-LEVEL-INIT      TO TRUE
           WHEN ( INNFILE-IO-AREA (126:1) = '0'
            AND   INNFILE-IO-AREA (127:1) = '3' )
               MOVE LOW-VALUES             TO INNFILE-LEVEL-03
               MOVE INNFILE-IO-AREA (113:3) TO INNFILE-03-L2-FNR
               MOVE INNFILE-IO-AREA (116:6) TO INNFILE-03-L1-FAKNR
               IF  INNFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFILE-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFILE-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFILE-03-L2         TO THE-PRIOR-L2
               MOVE  INNFILE-03-L1         TO THE-PRIOR-L1
               SET INNFILE-LEVEL-INIT      TO TRUE
           WHEN OTHER
               MOVE LOW-VALUES             TO INNFILE-LEVEL-04
               MOVE INNFILE-IO-AREA (113:3) TO INNFILE-04-L2-FNR
               MOVE INNFILE-IO-AREA (116:6) TO INNFILE-04-L1-FAKNR
               IF  INNFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFILE-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFILE-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFILE-04-L2         TO THE-PRIOR-L2
               MOVE  INNFILE-04-L1         TO THE-PRIOR-L1
               SET INNFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (777:1) TO FAKTPT (1:1)
               MOVE FIRMAF-IO-AREA (778:1) TO FAKTBK (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       TEKSTF-FLDSET SECTION.
       TEKSTF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE TEKSTF-IO-AREA (11:30) TO FINAVN (1:30)
               MOVE TEKSTF-IO-AREA (41:30) TO FIADR (1:30)
               MOVE TEKSTF-IO-AREA (101:4) TO FIPNR (1:4)
               MOVE TEKSTF-IO-AREA (105:30) TO FIPST (1:30)
           END-EVALUATE.
 
       TEKSTF-IDSET SECTION.
       TEKSTF-IDSET-P.
           SET I-06                        TO TRUE.
 
       PRF-PRINT-LINE SECTION.
       PRF-PRINT-LINE-P.
           IF  PRF-BEFORE-SKIP > 0
               PERFORM PRF-SKIP-BEFORE
           END-IF
           IF  PRF-BEFORE-SPACE > 0
               PERFORM PRF-SPACE-BEFORE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               IF  PRF-AFTER-SPACE > 0
                   PERFORM PRF-SPACE-AFTER
               END-IF
           ELSE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               PERFORM PRF-SPACE-AFTER
           END-IF
           IF  PRF-LINE-COUNT NOT < PRF-MAX-LINES
               MOVE 7                      TO PRF-AFTER-SKIP
           END-IF.
 
       PRF-SKIP-BEFORE SECTION.
       PRF-SKIP-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-BEFORE-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-BEFORE SECTION.
       PRF-SPACE-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER PRF-BEFORE-SPACE LINES
           ADD PRF-BEFORE-SPACE            TO PRF-LINE-COUNT
           MOVE SPACES TO PRF-IO-AREA
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-BEFORE-SPACE.
 
       PRF-SKIP-AFTER SECTION.
       PRF-SKIP-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-AFTER-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-AFTER SECTION.
       PRF-SPACE-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE PRF-AFTER-SPACE LINES
           ADD PRF-AFTER-SPACE             TO PRF-LINE-COUNT
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L2 AND I-U1)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPL49999'             TO EFAKT-IO-AREA (1:8)
               MOVE FINAVN                 TO EFAKT-IO-AREA (9:30)
               MOVE ';'                    TO EFAKT-IO-AREA (39:1)
               MOVE FIADR                  TO EFAKT-IO-AREA (40:30)
               MOVE ';'                    TO EFAKT-IO-AREA (70:1)
               MOVE FIPNR                  TO EFAKT-IO-AREA (71:4)
               MOVE FIPST                  TO EFAKT-IO-AREA (76:30)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-01 AND I-U1)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE HEDREC                 TO EFAKT-IO-AREA (1:128)
               MOVE 'B'                    TO EFAKT-IO-AREA (20:1)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-02 AND I-50 AND I-U1)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE EPLK                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-03 AND I-50 AND I-U1)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE REC3                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-04 AND I-50 AND I-U1)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE REC4                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-01 AND I-U6)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE HEDREC                 TO EFAKT-IO-AREA (1:128)
               MOVE 'B'                    TO EFAKT-IO-AREA (20:1)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-02 AND I-50 AND I-U6)
           AND (NOT-I-45)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE EPLK                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-03 AND I-50 AND I-U6)
           AND (NOT-I-45)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE REC3                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-04 AND I-50 AND I-U6)
           AND (NOT-I-45)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE REC4                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-01 AND I-U5)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE HEDREC                 TO EFAKT-IO-AREA (1:128)
               MOVE 'A'                    TO EFAKT-IO-AREA (20:1)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-02 AND I-50 AND I-U5)
           AND (I-45)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE EPLK                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-03 AND I-50 AND I-U5)
           AND (I-45)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE REC3                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-04 AND I-50 AND I-U5)
           AND (I-45)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE REC4                   TO EFAKT-IO-AREA (1:128)
               MOVE ' '                    TO EFAKT-IO-AREA (112:1)
               MOVE '                '     TO EFAKT-IO-AREA (113:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-01 AND I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE HEDREC                 TO UTFILE-IO-AREA (1:128)
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF
           IF  (I-02 AND I-51 AND I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE EPLK                   TO UTFILE-IO-AREA (1:128)
               MOVE '1'                    TO UTFILE-IO-AREA (15:1)
               MOVE '-BKOPI1'              TO UTFILE-IO-AREA (41:7)
               MOVE 'B'                    TO UTFILE-IO-AREA (112:1)
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF
           IF  (I-03 AND I-51 AND I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE REC3                   TO UTFILE-IO-AREA (1:128)
               IF  (I-15)
                   MOVE '50002'            TO UTFILE-IO-AREA (4:5)
               END-IF
               MOVE 'B'                    TO UTFILE-IO-AREA (112:1)
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF
           IF  (I-04 AND I-51 AND I-L1)
           AND (I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE EPLK                   TO UTFILE-IO-AREA (1:128)
               MOVE '1'                    TO UTFILE-IO-AREA (15:1)
               MOVE FNR                    TO UTFILE-IO-AREA (26:3)
               MOVE FAKNR                  TO UTFILE-IO-AREA (35:6)
               MOVE '-BKOPI2'              TO UTFILE-IO-AREA (41:7)
               MOVE 'B'                    TO UTFILE-IO-AREA (112:1)
               MOVE FNR                    TO UTFILE-IO-AREA (113:3)
               MOVE FAKNR                  TO UTFILE-IO-AREA (116:6)
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF
           IF  (I-04 AND I-51 AND I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE REC4                   TO UTFILE-IO-AREA (1:128)
               MOVE 'B'                    TO UTFILE-IO-AREA (112:1)
      *****************************************************************
      * FAKTURA  SOM IKKE SKAL SENDES UT. GÅR SAMMEN MED BOKH.KOPI.   *
      *****************************************************************
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF
           IF  (I-02 AND I-52 AND I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE EPLK                   TO UTFILE-IO-AREA (1:128)
               MOVE '-XFAKT '              TO UTFILE-IO-AREA (41:7)
               MOVE 'A'                    TO UTFILE-IO-AREA (112:1)
               MOVE RECSNR-IO              TO UTFILE-IO-AREA (116:6)
               MOVE '       '              TO UTFILE-IO-AREA (122:7)
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF
           IF  (I-03 AND I-52 AND I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE REC3                   TO UTFILE-IO-AREA (1:128)
               MOVE 'A'                    TO UTFILE-IO-AREA (112:1)
               MOVE RECSNR-IO              TO UTFILE-IO-AREA (116:6)
               MOVE '       '              TO UTFILE-IO-AREA (122:7)
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF
           IF  (I-04 AND I-52 AND I-U3)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE REC4                   TO UTFILE-IO-AREA (1:128)
               MOVE 'A'                    TO UTFILE-IO-AREA (112:1)
               MOVE RECSNR-IO              TO UTFILE-IO-AREA (116:6)
               MOVE '       '              TO UTFILE-IO-AREA (122:7)
               IF  I-U3
                   WRITE UTFILE-IO-AREA
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-U1 AND I-U8)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPLX END OF FILE'     TO EFAKT-IO-AREA (1:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-LR AND I-U6 AND I-U8)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPLX END OF FILE'     TO EFAKT-IO-AREA (1:16)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-LR AND I-U5 AND I-U8)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPLX END OF FILE'     TO EFAKT-IO-AREA (1:16)
      *****************************************************************
      * FAKTURA BOKHOLDERIBILAG.                                      *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-LR AND NOT-I-U4)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* Antallsopplysning til ' TO PRF-IO-AREA (1:24)
               MOVE 'Posten Norge BA -eBrev *' TO PRF-IO-AREA (25:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* fra Auto-Data as      ' TO PRF-IO-AREA (1:24)
               MOVE 'Kundeid. 112800        *' TO PRF-IO-AREA (25:24)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* KJØREDATO:            ' TO PRF-IO-AREA (1:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (14:8)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* FILE: AUTOFAKA  = SEND' TO PRF-IO-AREA (1:24)
               MOVE 'ES UT SOM A-PRIORITET  *' TO PRF-IO-AREA (25:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* ANTALL SIDER:         ' TO PRF-IO-AREA (1:24)
               MOVE ANTSSA                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (16:7)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* ANTALL BREV.:         ' TO PRF-IO-AREA (1:24)
               MOVE ANTSKA                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (16:7)
               MOVE 'OVER 8 SIDER:'        TO PRF-IO-AREA (28:13)
               MOVE ANTS8A                 TO XO-40YY9
               MOVE XO-40YY9               TO PRF-IO-AREA (41:5)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '*'                    TO PRF-IO-AREA (1:1)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* FILE: AUTOFAKB  = SEND' TO PRF-IO-AREA (1:24)
               MOVE 'ES UT SOM B-ØKONOMI    *' TO PRF-IO-AREA (25:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* ANTALL SIDER:         ' TO PRF-IO-AREA (1:24)
               MOVE ANTSSB                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (16:7)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* ANTALL BREV.:         ' TO PRF-IO-AREA (1:24)
               MOVE ANTSKB                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (16:7)
               MOVE 'OVER 8 SIDER:'        TO PRF-IO-AREA (28:13)
               MOVE ANTS8B                 TO XO-40YY9
               MOVE XO-40YY9               TO PRF-IO-AREA (41:5)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '*'                    TO PRF-IO-AREA (1:1)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* FILE: AUTOKOPI        ' TO PRF-IO-AREA (1:24)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE '* FILE: AUTOKOPI  = SEND' TO PRF-IO-AREA (1:24)
               MOVE 'ES MED BUD TIL AUTODATA*' TO PRF-IO-AREA (25:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* ANTALL SIDER:         ' TO PRF-IO-AREA (1:24)
               MOVE ANTSTT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (16:7)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* ANTALL BREV.:         ' TO PRF-IO-AREA (1:24)
               MOVE ANTSAT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (16:7)
               MOVE 'OVER 8 SIDER:'        TO PRF-IO-AREA (28:13)
               MOVE ANTS8T                 TO XO-40YY9
               MOVE XO-40YY9               TO PRF-IO-AREA (41:5)
               MOVE '*'                    TO PRF-IO-AREA (48:1)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '  V I K T I G :         ' TO PRF-IO-AREA (1:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '  FILE AUTOKOPI SKAL IKK' TO PRF-IO-AREA (1:24)
               MOVE 'E SORTERES ELLER SENDES ' TO PRF-IO-AREA (25:24)
               MOVE 'UT SOM BREV.            ' TO PRF-IO-AREA (49:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '  VENNLIGST SEND AUTOKOP' TO PRF-IO-AREA (1:24)
               MOVE 'I SAMLET I EN ESKE TIL A' TO PRF-IO-AREA (25:24)
               MOVE 'TODATA MED BILBUD.      ' TO PRF-IO-AREA (49:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '  ADRESSEN ER:          ' TO PRF-IO-AREA (1:24)
               MOVE 1                      TO PRF-BEFORE-SPACE
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '     AUTODATA A/S       ' TO PRF-IO-AREA (1:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '     ØSTRE AKERS VEI 61 ' TO PRF-IO-AREA (1:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '     0581 OSLO          ' TO PRF-IO-AREA (1:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
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
           SET INNFILE-LEVEL-INIT          TO TRUE
           INITIALIZE INNFILE-DATA-FIELDS
           SET INNFILE-EOF-OFF             TO TRUE
           SET INNFILE-PROCESS             TO TRUE
           OPEN INPUT INNFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE TEKSTF-DATA-FIELDS
           OPEN INPUT TEKSTF
           OPEN OUTPUT EFAKT
           IF I-U3
               OPEN OUTPUT UTFILE
           END-IF
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFILE
           CLOSE FIRMAF
           CLOSE TEKSTF
           CLOSE EFAKT
           IF I-U3
               CLOSE UTFILE
           END-IF
           IF PRF-IO-AREA NOT = SPACES
             WRITE PRF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRF-IO-AREA
           END-IF
           CLOSE PRF.
 
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
