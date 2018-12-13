       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD162R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: ORD162 (KOPI AV ORD061)                         *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMERT.: 13.03.97                                        *
      *  SIST RETTET: 08.01.05                                        *
      *                                                               *
      *  PROGRAMMET MERGER ALLE DIREKTEKUNDER MED ORDRESUMFILE OG     *
      *  DANNER EN SUMFILE TIL STATISTIKK.                            *
      *  DET ER KUN FAKTURERTE ORDRE/KR.NOTAGRUNNLAG SOM SKAL MED.    *
      *  PERIODE HENTES FRA PARAMETERFILE.                            *
      *  19.03.03 Hafnor vil ikke ha med bonus kreditnota.            *
      *  27.06.03 KREDITNOTA SUMMERES I EGNE FELT.                    *
      *  14.12.04 UPSI 1 = KUN WEB-ORDRE = DIR.ORDRE.                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD162.rpg
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
           SELECT ORDSUMF
               ASSIGN TO UT-S-ORDSUMF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDSUMF-STATUS.
           SELECT PFILE
               ASSIGN TO UT-S-PFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PFILE-STATUS.
           SELECT UTDATA
               ASSIGN TO UT-S-UTDATA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTDATA-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDSUMF
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  ORDSUMF-IO-AREA.
           05  ORDSUMF-IO-AREA-X           PICTURE X(100).
       FD PFILE
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  PFILE-IO-AREA.
           05  PFILE-IO-AREA-X             PICTURE X(100).
       FD UTDATA
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  UTDATA-IO-AREA.
           05  UTDATA-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDSUMF-STATUS              PICTURE 99 VALUE 0.
           10  PFILE-STATUS                PICTURE 99 VALUE 0.
           10  UTDATA-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMF-EOF-OFF         VALUE '0'.
               88  ORDSUMF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMF-READ-OFF        VALUE '0'.
               88  ORDSUMF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDSUMF-PROCESS-OFF     VALUE '0'.
               88  ORDSUMF-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDSUMF-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDSUMF-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-EOF-OFF           VALUE '0'.
               88  PFILE-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-READ-OFF          VALUE '0'.
               88  PFILE-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PFILE-PROCESS-OFF       VALUE '0'.
               88  PFILE-PROCESS           VALUE '1'.
           05  ORDSUMF-LEVEL-02.
               10  ORDSUMF-02-L2.
                   15  ORDSUMF-02-L2-FIRMA PICTURE X(3).
               10  ORDSUMF-02-L1.
                   15  ORDSUMF-02-L1-KUNDE PICTURE X(6).
           05  ORDSUMF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  AAR                     PICTURE X(2).
               10  MND                     PICTURE X(2).
               10  ORDMOT                  PICTURE X(2).
               10  KUNDE                   PICTURE X(6).
               10  RUT                     PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  WEBORD                  PICTURE X(2).
               10  ORDSUM-IO.
                   15  ORDSUM              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTVL-IO.
                   15  ANTVL               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  PFILE-DATA-FIELDS.
               10  PAR4-IO.
                   15  PAR4                PICTURE S9(4).
               10  PAR2                    PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  MNDNAV                  PICTURE X(9).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FJOR-IO.
                   15  FJOR                PICTURE S9(2).
               10  FJORA                   PICTURE X(2).
               10  PFJ4-IO.
                   15  PFJ4                PICTURE S9(4).
               10  PFJ2-IO.
                   15  PFJ2                PICTURE S9(2).
               10  PFJ                     PICTURE X(2).
               10  AODMT-IO.
                   15  AODMT               PICTURE S9(5).
               10  AOA-ELGRT-IO.
                   15  AOA-ELGRT           PICTURE S9(5).
               10  AOFJT-IO.
                   15  AOFJT               PICTURE S9(5).
               10  AODMDR-IO.
                   15  AODMDR              PICTURE S9(5).
               10  AOA-ELGRDR-IO.
                   15  AOA-ELGRDR          PICTURE S9(5).
               10  AOFJDR-IO.
                   15  AOFJDR              PICTURE S9(5).
               10  SODMT-IO.
                   15  SODMT               PICTURE S9(9)V9(2).
               10  SOA-ELGRT-IO.
                   15  SOA-ELGRT           PICTURE S9(9)V9(2).
               10  SOFJT-IO.
                   15  SOFJT               PICTURE S9(9)V9(2).
               10  SKDMT-IO.
                   15  SKDMT               PICTURE S9(9)V9(2).
               10  SKA-ELGRT-IO.
                   15  SKA-ELGRT           PICTURE S9(9)V9(2).
               10  SKFJT-IO.
                   15  SKFJT               PICTURE S9(9)V9(2).
               10  SODMDR-IO.
                   15  SODMDR              PICTURE S9(9)V9(2).
               10  SOA-ELGRDR-IO.
                   15  SOA-ELGRDR          PICTURE S9(9)V9(2).
               10  SOFJDR-IO.
                   15  SOFJDR              PICTURE S9(9)V9(2).
               10  BODMT-IO.
                   15  BODMT               PICTURE S9(9).
               10  BOA-ELGRT-IO.
                   15  BOA-ELGRT           PICTURE S9(9).
               10  BOFJT-IO.
                   15  BOFJT               PICTURE S9(9).
               10  BKDMT-IO.
                   15  BKDMT               PICTURE S9(9).
               10  BKA-ELGRT-IO.
                   15  BKA-ELGRT           PICTURE S9(9).
               10  BKFJT-IO.
                   15  BKFJT               PICTURE S9(9).
               10  BODMDR-IO.
                   15  BODMDR              PICTURE S9(9).
               10  BOA-ELGRDR-IO.
                   15  BOA-ELGRDR          PICTURE S9(9).
               10  BOFJDR-IO.
                   15  BOFJDR              PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
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
 
           IF  ORDSUMF-PROCESS
               SET ORDSUMF-PROCESS-OFF     TO TRUE
               SET ORDSUMF-READ            TO TRUE
           END-IF
 
           IF  ORDSUMF-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDSUMF-GET
               SET ORDSUMF-READ-OFF        TO TRUE
               IF  NOT ORDSUMF-EOF
                   SET ORDSUMF-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDSUMF-PROCESS
               PERFORM ORDSUMF-IDSET
           END-IF
 
           IF  PFILE-PROCESS
               PERFORM PFILE-IDSET
           END-IF
 
           IF  ORDSUMF-PROCESS
               PERFORM ORDSUMF-CHK-LEVEL
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
 
           IF  ORDSUMF-PROCESS
               PERFORM ORDSUMF-FLDSET
           END-IF
 
           IF  PFILE-PROCESS
               PERFORM PFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDSUMF-PROCESS
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
               SET NOT-I-25                TO TRUE
               IF  FIRMA = '956'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
           END-IF
           SUBTRACT 1 FROM UYEAR       GIVING FJOR
           MOVE FJOR                       TO FJORA
           IF  (I-03)
               SUBTRACT 1 FROM PAR4    GIVING PFJ4
               ADD PFJ4 TO ZERO        GIVING PFJ2
               MOVE PFJ2                   TO PFJ
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE PFJ (2:1)              TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO PFJ (2:1)
               GO TO SLUTT-T
      *  02NMR             GOTO SLUTT
      *****************************************************************
      * NULLSTILLING AV SUMFELTER.                                    *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SUBTRACT AODMT              FROM AODMT
               SUBTRACT AOA-ELGRT          FROM AOA-ELGRT
               SUBTRACT AOFJT              FROM AOFJT
               SUBTRACT AODMDR             FROM AODMDR
               SUBTRACT AOA-ELGRDR         FROM AOA-ELGRDR
               SUBTRACT AOFJDR             FROM AOFJDR
               SUBTRACT SODMT              FROM SODMT
               SUBTRACT SOA-ELGRT          FROM SOA-ELGRT
               SUBTRACT SOFJT              FROM SOFJT
               SUBTRACT SKDMT              FROM SKDMT
               SUBTRACT SKA-ELGRT          FROM SKA-ELGRT
               SUBTRACT SKFJT              FROM SKFJT
               SUBTRACT SODMDR             FROM SODMDR
               SUBTRACT SOA-ELGRDR         FROM SOA-ELGRDR
               SUBTRACT SOFJDR             FROM SOFJDR
      *****************************************************************
      * PERIODISERING  OG INDIKATORSETTING.                           *
      *****************************************************************
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RUT = 'L'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
      *          AAR       COMP PFJ                      10 I FJ.
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  AAR = FJORA
               SET I-10                    TO TRUE
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  AAR = PAR2
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-20                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  MND > PMND
               SET I-20                    TO TRUE
           END-IF
           IF  MND = PMND
               SET I-12                    TO TRUE
           END-IF
      *          AAR       COMP "17"                     10 I FJ.
      *          AAR       COMP "18"                     11 I ÅR.
      *          MND       COMP "03"                 20  12 DM ?
           IF  (I-20)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  ORDNR > '899999'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-U1)
               SET NOT-I-22                TO TRUE
               IF  ORDMOT = '**'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-U1 AND NOT-I-22)
               SET NOT-I-22                TO TRUE
               IF  ORDMOT = 'I8'
                   SET I-22                TO TRUE
               END-IF
      *  U1      WEBORD    COMP "I1"                     22 WEB-ORDRE
      *  U1N22   WEBORD    COMP "I2"                     22 WEB-ORDRE
      *  U1N22   WEBORD    COMP "I3"                     22 WEB-ORDRE
      *  U1N22   WEBORD    COMP "I4"                     22 WEB-ORDRE
      *  U1N22   WEBORD    COMP "I5"                     22 WEB-ORDRE
      *  U1N22   WEBORD    COMP "I6"                     22 WEB-ORDRE
           END-IF
           IF  (I-U1)
               SET NOT-I-22                TO TRUE
               IF  WEBORD = 'I5'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-U1 AND NOT-I-22)
               SET NOT-I-22                TO TRUE
               IF  WEBORD = 'I6'
                   SET I-22                TO TRUE
               END-IF
      *****************************************************************
      * SUMMERING PR. KUNDE.                                          *
      *****************************************************************
           END-IF
           IF  (I-21)
               GO TO SUMBEL-T
           END-IF
           IF  (I-11 AND I-12)
               ADD 1                       TO AODMT
           END-IF
           IF  (I-11)
               ADD 1                       TO AOA-ELGRT
           END-IF
           IF  (I-10)
               ADD 1                       TO AOFJT
           END-IF
           IF  (I-11 AND I-12 AND I-22)
               ADD 1                       TO AODMDR
           END-IF
           IF  (I-11 AND I-22)
               ADD 1                       TO AOA-ELGRDR
           END-IF
           IF  (I-10 AND I-22)
               ADD 1                       TO AOFJDR
           END-IF.
 
       SUMBEL-T.
           IF  (I-11 AND NOT-I-21 AND I-12)
               ADD ORDSUM                  TO SODMT
           END-IF
           IF  (I-11 AND NOT-I-21)
               ADD ORDSUM                  TO SOA-ELGRT
           END-IF
           IF  (I-10 AND NOT-I-21)
               ADD ORDSUM                  TO SOFJT
           END-IF
           IF  (I-11 AND I-21 AND I-12)
               ADD ORDSUM                  TO SKDMT
           END-IF
           IF  (I-11 AND I-21)
               ADD ORDSUM                  TO SKA-ELGRT
           END-IF
           IF  (I-10 AND I-21)
               ADD ORDSUM                  TO SKFJT
           END-IF
           IF  (I-11 AND I-12 AND I-22)
               ADD ORDSUM                  TO SODMDR
           END-IF
           IF  (I-11 AND I-22)
               ADD ORDSUM                  TO SOA-ELGRDR
           END-IF
           IF  (I-10 AND I-22)
               ADD ORDSUM                  TO SOFJDR
           END-IF
           SET I-50                        TO TRUE.
 
       SLUTT-T.
      *****************************************************************
      * TOTALRUTINE FOR OMREGNING TIL HELE KRONER.                    *
      *****************************************************************
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD SODMT TO ZERO       GIVING BODMT ROUNDED
               ADD SOA-ELGRT TO ZERO   GIVING BOA-ELGRT ROUNDED
               ADD SOFJT TO ZERO       GIVING BOFJT ROUNDED
               ADD SKDMT TO ZERO       GIVING BKDMT ROUNDED
               ADD SKA-ELGRT TO ZERO   GIVING BKA-ELGRT ROUNDED
               ADD SKFJT TO ZERO       GIVING BKFJT ROUNDED
               ADD SODMDR TO ZERO      GIVING BODMDR ROUNDED
               ADD SOA-ELGRDR TO ZERO  GIVING BOA-ELGRDR ROUNDED
               ADD SOFJDR TO ZERO      GIVING BOFJDR ROUNDED
           END-IF.
 
       ORDSUMF-GET SECTION.
       ORDSUMF-GET-P.
           IF  ORDSUMF-EOF-OFF
               READ ORDSUMF
               AT END
                   SET ORDSUMF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDSUMF-FLDSET SECTION.
       ORDSUMF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDSUMF-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDSUMF-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDSUMF-IO-AREA (15:2) TO AAR (1:2)
               MOVE ORDSUMF-IO-AREA (13:2) TO MND (1:2)
               MOVE ORDSUMF-IO-AREA (17:2) TO ORDMOT (1:2)
               MOVE ORDSUMF-IO-AREA (19:6) TO KUNDE (1:6)
               MOVE ORDSUMF-IO-AREA (26:1) TO RUT (1:1)
               MOVE ORDSUMF-IO-AREA (27:1) TO KRTYPE (1:1)
               MOVE ORDSUMF-IO-AREA (48:2) TO WEBORD (1:2)
               MOVE ORDSUMF-IO-AREA (55:6) TO ORDSUM-IO
               MOVE ORDSUMF-IO-AREA (73:5) TO ANTVL-IO
           END-EVALUATE.
 
       ORDSUMF-IDSET SECTION.
       ORDSUMF-IDSET-P.
           SET I-02                        TO TRUE.
 
       ORDSUMF-CHK-LEVEL SECTION.
       ORDSUMF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDSUMF-LEVEL-02
               MOVE ORDSUMF-IO-AREA (2:3)  TO ORDSUMF-02-L2-FIRMA
               MOVE ORDSUMF-IO-AREA (19:6) TO ORDSUMF-02-L1-KUNDE
               IF  ORDSUMF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDSUMF-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDSUMF-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDSUMF-02-L2         TO THE-PRIOR-L2
               MOVE  ORDSUMF-02-L1         TO THE-PRIOR-L1
               SET ORDSUMF-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
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
               MOVE PFILE-IO-AREA (3:4)    TO PAR4-IO
               INSPECT PAR4-IO REPLACING ALL ' ' BY '0'
               MOVE PFILE-IO-AREA (5:2)    TO PAR2 (1:2)
               MOVE PFILE-IO-AREA (7:2)    TO PMND (1:2)
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
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
      *TDATA  D        L1 50
               MOVE SPACES TO UTDATA-IO-AREA
               INITIALIZE UTDATA-IO-AREA
               MOVE 'C'                    TO UTDATA-IO-AREA (1:1)
               MOVE FIRMA                  TO UTDATA-IO-AREA (2:3)
               MOVE KUNDE                  TO UTDATA-IO-AREA (5:6)
               MOVE AODMT                  TO XO-50P
               MOVE XO-50P-EF              TO UTDATA-IO-AREA (11:3)
               MOVE AOA-ELGRT              TO XO-50P
               MOVE XO-50P-EF              TO UTDATA-IO-AREA (14:3)
               MOVE AOFJT                  TO XO-50P
               MOVE XO-50P-EF              TO UTDATA-IO-AREA (17:3)
               MOVE AODMDR                 TO XO-50P
               MOVE XO-50P-EF              TO UTDATA-IO-AREA (20:3)
               MOVE AOA-ELGRDR             TO XO-50P
               MOVE XO-50P-EF              TO UTDATA-IO-AREA (23:3)
               MOVE AOFJDR                 TO XO-50P
               MOVE XO-50P-EF              TO UTDATA-IO-AREA (26:3)
               MOVE BODMT                  TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (29:5)
               MOVE BOA-ELGRT              TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (34:5)
               MOVE BOFJT                  TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (39:5)
               MOVE BODMDR                 TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (44:5)
               MOVE BOA-ELGRDR             TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (49:5)
               MOVE BOFJDR                 TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (54:5)
               MOVE BKDMT                  TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (59:5)
               MOVE BKA-ELGRT              TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (64:5)
               MOVE BKFJT                  TO XO-90P
               MOVE XO-90P-EF              TO UTDATA-IO-AREA (69:5)
               WRITE UTDATA-IO-AREA
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
           SET ORDSUMF-LEVEL-INIT          TO TRUE
           INITIALIZE ORDSUMF-DATA-FIELDS
           SET ORDSUMF-EOF-OFF             TO TRUE
           SET ORDSUMF-PROCESS             TO TRUE
           OPEN INPUT ORDSUMF
           INITIALIZE PFILE-DATA-FIELDS
           SET PFILE-EOF-OFF               TO TRUE
           SET PFILE-PROCESS               TO TRUE
           OPEN INPUT PFILE
           OPEN OUTPUT UTDATA.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDSUMF
           CLOSE PFILE
           CLOSE UTDATA.
 
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
