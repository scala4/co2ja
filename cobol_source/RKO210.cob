       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO210R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV RSK.RSK210                 ***TXT***OK MT
      *  PROGRAM....: RKO210 - FØR SEPT.05-RSK210                     *
      *                                                                        *
      *   L E V E R A N D Ø R R E S K O N T R O                                *
      *   MERKING AV REMITTERTE POSTER OG 4 GRAD PURR PÅ RESKONTROFILE         *
      *   PROGRAMMET KJØRES OM KVELDEN SAMMEN MED DAGLIGE KJØRINGER            *
      *E 19.03.09 KOMMENTAR: LEVERANDØRRESKONTRO OPPDATERING HAR UTGÅTT.
      *           TAR MED OPPDATERINGER FRA VALGFRI PURRERUTINE, RK12,
      *           OG OPPDATERER SISTE BRUKTE PURREGRAD PÅ RESKOMA I            *
      *           I TILLEGG TIL Å OPPDATERE INKASSOBREV-MERKET, 4.GRADS        *
      *           MERKE I PERIODISK BATCH-PURRING                              *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO210.rpg
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
           SELECT UPFILE
               ASSIGN TO UT-S-UPFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UPFILE-STATUS.
           SELECT RESKOMA
               ASSIGN TO RESKOMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKOMA-STATUS
               RECORD KEY IS RESKOMA-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD UPFILE
               BLOCK CONTAINS 1540
               RECORD CONTAINS 20.
       01  UPFILE-IO-AREA.
           05  UPFILE-IO-AREA-X            PICTURE X(20).
      *BUGFILO O   F 132 132            PRINTERSYSLST
       FD RESKOMA
               RECORD CONTAINS 200.
       01  RESKOMA-IO-AREA.
           05  RESKOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  RESKOMA-KEY1.
                   15  RESKOMA-KEY1N       PICTURE S9(14).
               10  FILLER                  PICTURE X(185).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  UPFILE-STATUS               PICTURE 99 VALUE 0.
           10  RESKOMA-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  UPFILE-EOF-OFF          VALUE '0'.
               88  UPFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UPFILE-READ-OFF         VALUE '0'.
               88  UPFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UPFILE-PROCESS-OFF      VALUE '0'.
               88  UPFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  UPFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  UPFILE-LEVEL-INIT       VALUE '1'.
           05  RESKOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-EOF-OFF         VALUE '0'.
               88  RESKOMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-READ-OFF        VALUE '0'.
               88  RESKOMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-PROCESS-OFF     VALUE '0'.
               88  RESKOMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKOMA-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKOMA-LEVEL-INIT      VALUE '1'.
           05  UPFILE-LEVEL-01.
               10  UPFILE-01-L2.
                   15  UPFILE-01-L2-KNR    PICTURE X(9).
               10  UPFILE-01-L1.
                   15  UPFILE-01-L1-REFNR  PICTURE X(6).
           05  UPFILE-LEVEL-03.
               10  UPFILE-03-L2.
                   15  UPFILE-03-L2-KNR    PICTURE X(9).
               10  UPFILE-03-L1.
                   15  UPFILE-03-L1-REFNR  PICTURE X(6).
           05  UPFILE-DATA-FIELDS.
               10  KNR                     PICTURE X(9).
               10  REFNR                   PICTURE X(6).
               10  REC020                  PICTURE X(20).
               10  PUGRAD                  PICTURE X(1).
               10  FRARUT                  PICTURE X(1).
           05  UPFILE-MP                   PICTURE X(15).
           05  UPFILE-MC                   PICTURE X(15).
           05  UPFILE-M-01             REDEFINES UPFILE-MC.
               10  UPFILE-M-01-M2.
                   15  UPFILE-M-01-M2-KNR-G.
                       20  UPFILE-M-01-M2-KNR PICTURE X(9).
               10  UPFILE-M-01-M1.
                   15  UPFILE-M-01-M1-REFNR-G.
                       20  UPFILE-M-01-M1-REFNR PICTURE X(6).
           05  UPFILE-M-03             REDEFINES UPFILE-MC.
               10  UPFILE-M-03-M2.
                   15  UPFILE-M-03-M2-KNR-G.
                       20  UPFILE-M-03-M2-KNR PICTURE X(9).
               10  UPFILE-M-03-M1.
                   15  UPFILE-M-03-M1-REFNR-G.
                       20  UPFILE-M-03-M1-REFNR PICTURE X(6).
           05  RESKOMA-LEVEL-02.
               10  RESKOMA-02-L2.
                   15  RESKOMA-02-L2-KNR   PICTURE X(9).
               10  RESKOMA-02-L1.
                   15  RESKOMA-02-L1-REFNR PICTURE X(6).
           05  RESKOMA-DATA-FIELDS.
               10  REC120                  PICTURE X(120).
           05  RESKOMA-MP                  PICTURE X(15).
           05  RESKOMA-MC                  PICTURE X(15).
           05  RESKOMA-M-02            REDEFINES RESKOMA-MC.
               10  RESKOMA-M-02-M2.
                   15  RESKOMA-M-02-M2-KNR-G.
                       20  RESKOMA-M-02-M2-KNR PICTURE X(9).
               10  RESKOMA-M-02-M1.
                   15  RESKOMA-M-02-M1-REFNR-G.
                       20  RESKOMA-M-02-M1-REFNR PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(9).
               10  THE-PRIOR-L1            PICTURE X(6).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  UPFILE-PROCESS
               SET UPFILE-PROCESS-OFF      TO TRUE
               SET UPFILE-READ             TO TRUE
           END-IF
 
           IF  UPFILE-READ
               PERFORM UPFILE-GET
               SET UPFILE-READ-OFF         TO TRUE
               IF  NOT UPFILE-EOF
                   PERFORM UPFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM UPFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  RESKOMA-PROCESS
               SET RESKOMA-PROCESS-OFF     TO TRUE
               SET RESKOMA-READ            TO TRUE
           END-IF
 
           IF  RESKOMA-READ
               PERFORM RESKOMA-GET
               SET RESKOMA-READ-OFF        TO TRUE
               IF  NOT RESKOMA-EOF
                   PERFORM RESKOMA-MATCH-SET
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
 
           IF  UPFILE-PROCESS
               PERFORM UPFILE-IDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-IDSET
           END-IF
 
           IF  UPFILE-PROCESS
               PERFORM UPFILE-CHK-LEVEL
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  UPFILE-PROCESS
               PERFORM UPFILE-FLDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  UPFILE-PROCESS
           OR  RESKOMA-PROCESS
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
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
           END-IF
           IF  (I-01)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-03)
               SET I-11                    TO TRUE
               SET NOT-I-12                TO TRUE
               IF  FRARUT = 'V'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-12)
               SET NOT-I-13                TO TRUE
               IF  PUGRAD > '2'
                   SET I-13                TO TRUE
               END-IF
      *  03 MR             MOVE "REC020  "BUGFL2  8        LEDETXT DEBUG
      *  03 MR   BUGFL2    DEBUGBUGFILO   REC020           VIS FELT/IND
      *  02 MR             MOVE "REC120  "BUGFL2  8        LEDETXT DEBUG
      *  02 MR   BUGFL2    DEBUGBUGFILO   REC120           VIS FELT/IND
           END-IF
           .
 
       UPFILE-GET SECTION.
       UPFILE-GET-P.
           IF  UPFILE-EOF-OFF
               READ UPFILE
               AT END
                   SET UPFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       UPFILE-FLDSET SECTION.
       UPFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( UPFILE-IO-AREA (1:1) = 'U' )
               MOVE UPFILE-IO-AREA (2:9)   TO KNR (1:9)
               MOVE UPFILE-IO-AREA (11:6)  TO REFNR (1:6)
           WHEN ( UPFILE-IO-AREA (1:1) = 'V' )
               MOVE UPFILE-IO-AREA (1:20)  TO REC020 (1:20)
               MOVE UPFILE-IO-AREA (2:9)   TO KNR (1:9)
               MOVE UPFILE-IO-AREA (11:6)  TO REFNR (1:6)
               MOVE UPFILE-IO-AREA (17:1)  TO PUGRAD (1:1)
               MOVE UPFILE-IO-AREA (18:1)  TO FRARUT (1:1)
           END-EVALUATE.
 
       UPFILE-IDCHK SECTION.
       UPFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( UPFILE-IO-AREA (1:1) = 'U' )
             OR ( UPFILE-IO-AREA (1:1) = 'V' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       UPFILE-IDSET SECTION.
       UPFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( UPFILE-IO-AREA (1:1) = 'U' )
               SET I-01                    TO TRUE
           WHEN ( UPFILE-IO-AREA (1:1) = 'V' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       UPFILE-CHK-LEVEL SECTION.
       UPFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( UPFILE-IO-AREA (1:1) = 'U' )
               MOVE LOW-VALUES             TO UPFILE-LEVEL-01
               MOVE UPFILE-IO-AREA (2:9)   TO UPFILE-01-L2-KNR
               MOVE UPFILE-IO-AREA (11:6)  TO UPFILE-01-L1-REFNR
               IF  UPFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  UPFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  UPFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  UPFILE-01-L2          TO THE-PRIOR-L2
               MOVE  UPFILE-01-L1          TO THE-PRIOR-L1
               SET UPFILE-LEVEL-INIT       TO TRUE
           WHEN ( UPFILE-IO-AREA (1:1) = 'V' )
               MOVE LOW-VALUES             TO UPFILE-LEVEL-03
               MOVE UPFILE-IO-AREA (2:9)   TO UPFILE-03-L2-KNR
               MOVE UPFILE-IO-AREA (11:6)  TO UPFILE-03-L1-REFNR
               IF  UPFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  UPFILE-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  UPFILE-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  UPFILE-03-L2          TO THE-PRIOR-L2
               MOVE  UPFILE-03-L1          TO THE-PRIOR-L1
               SET UPFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       UPFILE-MATCH-SET SECTION.
       UPFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( UPFILE-IO-AREA (1:1) = 'U' )
               MOVE UPFILE-IO-AREA (2:9)   TO UPFILE-M-01-M2-KNR
               MOVE UPFILE-IO-AREA (11:6)  TO UPFILE-M-01-M1-REFNR
           WHEN ( UPFILE-IO-AREA (1:1) = 'V' )
               MOVE UPFILE-IO-AREA (2:9)   TO UPFILE-M-03-M2-KNR
               MOVE UPFILE-IO-AREA (11:6)  TO UPFILE-M-03-M1-REFNR
           END-EVALUATE.
 
       RESKOMA-GET SECTION.
       RESKOMA-GET-P.
           IF  RESKOMA-EOF-OFF
               READ RESKOMA
               AT END
                   SET RESKOMA-EOF         TO TRUE
               END-READ
           END-IF.
 
       RESKOMA-FLDSET SECTION.
       RESKOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (1:120) TO REC120 (1:120)
               MOVE RESKOMA-IO-AREA (3:9)  TO KNR (1:9)
               MOVE RESKOMA-IO-AREA (36:6) TO REFNR (1:6)
           END-EVALUATE.
 
       RESKOMA-IDSET SECTION.
       RESKOMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       RESKOMA-CHK-LEVEL SECTION.
       RESKOMA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKOMA-LEVEL-02
               MOVE RESKOMA-IO-AREA (3:9)  TO RESKOMA-02-L2-KNR
               MOVE RESKOMA-IO-AREA (36:6) TO RESKOMA-02-L1-REFNR
               IF  RESKOMA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKOMA-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKOMA-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKOMA-02-L2         TO THE-PRIOR-L2
               MOVE  RESKOMA-02-L1         TO THE-PRIOR-L1
               SET RESKOMA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESKOMA-MATCH-SET SECTION.
       RESKOMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (3:9)  TO RESKOMA-M-02-M2-KNR
               MOVE RESKOMA-IO-AREA (36:6) TO RESKOMA-M-02-M1-REFNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  UPFILE-EOF
               MOVE HIGH-VALUES            TO UPFILE-MC
                                              UPFILE-MP
           END-IF
           IF  RESKOMA-EOF
               MOVE HIGH-VALUES            TO RESKOMA-MC
                                              RESKOMA-MP
           END-IF
           IF  UPFILE-MC < UPFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESKOMA-MC < RESKOMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  UPFILE-MC < RESKOMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET UPFILE-PROCESS      TO TRUE
                   MOVE UPFILE-MC          TO UPFILE-MP
                   IF  UPFILE-MC = RESKOMA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESKOMA-MC < UPFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKOMA-PROCESS     TO TRUE
                   MOVE RESKOMA-MC         TO RESKOMA-MP
                   IF  RESKOMA-MC = UPFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  UPFILE-MC = RESKOMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET UPFILE-PROCESS      TO TRUE
                   MOVE UPFILE-MC          TO UPFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR)
               IF  (NOT-I-12 AND I-11)
                   MOVE '1'                TO RESKOMA-IO-AREA (59:1)
               END-IF
               IF  (I-11 AND I-12 AND I-13)
                   MOVE '1'                TO RESKOMA-IO-AREA (59:1)
               END-IF
               IF  (I-12 AND I-11)
                   MOVE PUGRAD             TO RESKOMA-IO-AREA (58:1)
               END-IF
               IF  (I-10)
                   MOVE '1'                TO RESKOMA-IO-AREA (77:1)
               END-IF
               REWRITE RESKOMA-IO-AREA
           END-IF
           IF  (I-01 AND I-U1 AND I-U2)
               MOVE REC020                 TO RESKOMA-IO-AREA (101:20)
               MOVE REC120                 TO RESKOMA-IO-AREA (1:120)
               REWRITE RESKOMA-IO-AREA
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
           SET UPFILE-LEVEL-INIT           TO TRUE
           INITIALIZE UPFILE-DATA-FIELDS
           SET UPFILE-EOF-OFF              TO TRUE
           SET UPFILE-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO UPFILE-MC
                                              UPFILE-MP
           OPEN INPUT UPFILE
           SET RESKOMA-LEVEL-INIT          TO TRUE
           INITIALIZE RESKOMA-DATA-FIELDS
           SET RESKOMA-EOF-OFF             TO TRUE
           SET RESKOMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESKOMA-MC
                                              RESKOMA-MP
           OPEN I-O RESKOMA.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE UPFILE
           CLOSE RESKOMA.
 
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
