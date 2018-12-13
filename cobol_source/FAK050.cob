       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK050R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM: FAK050  MERGE FAKTUAREC OG FAKTURA SAMLERECORDS.     *
      *    7/8-95 SAMT FAKT.BONUS.KR.NOTA.                            *
      *   19/7-01 EGEN RUTINE FOR SAMLEFAKT. PR. MND.                 *
      *   05/2-08 SPESIALRUTINE FOR Å TA MED MÅNEDLIGE SAMLERECORDS   *
      *           SOM IKKE BLE TATT MED PÅ SISTE FAKT. FOREG. MÅNED.  *
      *           DISSE REC. ER KORRIGERT MED KODE X I POS 12.        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK050.rpg
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
           SELECT FAKREC
               ASSIGN TO UT-S-FAKREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKREC-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT SAMREC
               ASSIGN TO UT-S-SAMREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SAMREC-STATUS.
           SELECT BONFAK
               ASSIGN TO UT-S-BONFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BONFAK-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT FAKRUT
               ASSIGN TO UT-S-FAKRUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKRUT-STATUS.
           SELECT SAMRUT
               ASSIGN TO UT-S-SAMRUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SAMRUT-STATUS.
           SELECT TOTSUM
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTSUM-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKREC
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKREC-IO-AREA.
           05  FAKREC-IO-AREA-X            PICTURE X(200).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD SAMREC
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  SAMREC-IO-AREA.
           05  SAMREC-IO-AREA-X            PICTURE X(200).
       FD BONFAK
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  BONFAK-IO-AREA.
           05  BONFAK-IO-AREA-X            PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD FAKRUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKRUT-IO-AREA.
           05  FAKRUT-IO-AREA-X            PICTURE X(200).
       FD SAMRUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  SAMRUT-IO-AREA.
           05  SAMRUT-IO-AREA-X            PICTURE X(200).
       FD TOTSUM
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  TOTSUM-IO-PRINT.
           05  TOTSUM-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 TOTSUM-IO-AREA.
           05  TOTSUM-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKREC-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  SAMREC-STATUS               PICTURE 99 VALUE 0.
           10  BONFAK-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  FAKRUT-STATUS               PICTURE 99 VALUE 0.
           10  SAMRUT-STATUS               PICTURE 99 VALUE 0.
           10  TOTSUM-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-EOF-OFF          VALUE '0'.
               88  FAKREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-READ-OFF         VALUE '0'.
               88  FAKREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-PROCESS-OFF      VALUE '0'.
               88  FAKREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKREC-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKREC-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SAMREC-EOF-OFF          VALUE '0'.
               88  SAMREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SAMREC-READ-OFF         VALUE '0'.
               88  SAMREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SAMREC-PROCESS-OFF      VALUE '0'.
               88  SAMREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SAMREC-LEVEL-INIT-OFF   VALUE '0'.
               88  SAMREC-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BONFAK-EOF-OFF          VALUE '0'.
               88  BONFAK-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BONFAK-READ-OFF         VALUE '0'.
               88  BONFAK-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BONFAK-PROCESS-OFF      VALUE '0'.
               88  BONFAK-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BONFAK-LEVEL-INIT-OFF   VALUE '0'.
               88  BONFAK-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  TOTSUM-DATA-FIELDS.
               10  TOTSUM-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTSUM-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTSUM-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTSUM-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTSUM-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTSUM-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTSUM-CLR-IO           PICTURE X VALUE 'Y'.
           05  FAKREC-LEVEL-01.
               10  FAKREC-01-L1.
                   15  FAKREC-01-L1-FNR    PICTURE X(3).
           05  FAKREC-DATA-FIELDS.
               10  REC1                    PICTURE X(200).
               10  FNR                     PICTURE X(3).
               10  FNRKNR                  PICTURE X(9).
               10  ORDNR                   PICTURE X(6).
           05  FAKREC-MP                   PICTURE X(15).
           05  FAKREC-MC                   PICTURE X(15).
           05  FAKREC-M-01             REDEFINES FAKREC-MC.
               10  FAKREC-M-01-M2.
                   15  FAKREC-M-01-M2-FNRKNR-G.
                       20  FAKREC-M-01-M2-FNRKNR PICTURE X(9).
               10  FAKREC-M-01-M1.
                   15  FAKREC-M-01-M1-ORDNR-G.
                       20  FAKREC-M-01-M1-ORDNR PICTURE X(6).
           05  FAKPAR-DATA-FIELDS.
               10  SAMFAK                  PICTURE X(1).
               10  SFIMND                  PICTURE X(1).
               10  PFGRP                   PICTURE X(1).
               10  KUNFNR                  PICTURE X(3).
           05  SAMREC-LEVEL-02.
               10  SAMREC-02-L1.
                   15  SAMREC-02-L1-FNR    PICTURE X(3).
           05  SAMREC-DATA-FIELDS.
               10  REC2                    PICTURE X(200).
               10  FAKMT                   PICTURE X(1).
           05  SAMREC-MP                   PICTURE X(15).
           05  SAMREC-MC                   PICTURE X(15).
           05  SAMREC-M-02             REDEFINES SAMREC-MC.
               10  SAMREC-M-02-M2.
                   15  SAMREC-M-02-M2-FNRKNR-G.
                       20  SAMREC-M-02-M2-FNRKNR PICTURE X(9).
               10  SAMREC-M-02-M1.
                   15  SAMREC-M-02-M1-ORDNR-G.
                       20  SAMREC-M-02-M1-ORDNR PICTURE X(6).
           05  BONFAK-LEVEL-04.
               10  BONFAK-04-L1.
                   15  BONFAK-04-L1-FNR    PICTURE X(3).
           05  BONFAK-DATA-FIELDS.
               10  REC3                    PICTURE X(200).
           05  BONFAK-MP                   PICTURE X(15).
           05  BONFAK-MC                   PICTURE X(15).
           05  BONFAK-M-04             REDEFINES BONFAK-MC.
               10  BONFAK-M-04-M2.
                   15  BONFAK-M-04-M2-FNRKNR-G.
                       20  BONFAK-M-04-M2-FNRKNR PICTURE X(9).
               10  BONFAK-M-04-M1.
                   15  BONFAK-M-04-M1-ORDNR-G.
                       20  BONFAK-M-04-M1-ORDNR PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  FAKGRP                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(6).
               10  ANTBON-IO.
                   15  ANTBON              PICTURE S9(6).
               10  ANTSAM-IO.
                   15  ANTSAM              PICTURE S9(6).
               10  ANTNYS-IO.
                   15  ANTNYS              PICTURE S9(6).
               10  FRASAM-IO.
                   15  FRASAM              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-09                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKREC-PROCESS
               SET FAKREC-PROCESS-OFF      TO TRUE
               SET FAKREC-READ             TO TRUE
           END-IF
 
           IF  FAKREC-READ
               PERFORM FAKREC-GET
               SET FAKREC-READ-OFF         TO TRUE
               IF  NOT FAKREC-EOF
                   PERFORM FAKREC-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKPAR-PROCESS
               SET FAKPAR-PROCESS-OFF      TO TRUE
               SET FAKPAR-READ             TO TRUE
           END-IF
 
           IF  FAKPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKPAR-GET
               SET FAKPAR-READ-OFF         TO TRUE
               IF  NOT FAKPAR-EOF
                   SET FAKPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  SAMREC-PROCESS
               SET SAMREC-PROCESS-OFF      TO TRUE
               SET SAMREC-READ             TO TRUE
           END-IF
 
           IF  SAMREC-READ
               PERFORM SAMREC-GET
               SET SAMREC-READ-OFF         TO TRUE
               IF  NOT SAMREC-EOF
                   PERFORM SAMREC-MATCH-SET
               END-IF
           END-IF
 
           IF  BONFAK-PROCESS
               SET BONFAK-PROCESS-OFF      TO TRUE
               SET BONFAK-READ             TO TRUE
           END-IF
 
           IF  BONFAK-READ
               PERFORM BONFAK-GET
               SET BONFAK-READ-OFF         TO TRUE
               IF  NOT BONFAK-EOF
                   PERFORM BONFAK-MATCH-SET
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
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-IDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
           END-IF
 
           IF  SAMREC-PROCESS
               PERFORM SAMREC-IDSET
           END-IF
 
           IF  BONFAK-PROCESS
               PERFORM BONFAK-IDSET
           END-IF
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-CHK-LEVEL
           END-IF
 
           IF  SAMREC-PROCESS
               PERFORM SAMREC-CHK-LEVEL
           END-IF
 
           IF  BONFAK-PROCESS
               PERFORM BONFAK-CHK-LEVEL
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
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-FLDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
           END-IF
 
           IF  SAMREC-PROCESS
               PERFORM SAMREC-FLDSET
           END-IF
 
           IF  BONFAK-PROCESS
               PERFORM BONFAK-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKREC-PROCESS
           OR  SAMREC-PROCESS
           OR  BONFAK-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-14                    TO TRUE
           SET NOT-I-91                    TO TRUE
           SET NOT-I-92                    TO TRUE
           IF  (I-03)
               SET NOT-I-90                TO TRUE
               IF  KUNFNR > '000'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               SET NOT-I-91                TO TRUE
               IF  FAKMT = 'M'
                   SET I-91                TO TRUE
               END-IF
      *****************************************************************
      * TEST OM DET ER GLEMTE SAMLERECORD SOM SKAL VÆRE MED.          *
      *****************************************************************
           END-IF
           IF  (I-02)
               SET NOT-I-92                TO TRUE
               IF  FAKMT = 'X'
                   SET I-92                TO TRUE
               END-IF
      *****************************************************************
      *     SKAL SAMLEFAKTURA VÆRE MED.
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-11                TO TRUE
               IF  SAMFAK = 'J'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  SAMFAK = 'N'
                   SET I-12                TO TRUE
               END-IF
      *****************************************************************
      *     SKAL SAMLEFAKTURA PR. MÅNED VÆRE MED.
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-15                TO TRUE
               IF  SFIMND = 'N'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-15 AND I-91)
               SET I-14                    TO TRUE
      *****************************************************************
      *     AVIKER DETTE FIRMAET VEDR. SAMLEFAKTURA DENNE GANG.
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-25)
               PERFORM FGRUT-S
           END-IF
           IF  (I-L1 AND I-89)
               SET I-12                    TO TRUE
               SET NOT-I-11                TO TRUE
      *****************************************************************
      * SUMMERINGSRUTINE                                              *
      *****************************************************************
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTFAK
           END-IF
           IF  (I-02 AND I-11 AND NOT-I-14)
               AND (NOT-I-92)
               ADD 1                       TO ANTFAK
           END-IF
           IF  (I-02 AND I-92)
               ADD 1                       TO ANTFAK
           END-IF
           IF  (I-04)
               ADD 1                       TO ANTFAK
               ADD 1                       TO ANTBON
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTSAM
           END-IF
           IF  (I-02 AND I-12 AND NOT-I-92)
               OR  (I-02 AND I-14)
               ADD 1                       TO ANTNYS
           END-IF
           IF  (I-02 AND I-11 AND NOT-I-14)
               AND (NOT-I-92)
               ADD 1                       TO FRASAM
           END-IF
           IF  (I-02 AND I-92)
               ADD 1                       TO FRASAM
      ****************************************************************
      *   SUBRUTINE FOR OM DETTE FIRMAET SKAL FAKTURES DENNE GANG.   *
      ****************************************************************
           END-IF
           .
 
       FGRUT-S SECTION.
       FGRUT-S-P.
           SET NOT-I-89                    TO TRUE
           IF  (I-90)
               SET NOT-I-89                TO TRUE
               IF  FNR NOT = KUNFNR
                   SET I-89                TO TRUE
               END-IF
               GO TO FGEND-T
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  PFGRP = '0'
               SET I-88                    TO TRUE
           END-IF
           IF  (I-88)
               GO TO FGEND-T
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  PFGRP = FAKGRP
               SET I-88                    TO TRUE
           END-IF
           IF  (I-88)
               GO TO FGEND-T
           END-IF
           SET I-89                        TO TRUE.
 
       FGEND-T.
           CONTINUE.
      ***************************************************************
 
       FAKREC-GET SECTION.
       FAKREC-GET-P.
           IF  FAKREC-EOF-OFF
               READ FAKREC
               AT END
                   SET FAKREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKREC-FLDSET SECTION.
       FAKREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKREC-IO-AREA (1:200) TO REC1 (1:200)
               MOVE FAKREC-IO-AREA (1:3)   TO FNR (1:3)
               MOVE FAKREC-IO-AREA (1:9)   TO FNRKNR (1:9)
               MOVE FAKREC-IO-AREA (19:6)  TO ORDNR (1:6)
           END-EVALUATE.
 
       FAKREC-IDSET SECTION.
       FAKREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKREC-CHK-LEVEL SECTION.
       FAKREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKREC-LEVEL-01
               MOVE FAKREC-IO-AREA (1:3)   TO FAKREC-01-L1-FNR
               IF  FAKREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKREC-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKREC-01-L1          TO THE-PRIOR-L1
               SET FAKREC-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FAKREC-MATCH-SET SECTION.
       FAKREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKREC-IO-AREA (1:9)   TO FAKREC-M-01-M2-FNRKNR
               MOVE FAKREC-IO-AREA (19:6)  TO FAKREC-M-01-M1-ORDNR
           END-EVALUATE.
 
       FAKPAR-GET SECTION.
       FAKPAR-GET-P.
           IF  FAKPAR-EOF-OFF
               READ FAKPAR
               AT END
                   SET FAKPAR-EOF          TO TRUE
               END-READ
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (67:1)  TO SAMFAK (1:1)
               MOVE FAKPAR-IO-AREA (68:1)  TO SFIMND (1:1)
               MOVE FAKPAR-IO-AREA (91:1)  TO PFGRP (1:1)
               MOVE FAKPAR-IO-AREA (92:3)  TO KUNFNR (1:3)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       SAMREC-GET SECTION.
       SAMREC-GET-P.
           IF  SAMREC-EOF-OFF
               READ SAMREC
               AT END
                   SET SAMREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SAMREC-FLDSET SECTION.
       SAMREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SAMREC-IO-AREA (1:200) TO REC2 (1:200)
               MOVE SAMREC-IO-AREA (1:3)   TO FNR (1:3)
               MOVE SAMREC-IO-AREA (1:9)   TO FNRKNR (1:9)
               MOVE SAMREC-IO-AREA (12:1)  TO FAKMT (1:1)
               MOVE SAMREC-IO-AREA (19:6)  TO ORDNR (1:6)
           END-EVALUATE.
 
       SAMREC-IDSET SECTION.
       SAMREC-IDSET-P.
           SET I-02                        TO TRUE.
 
       SAMREC-CHK-LEVEL SECTION.
       SAMREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SAMREC-LEVEL-02
               MOVE SAMREC-IO-AREA (1:3)   TO SAMREC-02-L1-FNR
               IF  SAMREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SAMREC-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SAMREC-02-L1          TO THE-PRIOR-L1
               SET SAMREC-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       SAMREC-MATCH-SET SECTION.
       SAMREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SAMREC-IO-AREA (1:9)   TO SAMREC-M-02-M2-FNRKNR
               MOVE SAMREC-IO-AREA (19:6)  TO SAMREC-M-02-M1-ORDNR
           END-EVALUATE.
 
       BONFAK-GET SECTION.
       BONFAK-GET-P.
           IF  BONFAK-EOF-OFF
               READ BONFAK
               AT END
                   SET BONFAK-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BONFAK-FLDSET SECTION.
       BONFAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BONFAK-IO-AREA (1:200) TO REC3 (1:200)
               MOVE BONFAK-IO-AREA (1:3)   TO FNR (1:3)
               MOVE BONFAK-IO-AREA (1:9)   TO FNRKNR (1:9)
               MOVE BONFAK-IO-AREA (19:6)  TO ORDNR (1:6)
           END-EVALUATE.
 
       BONFAK-IDSET SECTION.
       BONFAK-IDSET-P.
           SET I-04                        TO TRUE.
 
       BONFAK-CHK-LEVEL SECTION.
       BONFAK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BONFAK-LEVEL-04
               MOVE BONFAK-IO-AREA (1:3)   TO BONFAK-04-L1-FNR
               IF  BONFAK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BONFAK-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BONFAK-04-L1          TO THE-PRIOR-L1
               SET BONFAK-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       BONFAK-MATCH-SET SECTION.
       BONFAK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE BONFAK-IO-AREA (1:9)   TO BONFAK-M-04-M2-FNRKNR
               MOVE BONFAK-IO-AREA (19:6)  TO BONFAK-M-04-M1-ORDNR
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (503:1) TO FAKGRP (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-09                        TO TRUE.
 
       TOTSUM-PRINT-LINE SECTION.
       TOTSUM-PRINT-LINE-P.
           IF  TOTSUM-BEFORE-SKIP > 0
               PERFORM TOTSUM-SKIP-BEFORE
           END-IF
           IF  TOTSUM-BEFORE-SPACE > 0
               PERFORM TOTSUM-SPACE-BEFORE
               IF  TOTSUM-AFTER-SKIP > 0
                   PERFORM TOTSUM-SKIP-AFTER
               END-IF
               IF  TOTSUM-AFTER-SPACE > 0
                   PERFORM TOTSUM-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTSUM-AFTER-SKIP > 0
                   PERFORM TOTSUM-SKIP-AFTER
               END-IF
               PERFORM TOTSUM-SPACE-AFTER
           END-IF
           IF  TOTSUM-LINE-COUNT NOT < TOTSUM-MAX-LINES
               MOVE 7                      TO TOTSUM-AFTER-SKIP
           END-IF.
 
       TOTSUM-SKIP-BEFORE SECTION.
       TOTSUM-SKIP-BEFORE-P.
           WRITE TOTSUM-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO TOTSUM-LINE-COUNT
           MOVE 0                          TO TOTSUM-BEFORE-SKIP
           INITIALIZE TOTSUM-IO-AREA.
 
       TOTSUM-SPACE-BEFORE SECTION.
       TOTSUM-SPACE-BEFORE-P.
           WRITE TOTSUM-IO-PRINT        AFTER TOTSUM-BEFORE-SPACE LINES
           ADD TOTSUM-BEFORE-SPACE         TO TOTSUM-LINE-COUNT
           MOVE SPACES TO TOTSUM-IO-AREA
           INITIALIZE TOTSUM-IO-AREA
           MOVE 0                          TO TOTSUM-BEFORE-SPACE.
 
       TOTSUM-SKIP-AFTER SECTION.
       TOTSUM-SKIP-AFTER-P.
           WRITE TOTSUM-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTSUM-LINE-COUNT
           MOVE 0                          TO TOTSUM-AFTER-SKIP
           INITIALIZE TOTSUM-IO-AREA.
 
       TOTSUM-SPACE-AFTER SECTION.
       TOTSUM-SPACE-AFTER-P.
           WRITE TOTSUM-IO-PRINT       BEFORE TOTSUM-AFTER-SPACE LINES
           ADD TOTSUM-AFTER-SPACE          TO TOTSUM-LINE-COUNT
           INITIALIZE TOTSUM-IO-AREA
           MOVE 0                          TO TOTSUM-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FAKREC-EOF
               MOVE HIGH-VALUES            TO FAKREC-MC
                                              FAKREC-MP
           END-IF
           IF  SAMREC-EOF
               MOVE HIGH-VALUES            TO SAMREC-MC
                                              SAMREC-MP
           END-IF
           IF  BONFAK-EOF
               MOVE HIGH-VALUES            TO BONFAK-MC
                                              BONFAK-MP
           END-IF
           IF  FAKREC-MC < FAKREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  SAMREC-MC < SAMREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  BONFAK-MC < BONFAK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FAKREC-MC < SAMREC-MC
            AND  FAKREC-MC < BONFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKREC-PROCESS      TO TRUE
                   MOVE FAKREC-MC          TO FAKREC-MP
                   IF  FAKREC-MC = SAMREC-MP
                     OR  FAKREC-MC = BONFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SAMREC-MC < FAKREC-MC
            AND  SAMREC-MC < BONFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SAMREC-PROCESS      TO TRUE
                   MOVE SAMREC-MC          TO SAMREC-MP
                   IF  SAMREC-MC = FAKREC-MP
                     OR  SAMREC-MC = BONFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  BONFAK-MC < FAKREC-MC
            AND  BONFAK-MC < SAMREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BONFAK-PROCESS      TO TRUE
                   MOVE BONFAK-MC          TO BONFAK-MP
                   IF  BONFAK-MC = FAKREC-MP
                     OR  BONFAK-MC = SAMREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKREC-MC = SAMREC-MC
             OR  FAKREC-MC = BONFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKREC-PROCESS      TO TRUE
                   MOVE FAKREC-MC          TO FAKREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  SAMREC-MC = BONFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SAMREC-PROCESS      TO TRUE
                   MOVE SAMREC-MC          TO SAMREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO FAKRUT-IO-AREA
               INITIALIZE FAKRUT-IO-AREA
               MOVE REC1                   TO FAKRUT-IO-AREA (1:200)
               WRITE FAKRUT-IO-AREA
           END-IF
           IF  (I-02 AND I-11 AND NOT-I-14)
           OR  (I-02 AND I-92)
               MOVE SPACES TO FAKRUT-IO-AREA
               INITIALIZE FAKRUT-IO-AREA
               MOVE REC2                   TO FAKRUT-IO-AREA (1:200)
               IF  (I-91)
                   MOVE 'S'                TO FAKRUT-IO-AREA (12:1)
               END-IF
               IF  (I-92)
                   MOVE 'S'                TO FAKRUT-IO-AREA (12:1)
               END-IF
               WRITE FAKRUT-IO-AREA
           END-IF
           IF  (I-04)
               MOVE SPACES TO FAKRUT-IO-AREA
               INITIALIZE FAKRUT-IO-AREA
               MOVE REC3                   TO FAKRUT-IO-AREA (1:200)
               WRITE FAKRUT-IO-AREA
           END-IF
           IF  (I-02 AND I-12 AND NOT-I-92)
           OR  (I-02 AND I-14)
               MOVE SPACES TO SAMRUT-IO-AREA
               INITIALIZE SAMRUT-IO-AREA
               MOVE REC2                   TO SAMRUT-IO-AREA (1:200)
               WRITE SAMRUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO TOTSUM-IO-AREA
               INITIALIZE TOTSUM-IO-AREA
               MOVE '************************' TO TOTSUM-IO-AREA (1:24)
               MOVE '************************' TO TOTSUM-IO-AREA
                                                               (25:24)
               MOVE 01                     TO TOTSUM-BEFORE-SKIP
               MOVE 1                      TO TOTSUM-AFTER-SPACE
               PERFORM TOTSUM-PRINT-LINE
               MOVE SPACES TO TOTSUM-IO-AREA
               INITIALIZE TOTSUM-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO TOTSUM-IO-AREA (1:24)
               MOVE '    --- FAK050 ---   ***' TO TOTSUM-IO-AREA
                                                               (25:24)
               MOVE 1                      TO TOTSUM-AFTER-SPACE
               PERFORM TOTSUM-PRINT-LINE
               MOVE SPACES TO TOTSUM-IO-AREA
               INITIALIZE TOTSUM-IO-AREA
               MOVE '************************' TO TOTSUM-IO-AREA (1:24)
               MOVE '************************' TO TOTSUM-IO-AREA
                                                               (25:24)
               MOVE 3                      TO TOTSUM-AFTER-SPACE
               PERFORM TOTSUM-PRINT-LINE
               MOVE SPACES TO TOTSUM-IO-AREA
               INITIALIZE TOTSUM-IO-AREA
               MOVE ANTFAK                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTSUM-IO-AREA (4:7)
               MOVE 'RECORDS TIL FAKTURERING,' TO TOTSUM-IO-AREA
                                                               (12:24)
               MOVE FRASAM                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTSUM-IO-AREA (44:7)
               MOVE 'FRA SAMLEFAKT.MASTER.' TO TOTSUM-IO-AREA (52:21)
               MOVE 2                      TO TOTSUM-BEFORE-SPACE
               MOVE 1                      TO TOTSUM-AFTER-SPACE
               PERFORM TOTSUM-PRINT-LINE
               MOVE SPACES TO TOTSUM-IO-AREA
               INITIALIZE TOTSUM-IO-AREA
               MOVE ANTBON                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTSUM-IO-AREA (44:7)
               MOVE 'FRA FAKT.BONUS FILE. ' TO TOTSUM-IO-AREA (52:21)
               MOVE 3                      TO TOTSUM-AFTER-SPACE
               PERFORM TOTSUM-PRINT-LINE
               MOVE SPACES TO TOTSUM-IO-AREA
               INITIALIZE TOTSUM-IO-AREA
               MOVE ANTSAM                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTSUM-IO-AREA (4:7)
               MOVE 'SAMLEFAKTRECORDS LEST,' TO TOTSUM-IO-AREA (12:22)
               MOVE ANTNYS                 TO XO-60YY9
               MOVE XO-60YY9               TO TOTSUM-IO-AREA (44:7)
               MOVE 'KOPIERT TIL NESTE FAKT.' TO TOTSUM-IO-AREA (52:23)
               MOVE 0                      TO TOTSUM-AFTER-SPACE
               PERFORM TOTSUM-PRINT-LINE
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
           MOVE 3                          TO LR-CHECK
           SET FAKREC-LEVEL-INIT           TO TRUE
           INITIALIZE FAKREC-DATA-FIELDS
           SET FAKREC-EOF-OFF              TO TRUE
           SET FAKREC-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FAKREC-MC
                                              FAKREC-MP
           OPEN INPUT FAKREC
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           SET SAMREC-LEVEL-INIT           TO TRUE
           INITIALIZE SAMREC-DATA-FIELDS
           SET SAMREC-EOF-OFF              TO TRUE
           SET SAMREC-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO SAMREC-MC
                                              SAMREC-MP
           OPEN INPUT SAMREC
           SET BONFAK-LEVEL-INIT           TO TRUE
           INITIALIZE BONFAK-DATA-FIELDS
           SET BONFAK-EOF-OFF              TO TRUE
           SET BONFAK-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO BONFAK-MC
                                              BONFAK-MP
           OPEN INPUT BONFAK
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT FAKRUT
           OPEN OUTPUT SAMRUT
           OPEN OUTPUT TOTSUM
           INITIALIZE TOTSUM-IO-AREA
           INITIALIZE TOTSUM-DATA-FIELDS
           MOVE 57                         TO TOTSUM-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKREC
           CLOSE FAKPAR
           CLOSE SAMREC
           CLOSE BONFAK
           CLOSE FIRMAF
           CLOSE FAKRUT
           CLOSE SAMRUT
           IF TOTSUM-IO-AREA NOT = SPACES
             WRITE TOTSUM-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTSUM-IO-AREA
           END-IF
           CLOSE TOTSUM.
 
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
