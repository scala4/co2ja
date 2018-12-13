       IDENTIFICATION DIVISION.
       PROGRAM-ID. BEM001R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: BEM001  JCL= BEM.XBEM01A OG KUN.XKUN02A         *
      *  PROGRAMERER: ELIN                                            *
      *  PROGRAMERT.: 04.10.2007                                      *
      *                                                               *
      *  PROGRAMMET.: LISTER UT FELTER FRA KUNDEMA OG KUNDEMX TIL FIL *
      *                                                               *
      *  ENDR.DATO   TEKST.                                           *
      *  13.06.2007  lagt inn flere felter,snr,betmåte,               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BEM001.rpg
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
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT UTFILE
               ASSIGN TO UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(200).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD UTFILE
               RECORD CONTAINS 356.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(356).
       FD LISTE
               BLOCK CONTAINS 356
               RECORD CONTAINS 356.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(355).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  INFILE-LEVEL-01.
               10  INFILE-01-L2.
                   15  INFILE-01-L2-FIRMA  PICTURE X(3).
               10  INFILE-01-L1.
                   15  INFILE-01-L1-KNR    PICTURE X(6).
           05  INFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  KNR1                    PICTURE X(1).
               10  NAVN1                   PICTURE X(30).
               10  PBOKS                   PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  PNR                     PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  BM                      PICTURE X(2).
               10  RGR                     PICTURE X(9).
               10  FORS                    PICTURE X(1).
           05  KUNDEMX-DATA-FIELDS.
               10  TL1                     PICTURE X(16).
               10  PERS                    PICTURE X(30).
               10  SNR                     PICTURE X(3).
               10  LEVADR                  PICTURE X(30).
               10  LEVPNR                  PICTURE X(4).
               10  LPSTED                  PICTURE X(15).
               10  FAX                     PICTURE X(16).
               10  FAKKN1                  PICTURE X(6).
               10  ORGN1                   PICTURE X(9).
      ****
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KEY9                    PICTURE X(9).
               10  KEY10                   PICTURE X(10).
               10  TLF                     PICTURE X(16).
               10  SNR1                    PICTURE X(3).
               10  FAKKNR                  PICTURE X(6).
               10  ORGNR                   PICTURE X(9).
               10  KPERS                   PICTURE X(30).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   SET INFILE-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
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
               MOVE FIRMA                  TO KEY9 (1:3)
           END-IF
           IF  (I-L1)
               SET NOT-I-21                TO TRUE
               IF  BM = '02'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  BM = '03'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  BM = '04'
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  BM = '05'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  BM = '06'
                   SET I-25                TO TRUE
               END-IF
               SET NOT-I-26                TO TRUE
               IF  BM = '07'
                   SET I-26                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  BM = '08'
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-28                TO TRUE
               IF  BM = '11'
                   SET I-28                TO TRUE
               END-IF
               SET NOT-I-29                TO TRUE
               IF  BM = '12'
                   SET I-29                TO TRUE
               END-IF
               SET NOT-I-30                TO TRUE
               IF  BM = '14'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  BM = '15'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  BM = '16'
                   SET I-32                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               IF  BM = '17'
                   SET I-33                TO TRUE
               END-IF
               SET NOT-I-34                TO TRUE
               IF  BM = '18'
                   SET I-34                TO TRUE
               END-IF
      *  L1      BM        COMP "19"                     35
           END-IF
           IF  (I-L1)
               SET NOT-I-36                TO TRUE
               IF  BM = '20'
                   SET I-36                TO TRUE
               END-IF
               SET NOT-I-37                TO TRUE
               IF  BM = '21'
                   SET I-37                TO TRUE
               END-IF
               SET NOT-I-38                TO TRUE
               IF  BM = '22'
                   SET I-38                TO TRUE
               END-IF
               SET NOT-I-39                TO TRUE
               IF  BM = '23'
                   SET I-39                TO TRUE
               END-IF
               SET NOT-I-40                TO TRUE
               IF  BM = '24'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  BM = '25'
                   SET I-41                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  BM = '26'
                   SET I-42                TO TRUE
               END-IF
               SET NOT-I-43                TO TRUE
               IF  BM = '27'
                   SET I-43                TO TRUE
               END-IF
               SET NOT-I-44                TO TRUE
               IF  BM = '28'
                   SET I-44                TO TRUE
               END-IF
      *  L1      BM        COMP "29"                     45
           END-IF
           IF  (I-L1)
               SET NOT-I-46                TO TRUE
               IF  BM = '30'
                   SET I-46                TO TRUE
               END-IF
               SET NOT-I-47                TO TRUE
               IF  BM = '31'
                   SET I-47                TO TRUE
               END-IF
               SET NOT-I-48                TO TRUE
               IF  BM = '32'
                   SET I-48                TO TRUE
               END-IF
               SET NOT-I-49                TO TRUE
               IF  BM = '33'
                   SET I-49                TO TRUE
               END-IF
               SET NOT-I-51                TO TRUE
               IF  BM = '34'
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  BM = '35'
                   SET I-52                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  BM = '36'
                   SET I-53                TO TRUE
               END-IF
               SET NOT-I-54                TO TRUE
               IF  BM = '37'
                   SET I-54                TO TRUE
               END-IF
               SET NOT-I-55                TO TRUE
               IF  BM = '38'
                   SET I-55                TO TRUE
               END-IF
               SET NOT-I-56                TO TRUE
               IF  BM = '39'
                   SET I-56                TO TRUE
               END-IF
               SET NOT-I-57                TO TRUE
               IF  BM = '47'
                   SET I-57                TO TRUE
               END-IF
               SET NOT-I-58                TO TRUE
               IF  BM = '49'
                   SET I-58                TO TRUE
               END-IF
               SET NOT-I-59                TO TRUE
               IF  BM = '50'
                   SET I-59                TO TRUE
               END-IF
               SET NOT-I-60                TO TRUE
               IF  BM = '51'
                   SET I-60                TO TRUE
               END-IF
               SET NOT-I-61                TO TRUE
               IF  BM = '52'
                   SET I-61                TO TRUE
               END-IF
               SET NOT-I-62                TO TRUE
               IF  BM = '53'
                   SET I-62                TO TRUE
               END-IF
               SET NOT-I-63                TO TRUE
               IF  BM = '54'
                   SET I-63                TO TRUE
               END-IF
               SET NOT-I-64                TO TRUE
               IF  BM = '55'
                   SET I-64                TO TRUE
               END-IF
               SET NOT-I-65                TO TRUE
               IF  BM = '56'
                   SET I-65                TO TRUE
               END-IF
               SET NOT-I-66                TO TRUE
               IF  BM = '57'
                   SET I-66                TO TRUE
               END-IF
               SET NOT-I-67                TO TRUE
               IF  BM = '58'
                   SET I-67                TO TRUE
               END-IF
               SET NOT-I-68                TO TRUE
               IF  BM = '59'
                   SET I-68                TO TRUE
               END-IF
               SET NOT-I-69                TO TRUE
               IF  BM = '60'
                   SET I-69                TO TRUE
               END-IF
               SET NOT-I-70                TO TRUE
               IF  BM = '61'
                   SET I-70                TO TRUE
               END-IF
               SET NOT-I-71                TO TRUE
               IF  BM = '62'
                   SET I-71                TO TRUE
               END-IF
               SET NOT-I-72                TO TRUE
               IF  BM = '63'
                   SET I-72                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  BM = '64'
                   SET I-73                TO TRUE
               END-IF
               SET NOT-I-74                TO TRUE
               IF  BM = '65'
                   SET I-74                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  BM = '66'
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  BM = '67'
                   SET I-76                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  BM = '68'
                   SET I-77                TO TRUE
               END-IF
               SET NOT-I-78                TO TRUE
               IF  BM = '69'
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-79                TO TRUE
               IF  BM = '70'
                   SET I-79                TO TRUE
               END-IF
               SET NOT-I-80                TO TRUE
               IF  BM = '71'
                   SET I-80                TO TRUE
               END-IF
               SET NOT-I-81                TO TRUE
               IF  BM = '72'
                   SET I-81                TO TRUE
               END-IF
               SET NOT-I-82                TO TRUE
               IF  BM = '73'
                   SET I-82                TO TRUE
               END-IF
               SET NOT-I-83                TO TRUE
               IF  BM = '74'
                   SET I-83                TO TRUE
               END-IF
               SET NOT-I-84                TO TRUE
               IF  BM = '75'
                   SET I-84                TO TRUE
               END-IF
               SET NOT-I-85                TO TRUE
               IF  BM = '76'
                   SET I-85                TO TRUE
               END-IF
               SET NOT-I-86                TO TRUE
               IF  BM = '77'
                   SET I-86                TO TRUE
               END-IF
               SET NOT-I-87                TO TRUE
               IF  BM = '78'
                   SET I-87                TO TRUE
               END-IF
               SET NOT-I-88                TO TRUE
               IF  BM = '79'
                   SET I-88                TO TRUE
               END-IF
               SET NOT-I-89                TO TRUE
               IF  BM = '80'
                   SET I-89                TO TRUE
               END-IF
               SET NOT-I-90                TO TRUE
               IF  BM = '81'
                   SET I-90                TO TRUE
               END-IF
               SET NOT-I-91                TO TRUE
               IF  BM = '82'
                   SET I-91                TO TRUE
               END-IF
               SET NOT-I-92                TO TRUE
               IF  BM = '83'
                   SET I-92                TO TRUE
               END-IF
               SET NOT-I-93                TO TRUE
               IF  BM = '84'
                   SET I-93                TO TRUE
               END-IF
               SET NOT-I-94                TO TRUE
               IF  BM = '85'
                   SET I-94                TO TRUE
               END-IF
               MOVE KNR                    TO KEY9 (4:6)
               MOVE KEY9                   TO KEY10 (1:9)
               MOVE '1'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-11)
               MOVE TL1                    TO TLF
               MOVE SNR                    TO SNR1
               MOVE FAKKN1                 TO FAKKNR
               MOVE ORGN1                  TO ORGNR
               MOVE PERS                   TO KPERS
           END-IF
           IF  (I-L1)
               MOVE '2'                    TO KEY10 (10:1)
               SET NOT-I-50                TO TRUE
               IF  KNR1 = 'C'
                   SET I-50                TO TRUE
               END-IF
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
      *TFILE  H        1P
      *                                   1 """"
      *                                   7 "KNR"
      *                                  10 """;"""
      *                                  20 "NAVN"
      *                                  43 """;"""
      *                                  73 "POSTBOKS"
      *                                  79 """;"""";"""
      *                                  83 "PNR"
      *                                  86 """;"""
      *                                 101 "POSTSTED"
      *                                 104 """;"""
      *                                 130 "ADRESSE"
      *                                 138 """;"""
      *                                 154 "TELFONNR"
      *                                 157 """;"""
      *                                 174 "FAX.NR"
      *                                 177 """;"""
      *                                 207 "LEV.ADRESSE"
      *                                 210 """;"""
      *                                 214 "LPNR"
      *                                 217 """;"""
      *                                 232 "LEV.P.STED"
      *                                 235 """;"""
      *                                 244 "ORG.NR."
      *                                 250 """;"""
      *                                 259 "RABGRP"
      *                                 262 """;"""
      *                                 268 "FAKKNR"
      *                                 271 """;"""
      *                                 272 "F"
      *                                 275 """;"""
      *                                 278 "FNR"
      *                                 281 """;"""
      *                                 286 "SNRID"
      *                                 292 """;"""
      *                                 294 "BM"
      *                                 297 """;"""
      *                                 321 "BETALINGSMÅTE TEKST     "
      *                                 324 """;"""
      *                                 348 "KONTAKT PERSON          "
           END-IF
           .
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (6:6)   TO KNR (1:6)
               MOVE INFILE-IO-AREA (6:1)   TO KNR1 (1:1)
               MOVE INFILE-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE INFILE-IO-AREA (46:30) TO PBOKS (1:30)
               MOVE INFILE-IO-AREA (76:30) TO ADR (1:30)
               MOVE INFILE-IO-AREA (121:4) TO PNR (1:4)
               MOVE INFILE-IO-AREA (106:15) TO PSTED (1:15)
               MOVE INFILE-IO-AREA (127:2) TO BM (1:2)
               MOVE INFILE-IO-AREA (190:9) TO RGR (1:9)
               MOVE INFILE-IO-AREA (199:1) TO FORS (1:1)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (3:3)   TO INFILE-01-L2-FIRMA
               MOVE INFILE-IO-AREA (6:6)   TO INFILE-01-L1-KNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L2          TO THE-PRIOR-L2
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (11:16) TO TL1 (1:16)
               MOVE KUNDEMX-IO-AREA (45:30) TO PERS (1:30)
               MOVE KUNDEMX-IO-AREA (105:3) TO SNR (1:3)
               MOVE KUNDEMX-IO-AREA (11:30) TO LEVADR (1:30)
               MOVE KUNDEMX-IO-AREA (101:4) TO LEVPNR (1:4)
               MOVE KUNDEMX-IO-AREA (106:15) TO LPSTED (1:15)
               MOVE KUNDEMX-IO-AREA (152:16) TO FAX (1:16)
               MOVE KUNDEMX-IO-AREA (161:6) TO FAKKN1 (1:6)
               MOVE KUNDEMX-IO-AREA (180:9) TO ORGN1 (1:9)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-02                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-50)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE '"""""""""""""""""""""""""          T TIEI-RA(:)
               MOVE KNR                    TO UTFILE-IO-AREA (2:6)
               MOVE '"""""""""""""""""""""""""""         OUFL-OAE 83
               MOVE NAVN1                  TO UTFILE-IO-AREA (11:30)
               MOVE '""""""""""""""""""""""""";"         T TIEI-RA(13
               MOVE PBOKS                  TO UTFILE-IO-AREA (44:30)
               MOVE '""""""""""""""""""""""""";""""       T TIEI-RA(46)
               MOVE PNR                    TO UTFILE-IO-AREA (80:4)
               MOVE '""""""""""""""""""""""""";"         T TIEI-RA(43
               MOVE PSTED                  TO UTFILE-IO-AREA (87:15)
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 123
               MOVE ADR                    TO UTFILE-IO-AREA (105:30)
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 163
               IF  (NOT-I-11)
                   MOVE TLF                TO UTFILE-IO-AREA (139:16)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 153
               IF  (NOT-I-11)
                   MOVE FAX                TO UTFILE-IO-AREA (159:16)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 153
               IF  (NOT-I-10)
                   MOVE LEVADR             TO UTFILE-IO-AREA (178:30)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 283
               IF  (NOT-I-10)
                   MOVE LEVPNR             TO UTFILE-IO-AREA (211:4)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 253
               IF  (NOT-I-10)
                   MOVE LPSTED             TO UTFILE-IO-AREA (218:15)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 233
               IF  (NOT-I-10)
                   MOVE ORGNR              TO UTFILE-IO-AREA (236:9)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 283
               MOVE RGR                    TO UTFILE-IO-AREA (251:9)
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 203
               IF  (NOT-I-11)
                   MOVE FAKKNR             TO UTFILE-IO-AREA (263:6)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 293
               MOVE FORS                   TO UTFILE-IO-AREA (272:1)
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 233
               MOVE FIRMA                  TO UTFILE-IO-AREA (276:3)
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 293
               IF  (NOT-I-11)
                   MOVE 'NVSEL'            TO UTFILE-IO-AREA (282:5)
               END-IF
               IF  (NOT-I-11)
                   MOVE SNR1               TO UTFILE-IO-AREA (287:3)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 203
               MOVE BM                     TO UTFILE-IO-AREA (293:2)
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 253
               IF  (I-21)
                   MOVE '15 DAGER GIRO           ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-22)
                   MOVE '30 DAGER BANKGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-23)
                   MOVE '30 DAGER POSTGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-24)
                   MOVE 'FRI MND + 30 DGR B.GIRO ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-25)
                   MOVE '30 DAGER REMITERER SELV ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-26)
                   MOVE 'KONTANT                 ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-27)
                   MOVE '45 DAGER BANKGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-28)
                   MOVE '30 DAGER AUTOGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-29)
                   MOVE '15 DAGER AUTOGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-30)
                   MOVE 'OPPKRAV                 ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-31)
                   MOVE '60 DAGER BANKGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-32)
                   MOVE '90 DAGER BANKGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-33)
                   MOVE 'FRI MND + 60 DGR B.GIRO ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-34)
                   MOVE 'BOKREDITT               ' TO UTFILE-IO-AREA
                                                              (298:24)
      *                      35         321 "30 DGR. SHELL-FAKTURERER"
               END-IF
               IF  (I-36)
                   MOVE 'MASKINENHETER           ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-37)
                   MOVE '4 MND AKSEPT            ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-38)
                   MOVE '5 MND AKSEPT            ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-39)
                   MOVE '6 MND AKSEPT            ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-40)
                   MOVE 'SPESIELLE AVTALER       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-41)
                   MOVE 'FRI MND + 15 DAGER      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-42)
                   MOVE 'FRI MND + 45 DAGER      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-43)
                   MOVE 'FRI MND + 75 DAGER      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-44)
                   MOVE 'FRI MND + 90 DAGER      ' TO UTFILE-IO-AREA
                                                              (298:24)
      *                      45         321 "30 DGR. STATOIL-FAKTURA "
               END-IF
               IF  (I-46)
                   MOVE '15 DAGERS POSTGIRO      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-47)
                   MOVE '60 DAGERS POSTGIRO      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-48)
                   MOVE 'FRI MND + 30D POSTGIRO  ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-49)
                   MOVE '45 DAGER POSTGIRO       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-51)
                   MOVE '90 DAGERS POSTGIRO      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-52)
                   MOVE 'FRI MND + 60D POSTGIRO  ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-53)
                   MOVE 'FRI MND + 15D POSTGIRO  ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-54)
                   MOVE 'FRI MND + 45D POSTGIRO  ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-55)
                   MOVE 'FRI MND + 75D POSTGIRO  ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-56)
                   MOVE 'FRI MND + 90D POSTGIRO  ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-57)
                   MOVE 'KONTANT-FAKTURA         ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-58)
                   MOVE 'SALG STOPP PÅ KUNDEN    ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-59)
                   MOVE '30 DAGER NETTO          ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-60)
                   MOVE 'NETTO KONTANT           ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-61)
                   MOVE '60 DAGER                ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-62)
                   MOVE 'AKSEPT 60 DGR + FRI MND ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-63)
                   MOVE '90 DAGER ÅPEN REGNING   ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-64)
                   MOVE 'MOTREGNES               ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-65)
                   MOVE '60 DAGER ÅPEN REGNING   ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-66)
                   MOVE 'FRI MND + 90 DGR AKSEPT ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-67)
                   MOVE 'FRI MND + 30 DAGER      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-68)
                   MOVE 'AKSEPT 90 DGR -FAKT.DATO' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-69)
                   MOVE '150 DAGER AKSEPT        ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-70)
                   MOVE 'FRI MND + 15 DAGER      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-71)
                   MOVE '6 MND AKSEPT            ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-72)
                   MOVE '30 DAGER - 2 PROSENT    ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-73)
                   MOVE 'OPPKRAV                 ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-74)
                   MOVE 'NETTO PR 14 DAGER       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-75)
                   MOVE 'FRIGRENSE               ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-76)
                   MOVE '90 DGR ÅP.RGN+90 DGR AKS' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-77)
                   MOVE '9 MND AKSEPT            ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-78)
                   MOVE '10 DAGER NETTO          ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-79)
                   MOVE '3 MND AKSEPT -SKIPN.DATO' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-80)
                   MOVE '4 MND AKSEPT -FAKT.DATO ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-81)
                   MOVE 'FORSKUDDSBETALING       ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-82)
                   MOVE '1 MND AKSEPT            ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-83)
                   MOVE '10 DAGER - 2 PROSENT    ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-84)
                   MOVE 'FRI MND+90 DGR ÅPEN RGN ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-85)
                   MOVE 'FRI LEV.-MND+15 DAGER-3%' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-86)
                   MOVE '12 MND AKSEPT           ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-87)
                   MOVE '120 DAGER ÅPEN REGNING  ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-88)
                   MOVE 'FRI MND + 60 DAGER      ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-89)
                   MOVE 'BETALT/AUTOGIRO         ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-90)
                   MOVE '45 DAGER NETTO          ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-91)
                   MOVE 'FRI MND. + 45 DAGER     ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-92)
                   MOVE 'FRI MND. + 20 DAGER     ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-93)
                   MOVE '20 DAGER                ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-94)
                   MOVE '45 DAGER                ' TO UTFILE-IO-AREA
                                                              (298:24)
               END-IF
               MOVE '""""""""""""""""""""""""""""         OUFL-OAE 323
               IF  (NOT-I-11)
                   MOVE KPERS              TO UTFILE-IO-AREA (325:30)
               END-IF
               MOVE '""""""""""""""""""""""""""          T TIEI-RA(5:)
               MOVE ';'                    TO UTFILE-IO-AREA (356:1)
               WRITE UTFILE-IO-AREA
           END-IF
           IF  (I-L1 AND NOT-I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '"""""""""""""""""""""""""          OLSEI-RA(:)
               MOVE KNR                    TO LISTE-IO-AREA (2:6)
               MOVE '"""""""""""""""""""""""";"         T IT-OAE 83
               MOVE NAVN1                  TO LISTE-IO-AREA (11:30)
               MOVE '"""""""""""""""""""""""""""         OLSEI-RA(13
               MOVE PBOKS                  TO LISTE-IO-AREA (44:30)
               MOVE '""""""""""""""""""""""""""";"        OLSEI-RA(46)
               MOVE PNR                    TO LISTE-IO-AREA (80:4)
               MOVE ' '                    TO LISTE-IO-AREA (84:1)
               MOVE PSTED                  TO LISTE-IO-AREA (85:15)
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 103
               MOVE ADR                    TO LISTE-IO-AREA (103:30)
               MOVE '""""""""""""""""""""""""";""        T IT-OAE 134
               IF  (NOT-I-11)
                   MOVE TLF                TO LISTE-IO-AREA (137:16)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 133
               IF  (NOT-I-11)
                   MOVE FAX                TO LISTE-IO-AREA (157:16)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 133
               IF  (NOT-I-10)
                   MOVE LEVADR             TO LISTE-IO-AREA (176:30)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 263
               IF  (NOT-I-10)
                   MOVE LEVPNR             TO LISTE-IO-AREA (209:4)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 233
               IF  (NOT-I-10)
                   MOVE LPSTED             TO LISTE-IO-AREA (216:15)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 213
               IF  (NOT-I-10)
                   MOVE ORGNR              TO LISTE-IO-AREA (234:9)
               END-IF
               MOVE '""""""""""""""""""""""""";""""       T IT-OAE 236)
               MOVE RGR                    TO LISTE-IO-AREA (249:9)
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 283
               IF  (NOT-I-11)
                   MOVE FAKKNR             TO LISTE-IO-AREA (261:6)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 273
               MOVE FORS                   TO LISTE-IO-AREA (270:1)
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 213
               MOVE FIRMA                  TO LISTE-IO-AREA (274:3)
               MOVE '""""""""""""""""""""""""""          OLSEI-RA(7:)
               MOVE FORS                   TO LISTE-IO-AREA (272:1)
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 233
               MOVE FIRMA                  TO LISTE-IO-AREA (276:3)
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 293
               IF  (NOT-I-11)
                   MOVE 'NVSEL'            TO LISTE-IO-AREA (282:5)
               END-IF
               IF  (NOT-I-11)
                   MOVE SNR1               TO LISTE-IO-AREA (287:3)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 203
               MOVE BM                     TO LISTE-IO-AREA (293:2)
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 253
               IF  (I-21)
                   MOVE '15 DAGER GIRO           ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-22)
                   MOVE '30 DAGER BANKGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-23)
                   MOVE '30 DAGER POSTGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-24)
                   MOVE 'FRI MND + 30 DGR B.GIRO ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-25)
                   MOVE '30 DAGER REMITERER SELV ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-26)
                   MOVE 'KONTANT                 ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-27)
                   MOVE '45 DAGER BANKGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-28)
                   MOVE '30 DAGER AUTOGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-29)
                   MOVE '15 DAGER AUTOGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-30)
                   MOVE 'OPPKRAV                 ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-31)
                   MOVE '60 DAGER BANKGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-32)
                   MOVE '90 DAGER BANKGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-33)
                   MOVE 'FRI MND + 60 DGR B.GIRO ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-34)
                   MOVE 'BOKREDITT               ' TO LISTE-IO-AREA
                                                              (298:24)
      *                      35         321 "30 DGR. SHELL-FAKTURERER"
               END-IF
               IF  (I-36)
                   MOVE 'MASKINENHETER           ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-37)
                   MOVE '4 MND AKSEPT            ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-38)
                   MOVE '5 MND AKSEPT            ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-39)
                   MOVE '6 MND AKSEPT            ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-40)
                   MOVE 'SPESIELLE AVTALER       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-41)
                   MOVE 'FRI MND + 15 DAGER      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-42)
                   MOVE 'FRI MND + 45 DAGER      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-43)
                   MOVE 'FRI MND + 75 DAGER      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-44)
                   MOVE 'FRI MND + 90 DAGER      ' TO LISTE-IO-AREA
                                                              (298:24)
      *                      45         321 "30 DGR. STATOIL-FAKTURA "
               END-IF
               IF  (I-46)
                   MOVE '15 DAGERS POSTGIRO      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-47)
                   MOVE '60 DAGERS POSTGIRO      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-48)
                   MOVE 'FRI MND + 30D POSTGIRO  ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-49)
                   MOVE '45 DAGER POSTGIRO       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-51)
                   MOVE '90 DAGERS POSTGIRO      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-52)
                   MOVE 'FRI MND + 60D POSTGIRO  ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-53)
                   MOVE 'FRI MND + 15D POSTGIRO  ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-54)
                   MOVE 'FRI MND + 45D POSTGIRO  ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-55)
                   MOVE 'FRI MND + 75D POSTGIRO  ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-56)
                   MOVE 'FRI MND + 90D POSTGIRO  ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-57)
                   MOVE 'KONTANT-FAKTURA         ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-58)
                   MOVE 'SALG STOPP PÅ KUNDEN    ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-59)
                   MOVE '30 DAGER NETTO          ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-60)
                   MOVE 'NETTO KONTANT           ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-61)
                   MOVE '60 DAGER                ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-62)
                   MOVE 'AKSEPT 60 DGR + FRI MND ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-63)
                   MOVE '90 DAGER ÅPEN REGNING   ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-64)
                   MOVE 'MOTREGNES               ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-65)
                   MOVE '60 DAGER ÅPEN REGNING   ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-66)
                   MOVE 'FRI MND + 90 DGR AKSEPT ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-67)
                   MOVE 'FRI MND + 30 DAGER      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-68)
                   MOVE 'AKSEPT 90 DGR -FAKT.DATO' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-69)
                   MOVE '150 DAGER AKSEPT        ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-70)
                   MOVE 'FRI MND + 15 DAGER      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-71)
                   MOVE '6 MND AKSEPT            ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-72)
                   MOVE '30 DAGER - 2 PROSENT    ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-73)
                   MOVE 'OPPKRAV                 ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-74)
                   MOVE 'NETTO PR 14 DAGER       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-75)
                   MOVE 'FRIGRENSE               ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-76)
                   MOVE '90 DGR ÅP.RGN+90 DGR AKS' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-77)
                   MOVE '9 MND AKSEPT            ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-78)
                   MOVE '10 DAGER NETTO          ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-79)
                   MOVE '3 MND AKSEPT -SKIPN.DATO' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-80)
                   MOVE '4 MND AKSEPT -FAKT.DATO ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-81)
                   MOVE 'FORSKUDDSBETALING       ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-82)
                   MOVE '1 MND AKSEPT            ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-83)
                   MOVE '10 DAGER - 2 PROSENT    ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-84)
                   MOVE 'FRI MND+90 DGR ÅPEN RGN ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-85)
                   MOVE 'FRI LEV.-MND+15 DAGER-3%' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-86)
                   MOVE '12 MND AKSEPT           ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-87)
                   MOVE '120 DAGER ÅPEN REGNING  ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-88)
                   MOVE 'FRI MND + 60 DAGER      ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-89)
                   MOVE 'BETALT/AUTOGIRO         ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-90)
                   MOVE '45 DAGER NETTO          ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-91)
                   MOVE 'FRI MND. + 45 DAGER     ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-92)
                   MOVE 'FRI MND. + 20 DAGER     ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-93)
                   MOVE '20 DAGER                ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               IF  (I-94)
                   MOVE '45 DAGER                ' TO LISTE-IO-AREA
                                                              (298:24)
               END-IF
               MOVE '""""""""""""""""""""""""";"         T IT-OAE 323
               IF  (NOT-I-11)
                   MOVE KPERS              TO LISTE-IO-AREA (325:30)
               END-IF
               MOVE '""""""""""""""""""""""""""          OLSEI-RA(5:)
               MOVE 01                     TO LISTE-BEFORE-SKIP
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
           MOVE 1                          TO LR-CHECK
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           OPEN OUTPUT UTFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
           CLOSE KUNDEMX
           CLOSE UTFILE
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
