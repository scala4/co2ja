       IDENTIFICATION DIVISION.
       PROGRAM-ID. HEI011R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM:HEI011.  KOPIERT FRA HEI010 - SKRIVER PÅ UTFIL        *
      * LAGER PC-FILE FRA VAREMASTER, DANNE OPPSLAGSNR.               *
      * BEREGNER BEHOLDNING.                                          *
      * UPSI 1 = HENT STREKKODE.                                      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: HEI011.rpg
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
           SELECT VARDAT
               ASSIGN TO UT-S-VARDAT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARDAT-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARDAT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  VARDAT-IO-AREA.
           05  VARDAT-IO-AREA-X            PICTURE X(200).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD UTFILE
               RECORD CONTAINS 96.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(96).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VARDAT-STATUS               PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
           10  OPFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VARDAT-EOF-OFF          VALUE '0'.
               88  VARDAT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARDAT-READ-OFF         VALUE '0'.
               88  VARDAT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARDAT-PROCESS-OFF      VALUE '0'.
               88  VARDAT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARDAT-LEVEL-INIT-OFF   VALUE '0'.
               88  VARDAT-LEVEL-INIT       VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  OPFELT-XX-DATA-FIELDS.
               10  OARTNR                  PICTURE X(20).
           05  VARDAT-LEVEL-01.
               10  VARDAT-01-L1.
                   15  VARDAT-01-L1-FNR    PICTURE X(3).
           05  VARDAT-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  FNRENR                  PICTURE X(10).
               10  EDBNR                   PICTURE X(7).
               10  ALF                     PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VAREB                   PICTURE X(30).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTPAK-IO.
                   15  ANTPAK              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAGLOC                  PICTURE X(6).
               10  VBLK13-IO.
                   15  VBLK13              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VBLK93-IO.
                   15  VBLK93              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VBLK15-IO.
                   15  VBLK15              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VBLK17-IO.
                   15  VBLK17              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VBLK92-IO.
                   15  VBLK92              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  VBLK18-IO.
                   15  VBLK18              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  VARETIL-DATA-FIELDS.
               10  EANNR                   PICTURE X(14).
      *****************************************************************
      * RUTINE FOR Å LAGE OPPSLAGSNUMMER.                             *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  OPNR14                  PICTURE X(14).
               10  BEH-IO.
                   15  BEH                 PICTURE S9(7)V9(2).
               10  KINNH-IO.
                   15  KINNH               PICTURE S9(5).
               10  VXKEY                   PICTURE X(12).
           05  EDITTING-FIELDS.
               10  EDIT-KINNH              PICTURE Z9999.
               10  EDIT-BEH                PICTURE Z999999,99.
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
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
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
           IF  VARDAT-PROCESS
               SET VARDAT-PROCESS-OFF      TO TRUE
               SET VARDAT-READ             TO TRUE
           END-IF
 
           IF  VARDAT-READ
           AND RECORD-SELECTED-OFF
               PERFORM VARDAT-GET
               SET VARDAT-READ-OFF         TO TRUE
               IF  NOT VARDAT-EOF
                   SET VARDAT-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VARDAT-PROCESS
               PERFORM VARDAT-IDSET
           END-IF
 
           IF  VARDAT-PROCESS
               PERFORM VARDAT-CHK-LEVEL
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
 
           IF  VARDAT-PROCESS
               PERFORM VARDAT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARDAT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           MOVE ARTNR                      TO OARTNR
           CALL 'OPPSNR' USING OPFELT-XX-DATA-FIELDS
           MOVE OPFELT                     TO OPNR14 (1:0)
      *****************************************************************
      * RUTINE FOR Å BEREGNE VAREBEHOLDNING.                          *
      *****************************************************************
           SUBTRACT ANTUT FROM ANTINN  GIVING BEH
           SUBTRACT VBLK13                 FROM BEH
           SUBTRACT VBLK93                 FROM BEH
           SUBTRACT VBLK15                 FROM BEH
           SUBTRACT VBLK17                 FROM BEH
           SUBTRACT VBLK92                 FROM BEH
           SUBTRACT VBLK18                 FROM BEH
           SET NOT-I-66                    TO TRUE
           IF  BEH < 0
               SET I-66                    TO TRUE
           END-IF
      *****************************************************************
      * RUTINE FOR KARTONGINNHOLD.                                    *
      *****************************************************************
           ADD ANTPAK TO ZERO          GIVING KINNH
      *****************************************************************
      * RUTINE FOR Å HENTE EAN-NR.   = UPSI 1                         *
      *****************************************************************
           IF  (I-01 AND I-U1)
               SET NOT-I-41                TO TRUE
               MOVE '80'                   TO VXKEY (1:2)
               MOVE FNRENR                 TO VXKEY (3:10)
               MOVE VXKEY                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-19                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-19            TO TRUE
                   PERFORM VARETIL-FLDSET
                   PERFORM VARETIL-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-U1 AND NOT-I-19)
               SET I-41                    TO TRUE
           END-IF.
 
       VARDAT-GET SECTION.
       VARDAT-GET-P.
           IF  VARDAT-EOF-OFF
               READ VARDAT
               AT END
                   SET VARDAT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARDAT-FLDSET SECTION.
       VARDAT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARDAT-IO-AREA (3:3)   TO FNR (1:3)
               MOVE VARDAT-IO-AREA (3:10)  TO FNRENR (1:10)
               MOVE VARDAT-IO-AREA (6:7)   TO EDBNR (1:7)
               MOVE VARDAT-IO-AREA (13:3)  TO ALF (1:3)
               MOVE VARDAT-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VARDAT-IO-AREA (36:30) TO VAREB (1:30)
               MOVE VARDAT-IO-AREA (97:5)  TO ANTINN-IO
               MOVE VARDAT-IO-AREA (102:5) TO ANTUT-IO
               MOVE VARDAT-IO-AREA (108:3) TO ANTPAK-IO
               MOVE VARDAT-IO-AREA (140:6) TO LAGLOC (1:6)
               MOVE VARDAT-IO-AREA (179:3) TO VBLK13-IO
               MOVE VARDAT-IO-AREA (179:3) TO VBLK93-IO
               MOVE VARDAT-IO-AREA (179:3) TO VBLK15-IO
               MOVE VARDAT-IO-AREA (179:3) TO VBLK17-IO
               MOVE VARDAT-IO-AREA (179:3) TO VBLK92-IO
               MOVE VARDAT-IO-AREA (179:3) TO VBLK18-IO
           END-EVALUATE.
 
       VARDAT-IDSET SECTION.
       VARDAT-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARDAT-CHK-LEVEL SECTION.
       VARDAT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARDAT-LEVEL-01
               MOVE VARDAT-IO-AREA (3:3)   TO VARDAT-01-L1-FNR
               IF  VARDAT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARDAT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARDAT-01-L1          TO THE-PRIOR-L1
               SET VARDAT-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (148:14) TO EANNR (1:14)
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE ALF                    TO UTFILE-IO-AREA (1:3)
               MOVE OPNR14                 TO UTFILE-IO-AREA (4:14)
               MOVE ';'                    TO UTFILE-IO-AREA (18:1)
               MOVE VAREB                  TO UTFILE-IO-AREA (19:30)
               MOVE ';'                    TO UTFILE-IO-AREA (49:1)
               MOVE LAGLOC                 TO UTFILE-IO-AREA (50:6)
               MOVE ';'                    TO UTFILE-IO-AREA (56:1)
               MOVE KINNH                  TO EDIT-KINNH
               MOVE EDIT-KINNH             TO UTFILE-IO-AREA (57:5)
               MOVE ';'                    TO UTFILE-IO-AREA (62:1)
               MOVE BEH                    TO EDIT-BEH
               MOVE EDIT-BEH               TO UTFILE-IO-AREA (63:10)
               IF  (I-66)
                   MOVE '-'                TO UTFILE-IO-AREA (63:1)
               END-IF
               IF  (NOT-I-66)
                   MOVE '0'                TO UTFILE-IO-AREA (63:1)
               END-IF
               MOVE ';'                    TO UTFILE-IO-AREA (73:1)
               IF  (I-U1 AND I-41)
                   MOVE EANNR              TO UTFILE-IO-AREA (74:14)
               END-IF
               MOVE ';'                    TO UTFILE-IO-AREA (88:1)
               MOVE EDBNR                  TO UTFILE-IO-AREA (89:7)
               MOVE ';'                    TO UTFILE-IO-AREA (96:1)
               WRITE UTFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE 'ALF + ART.NR.    '    TO UTFILE-IO-AREA (1:17)
               MOVE ';'                    TO UTFILE-IO-AREA (18:1)
               MOVE 'VARENAVN'             TO UTFILE-IO-AREA (19:8)
               MOVE ';'                    TO UTFILE-IO-AREA (49:1)
               MOVE 'LAGLOC'               TO UTFILE-IO-AREA (50:6)
               MOVE ';'                    TO UTFILE-IO-AREA (56:1)
               MOVE 'PAKN.'                TO UTFILE-IO-AREA (57:5)
               MOVE ';'                    TO UTFILE-IO-AREA (62:1)
               MOVE 'BEHOLDNING'           TO UTFILE-IO-AREA (63:10)
               MOVE ';'                    TO UTFILE-IO-AREA (73:1)
               WRITE UTFILE-IO-AREA
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
           SET VARDAT-LEVEL-INIT           TO TRUE
           INITIALIZE VARDAT-DATA-FIELDS
           SET VARDAT-EOF-OFF              TO TRUE
           SET VARDAT-PROCESS              TO TRUE
           OPEN INPUT VARDAT
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           OPEN OUTPUT UTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARDAT
           CLOSE VARETIL
           CLOSE UTFILE.
 
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
