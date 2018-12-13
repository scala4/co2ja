       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK382R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: FAK382                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMERT.: 29.07.97                                        *
      *  RETTET.....: 29.07.97                                        *
      *                                                               *
      *  PROGRAMMET SELEKTERER RECORDS FRA FAKTURA.VARERECORD.        *
      *  SELEKTER  UTIFRA PARAMETERKORT.                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK382.rpg
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
           SELECT TABELL
               ASSIGN TO UT-S-TABELL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABELL-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT FAKTF
               ASSIGN TO UT-S-FAKTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTF-STATUS.
           SELECT OUTREC
               ASSIGN TO UT-S-OUTREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTREC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TABELL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABELL-IO-AREA.
           05  TABELL-IO-AREA-X            PICTURE X(80).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FAKTF
               BLOCK CONTAINS 164
               RECORD CONTAINS 82.
       01  FAKTF-IO-AREA.
           05  FAKTF-IO-AREA-X             PICTURE X(82).
       FD OUTREC
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  OUTREC-IO-AREA.
           05  OUTREC-IO-AREA-X            PICTURE X(30).
       WORKING-STORAGE SECTION.
       77  FIRTAB-MAX   VALUE 40           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  FIRTAB-TABLE.
               10  FIRTAB-ENTRY
                                           OCCURS 40 TIMES
                                           INDEXED BY FIRTAB-I
                                                      FIRTAB-S.
                   15  FIRTAB              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TABELL-STATUS               PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FAKTF-STATUS                PICTURE 99 VALUE 0.
           10  OUTREC-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TABELL-EOF-OFF          VALUE '0'.
               88  TABELL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTF-EOF-OFF           VALUE '0'.
               88  FAKTF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTF-READ-OFF          VALUE '0'.
               88  FAKTF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTF-PROCESS-OFF       VALUE '0'.
               88  FAKTF-PROCESS           VALUE '1'.
           05  PARAM-DATA-FIELDS.
               10  PFIRMA                  PICTURE X(3).
               10  PAVD                    PICTURE X(1).
               10  PKUNDE                  PICTURE X(6).
           05  FAKTF-DATA-FIELDS.
               10  AVD                     PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  UTPRIS-IO.
                   15  UTPRIS              PICTURE S9(7)V9(2).
               10  MND                     PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  KUNDNR                  PICTURE X(6).
               10  VGR                     PICTURE X(5).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7).
               10  FAKTKR                  PICTURE X(1).
      *                                      44  44 BK                         8
               10  KRETYP                  PICTURE X(1).
               10  LAGER                   PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  EDBNR3-IO.
                   15  EDBNR3              PICTURE S9(3).
               10  EDBNR2-IO.
                   15  EDBNR2              PICTURE S9(2).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(9)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(4).
           05  EDITTING-FIELDS.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FAKTF-PROCESS
               SET FAKTF-PROCESS-OFF       TO TRUE
               SET FAKTF-READ              TO TRUE
           END-IF
 
           IF  FAKTF-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKTF-GET
               SET FAKTF-READ-OFF          TO TRUE
               IF  NOT FAKTF-EOF
                   SET FAKTF-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  FAKTF-PROCESS
               PERFORM FAKTF-IDSET
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  FAKTF-PROCESS
               PERFORM FAKTF-FLDSET
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
           SET NOT-I-10                    TO TRUE
      *****************************************************************
      * PARAMETER RUTINE.                                             *
      *****************************************************************
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  PAVD > ' '
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  PKUNDE > '      '
                   SET I-12                TO TRUE
               END-IF
               GO TO SLUTT-T
      *****************************************************************
      * SELEKSJONSRUTINE.                                             *
      *****************************************************************
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  FIRMA = PFIRMA
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-21                TO TRUE
               SET FIRTAB-S                TO 1
               PERFORM WITH TEST AFTER
                       VARYING FIRTAB-I FROM 1 BY 1
                         UNTIL FIRTAB-I >= FIRTAB-MAX
                            OR I-21
                   IF  FIRMA = FIRTAB (FIRTAB-I)
                       SET I-21            TO TRUE
                       SET FIRTAB-S        TO FIRTAB-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (NOT-I-20 AND NOT-I-21)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-11)
               SET NOT-I-21                TO TRUE
               IF  AVD = PAVD
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-11 AND NOT-I-21)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-12)
               SET NOT-I-21                TO TRUE
               IF  KUNDNR = PKUNDE
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-12 AND NOT-I-21)
               GO TO SLUTT-T
      *****************************************************************
      *  KREDITNOTA INDIKATOR SETTING.                                *
      *****************************************************************
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  FAKTKR = '2'
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               SET NOT-I-23                TO TRUE
               IF  KRETYP = '2'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-22 AND NOT-I-23)
               SET NOT-I-23                TO TRUE
               IF  KRETYP = '5'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  LAGER = 'PT'
               SET I-25                    TO TRUE
           END-IF
      *****************************************************************
      * SJEKK OM BELØPET SKAL SNUS.                                   *
      *       PRISEN ER SATT TIL MINUS I FAK075, MEN IKKE SELVKOSTEN. *
      *****************************************************************
           DIVIDE EDBNR BY 10000       GIVING EDBNR3
           DIVIDE EDBNR BY 100000      GIVING EDBNR2
           SET NOT-I-98                    TO TRUE
           IF  EDBNR2 = 94
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  EDBNR3 = 995
                   SET I-98                TO TRUE
               END-IF
      *****************************************************************
      * UTREGNING AV NETTO SALGSUM.                                   *
      *****************************************************************
           END-IF
           MULTIPLY ANTLEV BY UTPRIS   GIVING NETTO ROUNDED
           MULTIPLY RAB1 BY NETTO      GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM NETTO ROUNDED
           MULTIPLY RAB2 BY NETTO      GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM NETTO ROUNDED
           MULTIPLY RAB3 BY NETTO      GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM NETTO ROUNDED
           IF  (I-22)
               MULTIPLY -1 BY NETTO    GIVING NETTO
           END-IF
           SET I-10                        TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARAM-IO-AREA (16:3)   TO PFIRMA (1:3)
               MOVE PARAM-IO-AREA (24:1)   TO PAVD (1:1)
               MOVE PARAM-IO-AREA (34:6)   TO PKUNDE (1:6)
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKTF-GET SECTION.
       FAKTF-GET-P.
           IF  FAKTF-EOF-OFF
               READ FAKTF
               AT END
                   SET FAKTF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKTF-FLDSET SECTION.
       FAKTF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTF-IO-AREA (2:1)    TO AVD (1:1)
               MOVE FAKTF-IO-AREA (51:3)   TO FIRMA (1:3)
               MOVE FAKTF-IO-AREA (12:4)   TO ANTLEV-IO
               MOVE FAKTF-IO-AREA (23:3)   TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTF-IO-AREA (26:3)   TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTF-IO-AREA (29:3)   TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTF-IO-AREA (32:9)   TO UTPRIS-IO
               INSPECT UTPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTF-IO-AREA (42:2)   TO MND (1:2)
               MOVE FAKTF-IO-AREA (58:2)   TO AAR (1:2)
               MOVE FAKTF-IO-AREA (45:6)   TO KUNDNR (1:6)
               MOVE FAKTF-IO-AREA (60:5)   TO VGR (1:5)
               MOVE FAKTF-IO-AREA (16:7)   TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTF-IO-AREA (41:1)   TO FAKTKR (1:1)
               MOVE FAKTF-IO-AREA (66:1)   TO KRETYP (1:1)
               MOVE FAKTF-IO-AREA (69:2)   TO LAGER (1:2)
           END-EVALUATE.
 
       FAKTF-IDSET SECTION.
       FAKTF-IDSET-P.
           SET I-02                        TO TRUE.
 
       TABELL-LOAD SECTION.
       TABELL-LOAD-P.
           OPEN INPUT TABELL
           SET FIRTAB-I                    TO 1
           PERFORM UNTIL TABELL-EOF
               READ TABELL
               AT END
                   SET TABELL-EOF          TO TRUE
               NOT AT END
                   MOVE TABELL-IO-AREA (1:3) TO FIRTAB-ENTRY (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (4:3) TO FIRTAB-ENTRY (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (7:3) TO FIRTAB-ENTRY (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (10:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (13:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (16:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (19:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (22:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (25:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (28:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (31:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (34:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (37:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (40:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (43:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (46:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (49:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (52:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (55:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (58:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABELL.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-10)
               MOVE SPACES TO OUTREC-IO-AREA
               INITIALIZE OUTREC-IO-AREA
               MOVE FIRMA                  TO OUTREC-IO-AREA (1:3)
               MOVE KUNDNR                 TO OUTREC-IO-AREA (4:6)
               MOVE VGR                    TO OUTREC-IO-AREA (10:5)
               MOVE AAR                    TO OUTREC-IO-AREA (15:2)
               MOVE MND                    TO OUTREC-IO-AREA (17:2)
               MOVE NETTO                  TO XO-92P
               MOVE XO-92P-EF              TO OUTREC-IO-AREA (19:6)
               INITIALIZE NETTO
               MOVE FAKTKR                 TO OUTREC-IO-AREA (25:1)
               WRITE OUTREC-IO-AREA
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
           PERFORM TABELL-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           INITIALIZE FAKTF-DATA-FIELDS
           SET FAKTF-EOF-OFF               TO TRUE
           SET FAKTF-PROCESS               TO TRUE
           OPEN INPUT FAKTF
           OPEN OUTPUT OUTREC.
           SET FIRTAB-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FAKTF
           CLOSE OUTREC.
 
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
