       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK195R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK195.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT MASTIN
               ASSIGN TO UT-S-MASTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTIN-STATUS.
           SELECT MASTUT
               ASSIGN TO UT-S-MASTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTUT-STATUS.
           SELECT PRF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(70).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD MASTIN
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  MASTIN-IO-AREA.
           05  MASTIN-IO-AREA-X            PICTURE X(80).
       FD MASTUT
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  MASTUT-IO-AREA.
           05  MASTUT-IO-AREA-X            PICTURE X(80).
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
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  MASTIN-STATUS               PICTURE 99 VALUE 0.
           10  MASTUT-STATUS               PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
 
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
               88  MASTIN-EOF-OFF          VALUE '0'.
               88  MASTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTIN-READ-OFF         VALUE '0'.
               88  MASTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTIN-PROCESS-OFF      VALUE '0'.
               88  MASTIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MASTIN-LEVEL-INIT-OFF   VALUE '0'.
               88  MASTIN-LEVEL-INIT       VALUE '1'.
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
           05  INFILE-LEVEL-01.
               10  INFILE-01-L2.
                   15  INFILE-01-L2-FIRMA  PICTURE X(3).
               10  INFILE-01-L1.
                   15  INFILE-01-L1-ENR1   PICTURE X(7).
           05  INFILE-DATA-FIELDS.
               10  LAGERK                  PICTURE X(2).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ENR1                    PICTURE X(7).
               10  RABA-IO.
                   15  RABA                PICTURE S9(2)V9(1).
               10  RABB-IO.
                   15  RABB                PICTURE S9(2)V9(1).
               10  RABC-IO.
                   15  RABC                PICTURE S9(2)V9(1).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  FAKT                    PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  KRTYPE                  PICTURE X(1).
           05  INFILE-MP                   PICTURE X(10).
           05  INFILE-MC                   PICTURE X(10).
           05  INFILE-M-01             REDEFINES INFILE-MC.
               10  INFILE-M-01-M2.
                   15  INFILE-M-01-M2-FIRMA-G.
                       20  INFILE-M-01-M2-FIRMA PICTURE X(3).
               10  INFILE-M-01-M1.
                   15  INFILE-M-01-M1-ENR1-G.
                       20  INFILE-M-01-M1-ENR1 PICTURE X(7).
           05  FAKPAR-DATA-FIELDS.
               10  MND                     PICTURE X(2).
           05  MASTIN-LEVEL-02.
               10  MASTIN-02-L2.
                   15  MASTIN-02-L2-FIRMA  PICTURE X(3).
               10  MASTIN-02-L1.
                   15  MASTIN-02-L1-ENR2   PICTURE X(7).
           05  MASTIN-DATA-FIELDS.
               10  ENR2                    PICTURE X(7).
               10  JAN-IO.
                   15  JAN                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FEB-IO.
                   15  FEB                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  MARS-IO.
                   15  MARS                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  APRIL-IO.
                   15  APRIL               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  MAI-IO.
                   15  MAI                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  JUNI-IO.
                   15  JUNI                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  JULI-IO.
                   15  JULI                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  AUG-IO.
                   15  AUG                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SEPT-IO.
                   15  SEPT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  OKT-IO.
                   15  OKT                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NOV-IO.
                   15  NOV                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  DES-IO.
                   15  DES                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SALG-IO.
                   15  SALG                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  MASTIN-MP                   PICTURE X(10).
           05  MASTIN-MC                   PICTURE X(10).
           05  MASTIN-M-02             REDEFINES MASTIN-MC.
               10  MASTIN-M-02-M2.
                   15  MASTIN-M-02-M2-FIRMA-G.
                       20  MASTIN-M-02-M2-FIRMA PICTURE X(3).
               10  MASTIN-M-02-M1.
                   15  MASTIN-M-02-M1-ENR2-G.
                       20  MASTIN-M-02-M1-ENR2 PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  SIAAR-IO.
                   15  SIAAR               PICTURE S9(7)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(9)V9(2).
               10  SUMA-IO.
                   15  SUMA                PICTURE S9(9)V9(2).
               10  EDBNR                   PICTURE X(7).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(10).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(7)V9(2).
               10  M1-IO.
                   15  M1                  PICTURE S9(7)V9(2).
               10  M2-IO.
                   15  M2                  PICTURE S9(7)V9(2).
               10  M3-IO.
                   15  M3                  PICTURE S9(7)V9(2).
               10  M4-IO.
                   15  M4                  PICTURE S9(7)V9(2).
               10  M5-IO.
                   15  M5                  PICTURE S9(7)V9(2).
               10  M6-IO.
                   15  M6                  PICTURE S9(7)V9(2).
               10  M7-IO.
                   15  M7                  PICTURE S9(7)V9(2).
               10  M8-IO.
                   15  M8                  PICTURE S9(7)V9(2).
               10  M9-IO.
                   15  M9                  PICTURE S9(7)V9(2).
               10  M10-IO.
                   15  M10                 PICTURE S9(7)V9(2).
               10  M11-IO.
                   15  M11                 PICTURE S9(7)V9(2).
               10  M12-IO.
                   15  M12                 PICTURE S9(7)V9(2).
               10  ANTGML-IO.
                   15  ANTGML              PICTURE S9(10).
               10  ANTM-IO.
                   15  ANTM                PICTURE S9(10).
               10  SIA-ELGR-IO.
                   15  SIA-ELGR            PICTURE S9(7)V9(2).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(10).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-100YY9               PICTURE Z.ZZZ.ZZZ.ZZ9.
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
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
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   PERFORM INFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM INFILE-MATCH-SET
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
 
           IF  MASTIN-PROCESS
               SET MASTIN-PROCESS-OFF      TO TRUE
               SET MASTIN-READ             TO TRUE
           END-IF
 
           IF  MASTIN-READ
               PERFORM MASTIN-GET
               SET MASTIN-READ-OFF         TO TRUE
               IF  NOT MASTIN-EOF
                   PERFORM MASTIN-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM MASTIN-MATCH-SET
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
           END-IF
 
           IF  MASTIN-PROCESS
               PERFORM MASTIN-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
           END-IF
 
           IF  MASTIN-PROCESS
               PERFORM MASTIN-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDOFF
               PERFORM INFILE-FLDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
           END-IF
 
           IF  MASTIN-PROCESS
               PERFORM MASTIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
           OR  MASTIN-PROCESS
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
               MOVE 0                      TO SIAAR
               SET NOT-I-60                TO TRUE
           END-IF
           IF  (I-02)
               GO TO HOPP2-T
           END-IF
           IF  (I-01)
               GO TO START-X-T
           END-IF
           IF  (I-03)
               SET NOT-I-21                TO TRUE
               IF  MND = '01'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  MND = '02'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  MND = '03'
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  MND = '04'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  MND = '05'
                   SET I-25                TO TRUE
               END-IF
               SET NOT-I-26                TO TRUE
               IF  MND = '06'
                   SET I-26                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  MND = '07'
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-28                TO TRUE
               IF  MND = '08'
                   SET I-28                TO TRUE
               END-IF
               SET NOT-I-29                TO TRUE
               IF  MND = '09'
                   SET I-29                TO TRUE
               END-IF
               SET NOT-I-30                TO TRUE
               IF  MND = '10'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  MND = '11'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  MND = '12'
                   SET I-32                TO TRUE
               END-IF
               GO TO UT-T
           END-IF.
 
       START-X-T.
           MOVE 0                          TO SUM-X
           MOVE 0                          TO SUMA
           MOVE ENR1                       TO EDBNR
           ADD 1                           TO ANT
           ADD ANTUT TO ZERO           GIVING ANTALL
           SET NOT-I-10                    TO TRUE
           IF  FAKT = '1'
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-12                TO TRUE
               IF  KRTYPE = '2'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  LAGERK = 'PT'
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-10 AND I-12)
               MULTIPLY -1 BY ANTALL   GIVING ANTALL
           END-IF
           IF  (NOT-I-10 AND NOT-I-12)
               MOVE 0                      TO ANTALL
           END-IF
           IF  (I-15)
               MOVE 0                      TO ANTALL
           END-IF
           SET I-60                        TO TRUE
           IF  (I-21)
               ADD ANTALL                  TO M1
           END-IF
           IF  (I-22)
               ADD ANTALL                  TO M2
           END-IF
           IF  (I-23)
               ADD ANTALL                  TO M3
           END-IF
           IF  (I-24)
               ADD ANTALL                  TO M4
           END-IF
           IF  (I-25)
               ADD ANTALL                  TO M5
           END-IF
           IF  (I-26)
               ADD ANTALL                  TO M6
           END-IF
           IF  (I-27)
               ADD ANTALL                  TO M7
           END-IF
           IF  (I-28)
               ADD ANTALL                  TO M8
           END-IF
           IF  (I-29)
               ADD ANTALL                  TO M9
           END-IF
           IF  (I-30)
               ADD ANTALL                  TO M10
           END-IF
           IF  (I-31)
               ADD ANTALL                  TO M11
           END-IF
           IF  (I-32)
               ADD ANTALL                  TO M12
      *
      *    RUTINE FOR Å DANNE SALG HITTIL I ÅR.  (STEIN 18.04.85)
           END-IF
           IF  (I-77)
               ADD PRIS TO ZERO        GIVING SUM-X
           END-IF
           IF  (NOT-I-77)
               MULTIPLY PRIS BY ANTUT  GIVING SUM-X
           END-IF
           IF  (NOT-I-50)
               MULTIPLY RABA BY SUM-X  GIVING SUMA ROUNDED
               DIVIDE SUMA BY 100      GIVING SUMA ROUNDED
               SUBTRACT SUMA               FROM SUM-X
           END-IF
           IF  (NOT-I-51)
               MULTIPLY RABB BY SUM-X  GIVING SUMA ROUNDED
               DIVIDE SUMA BY 100      GIVING SUMA ROUNDED
               SUBTRACT SUMA               FROM SUM-X
           END-IF
           IF  (NOT-I-52)
               MULTIPLY RABC BY SUM-X  GIVING SUMA ROUNDED
               DIVIDE SUMA BY 100      GIVING SUMA ROUNDED
               SUBTRACT SUMA               FROM SUM-X
           END-IF
           IF  (NOT-I-10 AND I-12)
               SUBTRACT SUM-X              FROM SIAAR
           END-IF
           ADD SUM-X                       TO SIAAR
           GO TO UT-T.
 
       HOPP2-T.
           SET I-60                        TO TRUE
           ADD 1                           TO ANTGML
           ADD JAN                         TO M1
           ADD FEB                         TO M2
           ADD MARS                        TO M3
           ADD APRIL                       TO M4
           ADD MAI                         TO M5
           ADD JUNI                        TO M6
           ADD JULI                        TO M7
           ADD AUG                         TO M8
           ADD SEPT                        TO M9
           ADD OKT                         TO M10
           ADD NOV                         TO M11
           ADD DES                         TO M12
           ADD SALG                        TO SIAAR
           MOVE ENR2                       TO EDBNR.
 
       UT-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-60)
               ADD 1                       TO ANTM
               ADD SIAAR                   TO SIA-ELGR
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           SUBTRACT ANTGML FROM ANTM   GIVING ANTNYE.
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDOFF SECTION.
       INFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '8' )
               SET NOT-I-77                TO TRUE
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-52                TO TRUE
           END-EVALUATE.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '8' )
               MOVE INFILE-IO-AREA (69:2)  TO LAGERK (1:2)
               MOVE INFILE-IO-AREA (12:4)  TO ANTUT-IO
               IF  ANTUT = ZERO
                   SET I-77                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (16:7)  TO ENR1 (1:7)
               MOVE INFILE-IO-AREA (23:3)  TO RABA-IO
               INSPECT RABA-IO REPLACING ALL ' ' BY '0'
               IF  RABA = ZERO
                   SET I-50                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (26:3)  TO RABB-IO
               INSPECT RABB-IO REPLACING ALL ' ' BY '0'
               IF  RABB = ZERO
                   SET I-51                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (29:3)  TO RABC-IO
               INSPECT RABC-IO REPLACING ALL ' ' BY '0'
               IF  RABC = ZERO
                   SET I-52                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (32:9)  TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (41:1)  TO FAKT (1:1)
               MOVE INFILE-IO-AREA (51:3)  TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (66:1)  TO KRTYPE (1:1)
           END-EVALUATE.
 
       INFILE-IDCHK SECTION.
       INFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '8' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '8' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '8' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (51:3)  TO INFILE-01-L2-FIRMA
               MOVE INFILE-IO-AREA (16:7)  TO INFILE-01-L1-ENR1
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
 
       INFILE-MATCH-SET SECTION.
       INFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '8' )
               MOVE INFILE-IO-AREA (51:3)  TO INFILE-M-01-M2-FIRMA
               MOVE INFILE-IO-AREA (16:7)  TO INFILE-M-01-M1-ENR1
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
               MOVE FAKPAR-IO-AREA (8:2)   TO MND (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       MASTIN-GET SECTION.
       MASTIN-GET-P.
           IF  MASTIN-EOF-OFF
               READ MASTIN
               AT END
                   SET MASTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MASTIN-FLDSET SECTION.
       MASTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( MASTIN-IO-AREA (1:1) = '9' )
               MOVE MASTIN-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE MASTIN-IO-AREA (5:7)   TO ENR2 (1:7)
               MOVE MASTIN-IO-AREA (12:5)  TO JAN-IO
               MOVE MASTIN-IO-AREA (17:5)  TO FEB-IO
               MOVE MASTIN-IO-AREA (22:5)  TO MARS-IO
               MOVE MASTIN-IO-AREA (27:5)  TO APRIL-IO
               MOVE MASTIN-IO-AREA (32:5)  TO MAI-IO
               MOVE MASTIN-IO-AREA (37:5)  TO JUNI-IO
               MOVE MASTIN-IO-AREA (42:5)  TO JULI-IO
               MOVE MASTIN-IO-AREA (47:5)  TO AUG-IO
               MOVE MASTIN-IO-AREA (52:5)  TO SEPT-IO
               MOVE MASTIN-IO-AREA (57:5)  TO OKT-IO
               MOVE MASTIN-IO-AREA (62:5)  TO NOV-IO
               MOVE MASTIN-IO-AREA (67:5)  TO DES-IO
               MOVE MASTIN-IO-AREA (72:5)  TO SALG-IO
           END-EVALUATE.
 
       MASTIN-IDCHK SECTION.
       MASTIN-IDCHK-P.
           EVALUATE TRUE
           WHEN ( MASTIN-IO-AREA (1:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       MASTIN-IDSET SECTION.
       MASTIN-IDSET-P.
           EVALUATE TRUE
           WHEN ( MASTIN-IO-AREA (1:1) = '9' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       MASTIN-CHK-LEVEL SECTION.
       MASTIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( MASTIN-IO-AREA (1:1) = '9' )
               MOVE LOW-VALUES             TO MASTIN-LEVEL-02
               MOVE MASTIN-IO-AREA (2:3)   TO MASTIN-02-L2-FIRMA
               MOVE MASTIN-IO-AREA (5:7)   TO MASTIN-02-L1-ENR2
               IF  MASTIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MASTIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  MASTIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MASTIN-02-L2          TO THE-PRIOR-L2
               MOVE  MASTIN-02-L1          TO THE-PRIOR-L1
               SET MASTIN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       MASTIN-MATCH-SET SECTION.
       MASTIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( MASTIN-IO-AREA (1:1) = '9' )
               MOVE MASTIN-IO-AREA (2:3)   TO MASTIN-M-02-M2-FIRMA
               MOVE MASTIN-IO-AREA (5:7)   TO MASTIN-M-02-M1-ENR2
           END-EVALUATE.
 
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  INFILE-EOF
               MOVE HIGH-VALUES            TO INFILE-MC
                                              INFILE-MP
           END-IF
           IF  MASTIN-EOF
               MOVE HIGH-VALUES            TO MASTIN-MC
                                              MASTIN-MP
           END-IF
           IF  INFILE-MC < INFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  MASTIN-MC < MASTIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INFILE-MC < MASTIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INFILE-PROCESS      TO TRUE
                   MOVE INFILE-MC          TO INFILE-MP
                   IF  INFILE-MC = MASTIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  MASTIN-MC < INFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MASTIN-PROCESS      TO TRUE
                   MOVE MASTIN-MC          TO MASTIN-MP
                   IF  MASTIN-MC = INFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INFILE-MC = MASTIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INFILE-PROCESS      TO TRUE
                   MOVE INFILE-MC          TO INFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-60)
               MOVE SPACES TO MASTUT-IO-AREA
               INITIALIZE MASTUT-IO-AREA
               MOVE '9'                    TO MASTUT-IO-AREA (1:1)
               MOVE FIRMA                  TO MASTUT-IO-AREA (2:3)
               MOVE EDBNR                  TO MASTUT-IO-AREA (5:7)
               MOVE M1                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (12:5)
               INITIALIZE M1
               MOVE M2                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (17:5)
               INITIALIZE M2
               MOVE M3                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (22:5)
               INITIALIZE M3
               MOVE M4                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (27:5)
               INITIALIZE M4
               MOVE M5                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (32:5)
               INITIALIZE M5
               MOVE M6                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (37:5)
               INITIALIZE M6
               MOVE M7                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (42:5)
               INITIALIZE M7
               MOVE M8                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (47:5)
               INITIALIZE M8
               MOVE M9                     TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (52:5)
               INITIALIZE M9
               MOVE M10                    TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (57:5)
               INITIALIZE M10
               MOVE M11                    TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (62:5)
               INITIALIZE M11
               MOVE M12                    TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (67:5)
               INITIALIZE M12
               MOVE SIA-ELGR               TO XO-72P
               MOVE XO-72P-EF              TO MASTUT-IO-AREA (72:5)
               INITIALIZE SIA-ELGR
               WRITE MASTUT-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO PRF-IO-AREA (1:24)
               MOVE ' FRA FAK195   '       TO PRF-IO-AREA (25:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (38:8)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANT                    TO XO-100YY9
               MOVE XO-100YY9              TO PRF-IO-AREA (3:13)
               MOVE 'VARERECORDS LEST'     TO PRF-IO-AREA (18:16)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTNYE                 TO XO-100YY9
               MOVE XO-100YY9              TO PRF-IO-AREA (3:13)
               MOVE 'NYE VARESTAT. RECORDS' TO PRF-IO-AREA (17:21)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTM                   TO XO-100YY9
               MOVE XO-100YY9              TO PRF-IO-AREA (3:13)
               MOVE 'RECORDS PÅ NY'        TO PRF-IO-AREA (17:13)
               MOVE 'VARESTAT.MASTER'      TO PRF-IO-AREA (31:15)
               MOVE 3                      TO PRF-AFTER-SPACE
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
           MOVE 2                          TO LR-CHECK
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INFILE-MC
                                              INFILE-MP
           OPEN INPUT INFILE
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           SET MASTIN-LEVEL-INIT           TO TRUE
           INITIALIZE MASTIN-DATA-FIELDS
           SET MASTIN-EOF-OFF              TO TRUE
           SET MASTIN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO MASTIN-MC
                                              MASTIN-MP
           OPEN INPUT MASTIN
           OPEN OUTPUT MASTUT
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
           CLOSE FAKPAR
           CLOSE MASTIN
           CLOSE MASTUT
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
