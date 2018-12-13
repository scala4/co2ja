       IDENTIFICATION DIVISION.
       PROGRAM-ID. BEM002R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: BEM002                                          *
      *  PROGRAMERER: ELIN                                            *
      *  PROGRAMERT.: 04.10.2007                                      *
      *                                                               *
      * PROGRAMMET DANNER OVERSIKT OVER RECORDS SOM ER ENDRET SIDEN   *     0080
      * FORRIGE KJØRING                                               *     0090
      *  ENDR.DATO   TEKST.                                           *
      *  XX.XX.XXXX                                                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BEM002.rpg
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
           SELECT VAREBUP
               ASSIGN TO UT-S-VAREBUP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREBUP-STATUS.
           SELECT VAREMAS
               ASSIGN TO UT-S-VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT BEHUT
               ASSIGN TO UT-S-BEHUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BEHUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREBUP
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VAREBUP-IO-AREA.
           05  VAREBUP-IO-AREA-X           PICTURE X(200).
       FD VAREMAS
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD VAREREC
               BLOCK CONTAINS 9400
               RECORD CONTAINS 200.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(200).
       FD BEHUT
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  BEHUT-IO-AREA.
           05  BEHUT-IO-AREA-X             PICTURE X(200).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREBUP-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  BEHUT-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-EOF-OFF         VALUE '0'.
               88  VAREBUP-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-READ-OFF        VALUE '0'.
               88  VAREBUP-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-PROCESS-OFF     VALUE '0'.
               88  VAREBUP-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  VAREBUP-DATA-FIELDS.
               10  EDBKEY                  PICTURE X(10).
               10  BUPREC                  PICTURE X(200).
           05  VAREBUP-MP                  PICTURE X(10).
           05  VAREBUP-MC                  PICTURE X(10).
           05  VAREBUP-M-01            REDEFINES VAREBUP-MC.
               10  VAREBUP-M-01-M1.
                   15  VAREBUP-M-01-M1-EDBKEY-G.
                       20  VAREBUP-M-01-M1-EDBKEY PICTURE X(10).
           05  VAREMAS-DATA-FIELDS.
               10  EDBNR                   PICTURE X(7).
               10  FIRMA                   PICTURE X(3).
               10  NYREC                   PICTURE X(200).
               10  ALFA                    PICTURE X(3).
               10  ART17                   PICTURE X(17).
               10  VAREN                   PICTURE X(30).
               10  UPRIS-IO.
                   15  UPRIS               PICTURE S9(7)V9(2).
               10  VRG                     PICTURE X(5).
               10  MRK                     PICTURE X(1).
               10  LOC                     PICTURE X(6).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUTN-IO.
                   15  ANTUTN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PTEK                    PICTURE X(1).
               10  KARTI-IO.
                   15  KARTI               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  PANT-IO.
                   15  PANT                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LAG13N-IO.
                   15  LAG13N              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG93N-IO.
                   15  LAG93N              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG15N-IO.
                   15  LAG15N              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG17N-IO.
                   15  LAG17N              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG92N-IO.
                   15  LAG92N              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG18N-IO.
                   15  LAG18N              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
      *
           05  VAREMAS-MP                  PICTURE X(10).
           05  VAREMAS-MC                  PICTURE X(10).
           05  VAREMAS-M-02            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-02-M1.
                   15  VAREMAS-M-02-M1-EDBKEY-G.
                       20  VAREMAS-M-02-M1-EDBKEY PICTURE X(10).
           05  VARETIL-DATA-FIELDS.
               10  ENHBET                  PICTURE X(3).
               10  EANK                    PICTURE X(14).
           05  TEMPORARY-FIELDS.
               10  BEHN-IO.
                   15  BEHN                PICTURE S9(6)V9(2).
               10  KEY-X                   PICTURE X(12).
               10  ANTE1-IO.
                   15  ANTE1               PICTURE S9(6).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(6).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(6).
               10  TOTNMR-IO.
                   15  TOTNMR              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  EDIT-UPRIS              PICTURE Z999999,9.99.
               10  EDIT-BEHN               PICTURE Z99999,9.99.
               10  XO-50YN9                PICTURE ZZZZ9.
               10  XO-52YY9                PICTURE ZZ.ZZZ,99.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREBUP-PROCESS
               SET VAREBUP-PROCESS-OFF     TO TRUE
               SET VAREBUP-READ            TO TRUE
           END-IF
 
           IF  VAREBUP-READ
               PERFORM VAREBUP-GET
               SET VAREBUP-READ-OFF        TO TRUE
               IF  NOT VAREBUP-EOF
                   PERFORM VAREBUP-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   PERFORM VAREMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  VAREBUP-PROCESS
               PERFORM VAREBUP-IDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
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
 
           IF  VAREBUP-PROCESS
               PERFORM VAREBUP-FLDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDSET
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
           IF  (I-02)
               SUBTRACT ANTUTN FROM ANTINN GIVING BEHN
               SET NOT-I-12                TO TRUE
               IF  BEHN < 0
                   SET I-12                TO TRUE
               END-IF
               SUBTRACT LAG13N             FROM BEHN
               SUBTRACT LAG93N             FROM BEHN
               SUBTRACT LAG15N             FROM BEHN
               SUBTRACT LAG17N             FROM BEHN
               SUBTRACT LAG92N             FROM BEHN
               SUBTRACT LAG18N             FROM BEHN
               MOVE '80'                   TO KEY-X (1:2)
               MOVE EDBKEY                 TO KEY-X (3:10)
               MOVE KEY-X                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM VARETIL-FLDSET
                   PERFORM VARETIL-IDSET
               END-READ
               SET NOT-I-21                TO TRUE
               IF  PTEK = 'A'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  PTEK = 'B'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  PTEK = 'C'
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  PTEK = 'D'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  PTEK = 'E'
                   SET I-25                TO TRUE
               END-IF
               SET NOT-I-26                TO TRUE
               IF  PTEK = 'F'
                   SET I-26                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  PTEK = 'G'
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-28                TO TRUE
               IF  PTEK = 'H'
                   SET I-28                TO TRUE
               END-IF
               SET NOT-I-29                TO TRUE
               IF  PTEK = 'I'
                   SET I-29                TO TRUE
               END-IF
               SET NOT-I-30                TO TRUE
               IF  PTEK = 'J'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  PTEK = 'K'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  PTEK = 'L'
                   SET I-32                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               IF  PTEK = 'M'
                   SET I-33                TO TRUE
               END-IF
               SET NOT-I-34                TO TRUE
               IF  PTEK = 'N'
                   SET I-34                TO TRUE
               END-IF
               SET NOT-I-35                TO TRUE
               IF  PTEK = 'P'
                   SET I-35                TO TRUE
               END-IF
               SET NOT-I-36                TO TRUE
               IF  PTEK = 'U'
                   SET I-36                TO TRUE
               END-IF
               SET NOT-I-37                TO TRUE
               IF  PTEK = 'Q'
                   SET I-37                TO TRUE
               END-IF
               SET NOT-I-38                TO TRUE
               IF  PTEK = 'R'
                   SET I-38                TO TRUE
               END-IF
               SET NOT-I-39                TO TRUE
               IF  PTEK = 'S'
                   SET I-39                TO TRUE
               END-IF
               SET NOT-I-40                TO TRUE
               IF  PTEK = 'T'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  PTEK = 'O'
                   SET I-41                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  PTEK = 'V'
                   SET I-42                TO TRUE
               END-IF
               SET NOT-I-43                TO TRUE
               IF  PTEK = 'W'
                   SET I-43                TO TRUE
               END-IF
               SET NOT-I-44                TO TRUE
               IF  PTEK = 'Y'
                   SET I-44                TO TRUE
               END-IF
               SET NOT-I-45                TO TRUE
               IF  PTEK = 'Z'
                   SET I-45                TO TRUE
               END-IF
               SET NOT-I-46                TO TRUE
               IF  PTEK = 'X'
                   SET I-46                TO TRUE
               END-IF
               SET NOT-I-47                TO TRUE
               IF  PTEK = 'Æ'
                   SET I-47                TO TRUE
               END-IF
               SET NOT-I-48                TO TRUE
               IF  PTEK = 'Ø'
                   SET I-48                TO TRUE
               END-IF
               SET NOT-I-49                TO TRUE
               IF  PTEK = 'Å'
                   SET I-49                TO TRUE
               END-IF
      *
      *
           END-IF
           IF  (I-02 AND I-MR)
               SET NOT-I-10                TO TRUE
               IF  BUPREC = NYREC
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-MR AND NOT-I-10)
               ADD 1                       TO ANTE1
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTMR
           END-IF
           IF  (I-01 AND NOT-I-MR)
               ADD 1                       TO ANTNMR
               ADD 1                       TO TOTNMR
           END-IF.
 
       VAREBUP-GET SECTION.
       VAREBUP-GET-P.
           IF  VAREBUP-EOF-OFF
               READ VAREBUP
               AT END
                   SET VAREBUP-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREBUP-FLDSET SECTION.
       VAREBUP-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREBUP-IO-AREA (3:10) TO EDBKEY (1:10)
               MOVE VAREBUP-IO-AREA (1:200) TO BUPREC (1:200)
           END-EVALUATE.
 
       VAREBUP-IDSET SECTION.
       VAREBUP-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREBUP-MATCH-SET SECTION.
       VAREBUP-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREBUP-IO-AREA (3:10) TO VAREBUP-M-01-M1-EDBKEY
           END-EVALUATE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:10) TO EDBKEY (1:10)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREMAS-IO-AREA (1:200) TO NYREC (1:200)
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:17) TO ART17 (1:17)
               MOVE VAREMAS-IO-AREA (36:30) TO VAREN (1:30)
               MOVE VAREMAS-IO-AREA (75:9) TO UPRIS-IO
               INSPECT UPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (118:5) TO VRG (1:5)
               MOVE VAREMAS-IO-AREA (127:1) TO MRK (1:1)
               MOVE VAREMAS-IO-AREA (140:6) TO LOC (1:6)
               MOVE VAREMAS-IO-AREA (97:5) TO ANTINN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO ANTUTN-IO
               MOVE VAREMAS-IO-AREA (107:1) TO PTEK (1:1)
               MOVE VAREMAS-IO-AREA (108:3) TO KARTI-IO
               MOVE VAREMAS-IO-AREA (161:4) TO PANT-IO
               MOVE VAREMAS-IO-AREA (179:3) TO LAG13N-IO
               MOVE VAREMAS-IO-AREA (182:3) TO LAG93N-IO
               MOVE VAREMAS-IO-AREA (185:3) TO LAG15N-IO
               MOVE VAREMAS-IO-AREA (188:3) TO LAG17N-IO
               MOVE VAREMAS-IO-AREA (191:3) TO LAG92N-IO
               MOVE VAREMAS-IO-AREA (194:3) TO LAG18N-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:10) TO VAREMAS-M-02-M1-EDBKEY
           END-EVALUATE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (63:3) TO ENHBET (1:3)
               MOVE VARETIL-IO-AREA (148:14) TO EANK (1:14)
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-03                        TO TRUE.
 
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
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VAREBUP-EOF
               MOVE HIGH-VALUES            TO VAREBUP-MC
                                              VAREBUP-MP
           END-IF
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  VAREBUP-MC < VAREBUP-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREBUP-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREBUP-PROCESS     TO TRUE
                   MOVE VAREBUP-MC         TO VAREBUP-MP
                   IF  VAREBUP-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC < VAREBUP-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = VAREBUP-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREBUP-MC = VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREBUP-PROCESS     TO TRUE
                   MOVE VAREBUP-MC         TO VAREBUP-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE NYREC                  TO VAREREC-IO-AREA (1:200)
      *EHUT   H        1P                                                 000290
      *                                   5 "VGRP"                          0300
      *                                   6 ";"                             0300
      *                                  26 "ARTIKKELNR"                    0300
      *                                  27 ";"
      *                                  57 "VARE BETEGNELSE"
      *                                  58 ";"
      *                                  69 "UTS.PRIS"
      *                                  70 ";"
      *                                  78 "BEHOLD"
      *                                  81 ";"
      *                                  82 "M"
      *                                  83 ";"
      *                                  98 "LOC."
      *                                  99 ";"
      *                                 109 "KJØREDAG"
      *                                 112 ";"
      *                                 115 "FNR"
      *                                 116 ";"
      *                                 130 "EANKODE   "
      *                                 131 ";"
      *                                 134 "ENH"
      *                                 135 ";"
      *                                 140 "KINNH"
      *                                 141 ";"
      *                                 150 "PTIL. "
      *                                 151 ";"
      *                                 175 "PRISTILLEGSTEKST        "
      *                                 176 ";"
      *                                 177 "P"
      *                                 178 ";"
      *                                 185 "EDB.NR."
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-10)
               MOVE SPACES TO BEHUT-IO-AREA
               INITIALIZE BEHUT-IO-AREA
               MOVE VRG                    TO BEHUT-IO-AREA (1:5)
               MOVE ';'                    TO BEHUT-IO-AREA (6:1)
               MOVE ALFA                   TO BEHUT-IO-AREA (7:3)
               MOVE ';'                    TO BEHUT-IO-AREA (10:1)
               MOVE ART17                  TO BEHUT-IO-AREA (11:17)
               MOVE ';'                    TO BEHUT-IO-AREA (28:1)
               MOVE VAREN                  TO BEHUT-IO-AREA (29:30)
               MOVE ';'                    TO BEHUT-IO-AREA (59:1)
               MOVE UPRIS                  TO EDIT-UPRIS
               MOVE EDIT-UPRIS             TO BEHUT-IO-AREA (59:12)
               MOVE ';'                    TO BEHUT-IO-AREA (71:1)
               IF  (I-12)
                   MOVE '-'                TO BEHUT-IO-AREA (72:1)
               END-IF
               MOVE BEHN                   TO EDIT-BEHN
               MOVE EDIT-BEHN              TO BEHUT-IO-AREA (72:11)
               MOVE ';'                    TO BEHUT-IO-AREA (83:1)
               MOVE MRK                    TO BEHUT-IO-AREA (84:1)
               MOVE ';'                    TO BEHUT-IO-AREA (85:1)
               MOVE LOC                    TO BEHUT-IO-AREA (93:6)
               MOVE ';'                    TO BEHUT-IO-AREA (99:1)
               MOVE '2005'                 TO BEHUT-IO-AREA (100:4)
               MOVE '-'                    TO BEHUT-IO-AREA (104:1)
               MOVE UYEAR                  TO BEHUT-IO-AREA (102:2)
               MOVE UMONTH                 TO BEHUT-IO-AREA (105:2)
               MOVE '-'                    TO BEHUT-IO-AREA (107:1)
               MOVE UDAY                   TO BEHUT-IO-AREA (108:2)
               MOVE ';'                    TO BEHUT-IO-AREA (112:1)
               MOVE FIRMA                  TO BEHUT-IO-AREA (113:3)
               MOVE ';'                    TO BEHUT-IO-AREA (116:1)
               IF  (NOT-I-16)
                   MOVE EANK               TO BEHUT-IO-AREA (117:14)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (131:1)
               IF  (NOT-I-16)
                   MOVE ENHBET             TO BEHUT-IO-AREA (132:3)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (135:1)
               MOVE KARTI                  TO XO-50YN9
               MOVE XO-50YN9               TO BEHUT-IO-AREA (136:5)
               MOVE ';'                    TO BEHUT-IO-AREA (141:1)
               MOVE PANT                   TO XO-52YY9
               MOVE XO-52YY9               TO BEHUT-IO-AREA (142:9)
               MOVE ';'                    TO BEHUT-IO-AREA (151:1)
               IF  (I-21)
                   MOVE 'Importør er utsolgt    ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-22)
                   MOVE 'Bly/Metallavgift       ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-23)
                   MOVE 'Bly/Metall og miljøavgi' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-24)
                   MOVE 'MILJØAVGIFT PERSONBILDE' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-25)
                   MOVE 'Sonetillegg og miljøavg' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-26)
                   MOVE 'Frakttillegg           ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-27)
                   MOVE 'Miljø og N.R.K. avgift ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-28)
                   MOVE 'HK-Avgift.             ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-29)
                   MOVE 'Piggtillegg            ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-30)
                   MOVE 'Miljø og embalasjeavgif' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-31)
                   MOVE 'Kassett avgift (TONO)  ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-32)
                   MOVE 'MILJØAVGIFT LASTEBILDEK' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-33)
                   MOVE 'Miljøavgift            ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-34)
                   MOVE 'N.R.K avgift           ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-35)
                   MOVE 'Pant                   ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-36)
                   MOVE 'Pant og miljøavgift    ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-37)
                   MOVE 'Smøreoljeavgift.       ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-38)
                   MOVE 'Miljø og frakt/assurans' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-39)
                   MOVE 'Stamme-tillegg         ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-40)
                   MOVE 'For-avgift.            ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-41)
                   MOVE 'Legerings-tillegg      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-42)
                   MOVE 'Verksted materiell     ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-43)
                   MOVE 'Stammetillegg og miljøa' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-44)
                   MOVE 'Parfymeavgift          ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-45)
                   MOVE 'Emballasje-og smøreolje' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-46)
                   MOVE 'ADR 1203-3,3B   1      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-47)
                   MOVE 'ADR 1993-3,31C  2      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-48)
                   MOVE 'ADR 1993-3,3B   3      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-49)
                   MOVE 'ADR 1300-3,31C  4      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (176:1)
               MOVE PTEK                   TO BEHUT-IO-AREA (177:1)
               MOVE ';'                    TO BEHUT-IO-AREA (178:1)
               MOVE EDBNR                  TO BEHUT-IO-AREA (179:7)
               WRITE BEHUT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO BEHUT-IO-AREA
               INITIALIZE BEHUT-IO-AREA
               MOVE VRG                    TO BEHUT-IO-AREA (1:5)
               MOVE ';'                    TO BEHUT-IO-AREA (6:1)
               MOVE ALFA                   TO BEHUT-IO-AREA (7:3)
               MOVE ';'                    TO BEHUT-IO-AREA (10:1)
               MOVE ART17                  TO BEHUT-IO-AREA (11:17)
               MOVE ';'                    TO BEHUT-IO-AREA (28:1)
               MOVE VAREN                  TO BEHUT-IO-AREA (29:30)
               MOVE ';'                    TO BEHUT-IO-AREA (59:1)
               MOVE UPRIS                  TO EDIT-UPRIS
               MOVE EDIT-UPRIS             TO BEHUT-IO-AREA (59:12)
               MOVE ';'                    TO BEHUT-IO-AREA (71:1)
               IF  (I-12)
                   MOVE '-'                TO BEHUT-IO-AREA (72:1)
               END-IF
               MOVE BEHN                   TO EDIT-BEHN
               MOVE EDIT-BEHN              TO BEHUT-IO-AREA (72:11)
               MOVE ';'                    TO BEHUT-IO-AREA (83:1)
               MOVE MRK                    TO BEHUT-IO-AREA (84:1)
               MOVE ';'                    TO BEHUT-IO-AREA (85:1)
               MOVE LOC                    TO BEHUT-IO-AREA (93:6)
               MOVE ';'                    TO BEHUT-IO-AREA (99:1)
               MOVE '2005'                 TO BEHUT-IO-AREA (100:4)
               MOVE '-'                    TO BEHUT-IO-AREA (104:1)
               MOVE UYEAR                  TO BEHUT-IO-AREA (102:2)
               MOVE UMONTH                 TO BEHUT-IO-AREA (105:2)
               MOVE '-'                    TO BEHUT-IO-AREA (107:1)
               MOVE UDAY                   TO BEHUT-IO-AREA (108:2)
               MOVE ';'                    TO BEHUT-IO-AREA (112:1)
               MOVE FIRMA                  TO BEHUT-IO-AREA (113:3)
               MOVE ';'                    TO BEHUT-IO-AREA (116:1)
               IF  (NOT-I-16)
                   MOVE EANK               TO BEHUT-IO-AREA (117:14)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (131:1)
               IF  (NOT-I-16)
                   MOVE ENHBET             TO BEHUT-IO-AREA (132:3)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (135:1)
               MOVE KARTI                  TO XO-50YN9
               MOVE XO-50YN9               TO BEHUT-IO-AREA (136:5)
               MOVE ';'                    TO BEHUT-IO-AREA (141:1)
               MOVE PANT                   TO XO-52YY9
               MOVE XO-52YY9               TO BEHUT-IO-AREA (142:9)
               MOVE ';'                    TO BEHUT-IO-AREA (151:1)
               IF  (I-21)
                   MOVE 'Importør er utsolgt    ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-22)
                   MOVE 'Bly/Metallavgift       ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-23)
                   MOVE 'Bly/Metall og miljøavgi' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-24)
                   MOVE 'MILJØAVGIFT PERSONBILDE' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-25)
                   MOVE 'Sonetillegg og miljøavg' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-26)
                   MOVE 'Frakttillegg           ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-27)
                   MOVE 'Miljø og N.R.K. avgift ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-28)
                   MOVE 'HK-Avgift.             ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-29)
                   MOVE 'Piggtillegg            ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-30)
                   MOVE 'Miljø og embalasjeavgif' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-31)
                   MOVE 'Kassett avgift (TONO)  ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-32)
                   MOVE 'MILJØAVGIFT LASTEBILDEK' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-33)
                   MOVE 'Miljøavgift            ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-34)
                   MOVE 'N.R.K avgift           ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-35)
                   MOVE 'Pant                   ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-36)
                   MOVE 'Pant og miljøavgift    ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-37)
                   MOVE 'Smøreoljeavgift.       ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-38)
                   MOVE 'Miljø og frakt/assurans' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-39)
                   MOVE 'Stamme-tillegg         ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-40)
                   MOVE 'For-avgift.            ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-41)
                   MOVE 'Legerings-tillegg      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-42)
                   MOVE 'Verksted materiell     ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-43)
                   MOVE 'Stammetillegg og miljøa' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-44)
                   MOVE 'Parfymeavgift          ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-45)
                   MOVE 'Emballasje-og smøreolje' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-46)
                   MOVE 'ADR 1203-3,3B   1      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-47)
                   MOVE 'ADR 1993-3,31C  2      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-48)
                   MOVE 'ADR 1993-3,3B   3      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               IF  (I-49)
                   MOVE 'ADR 1300-3,31C  4      ' TO BEHUT-IO-AREA
                                                              (153:23)
               END-IF
               MOVE ';'                    TO BEHUT-IO-AREA (176:1)
               MOVE PTEK                   TO BEHUT-IO-AREA (177:1)
               MOVE ';'                    TO BEHUT-IO-AREA (178:1)
               MOVE EDBNR                  TO BEHUT-IO-AREA (179:7)
               MOVE ';'                    TO BEHUT-IO-AREA (186:1)
               WRITE BEHUT-IO-AREA
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-10)
           AND (NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VRG                    TO LISTE-IO-AREA (1:5)
               MOVE ';'                    TO LISTE-IO-AREA (6:1)
               MOVE ALFA                   TO LISTE-IO-AREA (7:3)
               MOVE ART17                  TO LISTE-IO-AREA (10:17)
               MOVE ';'                    TO LISTE-IO-AREA (27:1)
               MOVE VAREN                  TO LISTE-IO-AREA (28:30)
               MOVE UPRIS                  TO EDIT-UPRIS
               MOVE EDIT-UPRIS             TO LISTE-IO-AREA (56:12)
               MOVE ';'                    TO LISTE-IO-AREA (58:1)
               MOVE ';'                    TO LISTE-IO-AREA (69:1)
               IF  (I-12)
                   MOVE '-'                TO LISTE-IO-AREA (71:1)
               END-IF
               MOVE BEHN                   TO EDIT-BEHN
               MOVE EDIT-BEHN              TO LISTE-IO-AREA (69:11)
               MOVE ';-1;;;;;;;;;'         TO LISTE-IO-AREA (80:12)
               MOVE LOC                    TO LISTE-IO-AREA (92:6)
               MOVE ';'                    TO LISTE-IO-AREA (98:1)
               MOVE '2005'                 TO LISTE-IO-AREA (99:4)
               MOVE '-'                    TO LISTE-IO-AREA (103:1)
               MOVE UYEAR                  TO LISTE-IO-AREA (101:2)
               MOVE UMONTH                 TO LISTE-IO-AREA (104:2)
               MOVE '-'                    TO LISTE-IO-AREA (106:1)
               MOVE UDAY                   TO LISTE-IO-AREA (107:2)
               MOVE ';;;;'                 TO LISTE-IO-AREA (109:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VRG                    TO LISTE-IO-AREA (1:5)
               MOVE ';'                    TO LISTE-IO-AREA (6:1)
               MOVE ALFA                   TO LISTE-IO-AREA (7:3)
               MOVE ART17                  TO LISTE-IO-AREA (10:17)
               MOVE ';'                    TO LISTE-IO-AREA (27:1)
               MOVE VAREN                  TO LISTE-IO-AREA (28:30)
               MOVE UPRIS                  TO EDIT-UPRIS
               MOVE EDIT-UPRIS             TO LISTE-IO-AREA (56:12)
               MOVE ';'                    TO LISTE-IO-AREA (58:1)
               MOVE ';'                    TO LISTE-IO-AREA (69:1)
               IF  (I-12)
                   MOVE '-'                TO LISTE-IO-AREA (71:1)
               END-IF
               MOVE BEHN                   TO EDIT-BEHN
               MOVE EDIT-BEHN              TO LISTE-IO-AREA (69:11)
               MOVE ';-1;;;;;;;;;'         TO LISTE-IO-AREA (80:12)
               MOVE LOC                    TO LISTE-IO-AREA (92:6)
               MOVE ';'                    TO LISTE-IO-AREA (98:1)
               MOVE '2005'                 TO LISTE-IO-AREA (99:4)
               MOVE '-'                    TO LISTE-IO-AREA (103:1)
               MOVE UYEAR                  TO LISTE-IO-AREA (101:2)
               MOVE UMONTH                 TO LISTE-IO-AREA (104:2)
               MOVE '-'                    TO LISTE-IO-AREA (106:1)
               MOVE UDAY                   TO LISTE-IO-AREA (107:2)
               MOVE ';;;;'                 TO LISTE-IO-AREA (109:4)
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE VAREBUP-DATA-FIELDS
           SET VAREBUP-EOF-OFF             TO TRUE
           SET VAREBUP-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREBUP-MC
                                              VAREBUP-MP
           OPEN INPUT VAREBUP
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN INPUT VAREMAS
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           OPEN OUTPUT VAREREC
           OPEN OUTPUT BEHUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREBUP
           CLOSE VAREMAS
           CLOSE VARETIL
           CLOSE VAREREC
           CLOSE BEHUT
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
