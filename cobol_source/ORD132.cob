       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD132R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD132.rpg
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
           SELECT FIRTAB
               ASSIGN TO UT-S-FIRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRTAB-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT FIRSUMF
               ASSIGN TO FIRSUMF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FIRSUMF-STATUS
               RECORD KEY IS FIRSUMF-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FIRTAB-IO-AREA.
           05  FIRTAB-IO-AREA-X            PICTURE X(80).
      *IRSUMF IS  F 160  80            DISK40 SYS012S
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FIRSUMF
               RECORD CONTAINS 80.
       01  FIRSUMF-IO-AREA.
           05  FIRSUMF-IO-AREA-X.
               10  FIRSUMF-KEY1.
                   15  FIRSUMF-KEY1N       PICTURE S9(11).
               10  FILLER                  PICTURE X(69).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD OUTFIL
               BLOCK CONTAINS 1280
               RECORD CONTAINS 128.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(128).
       WORKING-STORAGE SECTION.
       77  TABFIR-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFIR-TABLE.
               10  TABFIR-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY TABFIR-I
                                                      TABFIR-S.
                   15  TABFIR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FIRTAB-STATUS               PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FIRSUMF-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRTAB-EOF-OFF          VALUE '0'.
               88  FIRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FIRSUMF-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRSUMF-EOF-OFF         VALUE '0'.
               88  FIRSUMF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRSUMF-READ-OFF        VALUE '0'.
               88  FIRSUMF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRSUMF-PROCESS-OFF     VALUE '0'.
               88  FIRSUMF-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FIRSUMF-LEVEL-INIT-OFF  VALUE '0'.
               88  FIRSUMF-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  PARAM-DATA-FIELDS.
               10  PPDATO-IO.
                   15  PPDATO              PICTURE S9(6).
               10  PPMND                   PICTURE X(2).
               10  PPAAR                   PICTURE X(2).
           05  FIRSUMF-LEVEL-02.
               10  FIRSUMF-02-L1.
                   15  FIRSUMF-02-L1-FNR   PICTURE X(3).
           05  FIRSUMF-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  KONS                    PICTURE X(1).
               10  PAAR                    PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  ORDSUM-IO.
                   15  ORDSUM              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  ORDANT-IO.
                   15  ORDANT              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  LAGVER-IO.
                   15  LAGVER              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  SVSSUM-IO.
                   15  SVSSUM              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
      *                                       1   3 KONFNR
               10  FINAVN                  PICTURE X(30).
      **************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  NYLAGV-IO.
                   15  NYLAGV              PICTURE S9(11).
           05  EDITTING-FIELDS.
               10  XO-110YY9               PICTURE ZZ.ZZZ.ZZZ.ZZ9.
               10  XO-110YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZ9-.
               10  EDIT-ORDANT             PICTURE ZZZZZZZZZZZ.
               10  EDIT-ORDSUM             PICTURE ZZZZZZZZZZZ.
               10  EDIT-SVSSUM             PICTURE ZZZZZZZZZZZ.
               10  EDIT-LAGVER             PICTURE ZZZZZZZZZZZ.
               10  EDIT-NYLAGV             PICTURE ZZZZZZZZZZZ.
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
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
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
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FIRSUMF-PROCESS
               SET FIRSUMF-PROCESS-OFF     TO TRUE
               SET FIRSUMF-READ            TO TRUE
           END-IF
 
           IF  FIRSUMF-READ
           AND RECORD-SELECTED-OFF
               PERFORM FIRSUMF-GET
               SET FIRSUMF-READ-OFF        TO TRUE
               IF  NOT FIRSUMF-EOF
                   SET FIRSUMF-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  FIRSUMF-PROCESS
               PERFORM FIRSUMF-IDSET
           END-IF
 
           IF  FIRSUMF-PROCESS
               PERFORM FIRSUMF-CHK-LEVEL
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
           PERFORM DETAIL-OVERFLOW
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  FIRSUMF-PROCESS
               PERFORM FIRSUMF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FIRSUMF-PROCESS
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
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET NOT-I-15                TO TRUE
      *****************************************************************
      * SJEKK MOT TABELL OM FIRMAET  SKAL LISTES.                     *
      *****************************************************************
           END-IF
           IF  (I-02 AND I-L1)
               SET NOT-I-10                TO TRUE
               SET TABFIR-S                TO TABFIR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFIR-I FROM 1 BY 1
                         UNTIL TABFIR-I >= TABFIR-MAX
                            OR I-10
                   IF  FNR = TABFIR (TABFIR-I)
                       SET I-10            TO TRUE
                       SET TABFIR-S        TO TABFIR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-02 AND NOT-I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND I-L1)
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-02)
               SET NOT-I-12                TO TRUE
               IF  PAAR = PPAAR
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-12)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET NOT-I-12                TO TRUE
               IF  PMND = PPMND
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-12)
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND I-12)
               SET I-15                    TO TRUE
      *****************************************************************
           END-IF
           IF  (I-02)
               ADD SVSSUM TO LAGVER    GIVING NYLAGV
           END-IF.
 
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
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '1' )
               MOVE PARAM-IO-AREA (21:6)   TO PPDATO-IO
               INSPECT PPDATO-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (23:2)   TO PPMND (1:2)
               MOVE PARAM-IO-AREA (25:2)   TO PPAAR (1:2)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '1' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       FIRSUMF-GET SECTION.
       FIRSUMF-GET-P.
           IF  FIRSUMF-EOF-OFF
               READ FIRSUMF
               AT END
                   SET FIRSUMF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FIRSUMF-FLDSET SECTION.
       FIRSUMF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRSUMF-IO-AREA (1:3)  TO FNR (1:3)
               MOVE FIRSUMF-IO-AREA (4:1)  TO KONS (1:1)
               MOVE FIRSUMF-IO-AREA (6:2)  TO PAAR (1:2)
               MOVE FIRSUMF-IO-AREA (8:2)  TO PMND (1:2)
               MOVE FIRSUMF-IO-AREA (12:6) TO ORDSUM-IO
               MOVE FIRSUMF-IO-AREA (18:6) TO ORDANT-IO
               MOVE FIRSUMF-IO-AREA (24:6) TO LAGVER-IO
               MOVE FIRSUMF-IO-AREA (48:6) TO SVSSUM-IO
           END-EVALUATE.
 
       FIRSUMF-IDSET SECTION.
       FIRSUMF-IDSET-P.
           SET I-02                        TO TRUE.
 
       FIRSUMF-CHK-LEVEL SECTION.
       FIRSUMF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FIRSUMF-LEVEL-02
               MOVE FIRSUMF-IO-AREA (1:3)  TO FIRSUMF-02-L1-FNR
               IF  FIRSUMF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FIRSUMF-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FIRSUMF-02-L1         TO THE-PRIOR-L1
               SET FIRSUMF-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
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
 
       FIRTAB-LOAD SECTION.
       FIRTAB-LOAD-P.
           OPEN INPUT FIRTAB
           SET TABFIR-I                    TO 1
           PERFORM UNTIL FIRTAB-EOF
               READ FIRTAB
               AT END
                   SET FIRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FIRTAB-IO-AREA (1:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (4:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (7:3) TO TABFIR-ENTRY (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (10:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (13:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (16:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (19:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (22:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (25:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (28:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (31:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (34:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (37:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (40:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (43:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (46:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (49:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (52:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (55:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
                   MOVE FIRTAB-IO-AREA (58:3) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FIRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KORRIGERING AV VARELAGER' TO LISTE-IO-AREA (1:24)
               MOVE 'VERDI MED IKKE FAKTURERT' TO LISTE-IO-AREA (25:24)
               MOVE 'E ORDRE    '          TO LISTE-IO-AREA (49:11)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR'                  TO LISTE-IO-AREA (2:3)
               MOVE 'FIRMA NAVN'           TO LISTE-IO-AREA (6:10)
               MOVE 'ANT.ORDRE'            TO LISTE-IO-AREA (42:9)
               MOVE 'SALGSUM'              TO LISTE-IO-AREA (60:7)
               MOVE 'KOSTSUM'              TO LISTE-IO-AREA (76:7)
               MOVE 'LAGERVERDI'           TO LISTE-IO-AREA (89:10)
               MOVE 'KORR.VERDI'           TO LISTE-IO-AREA (105:10)
               MOVE 'PR.DATO'              TO LISTE-IO-AREA (122:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-15)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (2:3)
               IF  (NOT-I-11)
                   MOVE FINAVN             TO LISTE-IO-AREA (6:30)
               END-IF
               IF  (I-11)
                   MOVE '** FIRMA UKJENT **' TO LISTE-IO-AREA (6:18)
               END-IF
               MOVE ORDANT                 TO XO-110YY9
               MOVE XO-110YY9              TO LISTE-IO-AREA (37:14)
               MOVE ORDSUM                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (52:15)
               MOVE SVSSUM                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (68:15)
               MOVE LAGVER                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (84:15)
               MOVE NYLAGV                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (100:15)
               MOVE PPDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (121:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-1P AND I-U1)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE 'FNR;'                 TO OUTFIL-IO-AREA (1:4)
               MOVE 'FIRMA NAVN;'          TO OUTFIL-IO-AREA (5:11)
               MOVE 'ANT.ORDRE;'           TO OUTFIL-IO-AREA (16:10)
               MOVE 'SALGSUM;'             TO OUTFIL-IO-AREA (26:8)
               MOVE 'KOSTSUM;'             TO OUTFIL-IO-AREA (35:8)
               MOVE 'LAGERVERDI;'          TO OUTFIL-IO-AREA (43:11)
               MOVE 'KORR.VERDI;'          TO OUTFIL-IO-AREA (54:11)
               MOVE 'PR.DATO;'             TO OUTFIL-IO-AREA (65:8)
      *                                  80 "KONSERN;"
               WRITE OUTFIL-IO-AREA
           END-IF
           IF  (I-02 AND I-15 AND I-U1)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE FNR                    TO OUTFIL-IO-AREA (1:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (4:1)
               IF  (NOT-I-11)
                   MOVE FINAVN             TO OUTFIL-IO-AREA (5:30)
               END-IF
               IF  (I-11)
                   MOVE '** FIRMA UKJENT **' TO OUTFIL-IO-AREA (6:18)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (35:1)
               MOVE ORDANT                 TO EDIT-ORDANT
               MOVE EDIT-ORDANT            TO OUTFIL-IO-AREA (36:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (47:1)
               MOVE ORDSUM                 TO EDIT-ORDSUM
               MOVE EDIT-ORDSUM            TO OUTFIL-IO-AREA (49:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (60:1)
               MOVE SVSSUM                 TO EDIT-SVSSUM
               MOVE EDIT-SVSSUM            TO OUTFIL-IO-AREA (61:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (72:1)
               MOVE LAGVER                 TO EDIT-LAGVER
               MOVE EDIT-LAGVER            TO OUTFIL-IO-AREA (73:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (84:1)
               MOVE NYLAGV                 TO EDIT-NYLAGV
               MOVE EDIT-NYLAGV            TO OUTFIL-IO-AREA (85:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (96:1)
               MOVE PPDATO-IO              TO OUTFIL-IO-AREA (98:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (104:1)
      *                        KONS     105
      *                                 106 ";"
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KORRIGERING AV VARELAGER' TO LISTE-IO-AREA (1:24)
               MOVE 'VERDI MED IKKE FAKTURERT' TO LISTE-IO-AREA (25:24)
               MOVE 'E ORDRE    '          TO LISTE-IO-AREA (49:11)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR'                  TO LISTE-IO-AREA (2:3)
               MOVE 'FIRMA NAVN'           TO LISTE-IO-AREA (6:10)
               MOVE 'ANT.ORDRE'            TO LISTE-IO-AREA (42:9)
               MOVE 'SALGSUM'              TO LISTE-IO-AREA (60:7)
               MOVE 'KOSTSUM'              TO LISTE-IO-AREA (76:7)
               MOVE 'LAGERVERDI'           TO LISTE-IO-AREA (89:10)
               MOVE 'KORR.VERDI'           TO LISTE-IO-AREA (105:10)
               MOVE 'PR.DATO'              TO LISTE-IO-AREA (122:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           PERFORM FIRTAB-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET FIRSUMF-LEVEL-INIT          TO TRUE
           INITIALIZE FIRSUMF-DATA-FIELDS
           SET FIRSUMF-EOF-OFF             TO TRUE
           SET FIRSUMF-PROCESS             TO TRUE
           OPEN INPUT FIRSUMF
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTFIL.
           SET TABFIR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FIRSUMF
           CLOSE FIRMAF
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTFIL.
 
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
