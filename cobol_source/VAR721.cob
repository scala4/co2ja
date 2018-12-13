       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR721R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM...VAR721 - JCL=VAR.XVAR43MM                           *
      * PROGRAMERER:ElinN 11.11.11                                    *
      * OPPDATERER Merknad fra leverandør i varemas                   *
      *  FRA GIVER TIL MOTTAGER DER DETTE IKKE ER LIKT.               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR721.rpg
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
           SELECT LEVRA
               ASSIGN TO UT-S-LEVRA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LEVRA-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD LEVRA
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  LEVRA-IO-AREA.
           05  LEVRA-IO-AREA-X             PICTURE X(200).
       FD INNPUT
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(200).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
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
           10  LEVRA-STATUS                PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  LEVRA-EOF-OFF           VALUE '0'.
               88  LEVRA-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LEVRA-READ-OFF          VALUE '0'.
               88  LEVRA-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LEVRA-PROCESS-OFF       VALUE '0'.
               88  LEVRA-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  LEVRA-DATA-FIELDS.
               10  LEVFNR                  PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  LEVMRK                  PICTURE X(1).
           05  LEVRA-MP                    PICTURE X(23).
           05  LEVRA-MC                    PICTURE X(23).
           05  LEVRA-M-02              REDEFINES LEVRA-MC.
               10  LEVRA-M-02-M2.
                   15  LEVRA-M-02-M2-ALFA-G.
                       20  LEVRA-M-02-M2-ALFA PICTURE X(3).
               10  LEVRA-M-02-M1.
                   15  LEVRA-M-02-M1-ARTNR-G.
                       20  LEVRA-M-02-M1-ARTNR PICTURE X(20).
           05  INNPUT-DATA-FIELDS.
               10  KUNFNR                  PICTURE X(3).
               10  VALF                    PICTURE X(3).
               10  VARTNR                  PICTURE X(20).
               10  FNR                     PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  GMLMRK                  PICTURE X(1).
           05  INNPUT-MP                   PICTURE X(23).
           05  INNPUT-MC                   PICTURE X(23).
           05  INNPUT-M-03             REDEFINES INNPUT-MC.
               10  INNPUT-M-03-M2.
                   15  INNPUT-M-03-M2-VALF-G.
                       20  INNPUT-M-03-M2-VALF PICTURE X(3).
               10  INNPUT-M-03-M1.
                   15  INNPUT-M-03-M1-VARTNR-G.
                       20  INNPUT-M-03-M1-VARTNR PICTURE X(20).
           05  VAREMAS-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  TEMPORARY-FIELDS.
               10  LFNR                    PICTURE X(3).
               10  KFNR                    PICTURE X(3).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  ANTNM-IO.
                   15  ANTNM               PICTURE S9(6).
               10  KEY-X                   PICTURE X(10).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(6).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  LEVRA-PROCESS
               SET LEVRA-PROCESS-OFF       TO TRUE
               SET LEVRA-READ              TO TRUE
           END-IF
 
           IF  LEVRA-READ
               PERFORM LEVRA-GET
               SET LEVRA-READ-OFF          TO TRUE
               IF  NOT LEVRA-EOF
                   PERFORM LEVRA-MATCH-SET
               END-IF
           END-IF
 
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   PERFORM INNPUT-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  LEVRA-PROCESS
               PERFORM LEVRA-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  LEVRA-PROCESS
               PERFORM LEVRA-FLDOFF
               PERFORM LEVRA-FLDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
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
           IF  (I-03)
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-41                TO TRUE
               SET NOT-I-42                TO TRUE
      *
      *****************************************************************
      * SAVE LEVRANDØRDATA.                                           *
      *****************************************************************
           END-IF
           IF  (I-02 AND NOT-I-91)
               MOVE LEVFNR                 TO LFNR
           END-IF
           IF  (I-02)
               SET I-91                    TO TRUE
               GO TO SLUTT-T
      *
      *****************************************************************
      * TELLERUTINE OG SJEKK OM RECORD MERGER.                        *
      *****************************************************************
           END-IF
           IF  (I-03 AND NOT-I-92)
               MOVE KUNFNR                 TO KFNR
           END-IF
           IF  (I-03)
               SET I-92                    TO TRUE
               ADD 1                       TO ANT
           END-IF
           IF  (I-03 AND NOT-I-MR)
               ADD 1                       TO ANTNM
               GO TO SLUTT-T
      *
      *****************************************************************
      * SJEKK OM LEVR.DATA OG KUNDE DATA ER LIKE.                     *
      *****************************************************************
           END-IF
           IF  (I-03)
               SET NOT-I-26                TO TRUE
               IF  GMLMRK NOT = LEVMRK
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-26)
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR Å HENTE VARE.MASTER RECORD OG OPPDATERE DENNE.     *
      *****************************************************************
           END-IF
           IF  (I-03)
               MOVE FNR                    TO KEY-X (1:3)
               MOVE EDBNR                  TO KEY-X (4:7)
               MOVE KEY-X                  TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-03 AND I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-03 AND I-30)
               ADD 1                       TO ANTKOR
      *****************************************************************
      * Hente flere filer til oppdatering ?                           *
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       LEVRA-GET SECTION.
       LEVRA-GET-P.
           IF  LEVRA-EOF-OFF
               READ LEVRA
               AT END
                   SET LEVRA-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LEVRA-FLDOFF SECTION.
       LEVRA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       LEVRA-FLDSET SECTION.
       LEVRA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LEVRA-IO-AREA (3:3)    TO LEVFNR (1:3)
               MOVE LEVRA-IO-AREA (13:3)   TO ALFA (1:3)
               MOVE LEVRA-IO-AREA (16:20)  TO ARTNR (1:20)
               MOVE LEVRA-IO-AREA (127:1)  TO LEVMRK (1:1)
               IF  LEVMRK = SPACES
                   SET I-09                TO TRUE
               END-IF
           END-EVALUATE.
 
       LEVRA-IDSET SECTION.
       LEVRA-IDSET-P.
           SET I-02                        TO TRUE.
 
       LEVRA-MATCH-SET SECTION.
       LEVRA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE LEVRA-IO-AREA (13:3)   TO LEVRA-M-02-M2-ALFA
               MOVE LEVRA-IO-AREA (16:20)  TO LEVRA-M-02-M1-ARTNR
           END-EVALUATE.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
               END-READ
           END-IF.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (3:3)   TO KUNFNR (1:3)
               MOVE INNPUT-IO-AREA (13:3)  TO VALF (1:3)
               MOVE INNPUT-IO-AREA (16:20) TO VARTNR (1:20)
               MOVE INNPUT-IO-AREA (3:3)   TO FNR (1:3)
               MOVE INNPUT-IO-AREA (6:7)   TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (127:1) TO GMLMRK (1:1)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-03                        TO TRUE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (13:3)  TO INNPUT-M-03-M2-VALF
               MOVE INNPUT-IO-AREA (16:20) TO INNPUT-M-03-M1-VARTNR
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  LEVRA-EOF
               MOVE HIGH-VALUES            TO LEVRA-MC
                                              LEVRA-MP
           END-IF
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  LEVRA-MC < LEVRA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  LEVRA-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET LEVRA-PROCESS       TO TRUE
                   MOVE LEVRA-MC           TO LEVRA-MP
                   IF  LEVRA-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC < LEVRA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = LEVRA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  LEVRA-MC = INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET LEVRA-PROCESS       TO TRUE
                   MOVE LEVRA-MC           TO LEVRA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-30 AND I-26)
               MOVE LEVMRK                 TO VAREMAS-IO-AREA (127:1)
               REWRITE VAREMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAREMAS'
               END-REWRITE
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDAT. MERKNAD'      TO LISTE-IO-AREA (10:15)
               MOVE 'NY MRK'               TO LISTE-IO-AREA (93:6)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-30 AND I-26)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (1:3)
               MOVE VALF                   TO LISTE-IO-AREA (5:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (16:20)
               IF  (I-26 AND NOT-I-09)
                   MOVE LEVMRK             TO LISTE-IO-AREA (43:1)
               END-IF
               IF  (I-26 AND I-09)
                   MOVE 'BLANK'            TO LISTE-IO-AREA (39:5)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDAT. MERKNAD'      TO LISTE-IO-AREA (10:15)
               MOVE 'NY MRK'               TO LISTE-IO-AREA (93:6)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OVERFØRT FRA FIRMA'   TO LISTE-IO-AREA (3:18)
               MOVE LFNR                   TO LISTE-IO-AREA (22:3)
      *                                  44 "OVERFØRT TIL FIRMA"
      *                        KFNR      48
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ANT.VAREREC. TIL OPPDAT.' TO LISTE-IO-AREA (12:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTNM                  TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ANT.VAREREC. NOT MATCH. ' TO LISTE-IO-AREA (12:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTKOR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ANT.VAREM.REC. OPPDATERT' TO LISTE-IO-AREA (12:24)
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
           INITIALIZE LEVRA-DATA-FIELDS
           SET LEVRA-EOF-OFF               TO TRUE
           SET LEVRA-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO LEVRA-MC
                                              LEVRA-MP
           OPEN INPUT LEVRA
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           OPEN I-O VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE LEVRA
           CLOSE INNPUT
           CLOSE VAREMAS
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
