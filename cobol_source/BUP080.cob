       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUP080R.
      **********************************************  Z-WIN-RPG2   ****
      *                                          ***TXT***OK MT
      *  PROGRAM.......: BUP080 (ERSTATTER PROGRAM BUP081 E 09/05.    *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: BUP20UD.                                     *
      *  LAGET DATO....: 20.09.95                                     *
      *  ENDRET........: 06.03.96 SJEKKER OM BILAGET ER GODKJENT, TAR *
      *                           UT IKKE GODKJENTE BILAG.            *
      *                  XX.XX.XX UTVIDET TRANSRECORD TIL 140 BYTE    *
      *  INPUT.........: REGNSKAP.BOKF.TRANSER (REGTRA2).             *
      *  BEHANDLING....: LESER HEADING OG DELTALJ TRANS-RECORD (ART A)*
      *                  FRA REGISTRERING I FAKR, OG LEGGER UT TIL    *
      *                  INPUT I DOP12UD.                             *
      *  OUTPUT........: KOPI.FAKR.TRANSER (BILAGSART 0-3)            *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BUP080.rpg
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
           SELECT FAKRTRA
               ASSIGN TO UT-S-FAKRTRA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKRTRA-STATUS.
           SELECT KOPI
               ASSIGN TO UT-S-KOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPI-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKRTRA
               BLOCK CONTAINS 3000
               RECORD CONTAINS 300.
       01  FAKRTRA-IO-AREA.
           05  FAKRTRA-IO-AREA-X           PICTURE X(300).
       FD KOPI
               BLOCK CONTAINS 2400
               RECORD CONTAINS 240.
       01  KOPI-IO-AREA.
           05  KOPI-IO-AREA-X              PICTURE X(240).
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
           10  FAKRTRA-STATUS              PICTURE 99 VALUE 0.
           10  KOPI-STATUS                 PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKRTRA-EOF-OFF         VALUE '0'.
               88  FAKRTRA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKRTRA-READ-OFF        VALUE '0'.
               88  FAKRTRA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKRTRA-PROCESS-OFF     VALUE '0'.
               88  FAKRTRA-PROCESS         VALUE '1'.
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
           05  FAKRTRA-DATA-FIELDS.
               10  RECD1                   PICTURE X(100).
               10  RECD2                   PICTURE X(100).
               10  TRAREC                  PICTURE X(240).
               10  OKBIL                   PICTURE X(1).
               10  DEBKRE                  PICTURE X(1).
               10  BILTOT-IO.
                   15  BILTOT              PICTURE S9(8)V9(2).
           05  TEMPORARY-FIELDS.
               10  ANTTRA-IO.
                   15  ANTTRA              PICTURE S9(5).
               10  ANTGOD-IO.
                   15  ANTGOD              PICTURE S9(5).
               10  BELINN-IO.
                   15  BELINN              PICTURE S9(8)V9(2).
               10  BELGOD-IO.
                   15  BELGOD              PICTURE S9(8)V9(2).
               10  ANTDEL-IO.
                   15  ANTDEL              PICTURE S9(5).
               10  BELDEL-IO.
                   15  BELDEL              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-50YY9R               PICTURE ZZ.ZZ9-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKRTRA-PROCESS
               SET FAKRTRA-PROCESS-OFF     TO TRUE
               SET FAKRTRA-READ            TO TRUE
           END-IF
 
           IF  FAKRTRA-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKRTRA-GET
               SET FAKRTRA-READ-OFF        TO TRUE
               IF  NOT FAKRTRA-EOF
                   SET FAKRTRA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKRTRA-PROCESS
               PERFORM FAKRTRA-IDSET
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
 
           IF  FAKRTRA-PROCESS
               PERFORM FAKRTRA-FLDSET
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
           ADD 1                           TO ANTTRA
           IF  (I-03)
               SET NOT-I-10                TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               IF  OKBIL NOT = 'F'
                   SET I-11                TO TRUE
               END-IF
               IF  OKBIL = 'F'
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-13                TO TRUE
               SET NOT-I-14                TO TRUE
               IF  DEBKRE NOT = 'F'
                   SET I-13                TO TRUE
               END-IF
               IF  DEBKRE = 'F'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-12)
               ADD 1                       TO ANTGOD
           END-IF
           IF  (I-01 AND I-13)
               ADD BILTOT                  TO BELINN
           END-IF
           IF  (I-01 AND I-14)
               SUBTRACT BILTOT             FROM BELINN
           END-IF
           IF  (I-01 AND I-12 AND I-13)
               ADD BILTOT                  TO BELGOD
           END-IF
           IF  (I-01 AND I-12 AND I-14)
               SUBTRACT BILTOT             FROM BELGOD
           END-IF
           IF  (I-11)
               ADD 1                       TO ANTDEL
           END-IF
           IF  (I-01 AND I-11 AND I-13)
               ADD BILTOT                  TO BELDEL
           END-IF
           IF  (I-01 AND I-11 AND I-14)
               SUBTRACT BILTOT             FROM BELDEL
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  (I-12)
               SET I-10                    TO TRUE
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       FAKRTRA-GET SECTION.
       FAKRTRA-GET-P.
           IF  FAKRTRA-EOF-OFF
               READ FAKRTRA
               AT END
                   SET FAKRTRA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKRTRA-FLDSET SECTION.
       FAKRTRA-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FAKRTRA-IO-AREA (61:1) = '1' )
               MOVE FAKRTRA-IO-AREA (1:100) TO RECD1 (1:100)
               MOVE FAKRTRA-IO-AREA (101:100) TO RECD2 (1:100)
               MOVE FAKRTRA-IO-AREA (61:240) TO TRAREC (1:240)
               MOVE FAKRTRA-IO-AREA (56:1) TO OKBIL (1:1)
               MOVE FAKRTRA-IO-AREA (80:1) TO DEBKRE (1:1)
               MOVE FAKRTRA-IO-AREA (112:10) TO BILTOT-IO
               INSPECT BILTOT-IO REPLACING ALL ' ' BY '0'
           WHEN ( FAKRTRA-IO-AREA (61:1) = '2' )
               MOVE FAKRTRA-IO-AREA (1:100) TO RECD1 (1:100)
               MOVE FAKRTRA-IO-AREA (101:100) TO RECD2 (1:100)
               MOVE FAKRTRA-IO-AREA (61:240) TO TRAREC (1:240)
           END-EVALUATE.
 
       FAKRTRA-IDSET SECTION.
       FAKRTRA-IDSET-P.
           EVALUATE TRUE
           WHEN ( FAKRTRA-IO-AREA (61:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( FAKRTRA-IO-AREA (61:1) = '2' )
               SET I-02                    TO TRUE
           WHEN  OTHER
               SET I-03                    TO TRUE
           END-EVALUATE.
 
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
           IF  (I-10)
               MOVE SPACES TO KOPI-IO-AREA
               INITIALIZE KOPI-IO-AREA
               MOVE TRAREC                 TO KOPI-IO-AREA (1:240)
               WRITE KOPI-IO-AREA
           END-IF
           IF  (I-01 AND I-11)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RECD1                  TO LISTE-IO-AREA (1:100)
               MOVE 'DEL 1'                TO LISTE-IO-AREA (106:5)
               MOVE '* SLETTET, KODE='     TO LISTE-IO-AREA (114:16)
               MOVE OKBIL                  TO LISTE-IO-AREA (130:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RECD2                  TO LISTE-IO-AREA (1:100)
               MOVE 'DEL 2'                TO LISTE-IO-AREA (106:5)
               MOVE '* SLETTET, KODE='     TO LISTE-IO-AREA (114:16)
               MOVE OKBIL                  TO LISTE-IO-AREA (130:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RECD1                  TO LISTE-IO-AREA (1:100)
               MOVE 'DEL 1'                TO LISTE-IO-AREA (106:5)
               MOVE '* SLETTET, FEIL ART *' TO LISTE-IO-AREA (110:21)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RECD2                  TO LISTE-IO-AREA (1:100)
               MOVE 'DEL 2'                TO LISTE-IO-AREA (106:5)
               MOVE '* SLETTET, FEIL ART *' TO LISTE-IO-AREA (110:21)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'B A C K U P  O N - L I N' TO LISTE-IO-AREA (10:24)
               MOVE 'E  T R A N S F I L E' TO LISTE-IO-AREA (35:20)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'D A T O'              TO LISTE-IO-AREA (6:7)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (33:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'R U T I N E'          TO LISTE-IO-AREA (6:11)
               MOVE 'REG BILAGSART 0 - 3     ' TO LISTE-IO-AREA (33:24)
               MOVE '(FAKR)'               TO LISTE-IO-AREA (56:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'A N T  T R A N S   I N N' TO LISTE-IO-AREA (6:24)
               MOVE ANTTRA                 TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE-IO-AREA (33:7)
               MOVE 'A N T  T R A N S   I N N' TO LISTE-IO-AREA (6:24)
               MOVE ANTTRA                 TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE-IO-AREA (33:7)
               MOVE 'B E L  I N N            ' TO LISTE-IO-AREA (42:24)
               MOVE BELINN                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (72:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'A N T  S L E T T E T    ' TO LISTE-IO-AREA (6:24)
               MOVE ANTDEL                 TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE-IO-AREA (33:7)
               MOVE 'B E L  S L E T T E T    ' TO LISTE-IO-AREA (42:24)
               MOVE BELDEL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (72:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'A N T  G O D K J E N T  ' TO LISTE-IO-AREA (6:24)
               MOVE ANTGOD                 TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE-IO-AREA (33:7)
               MOVE 'B E L  G O D K J E N T  ' TO LISTE-IO-AREA (42:24)
               MOVE BELGOD                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (72:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'P R O G R A M'        TO LISTE-IO-AREA (6:13)
               MOVE 'BUP080'               TO LISTE-IO-AREA (33:6)
      *       D  1     10 12
      *                        RECD1    100
      *                                 120 "**** GODKJENT ****"
      *       D  1     10 12
      *                        RECD2    100
      *                                 120 "**** GODKJENT ****"
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE FAKRTRA-DATA-FIELDS
           SET FAKRTRA-EOF-OFF             TO TRUE
           SET FAKRTRA-PROCESS             TO TRUE
           OPEN INPUT FAKRTRA
           OPEN OUTPUT KOPI
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKRTRA
           CLOSE KOPI
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
