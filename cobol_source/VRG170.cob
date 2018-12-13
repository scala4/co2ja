       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG170R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET BEREGNER INNGÅENDE TILGANGSVERDI PR 1/1.         *
      *  LAGERVERDI PR 31/12 ETTER VAREOPPTELLING.                   *    VRG170
      *  UPSI-1 - SKAL STÅ PÅ FOR KJØRING ETTER VARETELLING.         *    VRG170
      *  UPSI-1 - SKAL STÅ AV FOR KJØRING PR 31/12.                  *    VRG170
      *                                                              *    VRG170
      *  KORRIGERING FORETAS KUNN VED KJØRINGER DEN 31.12, ELLER     *    VRG170
      *  I JANUAR MÅNED.                                                  VRG170
      *  DET SKAL IKKE FINNES NYE REC, DA DISSE DANNES I DEN FASTE   *    VRG170
      *  AVSLUTTNINGSKJØRINGEN FOR MÅNEDEN.                          *    VRG170
      ****************************************************************    VRG170
      *                                                                   VRG170
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG170.rpg
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
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT GMLFILE
               ASSIGN TO UT-S-GMLFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLFILE-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT NYFILE
               ASSIGN TO UT-S-NYFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREREC
               BLOCK CONTAINS 9415
               RECORD CONTAINS 35.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(35).
       FD GMLFILE
               BLOCK CONTAINS 4100
               RECORD CONTAINS 164.
       01  GMLFILE-IO-AREA.
           05  GMLFILE-IO-AREA-X           PICTURE X(164).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD NYFILE
               BLOCK CONTAINS 4100
               RECORD CONTAINS 164.
       01  NYFILE-IO-AREA.
           05  NYFILE-IO-AREA-X            PICTURE X(164).
      *                                                                   VRG170
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARF-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  ARA-ELG-MAX   VALUE 12          PICTURE 9(4) USAGE BINARY.
       77  NYF-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  NYA-ELG-MAX   VALUE 12          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARF-TABLE.
               10  ARF-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARF-I
                                                      ARF-S.
                   15  ARF                 PICTURE S9(9).
           05  ARA-ELG-TABLE.
               10  ARA-ELG-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARA-ELG-I
                                                      ARA-ELG-S.
                   15  ARA-ELG             PICTURE S9(9).
           05  NYF-TABLE.
               10  NYF-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY NYF-I
                                                      NYF-S.
                   15  NYF                 PICTURE S9(9).
      *                                                                   VRG170
           05  NYA-ELG-TABLE.
               10  NYA-ELG-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY NYA-ELG-I
                                                      NYA-ELG-S.
                   15  NYA-ELG             PICTURE S9(9).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  GMLFILE-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  NYFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-EOF-OFF         VALUE '0'.
               88  VAREREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-READ-OFF        VALUE '0'.
               88  VAREREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-PROCESS-OFF     VALUE '0'.
               88  VAREREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREREC-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREREC-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-EOF-OFF         VALUE '0'.
               88  GMLFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-READ-OFF        VALUE '0'.
               88  GMLFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-PROCESS-OFF     VALUE '0'.
               88  GMLFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  GMLFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  GMLFILE-LEVEL-INIT      VALUE '1'.
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
           05  VAREREC-LEVEL-01.
               10  VAREREC-01-L2.
                   15  VAREREC-01-L2-FIRMA PICTURE X(3).
               10  VAREREC-01-L1.
                   15  VAREREC-01-L1-VGR   PICTURE X(5).
           05  VAREREC-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  SKAF                    PICTURE X(1).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VGR                     PICTURE X(5).
               10  MERK                    PICTURE X(1).
               10  SLETT                   PICTURE X(1).
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRITYP                  PICTURE X(1).
           05  VAREREC-MP                  PICTURE X(8).
           05  VAREREC-MC                  PICTURE X(8).
           05  VAREREC-M-01            REDEFINES VAREREC-MC.
               10  VAREREC-M-01-M2.
                   15  VAREREC-M-01-M2-FIRMA-G.
                       20  VAREREC-M-01-M2-FIRMA PICTURE X(3).
               10  VAREREC-M-01-M1.
                   15  VAREREC-M-01-M1-VGR-G.
                       20  VAREREC-M-01-M1-VGR PICTURE X(5).
           05  GMLFILE-LEVEL-02.
               10  GMLFILE-02-L2.
                   15  GMLFILE-02-L2-FIRMA PICTURE X(3).
               10  GMLFILE-02-L1.
                   15  GMLFILE-02-L1-VGR   PICTURE X(5).
           05  GMLFILE-DATA-FIELDS.
               10  RA                      PICTURE X(2).
               10  XI-ARF-GRP.
                   15  XI-ARF              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  XI-ARA-ELG-GRP.
                   15  XI-ARA-ELG          PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  INNV-IO.
                   15  INNV                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  REC                     PICTURE X(164).
           05  GMLFILE-MP                  PICTURE X(8).
           05  GMLFILE-MC                  PICTURE X(8).
           05  GMLFILE-M-02            REDEFINES GMLFILE-MC.
               10  GMLFILE-M-02-M2.
                   15  GMLFILE-M-02-M2-FIRMA-G.
                       20  GMLFILE-M-02-M2-FIRMA PICTURE X(3).
               10  GMLFILE-M-02-M1.
                   15  GMLFILE-M-02-M1-VGR-G.
                       20  GMLFILE-M-02-M1-VGR PICTURE X(5).
           05  FIRMAF-DATA-FIELDS.
               10  REGM-IO.
                   15  REGM                PICTURE S9(2).
      *                                                                   VRG170
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  BEH-IO.
                   15  BEH                 PICTURE S9(7)V9(2).
               10  VERDI-IO.
                   15  VERDI               PICTURE S9(9)V9(2).
               10  L1V-IO.
                   15  L1V                 PICTURE S9(9)V9(2).
               10  TILGV-IO.
                   15  TILGV               PICTURE S9(9)V9(2).
               10  L1T-IO.
                   15  L1T                 PICTURE S9(9)V9(2).
               10  KORRV-IO.
                   15  KORRV               PICTURE S9(9)V9(2).
               10  L1K-IO.
                   15  L1K                 PICTURE S9(9)V9(2).
               10  VERDIT-IO.
                   15  VERDIT              PICTURE S9(9)V9(2).
               10  TILGVT-IO.
                   15  TILGVT              PICTURE S9(9)V9(2).
               10  KORRVT-IO.
                   15  KORRVT              PICTURE S9(9)V9(2).
               10  NYINNV-IO.
                   15  NYINNV              PICTURE S9(9).
               10  L2V-IO.
                   15  L2V                 PICTURE S9(9)V9(2).
               10  L2VA-IO.
                   15  L2VA                PICTURE S9(9).
               10  M-IO.
                   15  M                   PICTURE S9(2).
           05  EDITTING-FIELDS.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-90YY9R               PICTURE ZZZ.ZZZ.ZZ9-.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREREC-PROCESS
               SET VAREREC-PROCESS-OFF     TO TRUE
               SET VAREREC-READ            TO TRUE
           END-IF
 
           IF  VAREREC-READ
               PERFORM VAREREC-GET
               SET VAREREC-READ-OFF        TO TRUE
               IF  NOT VAREREC-EOF
                   PERFORM VAREREC-MATCH-SET
               END-IF
           END-IF
 
           IF  GMLFILE-PROCESS
               SET GMLFILE-PROCESS-OFF     TO TRUE
               SET GMLFILE-READ            TO TRUE
           END-IF
 
           IF  GMLFILE-READ
               PERFORM GMLFILE-GET
               SET GMLFILE-READ-OFF        TO TRUE
               IF  NOT GMLFILE-EOF
                   PERFORM GMLFILE-MATCH-SET
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
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-IDSET
           END-IF
 
           IF  GMLFILE-PROCESS
               PERFORM GMLFILE-IDSET
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-CHK-LEVEL
           END-IF
 
           IF  GMLFILE-PROCESS
               PERFORM GMLFILE-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-FLDOFF
               PERFORM VAREREC-FLDSET
           END-IF
 
           IF  GMLFILE-PROCESS
               PERFORM GMLFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREREC-PROCESS
           OR  GMLFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-20                    TO TRUE
           SET NOT-I-21                    TO TRUE
      *                                                                   VRG170
      *  TEST OM KORRIGERINGER SKAL FORETAS NÅ.                           VRG170
      *                                                                   VRG170
           IF  (NOT-I-99)
               SET NOT-I-98                TO TRUE
               IF  UMONTH = 01
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-99 AND NOT-I-98)
               SET NOT-I-97                TO TRUE
               IF  UMONTH = 12
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-99 AND NOT-I-98 AND I-97)
               SET NOT-I-98                TO TRUE
               IF  UDAY = 31
                   SET I-98                TO TRUE
               END-IF
      *                                                                   VRG170
           END-IF
           SET I-99                        TO TRUE
           IF  (NOT-I-98)
               GO TO SLUTT-T
      *                                                                   VRG170
      * ***************************************************               VRG170
      *                                                                   VRG170
           END-IF
           IF  (I-L1)
               SET NOT-I-30                TO TRUE
               PERFORM VARYING NYF-I FROM 1 BY 1
                         UNTIL NYF-I > NYF-MAX
                   MOVE 0                  TO NYF (NYF-I)
               END-PERFORM
               SET NYF-I                   TO 1
               PERFORM VARYING NYA-ELG-I FROM 1 BY 1
                         UNTIL NYA-ELG-I > NYA-ELG-MAX
                   MOVE 0                  TO NYA-ELG (NYA-ELG-I)
               END-PERFORM
               SET NYA-ELG-I               TO 1
               MOVE 0                      TO L1V
               MOVE 0                      TO L1T
               MOVE 0                      TO L1K
           END-IF
           IF  (I-L2)
               PERFORM MNDRUT-S
      *                                                                   VRG170
           END-IF
           IF  (I-01)
               SET NOT-I-10                TO TRUE
               IF  SKAF = '9'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  SLETT = 'S'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-10)
               OR  (I-01 AND I-11)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET I-30                    TO TRUE
      *                                                                   VRG170
           END-IF
           IF  (I-01)
               SUBTRACT ANTUT FROM ANTINN GIVING BEH
               MULTIPLY SELVK BY BEH   GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V
               MULTIPLY SELVK BY ANTINN GIVING TILGV ROUNDED
               ADD TILGV                   TO L1T
           END-IF
           IF  (I-01 AND I-75)
               MULTIPLY SELVK BY ANTUT GIVING KORRV ROUNDED
               ADD KORRV                   TO L1K
      ******************************************************
      * PRISTILEGGSVERDI LEGGES TIL                        *
      ******************************************************
           END-IF
           IF  (I-01 AND I-70)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-71                TO TRUE
               IF  PRITYP = ' '
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  PRITYP = 'I'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  PRITYP = 'S'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  PRITYP = 'V'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-71)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MULTIPLY PRITIL BY BEH  GIVING VERDIT ROUNDED
               ADD VERDIT                  TO L1V
      ***
           END-IF
           IF  (I-01)
               MULTIPLY PRITIL BY ANTINN GIVING TILGVT ROUNDED
               ADD TILGVT                  TO L1T
      ***
           END-IF
           IF  (I-01 AND I-75)
               MULTIPLY PRITIL BY ANTUT GIVING KORRVT ROUNDED
               ADD KORRVT                  TO L1K
      ******************************************************
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
      *                                                                   VRG170
           END-IF
           IF  (NOT-I-U1)
               SET NOT-I-20                TO TRUE
               IF  RA = 'AI'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-U1)
               SET NOT-I-21                TO TRUE
               IF  RA = 'LV'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20 AND NOT-I-21)
               GO TO SLUTT-T
      *                                                                   VRG170
           END-IF
           SET NYF-I                       TO 1
           PERFORM VARYING ARF-I FROM 1 BY 1
                     UNTIL ARF-I > ARF-MAX
                        OR NYF-I > NYF-MAX
               ADD ARF (ARF-I)  TO ZERO GIVING NYF (NYF-I)
               SET NYF-I                UP BY 1
           END-PERFORM
           SET NYF-I                       TO 1
           SET NYA-ELG-I                   TO 1
           PERFORM VARYING ARA-ELG-I FROM 1 BY 1
                     UNTIL ARA-ELG-I > ARA-ELG-MAX
                        OR NYA-ELG-I > NYA-ELG-MAX
               ADD ARA-ELG (ARA-ELG-I)  TO ZERO GIVING NYA-ELG
                                                           (NYA-ELG-I)
               SET NYA-ELG-I            UP BY 1
           END-PERFORM
           SET NYA-ELG-I                   TO 1
           IF  (NOT-I-30)
               MOVE 0                      TO NYA-ELG (M)
           END-IF
           IF  (NOT-I-30 AND I-75)
               MOVE 0                      TO NYINNV
           END-IF
           IF  (NOT-I-30)
               GO TO SLUTT-T
      *                                                                   VRG170
      *  VED KJØRING PR 31/12 FOR FIRMA SOM HAR AVIKENDE                  VRG170
      *  REGNSKAPSÅR SETTES INNGÅENDE VERDI AV TILLGANGER                 VRG170
      *  TIL GAMMEL ING.VERDI MINUS VERDI AV SALG. (ANTALL-UT)            VRG170
      *  DET VIL SI AT VI KORRIGERER FOR DEN TILGANGSVERDI SOM            VRG170
      *  BLIR NEDSKREVET VED KJØRING PR 31/12. (ANTUT=ANTUT-ANTINN)       VRG170
      *                                                                   VRG170
           END-IF
           IF  (I-20 AND I-75)
               SUBTRACT L1K FROM INNV  GIVING NYINNV ROUNDED
           END-IF
           IF  (I-20 AND NOT-I-75)
               ADD L1V TO ZERO         GIVING NYINNV ROUNDED
      *                                                                   VRG170
           END-IF
           IF  (I-20)
               SUBTRACT INNV               FROM L1T
               ADD L1T TO ZERO         GIVING NYA-ELG (M) ROUNDED
           END-IF
           IF  (I-21)
               ADD L1V TO ZERO         GIVING NYA-ELG (M) ROUNDED
      *                                                                   VRG170
           END-IF
           .
 
       SLUTT-T.
      *                                                                   VRG170
           CONTINUE.
 
       MNDRUT-S SECTION.
       MNDRUT-S-P.
           SET NOT-I-75                    TO TRUE
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-74                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-74                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (I-74)
               MOVE 12                     TO M
           END-IF
           IF  (NOT-I-74)
               SUBTRACT REGM FROM 12   GIVING M
               ADD 1                       TO M
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  M NOT = 12
               SET I-75                    TO TRUE
           END-IF.
      ***************************************                             VRG170
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD L1V                     TO L2V
               ADD L1V                     TO L2VA
      *                                                                   VRG170
      ***************************************                             VRG170
      *  RUTINE FOR Å FINNE REGNSKAPSÅRET   *                             VRG170
      ***************************************                             VRG170
           END-IF
           .
 
       VAREREC-GET SECTION.
       VAREREC-GET-P.
           IF  VAREREC-EOF-OFF
               READ VAREREC
               AT END
                   SET VAREREC-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREREC-FLDOFF SECTION.
       VAREREC-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-11                TO TRUE
               SET NOT-I-70                TO TRUE
           END-EVALUATE.
 
       VAREREC-FLDSET SECTION.
       VAREREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE VAREREC-IO-AREA (4:1)  TO SKAF (1:1)
               MOVE VAREREC-IO-AREA (5:9)  TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               IF  SELVK = ZERO
                   SET I-11                TO TRUE
               END-IF
               MOVE VAREREC-IO-AREA (14:5) TO ANTINN-IO
               MOVE VAREREC-IO-AREA (19:5) TO ANTUT-IO
               MOVE VAREREC-IO-AREA (24:5) TO VGR (1:5)
               MOVE VAREREC-IO-AREA (29:1) TO MERK (1:1)
               MOVE VAREREC-IO-AREA (30:1) TO SLETT (1:1)
               MOVE VAREREC-IO-AREA (31:4) TO PRITIL-IO
               IF  PRITIL = ZERO
                   SET I-70                TO TRUE
               END-IF
               MOVE VAREREC-IO-AREA (35:1) TO PRITYP (1:1)
           END-EVALUATE.
 
       VAREREC-IDSET SECTION.
       VAREREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREREC-CHK-LEVEL SECTION.
       VAREREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREREC-LEVEL-01
               MOVE VAREREC-IO-AREA (1:3)  TO VAREREC-01-L2-FIRMA
               MOVE VAREREC-IO-AREA (24:5) TO VAREREC-01-L1-VGR
               IF  VAREREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREREC-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREREC-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREREC-01-L2         TO THE-PRIOR-L2
               MOVE  VAREREC-01-L1         TO THE-PRIOR-L1
               SET VAREREC-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREREC-MATCH-SET SECTION.
       VAREREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (1:3)  TO VAREREC-M-01-M2-FIRMA
               MOVE VAREREC-IO-AREA (24:5) TO VAREREC-M-01-M1-VGR
           END-EVALUATE.
 
       GMLFILE-GET SECTION.
       GMLFILE-GET-P.
           IF  GMLFILE-EOF-OFF
               READ GMLFILE
               AT END
                   SET GMLFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLFILE-FLDSET SECTION.
       GMLFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLFILE-IO-AREA (1:2)  TO RA (1:2)
               MOVE GMLFILE-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE GMLFILE-IO-AREA (6:5)  TO VGR (1:5)
               MOVE 71                     TO BW-A
               PERFORM VARYING ARF-I FROM ARF-MAX BY -1
                         UNTIL ARF-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE GMLFILE-IO-AREA (BW-A:5) TO XI-ARF-GRP
                   MOVE XI-ARF             TO ARF (ARF-I)
               END-PERFORM
               MOVE 131                    TO BW-A
               PERFORM VARYING ARA-ELG-I FROM ARA-ELG-MAX BY -1
                         UNTIL ARA-ELG-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE GMLFILE-IO-AREA (BW-A:5) TO XI-ARA-ELG-GRP
                   MOVE XI-ARA-ELG         TO ARA-ELG (ARA-ELG-I)
               END-PERFORM
               MOVE GMLFILE-IO-AREA (131:5) TO INNV-IO
               MOVE GMLFILE-IO-AREA (1:164) TO REC (1:164)
           END-EVALUATE.
 
       GMLFILE-IDSET SECTION.
       GMLFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       GMLFILE-CHK-LEVEL SECTION.
       GMLFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO GMLFILE-LEVEL-02
               MOVE GMLFILE-IO-AREA (3:3)  TO GMLFILE-02-L2-FIRMA
               MOVE GMLFILE-IO-AREA (6:5)  TO GMLFILE-02-L1-VGR
               IF  GMLFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  GMLFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  GMLFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  GMLFILE-02-L2         TO THE-PRIOR-L2
               MOVE  GMLFILE-02-L1         TO THE-PRIOR-L1
               SET GMLFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       GMLFILE-MATCH-SET SECTION.
       GMLFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLFILE-IO-AREA (3:3)  TO GMLFILE-M-02-M2-FIRMA
               MOVE GMLFILE-IO-AREA (6:5)  TO GMLFILE-M-02-M1-VGR
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (134:2) TO REGM-IO
               INSPECT REGM-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
           IF  VAREREC-EOF
               MOVE HIGH-VALUES            TO VAREREC-MC
                                              VAREREC-MP
           END-IF
           IF  GMLFILE-EOF
               MOVE HIGH-VALUES            TO GMLFILE-MC
                                              GMLFILE-MP
           END-IF
           IF  VAREREC-MC < VAREREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  GMLFILE-MC < GMLFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREREC-MC < GMLFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREREC-PROCESS     TO TRUE
                   MOVE VAREREC-MC         TO VAREREC-MP
                   IF  VAREREC-MC = GMLFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLFILE-MC < VAREREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLFILE-PROCESS     TO TRUE
                   MOVE GMLFILE-MC         TO GMLFILE-MP
                   IF  GMLFILE-MC = VAREREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREREC-MC = GMLFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREREC-PROCESS     TO TRUE
                   MOVE VAREREC-MC         TO VAREREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-20 AND NOT-I-21)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               MOVE REC                    TO NYFILE-IO-AREA (1:164)
               WRITE NYFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-20)
           OR  (I-02 AND I-21)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               MOVE REC                    TO NYFILE-IO-AREA (1:164)
               MOVE 71                     TO BW-A
               PERFORM VARYING NYF-I FROM NYF-MAX BY -1
                         UNTIL NYF-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE NYF (NYF-I)        TO XO-90P
                   MOVE XO-90P-EF          TO NYFILE-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 131                    TO BW-A
               PERFORM VARYING NYA-ELG-I FROM NYA-ELG-MAX BY -1
                         UNTIL NYA-ELG-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE NYA-ELG (NYA-ELG-I) TO XO-90P
                   MOVE XO-90P-EF          TO NYFILE-IO-AREA (BW-A:5)
               END-PERFORM
               IF  (I-20)
                   MOVE NYINNV             TO XO-90P
                   MOVE XO-90P-EF          TO NYFILE-IO-AREA (131:5)
               END-IF
               WRITE NYFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***    KONTROLL-LISTE OV' TO LISTE-IO-AREA (1:24)
               MOVE 'ER LAGERVERDI OG VERDI T' TO LISTE-IO-AREA (25:24)
               MOVE 'ILGANG PR ÅRSSKIFTE.    ' TO LISTE-IO-AREA (49:24)
               IF  (NOT-I-U1)
                   MOVE '(KUNN TILGANG ENDRET) **' TO LISTE-IO-AREA
                                                               (73:24)
               END-IF
               IF  (I-U1)
                   MOVE '(KUNN LAGER ENDRET)  ***' TO LISTE-IO-AREA
                                                               (73:24)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'LAGERVERDI '          TO LISTE-IO-AREA (20:11)
               MOVE 'LAGERVERDI AVR. '     TO LISTE-IO-AREA (35:16)
               MOVE 'DATO'                 TO LISTE-IO-AREA (57:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (62:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***    KONTROLL-LISTE OV' TO LISTE-IO-AREA (1:24)
               MOVE 'ER LAGERVERDI OG VERDI T' TO LISTE-IO-AREA (25:24)
               MOVE 'ILGANG PR ÅRSSKIFTE.    ' TO LISTE-IO-AREA (49:24)
               IF  (NOT-I-U1)
                   MOVE '(KUNN TILGANG ENDRET) **' TO LISTE-IO-AREA
                                                               (73:24)
               END-IF
               IF  (I-U1)
                   MOVE '(KUNN LAGER ENDRET)  ***' TO LISTE-IO-AREA
                                                               (73:24)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'LAGERVERDI '          TO LISTE-IO-AREA (20:11)
               MOVE 'LAGERVERDI AVR. '     TO LISTE-IO-AREA (35:16)
               MOVE 'DATO'                 TO LISTE-IO-AREA (57:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (62:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (2:3)
               MOVE L2V                    TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (16:15)
               INITIALIZE L2V
               MOVE L2VA                   TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (39:12)
               INITIALIZE L2VA
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
           SET VAREREC-LEVEL-INIT          TO TRUE
           INITIALIZE VAREREC-DATA-FIELDS
           SET VAREREC-EOF-OFF             TO TRUE
           SET VAREREC-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREREC-MC
                                              VAREREC-MP
           OPEN INPUT VAREREC
           SET GMLFILE-LEVEL-INIT          TO TRUE
           INITIALIZE GMLFILE-DATA-FIELDS
           SET GMLFILE-EOF-OFF             TO TRUE
           SET GMLFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO GMLFILE-MC
                                              GMLFILE-MP
           OPEN INPUT GMLFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT NYFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARF-I FROM 1 BY 1
                     UNTIL ARF-I > ARF-MAX
               INITIALIZE ARF (ARF-I)
           END-PERFORM
           SET ARF-I                       TO 1
           PERFORM VARYING ARA-ELG-I FROM 1 BY 1
                     UNTIL ARA-ELG-I > ARA-ELG-MAX
               INITIALIZE ARA-ELG (ARA-ELG-I)
           END-PERFORM
           SET ARA-ELG-I                   TO 1
           PERFORM VARYING NYF-I FROM 1 BY 1
                     UNTIL NYF-I > NYF-MAX
               INITIALIZE NYF (NYF-I)
           END-PERFORM
           SET NYF-I                       TO 1
           PERFORM VARYING NYA-ELG-I FROM 1 BY 1
                     UNTIL NYA-ELG-I > NYA-ELG-MAX
               INITIALIZE NYA-ELG (NYA-ELG-I)
           END-PERFORM
           SET NYA-ELG-I                   TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREREC
           CLOSE GMLFILE
           CLOSE FIRMAF
           CLOSE NYFILE
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
