       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG014R.
      **********************************************  Z-WIN-RPG2      *
      *  PROGRAM...VRG014
      *  UTPLUKK AV ARTIKKLER SOM SKAL KOPIERES FRA        *
      *  ETT FIRMA TIL ETT ELLER FLERE ANDRE FIRMAER.      *
      *  RETTELSE 6/3-92: UTGÅR MERKES PÅ SLETTEDE VARER,  *
      *           OG PÅ VARER MERKET MED UTGÅR HVOR        *
      *           BEHOLDNINGEN ER 0.                       *
      *           ER DET BEHOLDNING BLIR UTGÅRMERKET       *
      *           FJERNET,KUN PÅ DE MED VALG09=J.24/3-99.  *
      *  7/12-94                                           *
      *  NY UTGAVE AV VRG014 SOM MERGER OG DANNER ARRAY    *
      *     ISTEDENFOR 3 TABELLOPPSLAG.                    *
      * 15/06-00                                           *
      *  FJERNET SLIK AT PRISTYPE IKKE SKAL HENSYNTAS.     *
      * 05.04.01 ENDRET AVRUNDINGSREGLER TIL SUBRUT AVR001 *
      * 14.03.03 ENDRET SLIK AT MERKNAD BLIR FJERNET HOS   *
      *     MOTTAKER NÅR DEN ER BLANK M/BEH HOS LEVERANDØR *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG014.rpg
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
           SELECT TILTAB
               ASSIGN TO UT-S-TILTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILTAB-STATUS.
           SELECT VARE
               ASSIGN TO UT-S-VARE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARE-STATUS.
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
       FD TILTAB
               BLOCK CONTAINS 120
               RECORD CONTAINS 60.
       01  TILTAB-IO-AREA.
           05  TILTAB-IO-AREA-X            PICTURE X(60).
       FD VARE
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  VARE-IO-AREA.
           05  VARE-IO-AREA-X              PICTURE X(164).
       FD KOPI
               BLOCK CONTAINS 340
               RECORD CONTAINS 170.
       01  KOPI-IO-AREA.
           05  KOPI-IO-AREA-X              PICTURE X(170).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ART-MAX   VALUE 200             PICTURE 9(4) USAGE BINARY.
       77  ARA-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       77  ARO-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ART-TABLE.
               10  ART-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY ART-I
                                                      ART-S.
                   15  ART                 PICTURE X(46).
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(1).
           05  ARO-TABLE.
               10  ARO-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARO-I
                                                      ARO-S.
                   15  ARO                 PICTURE X(1).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TILTAB-STATUS               PICTURE 99 VALUE 0.
           10  VARE-STATUS                 PICTURE 99 VALUE 0.
           10  KOPI-STATUS                 PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TILTAB-EOF-OFF          VALUE '0'.
               88  TILTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILTAB-READ-OFF         VALUE '0'.
               88  TILTAB-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILTAB-PROCESS-OFF      VALUE '0'.
               88  TILTAB-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  TILTAB-LEVEL-INIT-OFF   VALUE '0'.
               88  TILTAB-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-EOF-OFF            VALUE '0'.
               88  VARE-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-READ-OFF           VALUE '0'.
               88  VARE-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARE-PROCESS-OFF        VALUE '0'.
               88  VARE-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARE-LEVEL-INIT-OFF     VALUE '0'.
               88  VARE-LEVEL-INIT         VALUE '1'.
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
           05  LDATA-XX-DATA-FIELDS.
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(11).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(9)V9(2).
           05  TILTAB-LEVEL-02.
               10  TILTAB-02-L2.
                   15  TILTAB-02-L2-FRAFIR PICTURE X(3).
               10  TILTAB-02-L1.
                   15  TILTAB-02-L1-FRAVGR PICTURE X(5).
                   15  TILTAB-02-L1-FRAALF PICTURE X(3).
                   15  TILTAB-02-L1-FRAPT  PICTURE X(1).
           05  TILTAB-DATA-FIELDS.
               10  FRAFIR                  PICTURE X(3).
               10  FRAVGR                  PICTURE X(5).
               10  FRAALF                  PICTURE X(3).
               10  FRAPT                   PICTURE X(1).
               10  FRASEQ-IO.
                   15  FRASEQ              PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  FRADAT                  PICTURE X(46).
           05  TILTAB-MP                   PICTURE X(11).
           05  TILTAB-MC                   PICTURE X(11).
           05  TILTAB-M-02             REDEFINES TILTAB-MC.
               10  TILTAB-M-02-M4.
                   15  TILTAB-M-02-M4-FRAFIR-G.
                       20  TILTAB-M-02-M4-FRAFIR PICTURE X(3).
               10  TILTAB-M-02-M3.
                   15  TILTAB-M-02-M3-FRAVGR-G.
                       20  TILTAB-M-02-M3-FRAVGR PICTURE X(5).
               10  TILTAB-M-02-M2.
                   15  TILTAB-M-02-M2-FRAALF-G.
                       20  TILTAB-M-02-M2-FRAALF PICTURE X(3).
           05  VARE-LEVEL-01.
               10  VARE-01-L2.
                   15  VARE-01-L2-FIRMA    PICTURE X(3).
               10  VARE-01-L1.
                   15  VARE-01-L1-ALFA     PICTURE X(3).
                   15  VARE-01-L1-PT       PICTURE X(1).
                   15  VARE-01-L1-VGR      PICTURE X(5).
           05  VARE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  EDBNR1                  PICTURE X(1).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ART5                    PICTURE X(15).
               10  ART10                   PICTURE X(10).
               10  ART15                   PICTURE X(5).
               10  BETEGN                  PICTURE X(30).
               10  UTSPRI-IO.
                   15  UTSPRI              PICTURE S9(7)V9(2).
               10  ENDRET-IO.
                   15  ENDRET              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ALTNR                   PICTURE X(7).
               10  PT                      PICTURE X(1).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRISTT                  PICTURE X(1).
               10  FAB                     PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  UTGA-ELGR               PICTURE X(1).
               10  SLETT                   PICTURE X(1).
               10  PRISTI-IO.
                   15  PRISTI              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  VARE-MP                     PICTURE X(11).
           05  VARE-MC                     PICTURE X(11).
           05  VARE-M-01               REDEFINES VARE-MC.
               10  VARE-M-01-M4.
                   15  VARE-M-01-M4-FIRMA-G.
                       20  VARE-M-01-M4-FIRMA PICTURE X(3).
               10  VARE-M-01-M3.
                   15  VARE-M-01-M3-VGR-G.
                       20  VARE-M-01-M3-VGR PICTURE X(5).
               10  VARE-M-01-M2.
                   15  VARE-M-01-M2-ALFA-G.
                       20  VARE-M-01-M2-ALFA PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  Y-IO.
                   15  Y                   PICTURE S9(3).
               10  BEH-IO.
                   15  BEH                 PICTURE S9(7)V9(2).
               10  MAXANT-IO.
                   15  MAXANT              PICTURE S9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  Z-IO.
                   15  Z                   PICTURE S9(2).
               10  ANR                     PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(8).
               10  FELT46                  PICTURE X(46).
               10  FELT15                  PICTURE X(15).
               10  FELT31                  PICTURE X(31).
               10  NYFNR                   PICTURE X(3).
               10  FAKT                    PICTURE X(12).
               10  FSVS-IO.
                   15  FSVS                PICTURE S9(3)V9(3).
               10  FUTSP-IO.
                   15  FUTSP               PICTURE S9(3)V9(3).
               10  FELT10                  PICTURE X(10).
               10  VALGMU                  PICTURE X(18).
               10  VGRALF                  PICTURE X(8).
               10  NYVGR                   PICTURE X(5).
               10  NYALFA                  PICTURE X(3).
               10  FELT2                   PICTURE X(2).
               10  NYPT                    PICTURE X(1).
               10  VALG18                  PICTURE X(1).
               10  FELT17                  PICTURE X(17).
               10  VALG17                  PICTURE X(1).
               10  FELT16                  PICTURE X(16).
               10  VALG16                  PICTURE X(1).
               10  VALG15                  PICTURE X(1).
               10  FELT14                  PICTURE X(14).
               10  VALG14                  PICTURE X(1).
               10  FELT13                  PICTURE X(13).
               10  VALG13                  PICTURE X(1).
               10  FELT12                  PICTURE X(12).
               10  VALG12                  PICTURE X(1).
               10  FELT11                  PICTURE X(11).
               10  VALG11                  PICTURE X(1).
               10  VALG10                  PICTURE X(1).
               10  FELT09                  PICTURE X(9).
               10  VALG09                  PICTURE X(1).
               10  FELT08                  PICTURE X(8).
               10  VALG08                  PICTURE X(1).
               10  FELT07                  PICTURE X(7).
               10  VALG07                  PICTURE X(1).
               10  FELT06                  PICTURE X(6).
               10  VALG06                  PICTURE X(1).
               10  FELT05                  PICTURE X(5).
               10  VALG05                  PICTURE X(1).
               10  FELT04                  PICTURE X(4).
               10  VALG04                  PICTURE X(1).
               10  FELT03                  PICTURE X(3).
               10  VALG03                  PICTURE X(1).
               10  FELT02                  PICTURE X(2).
               10  VALG02                  PICTURE X(1).
               10  VALG01                  PICTURE X(1).
               10  NYSVS-IO.
                   15  NYSVS               PICTURE S9(7)V9(2).
               10  NYUTSP-IO.
                   15  NYUTSP              PICTURE S9(7)V9(2).
               10  NYLEVP-IO.
                   15  NYLEVP              PICTURE S9(7)V9(2).
               10  NYFAK-IO.
                   15  NYFAK               PICTURE S9(3)V9(3).
               10  NYLRAB-IO.
                   15  NYLRAB              PICTURE S9(2)V9(1).
               10  EDATO-IO.
                   15  EDATO               PICTURE S9(7).
               10  ANTNY-IO.
                   15  ANTNY               PICTURE S9(8).
               10  F3-IO.
                   15  F3                  PICTURE S9(3).
               10  F2-IO.
                   15  F2                  PICTURE S9(2).
               10  F1-IO.
                   15  F1                  PICTURE S9(1).
               10  US4                     PICTURE X(9).
               10  US5-IO.
                   15  US5                 PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-80YN9                PICTURE ZZZZZZZ9.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
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
           05  MOVEA-COUNT                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE1                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA1                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE2                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA2                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-LENGTH                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-OFFSET                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-TEMP                  PICTURE X(256).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  TILTAB-PROCESS
               SET TILTAB-PROCESS-OFF      TO TRUE
               SET TILTAB-READ             TO TRUE
           END-IF
 
           IF  TILTAB-READ
               PERFORM TILTAB-GET
               SET TILTAB-READ-OFF         TO TRUE
               IF  NOT TILTAB-EOF
                   PERFORM TILTAB-MATCH-SET
               END-IF
           END-IF
 
           IF  VARE-PROCESS
               SET VARE-PROCESS-OFF        TO TRUE
               SET VARE-READ               TO TRUE
           END-IF
 
           IF  VARE-READ
               PERFORM VARE-GET
               SET VARE-READ-OFF           TO TRUE
               IF  NOT VARE-EOF
                   PERFORM VARE-MATCH-SET
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
 
           IF  TILTAB-PROCESS
               PERFORM TILTAB-IDSET
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-IDSET
           END-IF
 
           IF  TILTAB-PROCESS
               PERFORM TILTAB-CHK-LEVEL
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-CHK-LEVEL
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
 
           IF  TILTAB-PROCESS
               PERFORM TILTAB-FLDSET
           END-IF
 
           IF  VARE-PROCESS
               PERFORM VARE-FLDOFF
               PERFORM VARE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  TILTAB-PROCESS
           OR  VARE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-65                    TO TRUE
      **********************************************************
      *  RUTINE FOR Å BYGGE OPP TIL.FIRMA.ARRAY                *
      **********************************************************
           IF  (I-02)
               ADD FRASEQ TO ZERO      GIVING Y
               MOVE FRADAT                 TO ART (Y)
               GO TO SLUTT-T
      **********************************************************
      *  TEST OM DET SKAL KOPIERES NOE FRA DETTE FIRMA.    *****
      **********************************************************
           END-IF
           IF  (NOT-I-MR)
               GO TO SLUTT-T
      *****************************************
      *    SKAFFEVARER KOPIERES IKKE          *
      *****************************************
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  EDBNR1 = '9'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-59)
               GO TO SLUTT-T
      ********************************************
      *    MERKNAD=N, INGEN OVERFØRING TIL KUNDER
      ********************************************
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  UTGA-ELGR = 'N'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-59)
               GO TO SLUTT-T
      **********************************************************
      *    VARER MERKET SLETTES, SKAL MERKES UTGÅR HOS MOTAGER *
      **********************************************************
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  SLETT = 'S'
               SET I-58                    TO TRUE
           END-IF
      ***********************************************************
      *    VARER MERKET MED UTGÅR SKAL MERKES UTGÅR HOS MOTAGER *
      *    HVIS BEHOLDNING ER 0                                 *
      ***********************************************************
           SET NOT-I-42                    TO TRUE
           SET NOT-I-43                    TO TRUE
           SET NOT-I-45                    TO TRUE
           SET NOT-I-46                    TO TRUE
           SUBTRACT ANTUT FROM ANTINN  GIVING BEH
           SET NOT-I-41                    TO TRUE
           IF  BEH > 0
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  UTGA-ELGR = '1'
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-58)
               SET NOT-I-43                TO TRUE
               IF  UTGA-ELGR = '2'
                   SET I-43                TO TRUE
               END-IF
               SET NOT-I-44                TO TRUE
               IF  UTGA-ELGR = '4'
                   SET I-44                TO TRUE
               END-IF
               SET NOT-I-48                TO TRUE
               IF  UTGA-ELGR = ' '
                   SET I-48                TO TRUE
               END-IF
           END-IF
           IF  (I-42 AND NOT-I-41)
               SET I-45                    TO TRUE
           END-IF
           IF  (I-58)
               SET I-45                    TO TRUE
           END-IF
           IF  (I-42 AND I-41 AND NOT-I-58)
               SET I-46                    TO TRUE
           END-IF
           IF  (I-48 AND I-41 AND NOT-I-58)
               SET I-46                    TO TRUE
      ****************************************************
      *     RUTINE FOR KONTROLL AV ARTIKKELNUMMER        *
      ****************************************************
           END-IF
           MOVE 20                         TO MAXANT
           IF  (I-07)
               MOVE 15                     TO MAXANT
           END-IF
           IF  (I-06)
               MOVE 10                     TO MAXANT
           END-IF
           IF  (I-05)
               MOVE 5                      TO MAXANT
      *
           END-IF
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               MOVE ' '                    TO ARO (ARO-I)
           END-PERFORM
           MOVE 1                          TO MOVEA-SA1 MOVEA-SA2
           MOVE 20                         TO MOVEA-SIZE1
           MULTIPLY ARA-MAX BY 1 GIVING MOVEA-SIZE2
           IF  MOVEA-SIZE1 > MOVEA-SIZE2
               MOVE MOVEA-SIZE2            TO MOVEA-SIZE1
           END-IF
           MOVE ARTNR
                    TO ARA-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           SUBTRACT X                      FROM X
           SUBTRACT Z                      FROM Z
      *
           .
 
       RUTA-T.
           ADD 1                           TO X
           SET NOT-I-21                    TO TRUE
           IF  X > MAXANT
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO PRINT-X-T
           END-IF
           MOVE ARA (X)                    TO ANR
           SET NOT-I-40                    TO TRUE
           IF  ANR = '.'
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '""""
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '&'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ','
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '+'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '_'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '-'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '/'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ')'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '('
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '*'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ' '
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '='
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '%'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '\'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '?'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '^'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '€'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '@'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ':'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               GO TO RUTA-T
      **********************************************************
      *      RUTINE FOR OPPBYGGING AV OPPSLAGSNUMMER           *
      **********************************************************
           END-IF
           ADD 1                           TO Z
           SET NOT-I-22                    TO TRUE
           IF  Z > MAXANT
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               GO TO PRINT-X-T
           END-IF
           MOVE ANR                        TO ARO (Z)
           GO TO RUTA-T
      **********************************************************
           .
 
       PRINT-X-T.
           MOVE 0                          TO Y
           ADD 1                           TO ANT
      ***********************************************************
      *  LOOP FOR FIRMAER SOM SKAL HA KOPI AV VAREARKIV.    *****
      ***********************************************************
           .
 
       LOOP-T.
           ADD 1                           TO Y
           SET NOT-I-62                    TO TRUE
           IF  Y > FRASEQ
               SET I-62                    TO TRUE
           END-IF
           IF  (I-62)
               GO TO SLUTT-T
           END-IF
           MOVE ART (Y)                    TO FELT46
      ***********************************************************
      *       SPLITTING AV FELTER FRA TABELL NYTT-FIRMA-KOPI.   *
      ***********************************************************
      *  TIL FIRMANR,FAKTOR FOR UTREGNING AV SELVKOST OG UTSALGSPRIS.
           MOVE FELT46 (1:15)              TO FELT15
           MOVE FELT46 (16:31)             TO FELT31
           MOVE FELT15 (1:3)               TO NYFNR
           MOVE FELT15 (4:12)              TO FAKT
           MOVE FAKT (1:6)                 TO FSVS
           MOVE FAKT (7:6)                 TO FUTSP-IO
      *  VAREGRUPPE,ALFAKODE,PRISTYPE.
           MOVE FELT31 (1:10)              TO FELT10
           MOVE FELT31 (14:18)             TO VALGMU
           MOVE FELT10 (1:8)               TO VGRALF
           MOVE VGRALF (1:5)               TO NYVGR
           MOVE VGRALF (6:3)               TO NYALFA
           MOVE FELT10 (9:2)               TO FELT2
           MOVE FELT2 (1:1)                TO NYPT
      * VALGMULIGHETER.
           MOVE VALGMU (18:1)              TO VALG18
           MOVE VALGMU (1:17)              TO FELT17
           MOVE FELT17 (17:1)              TO VALG17
           MOVE VALGMU (1:16)              TO FELT16
           MOVE FELT16 (16:1)              TO VALG16
           MOVE VALGMU (1:15)              TO FELT15
           MOVE FELT15 (15:1)              TO VALG15
           MOVE VALGMU (1:14)              TO FELT14
           MOVE FELT14 (14:1)              TO VALG14
           MOVE VALGMU (1:13)              TO FELT13
           MOVE FELT13 (13:1)              TO VALG13
           MOVE VALGMU (1:12)              TO FELT12
           MOVE FELT12 (12:1)              TO VALG12
           MOVE VALGMU (1:11)              TO FELT11
           MOVE FELT11 (11:1)              TO VALG11
           MOVE VALGMU (1:10)              TO FELT10
           MOVE FELT10 (10:1)              TO VALG10
           MOVE VALGMU (1:9)               TO FELT09
           MOVE FELT09 (9:1)               TO VALG09
           MOVE VALGMU (1:8)               TO FELT08
           MOVE FELT08 (8:1)               TO VALG08
           MOVE VALGMU (1:7)               TO FELT07
           MOVE FELT07 (7:1)               TO VALG07
           MOVE VALGMU (1:6)               TO FELT06
           MOVE FELT06 (6:1)               TO VALG06
           MOVE VALGMU (1:5)               TO FELT05
           MOVE FELT05 (5:1)               TO VALG05
           MOVE VALGMU (1:4)               TO FELT04
           MOVE FELT04 (4:1)               TO VALG04
           MOVE VALGMU (1:3)               TO FELT03
           MOVE FELT03 (3:1)               TO VALG03
           MOVE VALGMU (1:2)               TO FELT02
           MOVE FELT02 (2:1)               TO VALG02
           MOVE VALGMU (1:1)               TO VALG01
      *
      *  VALG09 = N INGEN ENDRING AV MERKNADSFELT. ****
           SET NOT-I-47                    TO TRUE
           IF  VALG09 = 'N'
               SET I-47                    TO TRUE
           END-IF
           IF  (I-47)
               SET NOT-I-46                TO TRUE
      *
      *  BEREGNING AV SELVKOST OG UTSALGSPRIS     *****
           END-IF
           MULTIPLY FSVS BY UTSPRI     GIVING NYSVS
           MULTIPLY FUTSP BY UTSPRI    GIVING NYUTSP
      *  BEREGNING AV LEVR.PRIS OG LEVR.REBATT    *****
           ADD UTSPRI TO ZERO          GIVING NYLEVP
           SUBTRACT FSVS FROM 1        GIVING NYFAK
           SET NOT-I-30                    TO TRUE
           IF  NYFAK > 0
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               MOVE 0                      TO NYLRAB
           END-IF
           IF  (I-30)
               MULTIPLY NYFAK BY 100   GIVING NYLRAB
      *
      *  SKAL UTSALGSPRIS RUNDES AV (STATENS-REGLER)   *****
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  VALG06 = 'J'
               SET I-70                    TO TRUE
           END-IF
           IF  (I-70)
               PERFORM RUNDAV-S
      *
           END-IF
           IF  (I-01)
               ADD ENDRET TO ZERO      GIVING EDATO
           END-IF
      ** MLLzo
           IF EDATO < 0
               MULTIPLY -1 BY EDATO
           END-IF
      *
           SET I-65                        TO TRUE
           PERFORM EXCEPTION-OUTPUT
           ADD 1                           TO ANTNY
           SET NOT-I-65                    TO TRUE
           GO TO LOOP-T
      *****************************************************
           .
 
       SLUTT-T.
      ******************************************************              PRI635
      *  SUBRUTINE FOR AVRUNDING AV PRISER.               *
      *    0,00 -   1,50  AVRUNDES IKKE.                  *               PRI635
      *    1,51 -  10,00  AVRUNDES TIL NÆRMESTE 0,05      *               PRI635
      *   10,01 -  25,00  AVRUNDES TIL NÆRMESTE 0,10      *               PRI635
      *   25,01 -  50,00  AVRUNDES TIL NÆRMESTE 0,25      *               PRI635
      *   50,01 - 100,00  AVRUNDES TIL NÆRMESTE 0,50      *               PRI635
      *  100,01 - 500,00  AVRUNDES TIL NÆRMESTE 1,00      *               PRI635
      *  500,01 OG STØRRE AVRUNDES TIL NÆRMESTE 5,00      *               PRI635
      ******************************************************              PRI635
           CONTINUE.
 
       RUNDAV-S SECTION.
       RUNDAV-S-P.
           SET NOT-I-81                    TO TRUE
           IF  NYUTSP NOT > 1,50
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               GO TO END-X-T
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  NYUTSP NOT > 10,00
               SET I-81                    TO TRUE
           END-IF
           MOVE NYUTSP (7:3)               TO F3-IO
           MOVE NYUTSP (8:2)               TO F2-IO
           MOVE NYUTSP (9:1)               TO F1-IO
           MOVE NYUTSP                     TO US4
           IF  (NOT-I-81)
               GO TO TEST3-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  F1 NOT < 3
               SET I-73                    TO TRUE
           END-IF
           SET NOT-I-78                    TO TRUE
           SET NOT-I-77                    TO TRUE
           IF  F1 NOT > 7
               SET I-77                    TO TRUE
           END-IF
           IF  F1 > 7
               SET I-78                    TO TRUE
           END-IF
           MOVE 0                          TO US4 (9:1)
           IF  (I-73 AND I-77)
               MOVE 5                      TO US4 (9:1)
           END-IF
           MOVE US4                        TO US5-IO
           IF  (I-78)
               ADD 10                      TO US5
           END-IF
           DIVIDE US5 BY 100           GIVING NYUTSP
           GO TO END-X-T.
 
       TEST3-T.
           SET NOT-I-81                    TO TRUE
           IF  NYUTSP NOT > 25,00
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               GO TO TEST4-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  F1 > 4
               SET I-73                    TO TRUE
           END-IF
           MOVE 0                          TO US4 (9:1)
           MOVE US4                        TO US5-IO
           IF  (I-73)
               ADD 10                      TO US5
           END-IF
           DIVIDE US5 BY 100           GIVING NYUTSP
           GO TO END-X-T.
 
       TEST4-T.
           SET NOT-I-81                    TO TRUE
           IF  NYUTSP NOT > 50,00
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               GO TO TEST5-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           ADD 12 TO F2                GIVING F3
           DIVIDE F3 BY 25             GIVING F1
           SET NOT-I-91                    TO TRUE
           SET NOT-I-92                    TO TRUE
           IF  F1 < 1
               SET I-91                    TO TRUE
           END-IF
           IF  F1 = 1
               SET I-92                    TO TRUE
           END-IF
           SET NOT-I-93                    TO TRUE
           IF  F1 = 2
               SET I-93                    TO TRUE
           END-IF
           SET NOT-I-95                    TO TRUE
           SET NOT-I-94                    TO TRUE
           IF  F1 > 3
               SET I-95                    TO TRUE
           END-IF
           IF  F1 = 3
               SET I-94                    TO TRUE
           END-IF
           IF  (I-91)
               MOVE 00                     TO US4 (8:2)
           END-IF
           IF  (I-92)
               MOVE 25                     TO US4 (8:2)
           END-IF
           IF  (I-93)
               MOVE 50                     TO US4 (8:2)
           END-IF
           IF  (I-94)
               MOVE 75                     TO US4 (8:2)
           END-IF
           IF  (I-95)
               MOVE 00                     TO US4 (8:2)
           END-IF
           MOVE US4                        TO US5-IO
           IF  (I-95)
               ADD 100                     TO US5
           END-IF
           DIVIDE US5 BY 100           GIVING NYUTSP
           GO TO END-X-T.
 
       TEST5-T.
           SET NOT-I-81                    TO TRUE
           IF  NYUTSP NOT > 100,00
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               GO TO TEST6-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  F2 NOT < 26
               SET I-73                    TO TRUE
           END-IF
           SET NOT-I-78                    TO TRUE
           SET NOT-I-77                    TO TRUE
           IF  F2 NOT > 75
               SET I-77                    TO TRUE
           END-IF
           IF  F2 > 75
               SET I-78                    TO TRUE
           END-IF
           MOVE 00                         TO US4 (8:2)
           IF  (I-73 AND I-77)
               MOVE 50                     TO US4 (8:2)
           END-IF
           MOVE US4                        TO US5-IO
           IF  (I-78)
               ADD 100                     TO US5
           END-IF
           DIVIDE US5 BY 100           GIVING NYUTSP
           GO TO END-X-T.
 
       TEST6-T.
           SET NOT-I-81                    TO TRUE
           IF  NYUTSP NOT > 500,00
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               GO TO TEST7-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  F2 > 50
               SET I-73                    TO TRUE
           END-IF
           MOVE 00                         TO US4 (8:2)
           MOVE US4                        TO US5-IO
           IF  (I-73)
               ADD 100                     TO US5
           END-IF
           DIVIDE US5 BY 100           GIVING NYUTSP
           GO TO END-X-T.
 
       TEST7-T.
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
           SET NOT-I-73                    TO TRUE
           IF  F3 NOT < 250
               SET I-73                    TO TRUE
           END-IF
           SET NOT-I-78                    TO TRUE
           SET NOT-I-77                    TO TRUE
           IF  F3 NOT > 750
               SET I-77                    TO TRUE
           END-IF
           IF  F3 > 750
               SET I-78                    TO TRUE
           END-IF
           MOVE 000                        TO US4 (7:3)
           IF  (I-73 AND I-77)
               MOVE 500                    TO US4 (7:3)
           END-IF
           MOVE US4                        TO US5-IO
           IF  (I-78)
               ADD 1000                    TO US5
           END-IF
           DIVIDE US5 BY 100           GIVING NYUTSP.
 
       END-X-T.
           CONTINUE.
 
       TILTAB-GET SECTION.
       TILTAB-GET-P.
           IF  TILTAB-EOF-OFF
               READ TILTAB
               AT END
                   SET TILTAB-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TILTAB-FLDSET SECTION.
       TILTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE TILTAB-IO-AREA (1:3)   TO FRAFIR (1:3)
               MOVE TILTAB-IO-AREA (4:5)   TO FRAVGR (1:5)
               MOVE TILTAB-IO-AREA (9:3)   TO FRAALF (1:3)
               MOVE TILTAB-IO-AREA (12:1)  TO FRAPT (1:1)
               MOVE TILTAB-IO-AREA (13:2)  TO FRASEQ-IO
               MOVE TILTAB-IO-AREA (15:46) TO FRADAT (1:46)
           END-EVALUATE.
 
       TILTAB-IDSET SECTION.
       TILTAB-IDSET-P.
           SET I-02                        TO TRUE.
 
       TILTAB-CHK-LEVEL SECTION.
       TILTAB-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO TILTAB-LEVEL-02
               MOVE TILTAB-IO-AREA (1:3)   TO TILTAB-02-L2-FRAFIR
               MOVE TILTAB-IO-AREA (4:5)   TO TILTAB-02-L1-FRAVGR
               MOVE TILTAB-IO-AREA (9:3)   TO TILTAB-02-L1-FRAALF
               MOVE TILTAB-IO-AREA (12:1)  TO TILTAB-02-L1-FRAPT
               IF  TILTAB-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  TILTAB-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  TILTAB-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  TILTAB-02-L2          TO THE-PRIOR-L2
               MOVE  TILTAB-02-L1          TO THE-PRIOR-L1
               SET TILTAB-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       TILTAB-MATCH-SET SECTION.
       TILTAB-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE TILTAB-IO-AREA (1:3)   TO TILTAB-M-02-M4-FRAFIR
               MOVE TILTAB-IO-AREA (4:5)   TO TILTAB-M-02-M3-FRAVGR
               MOVE TILTAB-IO-AREA (9:3)   TO TILTAB-M-02-M2-FRAALF
           END-EVALUATE.
 
       VARE-GET SECTION.
       VARE-GET-P.
           IF  VARE-EOF-OFF
               READ VARE
               AT END
                   SET VARE-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARE-FLDOFF SECTION.
       VARE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-05                TO TRUE
               SET NOT-I-06                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       VARE-FLDSET SECTION.
       VARE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE VARE-IO-AREA (6:7)     TO EDBNR (1:7)
               MOVE VARE-IO-AREA (6:1)     TO EDBNR1 (1:1)
               MOVE VARE-IO-AREA (13:3)    TO ALFA (1:3)
               MOVE VARE-IO-AREA (16:20)   TO ARTNR (1:20)
               MOVE VARE-IO-AREA (21:15)   TO ART5 (1:15)
               IF  ART5 = SPACES
                   SET I-05                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (26:10)   TO ART10 (1:10)
               IF  ART10 = SPACES
                   SET I-06                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (31:5)    TO ART15 (1:5)
               IF  ART15 = SPACES
                   SET I-07                TO TRUE
               END-IF
               MOVE VARE-IO-AREA (36:30)   TO BETEGN (1:30)
               MOVE VARE-IO-AREA (75:9)    TO UTSPRI-IO
               INSPECT UTSPRI-IO REPLACING ALL ' ' BY '0'
               MOVE VARE-IO-AREA (84:4)    TO ENDRET-IO
               MOVE VARE-IO-AREA (88:7)    TO ALTNR (1:7)
               MOVE VARE-IO-AREA (95:1)    TO PT (1:1)
               MOVE VARE-IO-AREA (97:5)    TO ANTINN-IO
               MOVE VARE-IO-AREA (102:5)   TO ANTUT-IO
               MOVE VARE-IO-AREA (107:1)   TO PRISTT (1:1)
               MOVE VARE-IO-AREA (117:1)   TO FAB (1:1)
               MOVE VARE-IO-AREA (118:5)   TO VGR (1:5)
               MOVE VARE-IO-AREA (127:1)   TO UTGA-ELGR (1:1)
               MOVE VARE-IO-AREA (128:1)   TO SLETT (1:1)
               MOVE VARE-IO-AREA (161:4)   TO PRISTI-IO
           END-EVALUATE.
 
       VARE-IDSET SECTION.
       VARE-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARE-CHK-LEVEL SECTION.
       VARE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARE-LEVEL-01
               MOVE VARE-IO-AREA (3:3)     TO VARE-01-L2-FIRMA
               MOVE VARE-IO-AREA (13:3)    TO VARE-01-L1-ALFA
               MOVE VARE-IO-AREA (95:1)    TO VARE-01-L1-PT
               MOVE VARE-IO-AREA (118:5)   TO VARE-01-L1-VGR
               IF  VARE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARE-01-L2            TO THE-PRIOR-L2
               MOVE  VARE-01-L1            TO THE-PRIOR-L1
               SET VARE-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       VARE-MATCH-SET SECTION.
       VARE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VARE-IO-AREA (3:3)     TO VARE-M-01-M4-FIRMA
               MOVE VARE-IO-AREA (118:5)   TO VARE-M-01-M3-VGR
               MOVE VARE-IO-AREA (13:3)    TO VARE-M-01-M2-ALFA
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  TILTAB-EOF
               MOVE HIGH-VALUES            TO TILTAB-MC
                                              TILTAB-MP
           END-IF
           IF  VARE-EOF
               MOVE HIGH-VALUES            TO VARE-MC
                                              VARE-MP
           END-IF
           IF  TILTAB-MC < TILTAB-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VARE-MC < VARE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  TILTAB-MC < VARE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILTAB-PROCESS      TO TRUE
                   MOVE TILTAB-MC          TO TILTAB-MP
                   IF  TILTAB-MC = VARE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARE-MC < TILTAB-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARE-PROCESS        TO TRUE
                   MOVE VARE-MC            TO VARE-MP
                   IF  VARE-MC = TILTAB-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  TILTAB-MC = VARE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILTAB-PROCESS      TO TRUE
                   MOVE TILTAB-MC          TO TILTAB-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-65)
               MOVE SPACES TO KOPI-IO-AREA
               INITIALIZE KOPI-IO-AREA
               MOVE '71'                   TO KOPI-IO-AREA (1:2)
               MOVE NYFNR                  TO KOPI-IO-AREA (3:3)
               MOVE EDBNR                  TO KOPI-IO-AREA (6:7)
               MOVE NYALFA                 TO KOPI-IO-AREA (13:3)
               MOVE ARTNR                  TO KOPI-IO-AREA (16:20)
               MOVE BETEGN                 TO KOPI-IO-AREA (36:30)
               MOVE NYSVS-IO               TO KOPI-IO-AREA (66:9)
               MOVE NYUTSP-IO              TO KOPI-IO-AREA (75:9)
               MOVE NYVGR                  TO KOPI-IO-AREA (84:5)
               MOVE NYPT                   TO KOPI-IO-AREA (89:1)
               IF  (I-47)
                   MOVE UTGA-ELGR          TO KOPI-IO-AREA (90:1)
               END-IF
               IF  (I-43)
                   MOVE '2'                TO KOPI-IO-AREA (90:1)
               END-IF
               IF  (I-44)
                   MOVE '4'                TO KOPI-IO-AREA (90:1)
               END-IF
               IF  (I-45)
                   MOVE '1'                TO KOPI-IO-AREA (90:1)
               END-IF
               IF  (I-46)
                   MOVE 'F'                TO KOPI-IO-AREA (90:1)
               END-IF
               MOVE FAB                    TO KOPI-IO-AREA (91:1)
               MOVE ALTNR                  TO KOPI-IO-AREA (92:7)
               MOVE 119                    TO BW-A
               PERFORM VARYING ARO-I FROM ARO-MAX BY -1
                         UNTIL ARO-I < 1
                   SUBTRACT 1            FROM BW-A
                   MOVE ARO-ENTRY (ARO-I)  TO KOPI-IO-AREA (BW-A:1)
               END-PERFORM
               MOVE EDATO                  TO XO-70P
               MOVE XO-70P-EF              TO KOPI-IO-AREA (119:4)
               MOVE VALGMU                 TO KOPI-IO-AREA (123:18)
               MOVE PRISTI                 TO XO-52P
               MOVE XO-52P-EF              TO KOPI-IO-AREA (141:4)
               MOVE PRISTT                 TO KOPI-IO-AREA (145:1)
               MOVE FIRMA                  TO KOPI-IO-AREA (146:3)
               MOVE NYLEVP                 TO XO-72P
               MOVE XO-72P-EF              TO KOPI-IO-AREA (149:5)
               MOVE NYLRAB                 TO XO-21P
               MOVE XO-21P-EF              TO KOPI-IO-AREA (154:2)
               WRITE KOPI-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG014 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT REC SOM HAR BLITT' TO LISTE-IO-AREA (1:24)
               MOVE ' KOPIERT'             TO LISTE-IO-AREA (25:8)
               MOVE ANT                    TO XO-80YN9
               MOVE XO-80YN9               TO LISTE-IO-AREA (38:8)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT KOPIER SOM HAR BL' TO LISTE-IO-AREA (1:24)
               MOVE 'ITT DANNET'           TO LISTE-IO-AREA (25:10)
               MOVE ANTNY                  TO XO-80YN9
               MOVE XO-80YN9               TO LISTE-IO-AREA (38:8)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (24:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               MOVE 01                     TO LISTE-AFTER-SKIP
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
           SET TILTAB-LEVEL-INIT           TO TRUE
           INITIALIZE TILTAB-DATA-FIELDS
           SET TILTAB-EOF-OFF              TO TRUE
           SET TILTAB-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO TILTAB-MC
                                              TILTAB-MP
           OPEN INPUT TILTAB
           SET VARE-LEVEL-INIT             TO TRUE
           INITIALIZE VARE-DATA-FIELDS
           SET VARE-EOF-OFF                TO TRUE
           SET VARE-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO VARE-MC
                                              VARE-MP
           OPEN INPUT VARE
           OPEN OUTPUT KOPI
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ART-I FROM 1 BY 1
                     UNTIL ART-I > ART-MAX
               INITIALIZE ART (ART-I)
           END-PERFORM
           SET ART-I                       TO 1
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               INITIALIZE ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILTAB
           CLOSE VARE
           CLOSE KOPI
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
