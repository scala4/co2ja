       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO040R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV RSK.RSK040                 ***TXT***OK MT
      *  PROGRAM....: RKO040 - FØR SEPT.05-RSK040                     *
      *  PROGRAM.......: RKO040                                       *
      * MERGE RESKONTROPOSTER OG DANNE HELPEFILE MED NULLPOSTER.      *
      *  3/12-93  MERKE HJELPEFILE (POS 19) OM POSTENE SKAL SAVES     *
      *           TIL KJØRING AV ALDERSFORDELT SALDOLISTE.            *
      * 17/12-93  IKKE DANNE HJELPEFILE MED NULLPOSTER OM BILAGSDATO  *
      *           PÅ EN AV POSTENE ER FRAMDATERT.                     *
      * 24/01-94  FJERNET ENDRING AV BET.MÅTE FRA KUNDEMASTER.        *
      * 22.06.98  TILPASSET ÅR 2000.                                  *
      *           KANSELLERER VED FEIL I MASKIN-DATO ELLER PARAMETER- *
      *           DATO. MELDING SKRIVES PÅ KONSOLLET.                 *
      *           VED FEIL I BILAGSDATO KAN FREMDATERINGER GÅ FEIL,   *
      *           RECORDEN LISTES, MEN TAS MED VIDERE (INPUTKONTROLL  *
      *           TAS DER RECORDEN LESES INN).                        *
      * 25.08.98  ENDRET FEILMELDING PÅ KONSOLLET.                    *
      * 26.08.98  KANSELLERER VED FEIL I PARAMETERDATO.               *
      * 20.05.99  LAGT INN TEKST FOR LISTESKILLING (RAPS) PÅ NY SIDE. *
      * 11.10.99  MERGER PÅ BILAGSÅRHUNDRE (MATCH FIELD FEIL).        *
      * 12.12.01  MERKER DE SOM HAR FAKTURA M/UNDERLIGGENDE GIRO MED  *
      *           AT DE HAR VÆRT PÅ INKASSO.                          *
      * 15.04.11  TAR MED UTVIDET BELØPSFELT                          *
      *           U1 PÅ : GAMMELT BELØPSFELT BENYTTES TIL Å BEREGNE   *
      *                   OM BILAGER ELLER RESKONTRONR GÅR I NULL.    *
      * 05.10.12  SKRIVER AVSTEMMINGSFIL                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO040.rpg
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
           SELECT GMLRESK
               ASSIGN TO UT-S-GMLRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLRESK-STATUS.
           SELECT DAGRESK
               ASSIGN TO UT-S-DAGRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGRESK-STATUS.
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
           SELECT OUTRESK
               ASSIGN TO UT-S-OUTRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTRESK-STATUS.
           SELECT REFNULL
               ASSIGN TO UT-S-REFNULL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REFNULL-STATUS.
           SELECT AVSTEMM
               ASSIGN TO UT-S-AVSTEMM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMM-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GMLRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  GMLRESK-IO-AREA.
           05  GMLRESK-IO-AREA-X           PICTURE X(200).
       FD DAGRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  DAGRESK-IO-AREA.
           05  DAGRESK-IO-AREA-X           PICTURE X(200).
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
       FD OUTRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  OUTRESK-IO-AREA.
           05  OUTRESK-IO-AREA-X           PICTURE X(200).
       FD REFNULL
               BLOCK CONTAINS 4100
               RECORD CONTAINS 20.
       01  REFNULL-IO-AREA.
           05  REFNULL-IO-AREA-X           PICTURE X(20).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  GMLRESK-STATUS              PICTURE 99 VALUE 0.
           10  DAGRESK-STATUS              PICTURE 99 VALUE 0.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  OUTRESK-STATUS              PICTURE 99 VALUE 0.
           10  REFNULL-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLRESK-EOF-OFF         VALUE '0'.
               88  GMLRESK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLRESK-READ-OFF        VALUE '0'.
               88  GMLRESK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLRESK-PROCESS-OFF     VALUE '0'.
               88  GMLRESK-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  GMLRESK-LEVEL-INIT-OFF  VALUE '0'.
               88  GMLRESK-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-EOF-OFF         VALUE '0'.
               88  DAGRESK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-READ-OFF        VALUE '0'.
               88  DAGRESK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-PROCESS-OFF     VALUE '0'.
               88  DAGRESK-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  DAGRESK-LEVEL-INIT-OFF  VALUE '0'.
               88  DAGRESK-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-EOF-OFF          VALUE '0'.
               88  RESPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-READ-OFF         VALUE '0'.
               88  RESPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-PROCESS-OFF      VALUE '0'.
               88  RESPAR-PROCESS          VALUE '1'.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  CONSOLE-IO-AREA.
               10  CONSOLE-IO-AREA-X       PICTURE X(800).
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
           05  DTOPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTO8SI                  PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
           05  GMLRESK-LEVEL-01.
               10  GMLRESK-01-L3.
                   15  GMLRESK-01-L3-FNR1  PICTURE X(3).
               10  GMLRESK-01-L2.
                   15  GMLRESK-01-L2-RES1  PICTURE X(6).
               10  GMLRESK-01-L1.
                   15  GMLRESK-01-L1-REF1  PICTURE X(6).
           05  GMLRESK-DATA-FIELDS.
               10  REC1                    PICTURE X(200).
               10  REC601                  PICTURE X(60).
               10  FNR1                    PICTURE X(3).
               10  RES1                    PICTURE X(6).
               10  BAARH1                  PICTURE X(2).
      *                                      17  18 BAARH1
               10  TK1                     PICTURE X(2).
               10  BDT1                    PICTURE X(6).
               10  REF1                    PICTURE X(6).
               10  FOF1                    PICTURE X(6).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(7)V9(2).
               10  INK1                    PICTURE X(1).
               10  BM1                     PICTURE X(2).
               10  VAL1-IO.
                   15  VAL1                PICTURE S9(8)V9(2).
               10  VTYP1                   PICTURE X(1).
               10  BEL1U-IO.
                   15  BEL1U               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VAL1U-IO.
                   15  VAL1U               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
           05  GMLRESK-MP                  PICTURE X(25).
           05  GMLRESK-MC                  PICTURE X(25).
           05  GMLRESK-M-01            REDEFINES GMLRESK-MC.
               10  GMLRESK-M-01-M6.
                   15  GMLRESK-M-01-M6-FNR1-G.
                       20  GMLRESK-M-01-M6-FNR1 PICTURE X(3).
               10  GMLRESK-M-01-M5.
                   15  GMLRESK-M-01-M5-RES1-G.
                       20  GMLRESK-M-01-M5-RES1 PICTURE X(6).
               10  GMLRESK-M-01-M4.
                   15  GMLRESK-M-01-M4-REF1-G.
                       20  GMLRESK-M-01-M4-REF1 PICTURE X(6).
               10  GMLRESK-M-01-M3.
                   15  GMLRESK-M-01-M3-TK1-G.
                       20  GMLRESK-M-01-M3-TK1 PICTURE X(2).
               10  GMLRESK-M-01-M2.
                   15  GMLRESK-M-01-M2-BAARH1-G.
                       20  GMLRESK-M-01-M2-BAARH1 PICTURE X(2).
               10  GMLRESK-M-01-M1.
                   15  GMLRESK-M-01-M1-BDT1-G.
                       20  GMLRESK-M-01-M1-BDT1 PICTURE X(6).
           05  DAGRESK-LEVEL-02.
               10  DAGRESK-02-L3.
                   15  DAGRESK-02-L3-FNR2  PICTURE X(3).
               10  DAGRESK-02-L2.
                   15  DAGRESK-02-L2-RES2  PICTURE X(6).
               10  DAGRESK-02-L1.
                   15  DAGRESK-02-L1-REF2  PICTURE X(6).
           05  DAGRESK-DATA-FIELDS.
               10  REC602                  PICTURE X(60).
               10  FNR2                    PICTURE X(3).
               10  RES2                    PICTURE X(6).
               10  REF2                    PICTURE X(6).
               10  TK2                     PICTURE X(2).
               10  BDT2                    PICTURE X(6).
               10  BN2                     PICTURE X(6).
               10  BA2                     PICTURE X(1).
               10  FOF2                    PICTURE X(6).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2).
               10  INK2                    PICTURE X(1).
               10  BM3                     PICTURE X(2).
               10  VAL2-IO.
                   15  VAL2                PICTURE S9(8)V9(2).
               10  VT2                     PICTURE X(1).
               10  BGIRO                   PICTURE X(1).
               10  BAARH2                  PICTURE X(2).
      *                                      82  83 BAARH2
               10  TEKST                   PICTURE X(24).
               10  BEL2U-IO.
                   15  BEL2U               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VAL2U-IO.
                   15  VAL2U               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  VT2U                    PICTURE X(3).
               10  OPPHA2                  PICTURE X(4).
               10  PRDDT2                  PICTURE X(5).
               10  PRDKL2                  PICTURE X(4).
           05  DAGRESK-MP                  PICTURE X(25).
           05  DAGRESK-MC                  PICTURE X(25).
           05  DAGRESK-M-02            REDEFINES DAGRESK-MC.
               10  DAGRESK-M-02-M6.
                   15  DAGRESK-M-02-M6-FNR2-G.
                       20  DAGRESK-M-02-M6-FNR2 PICTURE X(3).
               10  DAGRESK-M-02-M5.
                   15  DAGRESK-M-02-M5-RES2-G.
                       20  DAGRESK-M-02-M5-RES2 PICTURE X(6).
               10  DAGRESK-M-02-M4.
                   15  DAGRESK-M-02-M4-REF2-G.
                       20  DAGRESK-M-02-M4-REF2 PICTURE X(6).
               10  DAGRESK-M-02-M3.
                   15  DAGRESK-M-02-M3-TK2-G.
                       20  DAGRESK-M-02-M3-TK2 PICTURE X(2).
               10  DAGRESK-M-02-M2.
                   15  DAGRESK-M-02-M2-BAARH2-G.
                       20  DAGRESK-M-02-M2-BAARH2 PICTURE X(2).
               10  DAGRESK-M-02-M1.
                   15  DAGRESK-M-02-M1-BDT2-G.
                       20  DAGRESK-M-02-M1-BDT2 PICTURE X(6).
           05  RESPAR-DATA-FIELDS.
               10  PDAG                    PICTURE X(2).
               10  PA-ELGMD                PICTURE X(6).
               10  AA-ELGMD                PICTURE X(6).
           05  SYSPARM-DATA-FIELDS.
               10  SYSFMG                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  DTO6-IO.
                   15  DTO6                PICTURE S9(6).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DDATO                   PICTURE X(6).
               10  DDATO8                  PICTURE X(8).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  AAR                     PICTURE X(2).
               10  MND                     PICTURE X(2).
               10  DAG                     PICTURE X(2).
               10  F4                      PICTURE X(4).
               10  F6                      PICTURE X(6).
               10  KDATO                   PICTURE X(6).
               10  KDATO8                  PICTURE X(8).
               10  PA-ELGMD8               PICTURE X(8).
               10  AA-ELGMD8               PICTURE X(8).
               10  SYSKEY                  PICTURE X(10).
               10  BDT18                   PICTURE X(8).
               10  ANTFDT-IO.
                   15  ANTFDT              PICTURE S9(5).
               10  BDT28                   PICTURE X(8).
               10  AFOF                    PICTURE X(6).
               10  AINK                    PICTURE X(1).
               10  AVT                     PICTURE X(1).
               10  ABM                     PICTURE X(2).
               10  FNRUT                   PICTURE X(3).
               10  FRESUT                  PICTURE X(6).
               10  FREFUT                  PICTURE X(6).
               10  REFL1-IO.
                   15  REFL1               PICTURE S9(9)V9(2).
               10  REFL1U-IO.
                   15  REFL1U              PICTURE S9(11)V9(2).
               10  SALL2-IO.
                   15  SALL2               PICTURE S9(9)V9(2).
               10  SALL2U-IO.
                   15  SALL2U              PICTURE S9(11)V9(2).
               10  TOTREC-IO.
                   15  TOTREC              PICTURE S9(6).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
               10  TOTBEU-IO.
                   15  TOTBEU              PICTURE S9(11)V9(2).
               10  TOTVAL-IO.
                   15  TOTVAL              PICTURE S9(11)V9(2).
               10  TOTVAU-IO.
                   15  TOTVAU              PICTURE S9(11)V9(4).
               10  ANTFMG-IO.
                   15  ANTFMG              PICTURE S9(6).
               10  BELFMG-IO.
                   15  BELFMG              PICTURE S9(9)V9(2).
               10  BELFMU-IO.
                   15  BELFMU              PICTURE S9(11)V9(2).
               10  VALFMG-IO.
                   15  VALFMG              PICTURE S9(11)V9(2).
               10  VALFMU-IO.
                   15  VALFMU              PICTURE S9(11)V9(4).
               10  LRRSAN-IO.
                   15  LRRSAN              PICTURE S9(13).
               10  LRRSBE-IO.
                   15  LRRSBE              PICTURE S9(13)V9(2).
               10  LRRSBU-IO.
                   15  LRRSBU              PICTURE S9(13)V9(2).
           05  EDITTING-FIELDS.
               10  XO-112P-EF.
                 15  XO-112P               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-114P-EF.
                 15  XO-114P               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  XO-60YNZ                PICTURE ZZZZZZ.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
               10  XO-114YY9R              PICTURE
                                                 ZZ.ZZZ.ZZZ.ZZZ,9999-.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  GMLRESK-PROCESS
               SET GMLRESK-PROCESS-OFF     TO TRUE
               SET GMLRESK-READ            TO TRUE
           END-IF
 
           IF  GMLRESK-READ
               PERFORM GMLRESK-GET
               SET GMLRESK-READ-OFF        TO TRUE
               IF  NOT GMLRESK-EOF
                   PERFORM GMLRESK-MATCH-SET
               END-IF
           END-IF
 
           IF  DAGRESK-PROCESS
               SET DAGRESK-PROCESS-OFF     TO TRUE
               SET DAGRESK-READ            TO TRUE
           END-IF
 
           IF  DAGRESK-READ
               PERFORM DAGRESK-GET
               SET DAGRESK-READ-OFF        TO TRUE
               IF  NOT DAGRESK-EOF
                   PERFORM DAGRESK-MATCH-SET
               END-IF
           END-IF
 
           IF  RESPAR-PROCESS
               SET RESPAR-PROCESS-OFF      TO TRUE
               SET RESPAR-READ             TO TRUE
           END-IF
 
           IF  RESPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESPAR-GET
               SET RESPAR-READ-OFF         TO TRUE
               IF  NOT RESPAR-EOF
                   SET RESPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
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
 
           IF  GMLRESK-PROCESS
               PERFORM GMLRESK-IDSET
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-IDSET
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
           END-IF
 
           IF  GMLRESK-PROCESS
               PERFORM GMLRESK-CHK-LEVEL
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  GMLRESK-PROCESS
               PERFORM GMLRESK-FLDSET
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-FLDSET
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  GMLRESK-PROCESS
           OR  DAGRESK-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-88)
               SET NOT-I-89                TO TRUE
           END-IF
           IF  (NOT-I-88)
               SET I-88                    TO TRUE
               SET I-89                    TO TRUE
           END-IF
           IF  (I-89)
               MOVE UDATE                  TO DTO6-IO
               MOVE DTO6 (1:2)             TO DD
               MOVE DTO6 (5:2)             TO AA-IO
      ** MLLzo
               IF AA < 0
                   MULTIPLY -1 BY AA
               END-IF
               MOVE DTO6                   TO DDATO
               MOVE AA                     TO DDATO (1:2)
               MOVE DD                     TO DDATO (5:2)
               SET NOT-I-31                TO TRUE
               IF  AA > 80
                   SET I-31                TO TRUE
               END-IF
               MOVE DDATO                  TO DDATO8 (3:6)
           END-IF
           IF  (I-89 AND I-31)
               MOVE '19'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89 AND NOT-I-31)
               MOVE '20'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      *  89                MOVE "DDATO8  "BUGFL1  8        DISPLAY FIELD
      *  89      BUGFL1    DEBUGBUGFILO   DDATO8           VIS INDIKATOR
      *****************************************************************
      *  PARAMETER RUTINE FOR DATOKONTROLL.                           *
      *****************************************************************
           END-IF
           IF  (I-04)
               SET NOT-I-15                TO TRUE
               IF  PDAG = '15'
                   SET I-15                TO TRUE
               END-IF
               MOVE UYEAR                  TO AAR
               MOVE UMONTH                 TO MND
               MOVE UDAY                   TO DAG
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE AAR (2:1)              TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO AAR (2:1)
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE MND (2:1)              TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO MND (2:1)
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE DAG (2:1)              TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO DAG (2:1)
               MOVE AAR                    TO F4 (1:2)
               MOVE MND                    TO F4 (3:2)
               MOVE F4                     TO F6 (1:4)
               MOVE DAG                    TO F6 (5:2)
               MOVE F6                     TO KDATO
               MOVE KDATO                  TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-04 AND I-98)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               MOVE DTO8SI                 TO KDATO8
               MOVE PA-ELGMD               TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-04 AND I-98)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               MOVE DTO8SI                 TO PA-ELGMD8
               MOVE AA-ELGMD               TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-04 AND I-98)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               MOVE DTO8SI                 TO AA-ELGMD8
               SET NOT-I-11                TO TRUE
               IF  KDATO8 > PA-ELGMD8
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-11)
               SET NOT-I-12                TO TRUE
               IF  KDATO8 NOT > AA-ELGMD8
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-12 AND NOT-I-15)
               SET I-14                    TO TRUE
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
      *****************************************************************
      *  HOVED RUTINE.                                                *
      *****************************************************************
           END-IF
           IF  (I-99)
               SET I-H0                    TO TRUE
               GO TO SLUTT-T
           END-IF
           SET NOT-I-31                    TO TRUE
           SET NOT-I-32                    TO TRUE
           SET NOT-I-71                    TO TRUE
           IF  (I-L3)
               SET NOT-I-46                TO TRUE
           END-IF
           IF  (I-L3 AND I-01)
               MOVE FNR1                   TO SYSKEY (1:3)
           END-IF
           IF  (I-L3 AND I-02)
               MOVE FNR2                   TO SYSKEY (1:3)
           END-IF
           IF  (I-L3)
               MOVE 'RES*011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-45                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-45            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-45)
               SET NOT-I-46                TO TRUE
               IF  SYSFMG = 'J'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SUBTRACT SALL2              FROM SALL2
           END-IF
           IF  (I-L1)
               MOVE 0,00                   TO REFL1
      *****************************************************************
      * SJEKK ETTER OM BILAGSDATO ER FREMDATERT.                      *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-92                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-91                TO TRUE
           END-IF
           IF  (I-01)
               MOVE BDT1                   TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO BDT18
           END-IF
           IF  (I-01 AND I-98)
               SET NOT-I-21                TO TRUE
               IF  BDT1 > KDATO
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-98)
               SET NOT-I-21                TO TRUE
               IF  BDT18 > KDATO8
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-21)
               SET I-91                    TO TRUE
               SET I-92                    TO TRUE
               ADD 1                       TO ANTFDT
           END-IF
           IF  (I-02)
               MOVE BDT2                   TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-02 AND NOT-I-98)
               MOVE DTO8SI                 TO BDT28
           END-IF
           IF  (I-02 AND I-98)
               SET NOT-I-21                TO TRUE
               IF  BDT2 > KDATO
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-98)
               SET NOT-I-21                TO TRUE
               IF  BDT28 > KDATO8
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-21)
               SET I-91                    TO TRUE
               SET I-92                    TO TRUE
               ADD 1                       TO ANTFDT
      *****************************************************************
           END-IF
           IF  (I-01)
               GO TO M1-T
           END-IF
           IF  (I-02)
               SET NOT-I-95                TO TRUE
               IF  BGIRO = '1'
                   SET I-95                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  TK2 = '21'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  TK2 = '22'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31)
               GO TO A1-T
           END-IF
           IF  (I-02)
               SET NOT-I-32                TO TRUE
               IF  TK2 = '26'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-32)
               GO TO A1-T
           END-IF
           MOVE '1'                        TO INK2.
 
       A1-T.
           SET NOT-I-71                    TO TRUE
           IF  BM3 = '  '
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE '03'                   TO BM3
           END-IF.
 
       M1-T.
           IF  (I-L1 AND I-01)
               MOVE FOF1                   TO AFOF
               MOVE INK1                   TO AINK
               MOVE VTYP1                  TO AVT
               MOVE BM1                    TO ABM
           END-IF
           IF  (I-L1 AND I-02)
               MOVE FOF2                   TO AFOF
               MOVE INK2                   TO AINK
               MOVE VT2                    TO AVT
               MOVE BM3                    TO ABM
           END-IF
           IF  (I-L1 AND I-01)
               MOVE FNR1                   TO FNRUT
               MOVE RES1                   TO FRESUT
               MOVE REF1                   TO FREFUT
           END-IF
           IF  (I-L1 AND I-02)
               MOVE FNR2                   TO FNRUT
               MOVE RES2                   TO FRESUT
               MOVE REF2                   TO FREFUT
           END-IF
           IF  (I-01)
               ADD BEL1                    TO REFL1
               ADD BEL1U                   TO REFL1U
           END-IF
           IF  (I-02)
               ADD BEL2                    TO REFL1
               ADD BEL2U                   TO REFL1U
           END-IF
           IF  (I-01)
               ADD BEL1                    TO SALL2
               ADD BEL1U                   TO SALL2U
           END-IF
           IF  (I-02)
               ADD BEL2                    TO SALL2
               ADD BEL2U                   TO SALL2U
           END-IF
           IF  (I-01)
               ADD 1                       TO TOTREC
               ADD BEL1                    TO TOTBEL
               ADD BEL1U                   TO TOTBEU
               ADD VAL1                    TO TOTVAL
               ADD VAL1U                   TO TOTVAU
      *  01                SETOF                     10
      *  01      FNR1      COMP "172"                    10
      *  01 10   VAL1      COMP VAL1U                1010
      *  01 10             MOVE "REC1    "BUGFL2  8        LEDETXT DEBUG
      *  01 10   BUGFL2    DEBUGBUGFILO   REC1             VIS FELT/IND
           END-IF
           IF  (I-02)
               ADD 1                       TO TOTREC
               ADD BEL2                    TO TOTBEL
               ADD BEL2U                   TO TOTBEU
               ADD VAL2                    TO TOTVAL
               ADD VAL2U                   TO TOTVAU
           END-IF
           IF  (I-02 AND I-46)
               ADD 1                       TO ANTFMG
               ADD BEL2                    TO BELFMG
               ADD BEL2U                   TO BELFMU
               ADD VAL2                    TO VALFMG
               ADD VAL2U                   TO VALFMU
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * RUTINE FOR Å DANNE HJELPERECORD FOR Å FJERNE NULLPOSTER       *
      * OM BILAGSTOTALEN PR. REF.NR ER NULL.                          *
      * OM DET FINNES FREMDATERTE POSTER INNEN ETT REF.NR SKAL DET    *
      *    IKKE DANNES HJELPERECORD.                                  *
      * NULLPOSTER SAVES TIL ALDERSFORDELT SALDOLISTE NÅR:            *
      *     KJØREDATO ER ETTER PERIODEDATO OG                         *
      *     KJØREDATO ER LIK ELLER LAVERE REGNSKAPSAVSLUTTNINGSDATO.  *
      *****************************************************************
           CONTINUE.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-98                    TO TRUE
           MOVE 'B'                        TO DTOKOD
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-98                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-98                    TO TRUE
           END-IF.
      *
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-80                TO TRUE
           END-IF
           IF  (I-L1 AND I-91)
               GO TO X0RUT1-T
           END-IF
           IF  (I-L1 AND I-U1)
               SET NOT-I-80                TO TRUE
               IF  REFL1 = 0,00
                   SET I-80                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-U1)
               SET NOT-I-80                TO TRUE
               IF  REFL1U = 0,00
                   SET I-80                TO TRUE
               END-IF
           END-IF.
 
       X0RUT1-T.
      *****************************************************************
      * RUTINE FOR Å FJERNE NULLPOSTER OM ALLE POSTER PÅ ETT RESK.NR  *
      *        GÅR I NULL.  DENNE BENYTTES IKKE I NESTE PROGRAM.      *
      *****************************************************************
           IF  (I-L2)
               SET NOT-I-81                TO TRUE
           END-IF
           IF  (I-L2 AND I-92)
               GO TO X0RUT2-T
           END-IF
           IF  (I-L2 AND I-U1)
               SET NOT-I-81                TO TRUE
               IF  SALL2 = 0,00
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-U1)
               SET NOT-I-81                TO TRUE
               IF  SALL2U = 0,00
                   SET I-81                TO TRUE
               END-IF
           END-IF.
 
       X0RUT2-T.
           CONTINUE.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD TOTREC TO ZERO          GIVING LRRSAN
           ADD TOTBEL TO ZERO          GIVING LRRSBE
           ADD TOTBEU TO ZERO          GIVING LRRSBU
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
           .
 
       GMLRESK-GET SECTION.
       GMLRESK-GET-P.
           IF  GMLRESK-EOF-OFF
               READ GMLRESK
               AT END
                   SET GMLRESK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLRESK-FLDSET SECTION.
       GMLRESK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLRESK-IO-AREA (1:200) TO REC1 (1:200)
               MOVE GMLRESK-IO-AREA (1:60) TO REC601 (1:60)
               MOVE GMLRESK-IO-AREA (3:3)  TO FNR1 (1:3)
               MOVE GMLRESK-IO-AREA (6:6)  TO RES1 (1:6)
               MOVE GMLRESK-IO-AREA (17:2) TO BAARH1 (1:2)
               MOVE GMLRESK-IO-AREA (22:2) TO TK1 (1:2)
               MOVE GMLRESK-IO-AREA (24:6) TO BDT1 (1:6)
               MOVE GMLRESK-IO-AREA (36:6) TO REF1 (1:6)
               MOVE GMLRESK-IO-AREA (42:6) TO FOF1 (1:6)
               MOVE GMLRESK-IO-AREA (48:9) TO BEL1-IO
               INSPECT BEL1-IO REPLACING ALL ' ' BY '0'
               MOVE GMLRESK-IO-AREA (60:1) TO INK1 (1:1)
               MOVE GMLRESK-IO-AREA (61:2) TO BM1 (1:2)
               MOVE GMLRESK-IO-AREA (64:10) TO VAL1-IO
               INSPECT VAL1-IO REPLACING ALL ' ' BY '0'
               MOVE GMLRESK-IO-AREA (76:1) TO VTYP1 (1:1)
               MOVE GMLRESK-IO-AREA (114:7) TO BEL1U-IO
               MOVE GMLRESK-IO-AREA (121:8) TO VAL1U-IO
           END-EVALUATE.
 
       GMLRESK-IDSET SECTION.
       GMLRESK-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLRESK-CHK-LEVEL SECTION.
       GMLRESK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO GMLRESK-LEVEL-01
               MOVE GMLRESK-IO-AREA (3:3)  TO GMLRESK-01-L3-FNR1
               MOVE GMLRESK-IO-AREA (6:6)  TO GMLRESK-01-L2-RES1
               MOVE GMLRESK-IO-AREA (36:6) TO GMLRESK-01-L1-REF1
               IF  GMLRESK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  GMLRESK-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  GMLRESK-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  GMLRESK-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  GMLRESK-01-L3         TO THE-PRIOR-L3
               MOVE  GMLRESK-01-L2         TO THE-PRIOR-L2
               MOVE  GMLRESK-01-L1         TO THE-PRIOR-L1
               SET GMLRESK-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       GMLRESK-MATCH-SET SECTION.
       GMLRESK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLRESK-IO-AREA (3:3)  TO GMLRESK-M-01-M6-FNR1
               MOVE GMLRESK-IO-AREA (6:6)  TO GMLRESK-M-01-M5-RES1
               MOVE GMLRESK-IO-AREA (36:6) TO GMLRESK-M-01-M4-REF1
               MOVE GMLRESK-IO-AREA (22:2) TO GMLRESK-M-01-M3-TK1
               MOVE GMLRESK-IO-AREA (17:2) TO GMLRESK-M-01-M2-BAARH1
               MOVE GMLRESK-IO-AREA (24:6) TO GMLRESK-M-01-M1-BDT1
           END-EVALUATE.
 
       DAGRESK-GET SECTION.
       DAGRESK-GET-P.
           IF  DAGRESK-EOF-OFF
               READ DAGRESK
               AT END
                   SET DAGRESK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGRESK-FLDSET SECTION.
       DAGRESK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGRESK-IO-AREA (1:60) TO REC602 (1:60)
               MOVE DAGRESK-IO-AREA (3:3)  TO FNR2 (1:3)
               MOVE DAGRESK-IO-AREA (6:6)  TO RES2 (1:6)
               MOVE DAGRESK-IO-AREA (12:6) TO REF2 (1:6)
               MOVE DAGRESK-IO-AREA (18:2) TO TK2 (1:2)
               MOVE DAGRESK-IO-AREA (20:6) TO BDT2 (1:6)
               MOVE DAGRESK-IO-AREA (26:6) TO BN2 (1:6)
               MOVE DAGRESK-IO-AREA (32:1) TO BA2 (1:1)
               MOVE DAGRESK-IO-AREA (33:6) TO FOF2 (1:6)
               MOVE DAGRESK-IO-AREA (39:9) TO BEL2-IO
               INSPECT BEL2-IO REPLACING ALL ' ' BY '0'
               MOVE DAGRESK-IO-AREA (48:1) TO INK2 (1:1)
               MOVE DAGRESK-IO-AREA (49:2) TO BM3 (1:2)
               MOVE DAGRESK-IO-AREA (51:10) TO VAL2-IO
               INSPECT VAL2-IO REPLACING ALL ' ' BY '0'
               MOVE DAGRESK-IO-AREA (70:1) TO VT2 (1:1)
               MOVE DAGRESK-IO-AREA (71:1) TO BGIRO (1:1)
               MOVE DAGRESK-IO-AREA (82:2) TO BAARH2 (1:2)
               MOVE DAGRESK-IO-AREA (90:24) TO TEKST (1:24)
               MOVE DAGRESK-IO-AREA (114:7) TO BEL2U-IO
               MOVE DAGRESK-IO-AREA (121:8) TO VAL2U-IO
               MOVE DAGRESK-IO-AREA (129:3) TO VT2U (1:3)
               MOVE DAGRESK-IO-AREA (188:4) TO OPPHA2 (1:4)
               MOVE DAGRESK-IO-AREA (192:5) TO PRDDT2 (1:5)
               MOVE DAGRESK-IO-AREA (197:4) TO PRDKL2 (1:4)
           END-EVALUATE.
 
       DAGRESK-IDSET SECTION.
       DAGRESK-IDSET-P.
           SET I-02                        TO TRUE.
 
       DAGRESK-CHK-LEVEL SECTION.
       DAGRESK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGRESK-LEVEL-02
               MOVE DAGRESK-IO-AREA (3:3)  TO DAGRESK-02-L3-FNR2
               MOVE DAGRESK-IO-AREA (6:6)  TO DAGRESK-02-L2-RES2
               MOVE DAGRESK-IO-AREA (12:6) TO DAGRESK-02-L1-REF2
               IF  DAGRESK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGRESK-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  DAGRESK-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  DAGRESK-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  DAGRESK-02-L3         TO THE-PRIOR-L3
               MOVE  DAGRESK-02-L2         TO THE-PRIOR-L2
               MOVE  DAGRESK-02-L1         TO THE-PRIOR-L1
               SET DAGRESK-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DAGRESK-MATCH-SET SECTION.
       DAGRESK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGRESK-IO-AREA (3:3)  TO DAGRESK-M-02-M6-FNR2
               MOVE DAGRESK-IO-AREA (6:6)  TO DAGRESK-M-02-M5-RES2
               MOVE DAGRESK-IO-AREA (12:6) TO DAGRESK-M-02-M4-REF2
               MOVE DAGRESK-IO-AREA (18:2) TO DAGRESK-M-02-M3-TK2
               MOVE DAGRESK-IO-AREA (82:2) TO DAGRESK-M-02-M2-BAARH2
               MOVE DAGRESK-IO-AREA (20:6) TO DAGRESK-M-02-M1-BDT2
           END-EVALUATE.
 
       RESPAR-GET SECTION.
       RESPAR-GET-P.
           IF  RESPAR-EOF-OFF
               READ RESPAR
               AT END
                   SET RESPAR-EOF          TO TRUE
               END-READ
           END-IF.
 
       RESPAR-FLDSET SECTION.
       RESPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESPAR-IO-AREA (17:2)  TO PDAG (1:2)
               MOVE RESPAR-IO-AREA (13:6)  TO PA-ELGMD (1:6)
               MOVE RESPAR-IO-AREA (119:6) TO AA-ELGMD (1:6)
           END-EVALUATE.
 
       RESPAR-IDSET SECTION.
       RESPAR-IDSET-P.
           SET I-04                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (068:1) TO SYSFMG (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
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
           IF  GMLRESK-EOF
               MOVE HIGH-VALUES            TO GMLRESK-MC
                                              GMLRESK-MP
           END-IF
           IF  DAGRESK-EOF
               MOVE HIGH-VALUES            TO DAGRESK-MC
                                              DAGRESK-MP
           END-IF
           IF  GMLRESK-MC < GMLRESK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  DAGRESK-MC < DAGRESK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLRESK-MC < DAGRESK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLRESK-PROCESS     TO TRUE
                   MOVE GMLRESK-MC         TO GMLRESK-MP
                   IF  GMLRESK-MC = DAGRESK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGRESK-MC < GMLRESK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGRESK-PROCESS     TO TRUE
                   MOVE DAGRESK-MC         TO DAGRESK-MP
                   IF  DAGRESK-MC = GMLRESK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLRESK-MC = DAGRESK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLRESK-PROCESS     TO TRUE
                   MOVE GMLRESK-MC         TO GMLRESK-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTRESK-IO-AREA
               INITIALIZE OUTRESK-IO-AREA
               MOVE REC1                   TO OUTRESK-IO-AREA (1:200)
               MOVE AFOF                   TO OUTRESK-IO-AREA (42:6)
               MOVE AINK                   TO OUTRESK-IO-AREA (60:1)
               MOVE ABM                    TO OUTRESK-IO-AREA (61:2)
               MOVE AVT                    TO OUTRESK-IO-AREA (76:1)
               MOVE '1'                    TO OUTRESK-IO-AREA (89:1)
               WRITE OUTRESK-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO OUTRESK-IO-AREA
               INITIALIZE OUTRESK-IO-AREA
               MOVE '02'                   TO OUTRESK-IO-AREA (1:2)
               MOVE FNR2                   TO OUTRESK-IO-AREA (3:3)
               MOVE RES2                   TO OUTRESK-IO-AREA (6:6)
               MOVE '     '                TO OUTRESK-IO-AREA (12:5)
               MOVE BAARH2                 TO OUTRESK-IO-AREA (17:2)
               MOVE '   '                  TO OUTRESK-IO-AREA (19:3)
               MOVE TK2                    TO OUTRESK-IO-AREA (22:2)
               MOVE BDT2                   TO OUTRESK-IO-AREA (24:6)
               MOVE BN2                    TO OUTRESK-IO-AREA (30:6)
               MOVE REF2                   TO OUTRESK-IO-AREA (36:6)
               MOVE AFOF                   TO OUTRESK-IO-AREA (42:6)
               MOVE BEL2-IO                TO OUTRESK-IO-AREA (48:9)
               MOVE '   '                  TO OUTRESK-IO-AREA (57:3)
               MOVE AINK                   TO OUTRESK-IO-AREA (60:1)
               IF  (I-95)
                   MOVE '1'                TO OUTRESK-IO-AREA (60:1)
               END-IF
               IF  (I-46)
                   MOVE '1'                TO OUTRESK-IO-AREA (60:1)
               END-IF
               MOVE ABM                    TO OUTRESK-IO-AREA (61:2)
               MOVE ' '                    TO OUTRESK-IO-AREA (63:1)
               MOVE VAL2-IO                TO OUTRESK-IO-AREA (64:10)
               MOVE BA2                    TO OUTRESK-IO-AREA (74:1)
               MOVE ' '                    TO OUTRESK-IO-AREA (75:1)
               MOVE AVT                    TO OUTRESK-IO-AREA (76:1)
               MOVE ' '                    TO OUTRESK-IO-AREA (77:1)
               MOVE ' '                    TO OUTRESK-IO-AREA (78:1)
               MOVE '1'                    TO OUTRESK-IO-AREA (89:1)
               MOVE TEKST                  TO OUTRESK-IO-AREA (90:24)
               MOVE BEL2U                  TO XO-112P
               MOVE XO-112P-EF             TO OUTRESK-IO-AREA (114:7)
               MOVE VAL2U                  TO XO-114P
               MOVE XO-114P-EF             TO OUTRESK-IO-AREA (121:8)
               MOVE VT2U                   TO OUTRESK-IO-AREA (129:3)
               MOVE OPPHA2                 TO OUTRESK-IO-AREA (188:4)
               MOVE PRDDT2                 TO OUTRESK-IO-AREA (192:5)
               MOVE PRDKL2                 TO OUTRESK-IO-AREA (197:4)
               WRITE OUTRESK-IO-AREA
           END-IF
           IF  (I-01 AND I-98)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR1                   TO LISTE-IO-AREA (2:3)
               MOVE RES1                   TO LISTE-IO-AREA (6:6)
               MOVE REF1                   TO LISTE-IO-AREA (13:6)
               MOVE BDT1                   TO LISTE-IO-AREA (20:6)
               MOVE 'FEIL I BILAGSDATO      ' TO LISTE-IO-AREA (33:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-98)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR2                   TO LISTE-IO-AREA (2:3)
               MOVE RES2                   TO LISTE-IO-AREA (6:6)
               MOVE REF2                   TO LISTE-IO-AREA (13:6)
               MOVE BDT2                   TO LISTE-IO-AREA (20:6)
               MOVE 'FEIL I BILAGSDATO      ' TO LISTE-IO-AREA (33:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-04 AND I-98)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> CANCEL RKO040  ==>  ' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE 'FEIL I DATO (SUBRUT DATO' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE '8SIF)       '         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'KDATO : '             TO CONSOLE-IO-AREA (11:8)
               MOVE KDATO                  TO CONSOLE-IO-AREA (21:6)
               MOVE 'PÅMD  : '             TO CONSOLE-IO-AREA (29:8)
               MOVE PA-ELGMD               TO CONSOLE-IO-AREA (39:6)
               MOVE 'AÅMD  : '             TO CONSOLE-IO-AREA (47:8)
               MOVE AA-ELGMD               TO CONSOLE-IO-AREA (57:6)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (4:57)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-01 AND I-98)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> PROGRAM: RKO040 ' TO CONSOLE-IO-AREA (1:20)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (14:57)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> REC:'             TO CONSOLE-IO-AREA (1:8)
               MOVE REC601                 TO CONSOLE-IO-AREA (11:60)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-02 AND I-98)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> PROGRAM: RKO040 ' TO CONSOLE-IO-AREA (1:20)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (14:57)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> REC:'             TO CONSOLE-IO-AREA (1:8)
               MOVE REC602                 TO CONSOLE-IO-AREA (11:60)
               DISPLAY CONSOLE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO040 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO040 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-80)
               MOVE SPACES TO REFNULL-IO-AREA
               INITIALIZE REFNULL-IO-AREA
               MOVE '62'                   TO REFNULL-IO-AREA (1:2)
               MOVE FNRUT                  TO REFNULL-IO-AREA (3:3)
               MOVE FRESUT                 TO REFNULL-IO-AREA (6:6)
               MOVE FREFUT                 TO REFNULL-IO-AREA (12:6)
               IF  (I-14)
                   MOVE 'X'                TO REFNULL-IO-AREA (19:1)
               END-IF
               MOVE '1'                    TO REFNULL-IO-AREA (20:1)
               WRITE REFNULL-IO-AREA
           END-IF
           IF  (I-L2 AND I-81)
               MOVE SPACES TO REFNULL-IO-AREA
               INITIALIZE REFNULL-IO-AREA
               MOVE '62'                   TO REFNULL-IO-AREA (1:2)
               MOVE FNRUT                  TO REFNULL-IO-AREA (3:3)
               MOVE FRESUT                 TO REFNULL-IO-AREA (6:6)
               MOVE '999999'               TO REFNULL-IO-AREA (12:6)
               IF  (I-14)
                   MOVE 'X'                TO REFNULL-IO-AREA (19:1)
               END-IF
               MOVE '9'                    TO REFNULL-IO-AREA (20:1)
               WRITE REFNULL-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (1:8)
               MOVE '140'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO040'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO040*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '140'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO040*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC FREM-DATERT.   ' TO LISTE-IO-AREA (3:23)
               MOVE ANTFDT                 TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (28:5)
               INITIALIZE ANTFDT
               MOVE 'KJØREDATO     : '     TO LISTE-IO-AREA (51:16)
               MOVE KDATO8                 TO LISTE-IO-AREA (67:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'START PERIODE : '     TO LISTE-IO-AREA (51:16)
               MOVE PA-ELGMD8              TO LISTE-IO-AREA (67:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC RKO040 RESKFILE' TO LISTE-IO-AREA (3:23)
               MOVE TOTREC                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (27:6)
               INITIALIZE TOTREC
               MOVE 'SLUTT PERIODE : '     TO LISTE-IO-AREA (51:16)
               MOVE AA-ELGMD8              TO LISTE-IO-AREA (67:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BELØP   RKO040 RESKFILE' TO LISTE-IO-AREA (3:23)
               MOVE TOTBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (27:15)
               INITIALIZE TOTBEL
               MOVE TOTVAL                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (49:18)
               INITIALIZE TOTVAL
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTBEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (24:18)
               INITIALIZE TOTBEU
               MOVE TOTVAU                 TO XO-114YY9R
               MOVE XO-114YY9R             TO LISTE-IO-AREA (49:20)
               INITIALIZE TOTVAU
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FAKTURA M/GIRO  ' TO LISTE-IO-AREA (3:23)
               MOVE ANTFMG                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (27:6)
               INITIALIZE ANTFMG
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BELØP FAKTURA M/GIRO   ' TO LISTE-IO-AREA (3:23)
               MOVE BELFMG                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (27:15)
               INITIALIZE BELFMG
               MOVE VALFMG                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (49:18)
               INITIALIZE VALFMG
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELFMU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (24:18)
               INITIALIZE BELFMU
               MOVE VALFMU                 TO XO-114YY9R
               MOVE XO-114YY9R             TO LISTE-IO-AREA (49:20)
               INITIALIZE VALFMU
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U8 AND I-97 AND I-98)
           AND (I-03)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (1:80)
               MOVE R                      TO LISTE-IO-AREA (73:8)
               MOVE P-IO                   TO LISTE-IO-AREA (78:3)
               MOVE S-IO                   TO LISTE-IO-AREA (76:5)
               MOVE DTODTO                 TO LISTE-IO-AREA (75:6)
               MOVE DTOMEL                 TO LISTE-IO-AREA (24:57)
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
           SET GMLRESK-LEVEL-INIT          TO TRUE
           INITIALIZE GMLRESK-DATA-FIELDS
           SET GMLRESK-EOF-OFF             TO TRUE
           SET GMLRESK-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO GMLRESK-MC
                                              GMLRESK-MP
           OPEN INPUT GMLRESK
           SET DAGRESK-LEVEL-INIT          TO TRUE
           INITIALIZE DAGRESK-DATA-FIELDS
           SET DAGRESK-EOF-OFF             TO TRUE
           SET DAGRESK-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO DAGRESK-MC
                                              DAGRESK-MP
           OPEN INPUT DAGRESK
           INITIALIZE RESPAR-DATA-FIELDS
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN INPUT RESPAR
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           OPEN OUTPUT OUTRESK
           OPEN OUTPUT REFNULL
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLRESK
           CLOSE DAGRESK
           CLOSE RESPAR
           CLOSE SYSPARM
           CLOSE OUTRESK
           CLOSE REFNULL
           CLOSE AVSTEMM
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
