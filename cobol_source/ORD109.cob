       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD109R.
      **********************************************  Z-WIN-RPG2   ****
      *       O R D R E R U T I N E P R O G R A M   O R D 1 0 9       *
      *       -------------------------------------------------       *
      *  PROGRAMMET SJEKK NOEN VITALE DATA FØR ORDREOPPDAT.PROG.      *
      *  1. SJEKK OG KORRIGERE OM DET ER FEIL I ANT.BEST, REST OG LEV.*
      *     OM DET STÅR 404040XX I ANTALLSFELTENE ER DETTE FEIL OG    *
      *        ANTALLSFELTET RETTET TIL 0,00                          *
      *     OM DET STÅR 40XXXXXX I ANTALLSFELTENE BLIR DET  LISTET UT *
      *        ANTALLSFELTET BLIR IKKE RETTET. DETTE KAN VÆRE 40000,00*
      *  2. SJEKK OG KORRIGERE OM DET ER FEIL ORDREDATO.              *
      *  3. FJERNE KODE FOR UTLEV.ORDRE FOR FIRMA 977.                *
      *  4. ENDRER LAGERLOCATION PÅ FORHÅNDSALG FRA VAREMASTER. 1.10.05
      *  5. FJERNER KODE FOR FORD-ORDRE, DA DETTE ER AVSLUTTET.24.02.08
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD109.rpg
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
           SELECT ORDREM
               ASSIGN TO UT-S-ORDREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREM-STATUS.
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
       FD ORDREM
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X            PICTURE X(164).
      * NDEMX IC  F     200R10A        KSDS
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
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DFDATA-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-EOF-OFF          VALUE '0'.
               88  ORDREM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-READ-OFF         VALUE '0'.
               88  ORDREM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-PROCESS-OFF      VALUE '0'.
               88  ORDREM-PROCESS          VALUE '1'.
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
           05  DFDATA-XX-DATA-FIELDS.
               10  DFTYPE                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DFDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DFINN                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DFDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DFUT1                   PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DFDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DFUT2                   PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DFDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DFMELD                  PICTURE X(57).
           05  ORDREM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  KNR2F                   PICTURE X(2).
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KTSIFF                  PICTURE X(1).
               10  DIRREG                  PICTURE X(1).
               10  FRITT                   PICTURE X(1).
               10  LAGER                   PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  SKAF                    PICTURE X(1).
               10  BETM                    PICTURE X(2).
               10  GEBYR                   PICTURE X(1).
               10  FRAKT                   PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
               10  REST                    PICTURE X(1).
               10  PRIKOD                  PICTURE X(1).
               10  STAM                    PICTURE X(1).
               10  KIS                     PICTURE X(1).
               10  KOMUTA                  PICTURE X(1).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDDTO                  PICTURE X(6).
               10  ORDMA-ELGR              PICTURE X(4).
               10  ORDMND                  PICTURE X(2).
               10  ORDDAG                  PICTURE X(2).
               10  ORDAAR                  PICTURE X(2).
               10  ORDMOT                  PICTURE X(2).
               10  TERMID                  PICTURE X(4).
               10  SELGKP                  PICTURE X(1).
               10  FERDIM                  PICTURE X(1).
               10  FAKTNR                  PICTURE X(2).
               10  REGKL-IO.
                   15  REGKL               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RUTID                   PICTURE X(1).
               10  OPDATO-IO.
                   15  OPDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ANTPRT-IO.
                   15  ANTPRT              PICTURE S9(2).
               10  STATUS-X                PICTURE X(1).
               10  OHREC1                  PICTURE X(164).
               10  VALKOD                  PICTURE X(3).
               10  FAKREF                  PICTURE X(6).
               10  AVNAVN                  PICTURE X(11).
               10  OKKNR                   PICTURE X(6).
               10  REKVNR                  PICTURE X(15).
               10  FORSM                   PICTURE X(15).
               10  HND                     PICTURE X(3).
               10  KADR                    PICTURE X(30).
               10  POSTNR                  PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  OHREC2                  PICTURE X(164).
               10  VAADR1                  PICTURE X(30).
               10  VAADR2                  PICTURE X(30).
               10  VAADR3                  PICTURE X(30).
               10  VAADR4                  PICTURE X(20).
               10  OHREC3                  PICTURE X(164).
               10  LAGLOC                  PICTURE X(6).
               10  POSNR-IO.
                   15  POSNR               PICTURE S9(3).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTRES-IO.
                   15  ANTRES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTB01                  PICTURE X(1).
               10  ANTB03                  PICTURE X(3).
               10  ANTR01                  PICTURE X(1).
               10  ANTR03                  PICTURE X(3).
               10  ANTL01                  PICTURE X(1).
               10  ANTL03                  PICTURE X(3).
               10  NOREST                  PICTURE X(1).
               10  ALF                     PICTURE X(3).
               10  TEKST                   PICTURE X(50).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VGR-IO.
                   15  VGR                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ORPRIS-IO.
                   15  ORPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB1-IO.
                   15  ORRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB2-IO.
                   15  ORRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB3-IO.
                   15  ORRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGPRIS-IO.
                   15  RGPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB1-IO.
                   15  RGRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB2-IO.
                   15  RGRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB3-IO.
                   15  RGRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VEIPRI-IO.
                   15  VEIPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KOSPRI-IO.
                   15  KOSPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KODATO-IO.
                   15  KODATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KOSIGN                  PICTURE X(2).
               10  KOSTAT                  PICTURE X(1).
               10  PRITYP                  PICTURE X(1).
               10  GMLLOC                  PICTURE X(6).
               10  OVREC                   PICTURE X(164).
           05  VAREMAS-DATA-FIELDS.
               10  VLOC10                  PICTURE X(6).
      * NDEMX KX  06
      *                                     138 140 KXVALU
           05  TEMPORARY-FIELDS.
               10  ANTTOT-IO.
                   15  ANTTOT              PICTURE S9(6).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(6).
               10  ANTDAG-IO.
                   15  ANTDAG              PICTURE S9(6).
               10  ANTDIR-IO.
                   15  ANTDIR              PICTURE S9(6).
               10  DAGDIR-IO.
                   15  DAGDIR              PICTURE S9(6).
               10  ANTWAD-IO.
                   15  ANTWAD              PICTURE S9(6).
               10  DAGWAD-IO.
                   15  DAGWAD              PICTURE S9(6).
               10  ANTWNB-IO.
                   15  ANTWNB              PICTURE S9(6).
               10  DAGWNB-IO.
                   15  DAGWNB              PICTURE S9(6).
               10  ANTWHE-IO.
                   15  ANTWHE              PICTURE S9(6).
               10  DAGWHE-IO.
                   15  DAGWHE              PICTURE S9(6).
               10  ANTWFU-IO.
                   15  ANTWFU              PICTURE S9(6).
               10  DAGWFU-IO.
                   15  DAGWFU              PICTURE S9(6).
               10  ANTWEG-IO.
                   15  ANTWEG              PICTURE S9(6).
               10  DAGWEG-IO.
                   15  DAGWEG              PICTURE S9(6).
               10  ANTWKC-IO.
                   15  ANTWKC              PICTURE S9(6).
               10  DAGWKC-IO.
                   15  DAGWKC              PICTURE S9(6).
               10  NYANT-IO.
                   15  NYANT               PICTURE S9(5)V9(2).
               10  VKEY                    PICTURE X(10).
               10  EDBN-IO.
                   15  EDBN                PICTURE S9(7).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  EDBA                    PICTURE X(7).
               10  ANTFHS-IO.
                   15  ANTFHS              PICTURE S9(6).
               10  ANTFHK-IO.
                   15  ANTFHK              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-70YNZ                PICTURE ZZZZZZZ.
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  XO-52YN9R               PICTURE ZZZZZ,99-.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREM-PROCESS
               SET ORDREM-PROCESS-OFF      TO TRUE
               SET ORDREM-READ             TO TRUE
           END-IF
 
           IF  ORDREM-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREM-GET
               SET ORDREM-READ-OFF         TO TRUE
               IF  NOT ORDREM-EOF
                   PERFORM ORDREM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORDREM-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-IDSET
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
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-FLDSET
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
           SET NOT-I-19                    TO TRUE
           SET NOT-I-20                    TO TRUE
           SET NOT-I-91                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-31                    TO TRUE
           SET NOT-I-32                    TO TRUE
           SET NOT-I-33                    TO TRUE
           SET NOT-I-93                    TO TRUE
           SET NOT-I-76                    TO TRUE
           SET NOT-I-77                    TO TRUE
           SET NOT-I-88                    TO TRUE
           SET NOT-I-18                    TO TRUE
           SET NOT-I-47                    TO TRUE
           ADD 1                           TO ANTTOT
           IF  (I-01)
               SET NOT-I-93                TO TRUE
               IF  ORDNR < '500000'
                   SET I-93                TO TRUE
               END-IF
               SET NOT-I-82                TO TRUE
               IF  BK = 'F'
                   SET I-82                TO TRUE
               END-IF
               SET NOT-I-83                TO TRUE
               IF  STATUS-X = 'F'
                   SET I-83                TO TRUE
               END-IF
               SET NOT-I-81                TO TRUE
               IF  LAGER = '10'
                   SET I-81                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  FIRMA = '977'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-76)
               SET NOT-I-77                TO TRUE
               IF  KUNDNR = '500105'
                   SET I-77                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR Å RETTE ORDREDATO PÅ AUTODATA NORGE.               *
      *   DENNE RUTINE KAN SLETTES ETTER 2.03.2006                    *
      *   ER NÅ SLETTET.                                              *
      *****************************************************************
      *  01      FIRMA     COMP "399"                    35 = AUTODATA  NR.
      *  01      ORDNR     COMP "614588"             36  36 = FRAOGMED  NR.
      *  01      ORDNR     COMP "614648"               3737 = TILOGMED  NR.
      *****************************************************************
      * RUTINE FOR FJERNING AV KODE FOR FORD-FORHANDLER ORDRE         *
      * DETTE ER ORDRE FOR FIRMA 918 MED KODE FA,FB,FC I FAKT.OMG.NR. *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-18                TO TRUE
               IF  FIRMA = '918'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-18)
               SET NOT-I-47                TO TRUE
               IF  FAKTNR = 'FA'
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-18 AND NOT-I-47)
               SET NOT-I-47                TO TRUE
               IF  FAKTNR = 'FB'
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-18 AND NOT-I-47)
               SET NOT-I-47                TO TRUE
               IF  FAKTNR = 'FC'
                   SET I-47                TO TRUE
               END-IF
      *****************************************************************
           END-IF
           IF  (NOT-I-01)
               GO TO REC01X-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBRUTINE FOR SJEKKING AV ORDREDATO.             *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           MOVE 'A'                        TO DFTYPE
           MOVE ORDDTO                     TO DFINN
           CALL 'DATO8SIF' USING DFDATA-XX-DATA-FIELDS
           SET NOT-I-91                    TO TRUE
           IF  DFTYPE = 'F'
               SET I-91                    TO TRUE
           END-IF
           IF  (I-91)
               ADD 1                       TO ANT
           END-IF
           IF  (I-93 AND NOT-I-91)
               ADD 1                       TO ANT
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  RUTINE FOR TELLING  AV WEB-ORDRE.                      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-69                    TO TRUE
           IF  SELGKP = '*'
               SET I-69                    TO TRUE
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  DIRREG = 'J'
               SET I-70                    TO TRUE
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  ORDMOT = 'I1'
               SET I-71                    TO TRUE
           END-IF
           SET NOT-I-72                    TO TRUE
           IF  ORDMOT = 'I2'
               SET I-72                    TO TRUE
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  ORDMOT = 'I3'
               SET I-73                    TO TRUE
           END-IF
           SET NOT-I-74                    TO TRUE
           IF  ORDMOT = 'I4'
               SET I-74                    TO TRUE
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  ORDMOT = 'I5'
               SET I-75                    TO TRUE
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  ORDMOT = 'I6'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           SET NOT-I-77                    TO TRUE
           IF  ORDMOT = 'I7'
               SET I-77                    TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTORD
           END-IF
           IF  (I-69)
               ADD 1                       TO ANTDAG
           END-IF
           IF  (I-70)
               ADD 1                       TO ANTDIR
           END-IF
           IF  (I-70 AND I-69)
               ADD 1                       TO DAGDIR
           END-IF
           IF  (I-71)
               ADD 1                       TO ANTWAD
           END-IF
           IF  (I-71 AND I-69)
               ADD 1                       TO DAGWAD
           END-IF
           IF  (I-72)
               ADD 1                       TO ANTWNB
           END-IF
           IF  (I-72 AND I-69)
               ADD 1                       TO DAGWNB
           END-IF
           IF  (I-73)
               ADD 1                       TO ANTWHE
           END-IF
           IF  (I-73 AND I-69)
               ADD 1                       TO DAGWHE
           END-IF
           IF  (I-74)
               ADD 1                       TO ANTWFU
           END-IF
           IF  (I-74 AND I-69)
               ADD 1                       TO DAGWFU
           END-IF
           IF  (I-75)
               ADD 1                       TO ANTWEG
           END-IF
           IF  (I-75 AND I-69)
               ADD 1                       TO DAGWEG
           END-IF
           IF  (I-77)
               ADD 1                       TO ANTWKC
           END-IF
           IF  (I-77 AND I-69)
               ADD 1                       TO DAGWKC
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           .
 
       REC01X-T.
           IF  (NOT-I-04)
               GO TO SLUTT-T
      *****************************************************************
      * SJEKK ANTALLSFELTER.                                          *
      *****************************************************************
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  ANTB01 = ' '
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  ANTR01 = ' '
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  ANTL01 = ' '
               SET I-23                    TO TRUE
           END-IF
           IF  (I-21)
               SET I-19                    TO TRUE
           END-IF
           IF  (I-22)
               SET I-19                    TO TRUE
           END-IF
           IF  (I-23)
               SET I-19                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  ANTB03 = '   '
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  ANTR03 = '   '
               SET I-32                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  ANTL03 = '   '
               SET I-33                    TO TRUE
           END-IF
           IF  (I-31)
               SET I-20                    TO TRUE
           END-IF
           IF  (I-32)
               SET I-20                    TO TRUE
           END-IF
           IF  (I-33)
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               MOVE 0,00                   TO NYANT
      *****************************************************************
           END-IF
           IF  (I-19)
               ADD 1                       TO ANT
      *****************************************************************
      * RUTINE FOR Å ENDRE LAGERLOCATION PÅ FORHÅNDSALG.              *
      * DETTE FORDI AT VAREN KAN HA FÅTT NY LOCATION SIDEN ORDREN     *
      * HAR BLITT REGISTRERT.                                         *
      *****************************************************************
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  (NOT-I-82)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-83)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-81)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-84                    TO TRUE
           IF  EDBNR = 0000000
               SET I-84                    TO TRUE
           END-IF
           IF  (I-84)
               GO TO SLUTT-T
           END-IF
           MOVE FIRMA                      TO VKEY (1:3)
           MOVE EDBNR                      TO EDBNR-N
           MOVE EDBNR-N-IO                 TO EDBN-IO
           MOVE EDBN                       TO EDBA
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE EDBA (7:1)                 TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO EDBA (7:1)
           MOVE EDBA                       TO VKEY (4:7)
           MOVE VKEY                       TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-15                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-15                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (NOT-I-15)
               SET NOT-I-88                TO TRUE
               IF  LAGLOC NOT = VLOC10
                   SET I-88                TO TRUE
               END-IF
           END-IF
           IF  (I-04)
               ADD 1                       TO ANTFHS
           END-IF
           IF  (I-04 AND I-88)
               ADD 1                       TO ANTFHK
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       ORDREM-GET SECTION.
       ORDREM-GET-P.
           IF  ORDREM-EOF-OFF
               READ ORDREM
               AT END
                   SET ORDREM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:6)  TO KUNDNR (1:6)
               MOVE ORDREM-IO-AREA (21:2)  TO KNR2F (1:2)
               MOVE ORDREM-IO-AREA (27:30) TO KNAVN1 (1:30)
               MOVE ORDREM-IO-AREA (57:30) TO KNAVN2 (1:30)
               MOVE ORDREM-IO-AREA (87:1)  TO KTSIFF (1:1)
               MOVE ORDREM-IO-AREA (88:1)  TO DIRREG (1:1)
               MOVE ORDREM-IO-AREA (89:1)  TO FRITT (1:1)
               MOVE ORDREM-IO-AREA (90:2)  TO LAGER (1:2)
               MOVE ORDREM-IO-AREA (92:1)  TO BK (1:1)
               MOVE ORDREM-IO-AREA (93:1)  TO SKAF (1:1)
               MOVE ORDREM-IO-AREA (94:2)  TO BETM (1:2)
               MOVE ORDREM-IO-AREA (96:1)  TO GEBYR (1:1)
               MOVE ORDREM-IO-AREA (97:1)  TO FRAKT (1:1)
               MOVE ORDREM-IO-AREA (98:1)  TO AVD (1:1)
               MOVE ORDREM-IO-AREA (99:1)  TO KRETYP (1:1)
               MOVE ORDREM-IO-AREA (100:1) TO REST (1:1)
               MOVE ORDREM-IO-AREA (101:1) TO PRIKOD (1:1)
               MOVE ORDREM-IO-AREA (102:1) TO STAM (1:1)
               MOVE ORDREM-IO-AREA (103:1) TO KIS (1:1)
               MOVE ORDREM-IO-AREA (105:1) TO KOMUTA (1:1)
               MOVE ORDREM-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (136:6) TO ORDDTO (1:6)
               MOVE ORDREM-IO-AREA (138:4) TO ORDMA-ELGR (1:4)
               MOVE ORDREM-IO-AREA (138:2) TO ORDMND (1:2)
               MOVE ORDREM-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDREM-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDREM-IO-AREA (142:2) TO ORDMOT (1:2)
               MOVE ORDREM-IO-AREA (144:4) TO TERMID (1:4)
               MOVE ORDREM-IO-AREA (148:1) TO SELGKP (1:1)
               MOVE ORDREM-IO-AREA (149:1) TO FERDIM (1:1)
               MOVE ORDREM-IO-AREA (150:2) TO FAKTNR (1:2)
               MOVE ORDREM-IO-AREA (153:4) TO REGKL-IO
               MOVE ORDREM-IO-AREA (157:1) TO RUTID (1:1)
               MOVE ORDREM-IO-AREA (158:4) TO OPDATO-IO
               MOVE ORDREM-IO-AREA (162:2) TO ANTPRT-IO
               INSPECT ANTPRT-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (164:1) TO STATUS-X (1:1)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC1 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (31:3)  TO VALKOD (1:3)
               MOVE ORDREM-IO-AREA (44:6)  TO FAKREF (1:6)
               MOVE ORDREM-IO-AREA (50:11) TO AVNAVN (1:11)
               MOVE ORDREM-IO-AREA (61:6)  TO OKKNR (1:6)
               MOVE ORDREM-IO-AREA (67:15) TO REKVNR (1:15)
               MOVE ORDREM-IO-AREA (82:15) TO FORSM (1:15)
               MOVE ORDREM-IO-AREA (97:3)  TO HND (1:3)
               MOVE ORDREM-IO-AREA (101:30) TO KADR (1:30)
               MOVE ORDREM-IO-AREA (131:4) TO POSTNR (1:4)
               MOVE ORDREM-IO-AREA (135:15) TO PSTED (1:15)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC2 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:30) TO VAADR1 (1:30)
               MOVE ORDREM-IO-AREA (51:30) TO VAADR2 (1:30)
               MOVE ORDREM-IO-AREA (81:30) TO VAADR3 (1:30)
               MOVE ORDREM-IO-AREA (111:20) TO VAADR4 (1:20)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC3 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (11:6)  TO LAGLOC (1:6)
               MOVE ORDREM-IO-AREA (17:3)  TO POSNR-IO
               INSPECT POSNR-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (21:4)  TO ANTBES-IO
               MOVE ORDREM-IO-AREA (25:4)  TO ANTRES-IO
               MOVE ORDREM-IO-AREA (29:4)  TO ANTLEV-IO
               MOVE ORDREM-IO-AREA (21:1)  TO ANTB01 (1:1)
               MOVE ORDREM-IO-AREA (21:3)  TO ANTB03 (1:3)
               MOVE ORDREM-IO-AREA (25:1)  TO ANTR01 (1:1)
               MOVE ORDREM-IO-AREA (25:3)  TO ANTR03 (1:3)
               MOVE ORDREM-IO-AREA (29:1)  TO ANTL01 (1:1)
               MOVE ORDREM-IO-AREA (29:3)  TO ANTL03 (1:3)
               MOVE ORDREM-IO-AREA (33:1)  TO NOREST (1:1)
               MOVE ORDREM-IO-AREA (34:3)  TO ALF (1:3)
               MOVE ORDREM-IO-AREA (37:50) TO TEKST (1:50)
               MOVE ORDREM-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDREM-IO-AREA (57:30) TO VARBET (1:30)
               MOVE ORDREM-IO-AREA (87:4)  TO EDBNR-IO
               MOVE ORDREM-IO-AREA (91:3)  TO VGR-IO
               MOVE ORDREM-IO-AREA (94:5)  TO ORPRIS-IO
               MOVE ORDREM-IO-AREA (99:2)  TO ORRAB1-IO
               MOVE ORDREM-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDREM-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDREM-IO-AREA (105:5) TO RGPRIS-IO
               MOVE ORDREM-IO-AREA (110:2) TO RGRAB1-IO
               MOVE ORDREM-IO-AREA (112:2) TO RGRAB2-IO
               MOVE ORDREM-IO-AREA (114:2) TO RGRAB3-IO
               MOVE ORDREM-IO-AREA (116:5) TO VEIPRI-IO
               MOVE ORDREM-IO-AREA (121:5) TO KOSPRI-IO
               MOVE ORDREM-IO-AREA (126:4) TO PRITIL-IO
               MOVE ORDREM-IO-AREA (130:4) TO KODATO-IO
               MOVE ORDREM-IO-AREA (134:2) TO KOSIGN (1:2)
               MOVE ORDREM-IO-AREA (136:1) TO KOSTAT (1:1)
               MOVE ORDREM-IO-AREA (137:1) TO PRITYP (1:1)
               MOVE ORDREM-IO-AREA (138:6) TO GMLLOC (1:6)
               MOVE ORDREM-IO-AREA (1:164) TO OVREC (1:164)
           END-EVALUATE.
 
       ORDREM-IDCHK SECTION.
       ORDREM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (140:6) TO VLOC10 (1:6)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-05                        TO TRUE.
 
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
           IF  (I-01 AND I-93)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA='               TO LISTE-IO-AREA (2:6)
               MOVE FIRMA                  TO LISTE-IO-AREA (8:3)
               MOVE 'ORDRENR='             TO LISTE-IO-AREA (13:8)
               MOVE ORDNR                  TO LISTE-IO-AREA (21:6)
               MOVE 'DATO='                TO LISTE-IO-AREA (29:5)
               MOVE ORDDTO                 TO LISTE-IO-AREA (34:6)
               MOVE 'FEIL ORDRENR.  '      TO LISTE-IO-AREA (41:15)
      *                     NU1          67 "RETTET TIL "
      *                      U1          67 "RETTES TIL "
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-91)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA='               TO LISTE-IO-AREA (2:6)
               MOVE FIRMA                  TO LISTE-IO-AREA (8:3)
               MOVE 'ORDRENR='             TO LISTE-IO-AREA (13:8)
               MOVE ORDNR                  TO LISTE-IO-AREA (21:6)
               MOVE 'DATO='                TO LISTE-IO-AREA (29:5)
               MOVE ORDDTO                 TO LISTE-IO-AREA (34:6)
               MOVE 'FEIL ORDREDATO.'      TO LISTE-IO-AREA (41:15)
               IF  (NOT-I-U1)
                   MOVE 'RETTET TIL '      TO LISTE-IO-AREA (57:11)
               END-IF
               IF  (I-U1)
                   MOVE 'RETTES TIL '      TO LISTE-IO-AREA (57:11)
               END-IF
               MOVE UDAY                   TO LISTE-IO-AREA (68:2)
               MOVE UMONTH                 TO LISTE-IO-AREA (70:2)
               MOVE UYEAR                  TO LISTE-IO-AREA (72:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-47)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA='               TO LISTE-IO-AREA (2:6)
               MOVE FIRMA                  TO LISTE-IO-AREA (8:3)
               MOVE 'ORDRENR='             TO LISTE-IO-AREA (13:8)
               MOVE ORDNR                  TO LISTE-IO-AREA (21:6)
               MOVE 'DATO='                TO LISTE-IO-AREA (29:5)
               MOVE ORDDTO                 TO LISTE-IO-AREA (34:6)
               MOVE 'FORD MERKE BLANKET.'  TO LISTE-IO-AREA (40:19)
               MOVE FAKTNR                 TO LISTE-IO-AREA (61:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-04 AND I-19)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA='               TO LISTE-IO-AREA (2:6)
               MOVE FIRMA                  TO LISTE-IO-AREA (8:3)
               MOVE 'ORDRENR='             TO LISTE-IO-AREA (13:8)
               MOVE ORDNR                  TO LISTE-IO-AREA (21:6)
               MOVE 'DATO='                TO LISTE-IO-AREA (29:5)
               MOVE ORDDTO                 TO LISTE-IO-AREA (34:6)
               MOVE 'RUTINE='              TO LISTE-IO-AREA (44:7)
               MOVE RUTID                  TO LISTE-IO-AREA (51:1)
               MOVE 'KUNDE='               TO LISTE-IO-AREA (55:6)
               MOVE KUNDNR                 TO LISTE-IO-AREA (62:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFA='                TO LISTE-IO-AREA (12:5)
               MOVE ALF                    TO LISTE-IO-AREA (17:3)
               MOVE 'ARTNR='               TO LISTE-IO-AREA (21:6)
               MOVE ARTNR                  TO LISTE-IO-AREA (27:20)
               MOVE 'ENR='                 TO LISTE-IO-AREA (48:4)
               MOVE EDBNR                  TO XO-70YNZ
               MOVE XO-70YNZ               TO LISTE-IO-AREA (52:7)
               MOVE 'VGR='                 TO LISTE-IO-AREA (60:4)
               MOVE VGR                    TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (65:5)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT.BEST='            TO LISTE-IO-AREA (11:9)
               MOVE ANTBES                 TO XO-52YN9R
               MOVE XO-52YN9R              TO LISTE-IO-AREA (20:9)
               MOVE 'ANT.REST='            TO LISTE-IO-AREA (31:9)
               MOVE ANTRES                 TO XO-52YN9R
               MOVE XO-52YN9R              TO LISTE-IO-AREA (40:9)
               MOVE 'ANT.LEV.='            TO LISTE-IO-AREA (51:9)
               MOVE ANTLEV                 TO XO-52YN9R
               MOVE XO-52YN9R              TO LISTE-IO-AREA (60:9)
               IF  (I-31)
                   MOVE 'FEIL BEST.ANTALL' TO LISTE-IO-AREA (71:16)
               END-IF
               IF  (I-32)
                   MOVE 'FEIL REST.ANTALL' TO LISTE-IO-AREA (71:16)
               END-IF
               IF  (I-33)
                   MOVE 'FEIL LEV. ANTALL' TO LISTE-IO-AREA (71:16)
               END-IF
               IF  (I-31 AND I-32)
                   MOVE 'FEIL BEST+REST ANT.' TO LISTE-IO-AREA (71:19)
               END-IF
               IF  (I-20 AND NOT-I-U1)
                   MOVE '** KORR. TIL 0,00 **' TO LISTE-IO-AREA (91:20)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-91 AND NOT-I-U1)
               MOVE UDAY                   TO ORDREM-IO-AREA (136:2)
               MOVE UMONTH                 TO ORDREM-IO-AREA (138:2)
               MOVE UYEAR                  TO ORDREM-IO-AREA (140:2)
      * DREM  D        01 35 36
      *      AND       37NU1
      *                                 141 "280206"
               REWRITE ORDREM-IO-AREA
           END-IF
           IF  (I-01 AND I-77 AND NOT-I-U1)
               MOVE ' '                    TO ORDREM-IO-AREA (105:1)
               REWRITE ORDREM-IO-AREA
           END-IF
           IF  (I-01 AND I-47 AND NOT-I-U1)
               MOVE '  '                   TO ORDREM-IO-AREA (150:2)
      *****************************************************************
      * ENDRING AV VALUTAKODE KOLBERG                                 *
      *****************************************************************
      *****************************************************************
               REWRITE ORDREM-IO-AREA
           END-IF
           IF  (I-04 AND I-20 AND NOT-I-U1)
               IF  (I-31)
                   MOVE NYANT              TO XO-52P
                   MOVE XO-52P-EF          TO ORDREM-IO-AREA (21:4)
               END-IF
               IF  (I-32)
                   MOVE NYANT              TO XO-52P
                   MOVE XO-52P-EF          TO ORDREM-IO-AREA (25:4)
               END-IF
               IF  (I-33)
                   MOVE NYANT              TO XO-52P
                   MOVE XO-52P-EF          TO ORDREM-IO-AREA (29:4)
      *****************************************************************
      * ENDRING AV LAGERLOC  FORHÅNDSORDRE.                           *
      *****************************************************************
               END-IF
               REWRITE ORDREM-IO-AREA
           END-IF
           IF  (I-04 AND I-88 AND NOT-I-U1)
               MOVE VLOC10                 TO ORDREM-IO-AREA (11:6)
               REWRITE ORDREM-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DAGLIG ORDRERAPPORT OG F' TO LISTE-IO-AREA (1:24)
               MOVE 'EILRAPPORT.   FREMSTILT ' TO LISTE-IO-AREA (25:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (50:8)
               MOVE 'LEVERES ESPEN'        TO LISTE-IO-AREA (60:13)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOT. ORDRE REC. '     TO LISTE-IO-AREA (5:16)
               MOVE ANTTOT                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL MED FEIL '     TO LISTE-IO-AREA (5:16)
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. ORDRE TOTALT'    TO LISTE-IO-AREA (5:17)
               MOVE ANTORD                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE ANTDAG                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV DIR.ORDRE '     TO LISTE-IO-AREA (5:16)
               MOVE ANTDIR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE DAGDIR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV WEB AD.   '     TO LISTE-IO-AREA (5:16)
               MOVE ANTWAD                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE DAGWAD                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV WEB NBK.  '     TO LISTE-IO-AREA (5:16)
               MOVE ANTWNB                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE DAGWNB                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV WEB HELLIOS'    TO LISTE-IO-AREA (5:17)
               MOVE ANTWHE                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE DAGWHE                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV WEB FUTURE'     TO LISTE-IO-AREA (5:16)
               MOVE ANTWFU                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE DAGWFU                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV WEB EGEN  '     TO LISTE-IO-AREA (5:16)
               MOVE ANTWEG                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE DAGWEG                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'HERAV WEB KCL   '     TO LISTE-IO-AREA (5:16)
               MOVE ANTWKC                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'ANT. REG. I DAG'      TO LISTE-IO-AREA (33:15)
               MOVE DAGWKC                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. LOC.ENDRET '     TO LISTE-IO-AREA (5:16)
               MOVE ANTFHK                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'PÅ FORHÅNDS SALG.'    TO LISTE-IO-AREA (33:17)
               MOVE ANTFHS                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               MOVE 'VARELINJER TOTALT.'   TO LISTE-IO-AREA (57:18)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE ORDREM-DATA-FIELDS
           SET ORDREM-EOF-OFF              TO TRUE
           SET ORDREM-PROCESS              TO TRUE
           OPEN I-O ORDREM
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDREM
           CLOSE VAREMAS
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
