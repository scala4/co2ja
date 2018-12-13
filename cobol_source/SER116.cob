       IDENTIFICATION DIVISION.
       PROGRAM-ID. SER116R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: SER116.rpg
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
           SELECT PARFILE
               ASSIGN TO UT-S-PARFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARFILE-STATUS.
           SELECT SERFILE
               ASSIGN TO UT-S-SERFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SERFILE-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
           SELECT FIRMAK
               ASSIGN TO FIRMAK
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAK-STATUS
               RECORD KEY IS FIRMAK-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARFILE
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  PARFILE-IO-AREA.
           05  PARFILE-IO-AREA-X           PICTURE X(100).
       FD SERFILE
               BLOCK CONTAINS 9000
               RECORD CONTAINS 90.
       01  SERFILE-IO-AREA-2.
           05  SERFILE-IO-AREA-X           PICTURE X(90).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
       FD FIRMAK
               RECORD CONTAINS 1000.
       01  FIRMAK-IO-AREA.
           05  FIRMAK-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAK-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD OUTPUT-X
               RECORD CONTAINS 220.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(220).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARFILE-STATUS              PICTURE 99 VALUE 0.
           10  SERFILE-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAK-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-EOF-OFF         VALUE '0'.
               88  PARFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-READ-OFF        VALUE '0'.
               88  PARFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-PROCESS-OFF     VALUE '0'.
               88  PARFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SERFILE-EOF-OFF         VALUE '0'.
               88  SERFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SERFILE-READ-OFF        VALUE '0'.
               88  SERFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SERFILE-PROCESS-OFF     VALUE '0'.
               88  SERFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SERFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  SERFILE-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SERFILE-AHEAD-EOF-OFF   VALUE '0'.
               88  SERFILE-AHEAD-EOF       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SERFILE-AHEAD-READ-OFF  VALUE '0'.
               88  SERFILE-AHEAD-READ      VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAK-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
           05  PARFILE-DATA-FIELDS.
               10  PA-ELGR                 PICTURE X(4).
               10  PMND                    PICTURE X(9).
           05  SERFILE-LEVEL-02.
               10  SERFILE-02-L4.
                   15  SERFILE-02-L4-FIRM  PICTURE X(3).
               10  SERFILE-02-L3.
                   15  SERFILE-02-L3-AVD   PICTURE X(1).
               10  SERFILE-02-L2.
                   15  SERFILE-02-L2-VGR1  PICTURE X(2).
               10  SERFILE-02-L1.
                   15  SERFILE-02-L1-VGR   PICTURE X(5).
           05  SERFILE-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  REGDTO-IO.
                   15  REGDTO              PICTURE S9(6).
               10  VGR1                    PICTURE X(2).
               10  VGR                     PICTURE X(5).
               10  AVD                     PICTURE X(1).
               10  VLINJE-IO.
                   15  VLINJE              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  RLINJE-IO.
                   15  RLINJE              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ORDBEL-IO.
                   15  ORDBEL              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RESBEL-IO.
                   15  RESBEL              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KUNDNR                  PICTURE X(6).
               10  ORDNR                   PICTURE X(6).
               10  EDBNR                   PICTURE X(7).
               10  DIRREG                  PICTURE X(1).
               10  ARTNR                   PICTURE X(20).
               10  BEHOLD-IO.
                   15  BEHOLD              PICTURE S9(7)V9(2).
               10  VGR2                    PICTURE X(2).
               10  VGRN                    PICTURE X(5).
           05  VAGRMAS-DATA-FIELDS.
               10  VGNAVN                  PICTURE X(40).
           05  FIRMAK-DATA-FIELDS.
               10  AVDN1                   PICTURE X(10).
               10  AVDN2                   PICTURE X(10).
               10  AVDN3                   PICTURE X(10).
               10  AVDN4                   PICTURE X(10).
               10  AVDN5                   PICTURE X(10).
               10  AVDN6                   PICTURE X(10).
               10  AVDN7                   PICTURE X(10).
               10  AVDN8                   PICTURE X(10).
               10  AVDN9                   PICTURE X(10).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  SUMA-IO.
                   15  SUMA                PICTURE S9(10).
               10  SUMB-IO.
                   15  SUMB                PICTURE S9(9)V9(2).
               10  LINEPR-IO.
                   15  LINEPR              PICTURE S9(3)V9(1).
               10  VGRLIN-IO.
                   15  VGRLIN              PICTURE S9(8).
               10  AVDLIN-IO.
                   15  AVDLIN              PICTURE S9(8).
               10  TOTLIN-IO.
                   15  TOTLIN              PICTURE S9(8).
               10  VGRRLI-IO.
                   15  VGRRLI              PICTURE S9(8).
               10  AVDRLI-IO.
                   15  AVDRLI              PICTURE S9(8).
               10  TOTRLI-IO.
                   15  TOTRLI              PICTURE S9(8).
               10  VGRBEL-IO.
                   15  VGRBEL              PICTURE S9(7)V9(2).
               10  AVDBEL-IO.
                   15  AVDBEL              PICTURE S9(7)V9(2).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(8)V9(2).
               10  VGRREL-IO.
                   15  VGRREL              PICTURE S9(7)V9(2).
               10  AVDREL-IO.
                   15  AVDREL              PICTURE S9(7)V9(2).
               10  TOTREL-IO.
                   15  TOTREL              PICTURE S9(8)V9(2).
               10  VGRNLI-IO.
                   15  VGRNLI              PICTURE S9(8).
               10  VGRSPR-IO.
                   15  VGRSPR              PICTURE S9(3)V9(1).
               10  VGRRPR-IO.
                   15  VGRRPR              PICTURE S9(3)V9(1).
               10  FNRVGR                  PICTURE X(8).
               10  AVDNLI-IO.
                   15  AVDNLI              PICTURE S9(8).
               10  AVDSPR-IO.
                   15  AVDSPR              PICTURE S9(3)V9(1).
               10  AVDRPR-IO.
                   15  AVDRPR              PICTURE S9(3)V9(1).
               10  TOTNLI-IO.
                   15  TOTNLI              PICTURE S9(8).
               10  TOTSPR-IO.
                   15  TOTSPR              PICTURE S9(3)V9(1).
               10  TOTRPR-IO.
                   15  TOTRPR              PICTURE S9(3)V9(1).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-31YY9R               PICTURE ZZZ,9-.
               10  XO-40YY9                PICTURE Z.ZZ9.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
           05  SERFILE-IO-AREA.
               10  FILLER                  PICTURE X(90).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARFILE-PROCESS
               SET PARFILE-PROCESS-OFF     TO TRUE
               SET PARFILE-READ            TO TRUE
           END-IF
 
           IF  PARFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARFILE-GET
               SET PARFILE-READ-OFF        TO TRUE
               IF  NOT PARFILE-EOF
                   PERFORM PARFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  SERFILE-PROCESS
               SET SERFILE-PROCESS-OFF     TO TRUE
               SET SERFILE-READ            TO TRUE
           END-IF
 
           IF  SERFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM SERFILE-GET
               SET SERFILE-READ-OFF        TO TRUE
               IF  NOT SERFILE-EOF
                   SET SERFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-IDSET
           END-IF
 
           IF  SERFILE-PROCESS
               PERFORM SERFILE-IDSET
           END-IF
 
           IF  SERFILE-PROCESS
               PERFORM SERFILE-CHK-LEVEL
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
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-FLDSET
           END-IF
 
           IF  SERFILE-PROCESS
               PERFORM SERFILE-FLDOFF
               PERFORM SERFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SERFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L4)
               MOVE FIRM                   TO FIRMAK-KEY1
               READ FIRMAK RECORD KEY IS FIRMAK-KEY1
               INVALID KEY
                   SET I-08                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-08            TO TRUE
                   PERFORM FIRMAK-FLDSET
                   PERFORM FIRMAK-IDSET
               END-READ
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               SET NOT-I-50                TO TRUE
               IF  FIRM = '918'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-51)
               OR  (I-L1)
               SET NOT-I-52                TO TRUE
           END-IF
           SUBTRACT SUMA                   FROM SUMA
           SUBTRACT SUMB                   FROM SUMB
      ***********************************************************
      * DATALJRAPPORTERING NÅR REST.
      ***********************************************************
           IF  (I-02)
               SET NOT-I-31                TO TRUE
               IF  RLINJE > 0
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31)
               SET NOT-I-32                TO TRUE
               IF  DIRREG = 'J'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31 AND NOT-I-10)
               MULTIPLY 100 BY RESBEL  GIVING SUMB
               DIVIDE SUMB BY ORDBEL   GIVING LINEPR ROUNDED
      ***********************************************************
      ** SUMMERING AV ANTALL VARELINJER OG TOTALSUMER
      ***********************************************************
           END-IF
           IF  (I-02)
               ADD VLINJE                  TO VGRLIN
               SET NOT-I-20                TO TRUE
               IF  VGRLIN = 0
                   SET I-20                TO TRUE
               END-IF
               ADD VLINJE                  TO AVDLIN
               SET NOT-I-21                TO TRUE
               IF  AVDLIN = 0
                   SET I-21                TO TRUE
               END-IF
               ADD VLINJE                  TO TOTLIN
               SET NOT-I-22                TO TRUE
               IF  TOTLIN = 0
                   SET I-22                TO TRUE
               END-IF
               ADD RLINJE                  TO VGRRLI
               ADD RLINJE                  TO AVDRLI
               ADD RLINJE                  TO TOTRLI
               ADD ORDBEL                  TO VGRBEL
               SET NOT-I-23                TO TRUE
               IF  VGRBEL = 0
                   SET I-23                TO TRUE
               END-IF
               ADD ORDBEL                  TO AVDBEL
               SET NOT-I-24                TO TRUE
               IF  AVDBEL = 0
                   SET I-24                TO TRUE
               END-IF
               ADD ORDBEL                  TO TOTBEL
               SET NOT-I-25                TO TRUE
               IF  TOTBEL = 0
                   SET I-25                TO TRUE
               END-IF
               ADD RESBEL                  TO VGRREL
               ADD RESBEL                  TO AVDREL
               ADD RESBEL                  TO TOTREL
      ***
           END-IF
           .
 
       SLUTT-T.
      *  SERVICEPROSENT OG RESTORDRE I PROSENT PR VGR.
      ***
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'SER03'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'SER116  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-86)
               GO TO L1END-T
      ***********************************************************
      *  SJEKKE OM L3-BRUDD ER AVDELINGSBRUDD PÅ S & B        ***
      ***********************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-51                TO TRUE
           END-IF
           IF  (I-L4)
               GO TO START1-T
           END-IF
           IF  (I-L2 AND NOT-I-50)
               GO TO START1-T
           END-IF
           IF  (I-L2)
               SET NOT-I-51                TO TRUE
               IF  VGR1 = VGR2
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-52                TO TRUE
               IF  VGR = VGRN
                   SET I-52                TO TRUE
               END-IF
           END-IF.
 
       START1-T.
      ***********************************************************
           IF  (I-L1 AND NOT-I-52)
               SUBTRACT VGRRLI FROM VGRLIN GIVING VGRNLI
               MULTIPLY 100 BY VGRNLI  GIVING SUMA
           END-IF
           IF  (I-L1 AND NOT-I-20 AND NOT-I-52)
               DIVIDE SUMA BY VGRLIN   GIVING VGRSPR ROUNDED
           END-IF
           IF  (I-L1 AND NOT-I-52)
               MULTIPLY 100 BY VGRREL  GIVING SUMB
           END-IF
           IF  (I-L1 AND NOT-I-23 AND NOT-I-52)
               DIVIDE SUMB BY VGRBEL   GIVING VGRRPR ROUNDED
      ***
      *  HENTING AV VAREGR.NAVN FRA VAREGRUPPEARKIV
      ***
           END-IF
           IF  (I-L1 AND NOT-I-52)
               MOVE FIRM                   TO FNRVGR (1:3)
               MOVE VGR                    TO FNRVGR (4:5)
               MOVE FNRVGR                 TO VAGRMAS-KEY1
               READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM VAGRMAS-FLDSET
                   PERFORM VAGRMAS-IDSET
               END-READ
      ***
      * AVDELINGSTOTALER
      ***
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-51)
               SUBTRACT AVDRLI FROM AVDLIN GIVING AVDNLI
           END-IF
           IF  (I-L3 AND NOT-I-50)
               SUBTRACT AVDRLI FROM AVDLIN GIVING AVDNLI
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-51)
               MULTIPLY 100 BY AVDNLI  GIVING SUMA
           END-IF
           IF  (I-L3 AND NOT-I-50)
               MULTIPLY 100 BY AVDNLI  GIVING SUMA
           END-IF
           IF  (I-L2 AND NOT-I-21 AND I-50 AND NOT-I-51)
               DIVIDE SUMA BY AVDLIN   GIVING AVDSPR ROUNDED
           END-IF
           IF  (I-L3 AND NOT-I-21 AND NOT-I-50)
               DIVIDE SUMA BY AVDLIN   GIVING AVDSPR
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-51)
               MULTIPLY 100 BY AVDREL  GIVING SUMB
           END-IF
           IF  (I-L3 AND NOT-I-50)
               MULTIPLY 100 BY AVDREL  GIVING SUMB
           END-IF
           IF  (I-L2 AND NOT-I-24 AND I-50 AND NOT-I-51)
               DIVIDE SUMB BY AVDBEL   GIVING AVDRPR
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-50)
               DIVIDE SUMB BY AVDBEL   GIVING AVDRPR
      ***
      * FIRMATOTALER
      ***
           END-IF
           IF  (I-L4)
               SUBTRACT TOTRLI FROM TOTLIN GIVING TOTNLI
               MULTIPLY 100 BY TOTNLI  GIVING SUMA
           END-IF
           IF  (I-L4 AND NOT-I-22)
               DIVIDE SUMA BY TOTLIN   GIVING TOTSPR ROUNDED
           END-IF
           IF  (I-L4)
               MULTIPLY 100 BY TOTREL  GIVING SUMB
           END-IF
           IF  (I-L4 AND NOT-I-25)
               DIVIDE SUMB BY TOTBEL   GIVING TOTRPR
           END-IF.
 
       L1END-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       PARFILE-GET SECTION.
       PARFILE-GET-P.
           IF  PARFILE-EOF-OFF
               READ PARFILE
               AT END
                   SET PARFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARFILE-FLDSET SECTION.
       PARFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = '9'
            AND   PARFILE-IO-AREA (2:1) = '0' )
               MOVE PARFILE-IO-AREA (3:4)  TO PA-ELGR (1:4)
               MOVE PARFILE-IO-AREA (9:9)  TO PMND (1:9)
           END-EVALUATE.
 
       PARFILE-IDCHK SECTION.
       PARFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = '9'
            AND   PARFILE-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARFILE-IDSET SECTION.
       PARFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = '9'
            AND   PARFILE-IO-AREA (2:1) = '0' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       SERFILE-GET SECTION.
       SERFILE-GET-P.
           IF  SERFILE-EOF-OFF
               IF  SERFILE-AHEAD-EOF-OFF
                   IF  SERFILE-AHEAD-READ-OFF
                       SET SERFILE-AHEAD-READ TO TRUE
                       READ SERFILE
                       AT END
                           SET SERFILE-AHEAD-EOF TO TRUE
                           INITIALIZE SERFILE-IO-AREA-2
                       END-READ
                   END-IF
                   MOVE SERFILE-IO-AREA-2  TO SERFILE-IO-AREA
                   IF  SERFILE-AHEAD-EOF-OFF
                       READ SERFILE
                       AT END
                           SET SERFILE-AHEAD-EOF TO TRUE
                           INITIALIZE SERFILE-IO-AREA-2
                       END-READ
                   ELSE
                       SET SERFILE-EOF     TO TRUE
                       SUBTRACT 1        FROM LR-CHECK
                   END-IF
                   PERFORM SERFILE-AHEAD-FLDSET
               ELSE
                   SET SERFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-IF
           END-IF.
 
       SERFILE-FLDOFF SECTION.
       SERFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-10                TO TRUE
           END-EVALUATE.
 
       SERFILE-FLDSET SECTION.
       SERFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SERFILE-IO-AREA (3:3)  TO FIRM (1:3)
               MOVE SERFILE-IO-AREA (6:3)  TO ALFA (1:3)
               MOVE SERFILE-IO-AREA (9:6)  TO REGDTO-IO
               INSPECT REGDTO-IO REPLACING ALL ' ' BY '0'
               MOVE SERFILE-IO-AREA (31:2) TO VGR1 (1:2)
               MOVE SERFILE-IO-AREA (31:5) TO VGR (1:5)
               MOVE SERFILE-IO-AREA (36:1) TO AVD (1:1)
               MOVE SERFILE-IO-AREA (15:3) TO VLINJE-IO
               MOVE SERFILE-IO-AREA (18:3) TO RLINJE-IO
               MOVE SERFILE-IO-AREA (21:5) TO ORDBEL-IO
               IF  ORDBEL = ZERO
                   SET I-10                TO TRUE
               END-IF
               MOVE SERFILE-IO-AREA (26:5) TO RESBEL-IO
               MOVE SERFILE-IO-AREA (41:6) TO KUNDNR (1:6)
               MOVE SERFILE-IO-AREA (47:6) TO ORDNR (1:6)
               MOVE SERFILE-IO-AREA (53:7) TO EDBNR (1:7)
               MOVE SERFILE-IO-AREA (60:1) TO DIRREG (1:1)
               MOVE SERFILE-IO-AREA (61:20) TO ARTNR (1:20)
               MOVE SERFILE-IO-AREA (82:9) TO BEHOLD-IO
               INSPECT BEHOLD-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       SERFILE-IDSET SECTION.
       SERFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       SERFILE-CHK-LEVEL SECTION.
       SERFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SERFILE-LEVEL-02
               MOVE SERFILE-IO-AREA (3:3)  TO SERFILE-02-L4-FIRM
               MOVE SERFILE-IO-AREA (36:1) TO SERFILE-02-L3-AVD
               MOVE SERFILE-IO-AREA (31:2) TO SERFILE-02-L2-VGR1
               MOVE SERFILE-IO-AREA (31:5) TO SERFILE-02-L1-VGR
               IF  SERFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SERFILE-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  SERFILE-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  SERFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SERFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SERFILE-02-L4         TO THE-PRIOR-L4
               MOVE  SERFILE-02-L3         TO THE-PRIOR-L3
               MOVE  SERFILE-02-L2         TO THE-PRIOR-L2
               MOVE  SERFILE-02-L1         TO THE-PRIOR-L1
               SET SERFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       SERFILE-AHEAD-FLDSET SECTION.
       SERFILE-AHEAD-FLDSET-P.
           MOVE SERFILE-IO-AREA-2 (31:2)   TO VGR2 (1:2)
           MOVE SERFILE-IO-AREA-2 (31:5)   TO VGRN (1:5).
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (11:40) TO VGNAVN (1:40)
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRMAK-FLDSET SECTION.
       FIRMAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAK-IO-AREA (405:10) TO AVDN1 (1:10)
               MOVE FIRMAK-IO-AREA (416:10) TO AVDN2 (1:10)
               MOVE FIRMAK-IO-AREA (427:10) TO AVDN3 (1:10)
               MOVE FIRMAK-IO-AREA (438:10) TO AVDN4 (1:10)
               MOVE FIRMAK-IO-AREA (449:10) TO AVDN5 (1:10)
               MOVE FIRMAK-IO-AREA (460:10) TO AVDN6 (1:10)
               MOVE FIRMAK-IO-AREA (471:10) TO AVDN7 (1:10)
               MOVE FIRMAK-IO-AREA (482:10) TO AVDN8 (1:10)
               MOVE FIRMAK-IO-AREA (493:10) TO AVDN9 (1:10)
           END-EVALUATE.
 
       FIRMAK-IDSET SECTION.
       FIRMAK-IDSET-P.
           SET I-06                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L4)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE 'VGR.'                 TO OUTPUT-X-IO-AREA (2:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (6:1)
               MOVE 'BEHOLDNING'           TO OUTPUT-X-IO-AREA (11:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (21:1)
               MOVE 'ALF'                  TO OUTPUT-X-IO-AREA (22:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (25:1)
               MOVE 'ARTIKKELNUMMER'       TO OUTPUT-X-IO-AREA (33:14)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (47:1)
               MOVE 'ORDRENR'              TO OUTPUT-X-IO-AREA (52:7)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (59:1)
               MOVE 'DIR.R'                TO OUTPUT-X-IO-AREA (60:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (65:1)
               MOVE 'DATO '                TO OUTPUT-X-IO-AREA (70:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (75:1)
               MOVE 'KUNDE'                TO OUTPUT-X-IO-AREA (82:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (87:1)
               MOVE 'ORDREBEL'             TO OUTPUT-X-IO-AREA (95:8)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (103:1)
               MOVE 'REST.BEL'             TO OUTPUT-X-IO-AREA (111:8)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (119:1)
               MOVE 'REST.%'               TO OUTPUT-X-IO-AREA (122:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (128:1)
               WRITE OUTPUT-X-IO-AREA
           END-IF
           IF  (I-02 AND I-31 AND NOT-I-86)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE VGR                    TO OUTPUT-X-IO-AREA (1:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (6:1)
               MOVE BEHOLD                 TO XO-72YY9R
               MOVE XO-72YY9R              TO OUTPUT-X-IO-AREA (8:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (21:1)
               MOVE ALFA                   TO OUTPUT-X-IO-AREA (23:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (26:1)
               MOVE ARTNR                  TO OUTPUT-X-IO-AREA (27:20)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (47:1)
               MOVE ORDNR                  TO OUTPUT-X-IO-AREA (53:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (59:1)
               IF  (I-32)
                   MOVE 'JA'               TO OUTPUT-X-IO-AREA (63:2)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (65:1)
               MOVE REGDTO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO OUTPUT-X-IO-AREA (67:8)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (75:1)
               MOVE KUNDNR                 TO OUTPUT-X-IO-AREA (81:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (87:1)
               MOVE ORDBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO OUTPUT-X-IO-AREA (90:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (103:1)
               MOVE RESBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO OUTPUT-X-IO-AREA (106:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (119:1)
               MOVE LINEPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO OUTPUT-X-IO-AREA (122:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (128:1)
               WRITE OUTPUT-X-IO-AREA
           END-IF
           IF  (I-02 AND I-31 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VGR                    TO LISTE-IO-AREA (1:5)
               MOVE BEHOLD                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (8:13)
               MOVE ALFA                   TO LISTE-IO-AREA (23:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (27:20)
               MOVE ORDNR                  TO LISTE-IO-AREA (53:6)
               IF  (I-32)
                   MOVE 'JA'               TO LISTE-IO-AREA (63:2)
               END-IF
               MOVE REGDTO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (67:8)
               MOVE KUNDNR                 TO LISTE-IO-AREA (81:6)
               MOVE ORDBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (90:13)
               MOVE RESBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (106:13)
               MOVE LINEPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (122:6)
               INITIALIZE LINEPR
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND I-50 AND NOT-I-51)
           AND (NOT-I-86)
           OR  (I-L3 AND NOT-I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '    * * *    SERVICEPROS' TO LISTE-IO-AREA (32:24)
               MOVE 'ENT-RAPPORT PR. VGR MED' TO LISTE-IO-AREA (56:23)
               MOVE 'SPESIFIKASJON'        TO LISTE-IO-AREA (80:13)
               MOVE PMND                   TO LISTE-IO-AREA (95:9)
               MOVE PA-ELGR                TO LISTE-IO-AREA (106:4)
               MOVE '* * *'                TO LISTE-IO-AREA (114:5)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (121:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (125:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEHOLDN.'             TO LISTE-IO-AREA (13:8)
               MOVE 'ALFA'                 TO LISTE-IO-AREA (22:4)
               MOVE 'ARTIKKEL NUMMER     ' TO LISTE-IO-AREA (27:20)
               MOVE 'ORDRENR'              TO LISTE-IO-AREA (52:7)
               MOVE 'DIR.REG'              TO LISTE-IO-AREA (60:7)
               MOVE 'DATO'                 TO LISTE-IO-AREA (71:4)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (82:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '--------------'       TO LISTE-IO-AREA (73:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VARE       V A R E G R U' TO LISTE-IO-AREA (2:24)
               MOVE 'P P E N A V N'        TO LISTE-IO-AREA (27:13)
               MOVE 'ANT. VARELINJER  ANT.VAR' TO LISTE-IO-AREA (47:24)
               MOVE 'ELINJER  SERVICE    ORDR' TO LISTE-IO-AREA (71:24)
               MOVE 'EBELØP      RESTORDRE I' TO LISTE-IO-AREA (95:23)
               MOVE 'RESTORDRE'            TO LISTE-IO-AREA (121:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRP.'                 TO LISTE-IO-AREA (2:4)
               MOVE 'TOTALT        MED RESTOR' TO LISTE-IO-AREA (51:24)
               MOVE 'DRE  PROSENT      TOTALT' TO LISTE-IO-AREA (75:24)
               MOVE 'BELØP       I PROSENT' TO LISTE-IO-AREA (109:21)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '    * * *    SERVICEPROS' TO LISTE-IO-AREA (32:24)
               MOVE 'ENT-RAPPORT PR. VGR MED' TO LISTE-IO-AREA (56:23)
               MOVE 'SPESIFIKASJON'        TO LISTE-IO-AREA (80:13)
               MOVE PMND                   TO LISTE-IO-AREA (95:9)
               MOVE PA-ELGR                TO LISTE-IO-AREA (106:4)
               MOVE '* * *'                TO LISTE-IO-AREA (114:5)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (121:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (125:5)
               MOVE 01                     TO LISTE-BEFORE-SKIP
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEHOLDN.'             TO LISTE-IO-AREA (13:8)
               MOVE 'ALFA'                 TO LISTE-IO-AREA (22:4)
               MOVE 'ARTIKKEL NUMMER     ' TO LISTE-IO-AREA (27:20)
               MOVE 'ORDRENR'              TO LISTE-IO-AREA (52:7)
               MOVE 'DIR.REG'              TO LISTE-IO-AREA (60:7)
               MOVE 'DATO'                 TO LISTE-IO-AREA (71:4)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (82:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '--------------'       TO LISTE-IO-AREA (73:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VARE       V A R E G R U' TO LISTE-IO-AREA (2:24)
               MOVE 'P P E N A V N'        TO LISTE-IO-AREA (27:13)
               MOVE 'ANT. VARELINJER  ANT.VAR' TO LISTE-IO-AREA (47:24)
               MOVE 'ELINJER  SERVICE    ORDR' TO LISTE-IO-AREA (71:24)
               MOVE 'EBELØP      RESTORDRE I' TO LISTE-IO-AREA (95:23)
               MOVE 'RESTORDRE'            TO LISTE-IO-AREA (121:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRP.'                 TO LISTE-IO-AREA (2:4)
               MOVE 'TOTALT        MED RESTOR' TO LISTE-IO-AREA (51:24)
               MOVE 'DRE  PROSENT      TOTALT' TO LISTE-IO-AREA (75:24)
               MOVE 'BELØP       I PROSENT' TO LISTE-IO-AREA (109:21)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-52 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VGR                    TO LISTE-IO-AREA (1:5)
               IF  (NOT-I-11)
                   MOVE VGNAVN             TO LISTE-IO-AREA (7:40)
               END-IF
               MOVE VGRLIN                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (49:10)
               INITIALIZE VGRLIN
               MOVE VGRRLI                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (65:10)
               INITIALIZE VGRRLI
               MOVE VGRSPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (81:6)
               INITIALIZE VGRSPR
               MOVE VGRBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (90:13)
               INITIALIZE VGRBEL
               MOVE VGRREL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (106:13)
               INITIALIZE VGRREL
               MOVE VGRRPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (122:6)
               INITIALIZE VGRRPR
               MOVE ' *'                   TO LISTE-IO-AREA (131:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-51)
           AND (NOT-I-86)
           OR  (I-L3 AND NOT-I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'T O T A L'            TO LISTE-IO-AREA (2:9)
      *                     N50AVDNAV    44
               IF  (I-50)
                   MOVE '   H O V E D G R U P P E' TO LISTE-IO-AREA
                                                               (11:24)
               END-IF
               IF  (I-50)
                   MOVE VGR1               TO LISTE-IO-AREA (38:2)
               END-IF
               MOVE AVDLIN                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (49:10)
               INITIALIZE AVDLIN
               MOVE AVDRLI                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (65:10)
               INITIALIZE AVDRLI
               MOVE AVDSPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (81:6)
               INITIALIZE AVDSPR
               MOVE AVDBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (90:13)
               INITIALIZE AVDBEL
               MOVE AVDREL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (106:13)
               INITIALIZE AVDREL
               MOVE AVDRPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (122:6)
               INITIALIZE AVDRPR
               MOVE '**'                   TO LISTE-IO-AREA (131:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTLIN                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (49:10)
               INITIALIZE TOTLIN
               MOVE TOTRLI                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (65:10)
               INITIALIZE TOTRLI
               MOVE TOTSPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (81:6)
               INITIALIZE TOTSPR
               MOVE TOTBEL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (89:14)
               INITIALIZE TOTBEL
               MOVE TOTREL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (105:14)
               INITIALIZE TOTREL
               MOVE TOTRPR                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (122:6)
               INITIALIZE TOTRPR
               MOVE 'F I R M A  T O T A L' TO LISTE-IO-AREA (2:20)
      *******************************************
               MOVE 01                     TO LISTE-BEFORE-SKIP
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
           INITIALIZE PARFILE-DATA-FIELDS
           SET PARFILE-EOF-OFF             TO TRUE
           SET PARFILE-PROCESS             TO TRUE
           OPEN INPUT PARFILE
           SET SERFILE-LEVEL-INIT          TO TRUE
           SET SERFILE-AHEAD-EOF-OFF       TO TRUE
           SET SERFILE-AHEAD-READ-OFF      TO TRUE
           INITIALIZE SERFILE-DATA-FIELDS
           SET SERFILE-EOF-OFF             TO TRUE
           SET SERFILE-PROCESS             TO TRUE
           OPEN INPUT SERFILE
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           INITIALIZE FIRMAK-DATA-FIELDS
           OPEN INPUT FIRMAK
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTPUT-X.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARFILE
           CLOSE SERFILE
           CLOSE VAGRMAS
           CLOSE FIRMAK
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTPUT-X.
 
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
