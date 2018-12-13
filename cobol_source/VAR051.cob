       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR051R.
      **********************************************  Z-WIN-RPG2      *
      *****************************************************************
      *ERSTATTER UGYLDIGE KARAKTER (< X"40") I ALFAKODE, ARTIKKELNR,  *
      * OG VARENAVN I VARE.MASTER.                                    *
      *  28.12.2001 ESPEN LARSEN                                      *
      *  UPSI 1 = TEST UTEN OPPDATERING.                              *
      *  29.03.2005 TESTER OG FJERNER SEMIKOLON ;                     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR051.rpg
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
           SELECT ENRLIM
               ASSIGN TO ENRLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ENRLIM-STATUS.
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
       FD ENRLIM
               RECORD CONTAINS 80.
       01  ENRLIM-IO-AREA.
           05  ENRLIM-IO-AREA-X.
               10  ENRLIM-KEY1.
                   15  ENRLIM-KEY1N        PICTURE S9(10).
               10  FILLER                  PICTURE X(70).
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
           10  ENRLIM-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ENRLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-EOF-OFF          VALUE '0'.
               88  ENRLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-READ-OFF         VALUE '0'.
               88  ENRLIM-READ             VALUE '1'.
           05  ENRLIM-LOW-KEY              PICTURE X(10).
           05  ENRLIM-HIGH-KEY             PICTURE X(10).
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
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
           05  VAREMAS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  AA01                    PICTURE X(1).
               10  AA02                    PICTURE X(1).
               10  AA03                    PICTURE X(1).
               10  ARTNR                   PICTURE X(20).
               10  AN01                    PICTURE X(1).
               10  AN02                    PICTURE X(1).
               10  AN03                    PICTURE X(1).
               10  AN04                    PICTURE X(1).
               10  AN05                    PICTURE X(1).
               10  AN06                    PICTURE X(1).
               10  AN07                    PICTURE X(1).
               10  AN08                    PICTURE X(1).
               10  AN09                    PICTURE X(1).
               10  AN10                    PICTURE X(1).
               10  AN11                    PICTURE X(1).
               10  AN12                    PICTURE X(1).
               10  AN13                    PICTURE X(1).
               10  AN14                    PICTURE X(1).
               10  AN15                    PICTURE X(1).
               10  AN16                    PICTURE X(1).
               10  AN17                    PICTURE X(1).
               10  AN18                    PICTURE X(1).
               10  AN19                    PICTURE X(1).
               10  AN20                    PICTURE X(1).
               10  BETEG                   PICTURE X(30).
               10  VB01                    PICTURE X(1).
               10  VB02                    PICTURE X(1).
               10  VB03                    PICTURE X(1).
               10  VB04                    PICTURE X(1).
               10  VB05                    PICTURE X(1).
               10  VB06                    PICTURE X(1).
               10  VB07                    PICTURE X(1).
               10  VB08                    PICTURE X(1).
               10  VB09                    PICTURE X(1).
               10  VB10                    PICTURE X(1).
               10  VB11                    PICTURE X(1).
               10  VB12                    PICTURE X(1).
               10  VB13                    PICTURE X(1).
               10  VB14                    PICTURE X(1).
               10  VB15                    PICTURE X(1).
               10  VB16                    PICTURE X(1).
               10  VB17                    PICTURE X(1).
               10  VB18                    PICTURE X(1).
               10  VB19                    PICTURE X(1).
               10  VB20                    PICTURE X(1).
               10  VB21                    PICTURE X(1).
               10  VB22                    PICTURE X(1).
               10  VB23                    PICTURE X(1).
               10  VB24                    PICTURE X(1).
               10  VB25                    PICTURE X(1).
               10  VB26                    PICTURE X(1).
               10  VB27                    PICTURE X(1).
               10  VB28                    PICTURE X(1).
               10  VB29                    PICTURE X(1).
               10  VB30                    PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  ANTSEL-IO.
                   15  ANTSEL              PICTURE S9(8).
               10  ANTKAA-IO.
                   15  ANTKAA              PICTURE S9(8).
               10  ANTKAN-IO.
                   15  ANTKAN              PICTURE S9(8).
               10  ANTKVB-IO.
                   15  ANTKVB              PICTURE S9(8).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   SET VAREMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
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
           PERFORM HEADING-OVERFLOW
 
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
           SET NOT-I-15                    TO TRUE
           SET NOT-I-16                    TO TRUE
           SET NOT-I-17                    TO TRUE
           SET NOT-I-20                    TO TRUE
           SET NOT-I-14                    TO TRUE
      *****************************************************************
      * TEST ALFAKODE.                                                *
      * LAVERE ENN BLANK OG  SEMIKOLON SKAL SETTES TIL BLANK.         *
      *****************************************************************
           SET NOT-I-11                    TO TRUE
           IF  AA01 < ' '
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  AA02 < ' '
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  AA03 < ' '
               SET I-13                    TO TRUE
           END-IF
           IF  (NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  AA01 = ';'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  AA01 = ';'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  AA01 = ';'
                   SET I-13                TO TRUE
               END-IF
      *****************************************************************
      * TEST ARTIKKELNR.                                              *
      * LAVERE ENN BLANK OG  SEMIKOLON SKAL SETTES TIL BLANK.         *
      *****************************************************************
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  AN01 < ' '
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  AN02 < ' '
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  AN03 < ' '
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  AN04 < ' '
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  AN05 < ' '
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  AN06 < ' '
               SET I-26                    TO TRUE
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  AN07 < ' '
               SET I-27                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  AN08 < ' '
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  AN09 < ' '
               SET I-29                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  AN10 < ' '
               SET I-30                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  AN11 < ' '
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  AN12 < ' '
               SET I-32                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  AN13 < ' '
               SET I-33                    TO TRUE
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  AN14 < ' '
               SET I-34                    TO TRUE
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  AN15 < ' '
               SET I-35                    TO TRUE
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  AN16 < ' '
               SET I-36                    TO TRUE
           END-IF
           SET NOT-I-37                    TO TRUE
           IF  AN17 < ' '
               SET I-37                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  AN18 < ' '
               SET I-38                    TO TRUE
           END-IF
           SET NOT-I-39                    TO TRUE
           IF  AN19 < ' '
               SET I-39                    TO TRUE
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  AN20 < ' '
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  AN01 = ';'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-22)
               SET NOT-I-22                TO TRUE
               IF  AN02 = ';'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-23)
               SET NOT-I-23                TO TRUE
               IF  AN03 = ';'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-24)
               SET NOT-I-24                TO TRUE
               IF  AN04 = ';'
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  AN05 = ';'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-26)
               SET NOT-I-26                TO TRUE
               IF  AN06 = ';'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-27)
               SET NOT-I-27                TO TRUE
               IF  AN07 = ';'
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  AN08 = ';'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-29)
               SET NOT-I-29                TO TRUE
               IF  AN09 = ';'
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  AN10 = ';'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  AN11 = ';'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  AN12 = ';'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  AN13 = ';'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  AN14 = ';'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  AN15 = ';'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  AN16 = ';'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-37)
               SET NOT-I-37                TO TRUE
               IF  AN17 = ';'
                   SET I-37                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-38)
               SET NOT-I-38                TO TRUE
               IF  AN18 = ';'
                   SET I-38                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-39)
               SET NOT-I-39                TO TRUE
               IF  AN19 = ';'
                   SET I-39                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  AN20 = ';'
                   SET I-40                TO TRUE
               END-IF
      *****************************************************************
      * TEST PÅ VARENAVN.                                             *
      * LAVERE ENN BLANK OG  SEMIKOLON SKAL SETTES TIL BLANK.         *
      *****************************************************************
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  VB01 < ' '
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  VB02 < ' '
               SET I-42                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  VB03 < ' '
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  VB04 < ' '
               SET I-44                    TO TRUE
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  VB05 < ' '
               SET I-45                    TO TRUE
           END-IF
           SET NOT-I-46                    TO TRUE
           IF  VB06 < ' '
               SET I-46                    TO TRUE
           END-IF
           SET NOT-I-47                    TO TRUE
           IF  VB07 < ' '
               SET I-47                    TO TRUE
           END-IF
           SET NOT-I-48                    TO TRUE
           IF  VB08 < ' '
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-49                    TO TRUE
           IF  VB09 < ' '
               SET I-49                    TO TRUE
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  VB10 < ' '
               SET I-50                    TO TRUE
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  VB11 < ' '
               SET I-51                    TO TRUE
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  VB12 < ' '
               SET I-52                    TO TRUE
           END-IF
           SET NOT-I-53                    TO TRUE
           IF  VB13 < ' '
               SET I-53                    TO TRUE
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  VB14 < ' '
               SET I-54                    TO TRUE
           END-IF
           SET NOT-I-55                    TO TRUE
           IF  VB15 < ' '
               SET I-55                    TO TRUE
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  VB16 < ' '
               SET I-56                    TO TRUE
           END-IF
           SET NOT-I-57                    TO TRUE
           IF  VB17 < ' '
               SET I-57                    TO TRUE
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  VB18 < ' '
               SET I-58                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  VB19 < ' '
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-60                    TO TRUE
           IF  VB20 < ' '
               SET I-60                    TO TRUE
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  VB21 < ' '
               SET I-61                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  VB22 < ' '
               SET I-62                    TO TRUE
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  VB23 < ' '
               SET I-63                    TO TRUE
           END-IF
           SET NOT-I-64                    TO TRUE
           IF  VB24 < ' '
               SET I-64                    TO TRUE
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  VB25 < ' '
               SET I-65                    TO TRUE
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  VB26 < ' '
               SET I-66                    TO TRUE
           END-IF
           SET NOT-I-67                    TO TRUE
           IF  VB27 < ' '
               SET I-67                    TO TRUE
           END-IF
           SET NOT-I-68                    TO TRUE
           IF  VB28 < ' '
               SET I-68                    TO TRUE
           END-IF
           SET NOT-I-69                    TO TRUE
           IF  VB29 < ' '
               SET I-69                    TO TRUE
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  VB30 < ' '
               SET I-70                    TO TRUE
           END-IF
           IF  (NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  VB01 = ';'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VB02 = ';'
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-43)
               SET NOT-I-43                TO TRUE
               IF  VB03 = ';'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VB04 = ';'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VB05 = ';'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  VB06 = ';'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-47)
               SET NOT-I-47                TO TRUE
               IF  VB07 = ';'
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-48)
               SET NOT-I-48                TO TRUE
               IF  VB08 = ';'
                   SET I-48                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-49)
               SET NOT-I-49                TO TRUE
               IF  VB09 = ';'
                   SET I-49                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  VB10 = ';'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  VB11 = ';'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-52)
               SET NOT-I-52                TO TRUE
               IF  VB12 = ';'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-53)
               SET NOT-I-53                TO TRUE
               IF  VB13 = ';'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-54)
               SET NOT-I-54                TO TRUE
               IF  VB14 = ';'
                   SET I-54                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-55)
               SET NOT-I-55                TO TRUE
               IF  VB15 = ';'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-56)
               SET NOT-I-56                TO TRUE
               IF  VB16 = ';'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-57)
               SET NOT-I-57                TO TRUE
               IF  VB17 = ';'
                   SET I-57                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  VB18 = ';'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  VB19 = ';'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-60)
               SET NOT-I-60                TO TRUE
               IF  VB20 = ';'
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VB21 = ';'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  VB22 = ';'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-63)
               SET NOT-I-63                TO TRUE
               IF  VB23 = ';'
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  VB24 = ';'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-65)
               SET NOT-I-65                TO TRUE
               IF  VB25 = ';'
                   SET I-65                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-66)
               SET NOT-I-66                TO TRUE
               IF  VB26 = ';'
                   SET I-66                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-67)
               SET NOT-I-67                TO TRUE
               IF  VB27 = ';'
                   SET I-67                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-68)
               SET NOT-I-68                TO TRUE
               IF  VB28 = ';'
                   SET I-68                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-69)
               SET NOT-I-69                TO TRUE
               IF  VB29 = ';'
                   SET I-69                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  VB30 = ';'
                   SET I-70                TO TRUE
               END-IF
      *****************************************************************
      *                                                               *
      *****************************************************************
           END-IF
           IF  (NOT-I-11 AND NOT-I-12 AND NOT-I-13)
               GO TO TESTAN-T
           END-IF
           SET I-14                        TO TRUE
           SET I-20                        TO TRUE.
 
       TESTAN-T.
           IF  (NOT-I-21 AND NOT-I-22 AND NOT-I-23)
               AND (NOT-I-24 AND NOT-I-25 AND NOT-I-26)
               AND (NOT-I-27 AND NOT-I-28 AND NOT-I-29)
               AND (NOT-I-30 AND NOT-I-31 AND NOT-I-32)
               AND (NOT-I-33 AND NOT-I-34 AND NOT-I-35)
               AND (NOT-I-36 AND NOT-I-37 AND NOT-I-38)
               AND (NOT-I-39 AND NOT-I-40)
               GO TO TESTVB-T
           END-IF
           SET I-15                        TO TRUE
           SET I-20                        TO TRUE.
 
       TESTVB-T.
           IF  (NOT-I-41 AND NOT-I-42 AND NOT-I-43)
               AND (NOT-I-44 AND NOT-I-45 AND NOT-I-46)
               AND (NOT-I-47 AND NOT-I-48 AND NOT-I-49)
               AND (NOT-I-50 AND NOT-I-51 AND NOT-I-52)
               AND (NOT-I-53 AND NOT-I-54 AND NOT-I-55)
               AND (NOT-I-56 AND NOT-I-57 AND NOT-I-58)
               AND (NOT-I-59 AND NOT-I-60)
               SET I-17                    TO TRUE
           END-IF
           IF  (NOT-I-61 AND NOT-I-62 AND NOT-I-63)
               AND (NOT-I-64 AND NOT-I-65 AND NOT-I-66)
               AND (NOT-I-67 AND NOT-I-68 AND NOT-I-69)
               AND (NOT-I-70 AND I-17)
               GO TO TESTXX-T
           END-IF
           SET I-16                        TO TRUE
           SET I-20                        TO TRUE.
 
       TESTXX-T.
           IF  (I-01)
               ADD 1                       TO ANTSEL
           END-IF
           IF  (NOT-I-20)
               GO TO END-X-T
           END-IF
           IF  (I-01 AND I-14)
               ADD 1                       TO ANTKAA
           END-IF
           IF  (I-01 AND I-15)
               ADD 1                       TO ANTKAN
           END-IF
           IF  (I-01 AND I-16)
               ADD 1                       TO ANTKVB
           END-IF
           IF  (I-01 AND I-20)
               ADD 1                       TO ANTKOR
           END-IF.
 
       END-X-T.
           CONTINUE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ENRLIM-READ-OFF
                    OR ENRLIM-EOF
                   IF  ENRLIM-READ
                       SET ENRLIM-READ-OFF TO TRUE
                       READ ENRLIM
                       AT END
                           SET ENRLIM-EOF  TO TRUE
                           SET VAREMAS-EOF TO TRUE
                       NOT AT END
                           MOVE ENRLIM-IO-AREA (1:4) TO VAREMAS-KEY1
                       END-READ
                   END-IF
                   IF  ENRLIM-EOF-OFF
                   AND ENRLIM-READ-OFF
                       READ VAREMAS
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (13:1) TO AA01 (1:1)
               MOVE VAREMAS-IO-AREA (14:1) TO AA02 (1:1)
               MOVE VAREMAS-IO-AREA (15:1) TO AA03 (1:1)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (16:1) TO AN01 (1:1)
               MOVE VAREMAS-IO-AREA (17:1) TO AN02 (1:1)
               MOVE VAREMAS-IO-AREA (18:1) TO AN03 (1:1)
               MOVE VAREMAS-IO-AREA (19:1) TO AN04 (1:1)
               MOVE VAREMAS-IO-AREA (20:1) TO AN05 (1:1)
               MOVE VAREMAS-IO-AREA (21:1) TO AN06 (1:1)
               MOVE VAREMAS-IO-AREA (22:1) TO AN07 (1:1)
               MOVE VAREMAS-IO-AREA (23:1) TO AN08 (1:1)
               MOVE VAREMAS-IO-AREA (24:1) TO AN09 (1:1)
               MOVE VAREMAS-IO-AREA (25:1) TO AN10 (1:1)
               MOVE VAREMAS-IO-AREA (26:1) TO AN11 (1:1)
               MOVE VAREMAS-IO-AREA (27:1) TO AN12 (1:1)
               MOVE VAREMAS-IO-AREA (28:1) TO AN13 (1:1)
               MOVE VAREMAS-IO-AREA (29:1) TO AN14 (1:1)
               MOVE VAREMAS-IO-AREA (30:1) TO AN15 (1:1)
               MOVE VAREMAS-IO-AREA (31:1) TO AN16 (1:1)
               MOVE VAREMAS-IO-AREA (32:1) TO AN17 (1:1)
               MOVE VAREMAS-IO-AREA (33:1) TO AN18 (1:1)
               MOVE VAREMAS-IO-AREA (34:1) TO AN19 (1:1)
               MOVE VAREMAS-IO-AREA (35:1) TO AN20 (1:1)
               MOVE VAREMAS-IO-AREA (36:30) TO BETEG (1:30)
               MOVE VAREMAS-IO-AREA (36:1) TO VB01 (1:1)
               MOVE VAREMAS-IO-AREA (37:1) TO VB02 (1:1)
               MOVE VAREMAS-IO-AREA (38:1) TO VB03 (1:1)
               MOVE VAREMAS-IO-AREA (39:1) TO VB04 (1:1)
               MOVE VAREMAS-IO-AREA (40:1) TO VB05 (1:1)
               MOVE VAREMAS-IO-AREA (41:1) TO VB06 (1:1)
               MOVE VAREMAS-IO-AREA (42:1) TO VB07 (1:1)
               MOVE VAREMAS-IO-AREA (43:1) TO VB08 (1:1)
               MOVE VAREMAS-IO-AREA (44:1) TO VB09 (1:1)
               MOVE VAREMAS-IO-AREA (45:1) TO VB10 (1:1)
               MOVE VAREMAS-IO-AREA (46:1) TO VB11 (1:1)
               MOVE VAREMAS-IO-AREA (47:1) TO VB12 (1:1)
               MOVE VAREMAS-IO-AREA (48:1) TO VB13 (1:1)
               MOVE VAREMAS-IO-AREA (49:1) TO VB14 (1:1)
               MOVE VAREMAS-IO-AREA (50:1) TO VB15 (1:1)
               MOVE VAREMAS-IO-AREA (51:1) TO VB16 (1:1)
               MOVE VAREMAS-IO-AREA (52:1) TO VB17 (1:1)
               MOVE VAREMAS-IO-AREA (53:1) TO VB18 (1:1)
               MOVE VAREMAS-IO-AREA (54:1) TO VB19 (1:1)
               MOVE VAREMAS-IO-AREA (55:1) TO VB20 (1:1)
               MOVE VAREMAS-IO-AREA (56:1) TO VB21 (1:1)
               MOVE VAREMAS-IO-AREA (57:1) TO VB22 (1:1)
               MOVE VAREMAS-IO-AREA (58:1) TO VB23 (1:1)
               MOVE VAREMAS-IO-AREA (59:1) TO VB24 (1:1)
               MOVE VAREMAS-IO-AREA (60:1) TO VB25 (1:1)
               MOVE VAREMAS-IO-AREA (61:1) TO VB26 (1:1)
               MOVE VAREMAS-IO-AREA (62:1) TO VB27 (1:1)
               MOVE VAREMAS-IO-AREA (63:1) TO VB28 (1:1)
               MOVE VAREMAS-IO-AREA (64:1) TO VB29 (1:1)
               MOVE VAREMAS-IO-AREA (65:1) TO VB30 (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-20 AND NOT-I-U1)
               IF  (I-11)
                   MOVE ' '                TO VAREMAS-IO-AREA (13:1)
               END-IF
               IF  (I-12)
                   MOVE ' '                TO VAREMAS-IO-AREA (14:1)
               END-IF
               IF  (I-13)
                   MOVE ' '                TO VAREMAS-IO-AREA (15:1)
               END-IF
               IF  (I-21)
                   MOVE ' '                TO VAREMAS-IO-AREA (16:1)
               END-IF
               IF  (I-22)
                   MOVE ' '                TO VAREMAS-IO-AREA (17:1)
               END-IF
               IF  (I-23)
                   MOVE ' '                TO VAREMAS-IO-AREA (18:1)
               END-IF
               IF  (I-24)
                   MOVE ' '                TO VAREMAS-IO-AREA (19:1)
               END-IF
               IF  (I-25)
                   MOVE ' '                TO VAREMAS-IO-AREA (20:1)
               END-IF
               IF  (I-26)
                   MOVE ' '                TO VAREMAS-IO-AREA (21:1)
               END-IF
               IF  (I-27)
                   MOVE ' '                TO VAREMAS-IO-AREA (22:1)
               END-IF
               IF  (I-28)
                   MOVE ' '                TO VAREMAS-IO-AREA (23:1)
               END-IF
               IF  (I-29)
                   MOVE ' '                TO VAREMAS-IO-AREA (24:1)
               END-IF
               IF  (I-30)
                   MOVE ' '                TO VAREMAS-IO-AREA (25:1)
               END-IF
               IF  (I-31)
                   MOVE ' '                TO VAREMAS-IO-AREA (26:1)
               END-IF
               IF  (I-32)
                   MOVE ' '                TO VAREMAS-IO-AREA (27:1)
               END-IF
               IF  (I-33)
                   MOVE ' '                TO VAREMAS-IO-AREA (28:1)
               END-IF
               IF  (I-34)
                   MOVE ' '                TO VAREMAS-IO-AREA (29:1)
               END-IF
               IF  (I-35)
                   MOVE ' '                TO VAREMAS-IO-AREA (30:1)
               END-IF
               IF  (I-36)
                   MOVE ' '                TO VAREMAS-IO-AREA (31:1)
               END-IF
               IF  (I-37)
                   MOVE ' '                TO VAREMAS-IO-AREA (32:1)
               END-IF
               IF  (I-38)
                   MOVE ' '                TO VAREMAS-IO-AREA (33:1)
               END-IF
               IF  (I-39)
                   MOVE ' '                TO VAREMAS-IO-AREA (34:1)
               END-IF
               IF  (I-40)
                   MOVE ' '                TO VAREMAS-IO-AREA (35:1)
               END-IF
               IF  (I-41)
                   MOVE ' '                TO VAREMAS-IO-AREA (36:1)
               END-IF
               IF  (I-42)
                   MOVE ' '                TO VAREMAS-IO-AREA (37:1)
               END-IF
               IF  (I-43)
                   MOVE ' '                TO VAREMAS-IO-AREA (38:1)
               END-IF
               IF  (I-44)
                   MOVE ' '                TO VAREMAS-IO-AREA (39:1)
               END-IF
               IF  (I-45)
                   MOVE ' '                TO VAREMAS-IO-AREA (40:1)
               END-IF
               IF  (I-46)
                   MOVE ' '                TO VAREMAS-IO-AREA (41:1)
               END-IF
               IF  (I-47)
                   MOVE ' '                TO VAREMAS-IO-AREA (42:1)
               END-IF
               IF  (I-48)
                   MOVE ' '                TO VAREMAS-IO-AREA (43:1)
               END-IF
               IF  (I-49)
                   MOVE ' '                TO VAREMAS-IO-AREA (44:1)
               END-IF
               IF  (I-50)
                   MOVE ' '                TO VAREMAS-IO-AREA (45:1)
               END-IF
               IF  (I-51)
                   MOVE ' '                TO VAREMAS-IO-AREA (46:1)
               END-IF
               IF  (I-52)
                   MOVE ' '                TO VAREMAS-IO-AREA (47:1)
               END-IF
               IF  (I-53)
                   MOVE ' '                TO VAREMAS-IO-AREA (48:1)
               END-IF
               IF  (I-54)
                   MOVE ' '                TO VAREMAS-IO-AREA (49:1)
               END-IF
               IF  (I-55)
                   MOVE ' '                TO VAREMAS-IO-AREA (50:1)
               END-IF
               IF  (I-56)
                   MOVE ' '                TO VAREMAS-IO-AREA (51:1)
               END-IF
               IF  (I-57)
                   MOVE ' '                TO VAREMAS-IO-AREA (52:1)
               END-IF
               IF  (I-58)
                   MOVE ' '                TO VAREMAS-IO-AREA (53:1)
               END-IF
               IF  (I-59)
                   MOVE ' '                TO VAREMAS-IO-AREA (54:1)
               END-IF
               IF  (I-60)
                   MOVE ' '                TO VAREMAS-IO-AREA (55:1)
               END-IF
               IF  (I-61)
                   MOVE ' '                TO VAREMAS-IO-AREA (56:1)
               END-IF
               IF  (I-62)
                   MOVE ' '                TO VAREMAS-IO-AREA (57:1)
               END-IF
               IF  (I-63)
                   MOVE ' '                TO VAREMAS-IO-AREA (58:1)
               END-IF
               IF  (I-64)
                   MOVE ' '                TO VAREMAS-IO-AREA (59:1)
               END-IF
               IF  (I-65)
                   MOVE ' '                TO VAREMAS-IO-AREA (60:1)
               END-IF
               IF  (I-66)
                   MOVE ' '                TO VAREMAS-IO-AREA (61:1)
               END-IF
               IF  (I-67)
                   MOVE ' '                TO VAREMAS-IO-AREA (62:1)
               END-IF
               IF  (I-68)
                   MOVE ' '                TO VAREMAS-IO-AREA (63:1)
               END-IF
               IF  (I-69)
                   MOVE ' '                TO VAREMAS-IO-AREA (64:1)
               END-IF
               IF  (I-70)
                   MOVE ' '                TO VAREMAS-IO-AREA (65:1)
               END-IF
               REWRITE VAREMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAREMAS'
               END-REWRITE
           END-IF
           IF  (I-20)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE ALFA                   TO LISTE-IO-AREA (5:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (13:20)
               MOVE BETEG                  TO LISTE-IO-AREA (36:30)
               IF  (I-14)
                   MOVE 'FEIL ALFAKODE'    TO LISTE-IO-AREA (68:13)
               END-IF
               IF  (I-15)
                   MOVE 'FEIL ART.NR. '    TO LISTE-IO-AREA (68:13)
               END-IF
               IF  (I-16)
                   MOVE 'FEIL VAREBET.'    TO LISTE-IO-AREA (68:13)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KORRIGERE'            TO LISTE-IO-AREA (2:9)
               MOVE 'ARTIKKELNR/VARE'      TO LISTE-IO-AREA (12:15)
               MOVE 'BETEGNELSE'           TO LISTE-IO-AREA (27:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (114:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (118:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR ALF ARTNUMMER'    TO LISTE-IO-AREA (1:17)
               MOVE 'BETEGNELSE    '       TO LISTE-IO-AREA (36:14)
               MOVE 'FEIL TYPE    '        TO LISTE-IO-AREA (68:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KORRIGERE'            TO LISTE-IO-AREA (2:9)
               MOVE 'ARTIKKELNR/VARE'      TO LISTE-IO-AREA (12:15)
               MOVE 'BETEGNELSE'           TO LISTE-IO-AREA (27:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (114:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (118:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FNR ALF ARTNUMMER'    TO LISTE-IO-AREA (1:17)
               MOVE 'BETEGNELSE    '       TO LISTE-IO-AREA (36:14)
               MOVE 'FEIL TYPE    '        TO LISTE-IO-AREA (68:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL SELEKTERT =    ' TO LISTE-IO-AREA (3:22)
               MOVE ANTSEL                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (31:10)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL KORRIGERT =    ' TO LISTE-IO-AREA (3:22)
               MOVE ANTKOR                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (31:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFAK. KORRIGERT =    ' TO LISTE-IO-AREA (3:22)
               MOVE ANTKAA                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (31:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ARTNR. KORRIGERT =    ' TO LISTE-IO-AREA (3:22)
               MOVE ANTKAN                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (31:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VAREB. KORRIGERT =    ' TO LISTE-IO-AREA (3:22)
               MOVE ANTKVB                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (31:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'JOB = KORRVARE  DATO =' TO LISTE-IO-AREA (3:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
               IF  (I-U1)
                   MOVE '** KUN TEST **'   TO LISTE-IO-AREA (35:14)
               END-IF
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
           SET ENRLIM-EOF-OFF              TO TRUE
           SET ENRLIM-READ                 TO TRUE
           OPEN INPUT ENRLIM
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           OPEN I-O VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ENRLIM
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
