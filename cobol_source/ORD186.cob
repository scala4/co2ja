       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD186R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD186.rpg
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
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
           SELECT INNFIL2
               ASSIGN TO UT-S-INNFIL2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL2-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
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
       FD INNFIL
               BLOCK CONTAINS 1000
               RECORD CONTAINS 500.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(500).
       FD INNFIL2
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  INNFIL2-IO-AREA.
           05  INNFIL2-IO-AREA-X           PICTURE X(100).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      *UTPUT  O   F 500 250            DISK40 SYS012S
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD OUTPUT-X
               RECORD CONTAINS 500.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(500).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  INNFIL2-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  INNFIL-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL2-EOF-OFF         VALUE '0'.
               88  INNFIL2-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL2-READ-OFF        VALUE '0'.
               88  INNFIL2-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL2-PROCESS-OFF     VALUE '0'.
               88  INNFIL2-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL2-LEVEL-INIT-OFF  VALUE '0'.
               88  INNFIL2-LEVEL-INIT      VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L3.
                   15  INNFIL-01-L3-FIRMA  PICTURE X(3).
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-KNR    PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-VGR    PICTURE X(5).
           05  INNFIL-DATA-FIELDS.
               10  REC1                    PICTURE X(250).
               10  REC2                    PICTURE X(250).
               10  KNR                     PICTURE X(6).
               10  VGR                     PICTURE X(5).
               10  FIRMA                   PICTURE X(3).
      *                                      55  590ANTDM1
      *                                      61  630ANTDM2
               10  ANTAK1-IO.
                   15  ANTAK1              PICTURE S9(5).
               10  ANTAK2-IO.
                   15  ANTAK2              PICTURE S9(3).
      *                                      96 1000BELDM1
      *                                     102 1040BELDM2
      *                                     111 1150BELMF1
      *                                     117 1190BELMF2
               10  BELAK1-IO.
                   15  BELAK1              PICTURE S9(5).
               10  BELAK2-IO.
                   15  BELAK2              PICTURE S9(3).
      *                                     142 1460BELAF1
      *                                     148 1500BELAF2
           05  INNFIL-MP                   PICTURE X(14).
           05  INNFIL-MC                   PICTURE X(14).
           05  INNFIL-M-01             REDEFINES INNFIL-MC.
               10  INNFIL-M-01-M3.
                   15  INNFIL-M-01-M3-FIRMA-G.
                       20  INNFIL-M-01-M3-FIRMA PICTURE X(3).
               10  INNFIL-M-01-M2.
                   15  INNFIL-M-01-M2-KNR-G.
                       20  INNFIL-M-01-M2-KNR PICTURE X(6).
               10  INNFIL-M-01-M1.
                   15  INNFIL-M-01-M1-VGR-G.
                       20  INNFIL-M-01-M1-VGR PICTURE X(5).
           05  INNFIL2-LEVEL-02.
               10  INNFIL2-02-L3.
                   15  INNFIL2-02-L3-FIRMA PICTURE X(3).
               10  INNFIL2-02-L2.
                   15  INNFIL2-02-L2-KNR   PICTURE X(6).
               10  INNFIL2-02-L1.
                   15  INNFIL2-02-L1-VGR   PICTURE X(5).
           05  INNFIL2-DATA-FIELDS.
      *                                      15  232ANTVGR
               10  ANTVGR-IO.
                   15  ANTVGR              PICTURE S9(7).
      *                                      24  322SUMVGR
               10  SUMVGR-IO.
                   15  SUMVGR              PICTURE S9(7).
               10  TGNANT                  PICTURE X(1).
               10  TGNSUM                  PICTURE X(1).
           05  INNFIL2-MP                  PICTURE X(14).
           05  INNFIL2-MC                  PICTURE X(14).
           05  INNFIL2-M-02            REDEFINES INNFIL2-MC.
               10  INNFIL2-M-02-M3.
                   15  INNFIL2-M-02-M3-FIRMA-G.
                       20  INNFIL2-M-02-M3-FIRMA PICTURE X(3).
               10  INNFIL2-M-02-M2.
                   15  INNFIL2-M-02-M2-KNR-G.
                       20  INNFIL2-M-02-M2-KNR PICTURE X(6).
               10  INNFIL2-M-02-M1.
                   15  INNFIL2-M-02-M1-VGR-G.
                       20  INNFIL2-M-02-M1-VGR PICTURE X(5).
           05  VAGRMAS-DATA-FIELDS.
               10  VTEKST                  PICTURE X(30).
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN1                   PICTURE X(30).
               10  KAT-IO.
                   15  KAT                 PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  HDIST                   PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(6).
               10  ANTMR2-IO.
                   15  ANTMR2              PICTURE S9(6).
               10  ANTMR3-IO.
                   15  ANTMR3              PICTURE S9(6).
               10  SKEY5                   PICTURE X(5).
               10  SKEY                    PICTURE X(8).
               10  VGRKEY                  PICTURE X(8).
               10  KUNKEY                  PICTURE X(9).
               10  ANTAK-IO.
                   15  ANTAK               PICTURE S9(8).
               10  BELAK-IO.
                   15  BELAK               PICTURE S9(8).
               10  ANTDMN-IO.
                   15  ANTDMN              PICTURE S9(8).
               10  BELDMN-IO.
                   15  BELDMN              PICTURE S9(8).
               10  NULL8-IO.
                   15  NULL8               PICTURE S9(8).
               10  ANTAKM-IO.
                   15  ANTAKM              PICTURE S9(8).
               10  BELAKM-IO.
                   15  BELAKM              PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  EDIT-NULL8              PICTURE ZZZZZ.ZZ9.
               10  EDIT-ANTDMN             PICTURE ZZZZZ.ZZ9.
               10  EDIT-ANTAKM             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BELDMN             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BELAKM             PICTURE ZZZZZ.ZZ9.
               10  XO-30D                  PICTURE S9(3).
               10  XO-30U                  PICTURE 9(3).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   PERFORM INNFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  INNFIL2-PROCESS
               SET INNFIL2-PROCESS-OFF     TO TRUE
               SET INNFIL2-READ            TO TRUE
           END-IF
 
           IF  INNFIL2-READ
               PERFORM INNFIL2-GET
               SET INNFIL2-READ-OFF        TO TRUE
               IF  NOT INNFIL2-EOF
                   PERFORM INNFIL2-MATCH-SET
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
           END-IF
 
           IF  INNFIL2-PROCESS
               PERFORM INNFIL2-IDSET
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-CHK-LEVEL
           END-IF
 
           IF  INNFIL2-PROCESS
               PERFORM INNFIL2-CHK-LEVEL
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
           END-IF
 
           IF  INNFIL2-PROCESS
               PERFORM INNFIL2-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFIL-PROCESS
           OR  INNFIL2-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
           IF  (I-02 AND I-MR)
               SET NOT-I-22                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-41                TO TRUE
               SET NOT-I-42                TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTMR
           END-IF
           IF  (I-01 AND NOT-I-MR)
               ADD 1                       TO ANTMR2
           END-IF
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTMR3
      *
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  VGR = '00000'
               SET I-15                    TO TRUE
           END-IF
           IF  (I-15)
               GO TO SLUTT-T
      *****************************************************
      *  RUTINE FOR OPPSLAG PÅ VAREGRUPPEMASTER.          *
      *****************************************************
           END-IF
           MOVE FIRMA                      TO SKEY5 (1:3)
           MOVE '04'                       TO SKEY5 (4:2)
           MOVE SKEY5                      TO SKEY (1:5)
           MOVE FIRMA                      TO VGRKEY (1:3)
           MOVE VGR                        TO VGRKEY (4:5)
           MOVE VGRKEY                     TO VAGRMAS-KEY1
           READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM VAGRMAS-FLDSET
               PERFORM VAGRMAS-IDSET
           END-READ
      *****************************************************
      *  RUTINE FOR OPPSLAG PÅ KUNDEMASTER.
      *****************************************************
           MOVE FIRMA                      TO KUNKEY (1:3)
           MOVE KNR                        TO KUNKEY (4:6)
           MOVE KUNKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-12                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-12                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
      *
           IF  (I-01)
               MOVE ANTAK1                 TO ANTAK (1:5)
               MOVE ANTAK2                 TO ANTAK-IO (6:3)
               MOVE BELAK1                 TO BELAK (1:5)
               MOVE BELAK2                 TO BELAK-IO (6:3)
      *
      *****************************************************************
           END-IF
           ADD ANTVGR TO ZERO          GIVING ANTDMN
           ADD SUMVGR TO ZERO          GIVING BELDMN
           MOVE 0                          TO NULL8
           IF  (I-02 AND I-MR)
               SET NOT-I-22                TO TRUE
               IF  TGNANT = '-'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  TGNSUM = '-'
                   SET I-23                TO TRUE
               END-IF
      *****************************************************************
      * SUMMERER PR VAREGRUPPE                                        *
      *****************************************************************
           END-IF
           IF  (I-01 AND NOT-I-MR)
               ADD ANTAK TO ZERO       GIVING ANTAKM
               ADD BELAK TO ZERO       GIVING BELAKM
           END-IF
           IF  (I-02 AND I-MR)
               ADD ANTVGR TO ANTAK     GIVING ANTAKM
               ADD SUMVGR TO BELAK     GIVING BELAKM
           END-IF
           IF  (I-02 AND I-MR AND I-22)
               SUBTRACT ANTVGR FROM ANTAK GIVING ANTAKM
           END-IF
           IF  (I-02 AND I-MR AND I-23)
               SUBTRACT SUMVGR FROM BELAK GIVING BELAKM
      *  16      ANTVGR    SUB  ANTLEV    ANTVGR  92
      *  16      SUMVGR    SUB  SUMBEL    SUMVGR  92
      * SJEKKER FOR NEGATIVE VERDIER
           END-IF
           IF  (I-02 AND I-MR)
               SET NOT-I-42                TO TRUE
               IF  ANTAKM < 0
                   SET I-42                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  BELAKM < 0
                   SET I-41                TO TRUE
               END-IF
           END-IF
           SET I-10                        TO TRUE.
 
       SLUTT-T.
      *ISTE   D 1      01 10
      *                        FIRMA      3
      *                        KNR       10
      *                        VGR       16
      *                        ANTVGRX   28
      *                        SUMVGRX   40
           CONTINUE.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:250) TO REC1 (1:250)
               MOVE INNFIL-IO-AREA (251:250) TO REC2 (1:250)
               MOVE INNFIL-IO-AREA (9:6)   TO KNR (1:6)
               MOVE INNFIL-IO-AREA (16:5)  TO VGR (1:5)
               MOVE INNFIL-IO-AREA (198:3) TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (68:5)  TO ANTAK1-IO
               INSPECT ANTAK1-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (74:3)  TO ANTAK2-IO
               INSPECT ANTAK2-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (127:5) TO BELAK1-IO
               INSPECT BELAK1-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (133:3) TO BELAK2-IO
               INSPECT BELAK2-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (198:3) TO INNFIL-01-L3-FIRMA
               MOVE INNFIL-IO-AREA (9:6)   TO INNFIL-01-L2-KNR
               MOVE INNFIL-IO-AREA (16:5)  TO INNFIL-01-L1-VGR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L3          TO THE-PRIOR-L3
               MOVE  INNFIL-01-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       INNFIL-MATCH-SET SECTION.
       INNFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (198:3) TO INNFIL-M-01-M3-FIRMA
               MOVE INNFIL-IO-AREA (9:6)   TO INNFIL-M-01-M2-KNR
               MOVE INNFIL-IO-AREA (16:5)  TO INNFIL-M-01-M1-VGR
           END-EVALUATE.
 
       INNFIL2-GET SECTION.
       INNFIL2-GET-P.
           IF  INNFIL2-EOF-OFF
               READ INNFIL2
               AT END
                   SET INNFIL2-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL2-FLDSET SECTION.
       INNFIL2-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL2-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE INNFIL2-IO-AREA (4:6)  TO KNR (1:6)
               MOVE INNFIL2-IO-AREA (10:5) TO VGR (1:5)
               MOVE INNFIL2-IO-AREA (15:7) TO ANTVGR-IO
               INSPECT ANTVGR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL2-IO-AREA (24:7) TO SUMVGR-IO
               INSPECT SUMVGR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL2-IO-AREA (33:1) TO TGNANT (1:1)
               MOVE INNFIL2-IO-AREA (34:1) TO TGNSUM (1:1)
           END-EVALUATE.
 
       INNFIL2-IDSET SECTION.
       INNFIL2-IDSET-P.
           SET I-02                        TO TRUE.
 
       INNFIL2-CHK-LEVEL SECTION.
       INNFIL2-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL2-LEVEL-02
               MOVE INNFIL2-IO-AREA (1:3)  TO INNFIL2-02-L3-FIRMA
               MOVE INNFIL2-IO-AREA (4:6)  TO INNFIL2-02-L2-KNR
               MOVE INNFIL2-IO-AREA (10:5) TO INNFIL2-02-L1-VGR
               IF  INNFIL2-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL2-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL2-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL2-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL2-02-L3         TO THE-PRIOR-L3
               MOVE  INNFIL2-02-L2         TO THE-PRIOR-L2
               MOVE  INNFIL2-02-L1         TO THE-PRIOR-L1
               SET INNFIL2-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       INNFIL2-MATCH-SET SECTION.
       INNFIL2-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL2-IO-AREA (1:3)  TO INNFIL2-M-02-M3-FIRMA
               MOVE INNFIL2-IO-AREA (4:6)  TO INNFIL2-M-02-M2-KNR
               MOVE INNFIL2-IO-AREA (10:5) TO INNFIL2-M-02-M1-VGR
           END-EVALUATE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (11:30) TO VTEKST (1:30)
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (162:2) TO KAT-IO
               MOVE KUNDEMA-IO-AREA (185:3) TO HDIST (1:3)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
           IF  INNFIL-EOF
               MOVE HIGH-VALUES            TO INNFIL-MC
                                              INNFIL-MP
           END-IF
           IF  INNFIL2-EOF
               MOVE HIGH-VALUES            TO INNFIL2-MC
                                              INNFIL2-MP
           END-IF
           IF  INNFIL-MC < INNFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNFIL2-MC < INNFIL2-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INNFIL-MC < INNFIL2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNFIL-PROCESS      TO TRUE
                   MOVE INNFIL-MC          TO INNFIL-MP
                   IF  INNFIL-MC = INNFIL2-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNFIL2-MC < INNFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNFIL2-PROCESS     TO TRUE
                   MOVE INNFIL2-MC         TO INNFIL2-MP
                   IF  INNFIL2-MC = INNFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNFIL-MC = INNFIL2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNFIL-PROCESS      TO TRUE
                   MOVE INNFIL-MC          TO INNFIL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-MR AND I-10)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               IF  (NOT-I-12)
                   MOVE KAT                TO XO-30U
                   MOVE XO-30U (1:3)       TO OUTPUT-X-IO-AREA (1:3)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (4:1)
               IF  (NOT-I-12)
                   MOVE HDIST              TO OUTPUT-X-IO-AREA (5:3)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (8:1)
               MOVE KNR                    TO OUTPUT-X-IO-AREA (9:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (15:1)
               MOVE VGR                    TO OUTPUT-X-IO-AREA (16:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (21:1)
               IF  (NOT-I-11)
                   MOVE VTEKST             TO OUTPUT-X-IO-AREA (22:30)
               END-IF
               IF  (I-11)
                   MOVE 'VAREGRUPPE UKJENT' TO OUTPUT-X-IO-AREA (25:17)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (52:1)
      *                      40          53 "-"
               MOVE TGNANT                 TO OUTPUT-X-IO-AREA (53:1)
               MOVE ANTDMN                 TO EDIT-ANTDMN
               MOVE EDIT-ANTDMN            TO OUTPUT-X-IO-AREA (55:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (64:1)
      *                      40          65 "-"
               MOVE TGNANT                 TO OUTPUT-X-IO-AREA (65:1)
               MOVE ANTDMN                 TO EDIT-ANTDMN
               MOVE EDIT-ANTDMN            TO OUTPUT-X-IO-AREA (68:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (77:1)
      *                      37          78 "-"
      *                        ANTAKF B  89 "     . 0 "
               MOVE ';'                    TO OUTPUT-X-IO-AREA (90:1)
      *                      39          91 "-"
               MOVE TGNSUM                 TO OUTPUT-X-IO-AREA (91:1)
               MOVE BELDMN                 TO EDIT-BELDMN
               MOVE EDIT-BELDMN            TO OUTPUT-X-IO-AREA (96:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (105:1)
      *                      39         106 "-"
      *                        BELDF  B 119 "     . 0 "
               MOVE ';'                    TO OUTPUT-X-IO-AREA (120:1)
      *                      39         121 "-"
               MOVE TGNSUM                 TO OUTPUT-X-IO-AREA (121:1)
               MOVE BELDMN                 TO EDIT-BELDMN
               MOVE EDIT-BELDMN            TO OUTPUT-X-IO-AREA (127:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (136:1)
      *                      41         137 "-"
      *                        BELAKF B 150 "     . 0 "
               MOVE ';'                    TO OUTPUT-X-IO-AREA (151:1)
      *                      42         152 "-"
      *                        BELAV  B 165 "     . 0 "
      *                        NULL8    165 "     . 0 "
               MOVE ';'                    TO OUTPUT-X-IO-AREA (166:1)
               IF  (NOT-I-12)
                   MOVE NAVN1              TO OUTPUT-X-IO-AREA (167:30)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (197:1)
               MOVE FIRMA                  TO OUTPUT-X-IO-AREA (198:3)
               MOVE '_'                    TO OUTPUT-X-IO-AREA (201:1)
               MOVE KNR                    TO OUTPUT-X-IO-AREA (202:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (208:1)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (219:1)
               WRITE OUTPUT-X-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
      *                        FINAVN    30
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE '***  SALG I ANTALL OG' TO OUTPUT-X-IO-AREA (33:21)
               MOVE 'BELØP PR KUNDE OG'    TO OUTPUT-X-IO-AREA (55:17)
               MOVE 'VAREGRUPPE  ***'      TO OUTPUT-X-IO-AREA (73:15)
      *                        MNDNA     99
      *                        AAR      104
               WRITE OUTPUT-X-IO-AREA
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE 'KAT'                  TO OUTPUT-X-IO-AREA (1:3)
      *                      76           3 "SNR"
               MOVE ';'                    TO OUTPUT-X-IO-AREA (4:1)
               MOVE 'HND'                  TO OUTPUT-X-IO-AREA (5:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (8:1)
               MOVE 'KUNDE '               TO OUTPUT-X-IO-AREA (9:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (15:1)
               MOVE 'VGR. '                TO OUTPUT-X-IO-AREA (16:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (21:1)
               MOVE 'VAREGRUPPENAVN'       TO OUTPUT-X-IO-AREA (22:14)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (52:1)
               MOVE 'ANT.D.MND '           TO OUTPUT-X-IO-AREA (54:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (64:1)
               MOVE 'ANT.AKK.ÅR'           TO OUTPUT-X-IO-AREA (67:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (77:1)
               MOVE 'ANT.AKK.FJ'           TO OUTPUT-X-IO-AREA (80:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (90:1)
               MOVE 'BEL.DENNE MND'        TO OUTPUT-X-IO-AREA (92:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (105:1)
               MOVE 'BEL.D.MND.FJ.'        TO OUTPUT-X-IO-AREA (107:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (120:1)
               MOVE 'BEL.AKK.I ÅR '        TO OUTPUT-X-IO-AREA (123:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (136:1)
               MOVE 'BEL.AKK.I FJ.'        TO OUTPUT-X-IO-AREA (138:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (151:1)
               MOVE 'AVVIK BELØP  '        TO OUTPUT-X-IO-AREA (153:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (166:1)
               MOVE 'KUNDENAVN  '          TO OUTPUT-X-IO-AREA (167:11)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (197:1)
               MOVE 'FNR/KNR'              TO OUTPUT-X-IO-AREA (201:7)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (208:1)
               MOVE 'BEL I FJOR'           TO OUTPUT-X-IO-AREA (209:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (219:1)
               MOVE 'JAN FJOR '            TO OUTPUT-X-IO-AREA (221:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (230:1)
               MOVE 'FEB FJOR '            TO OUTPUT-X-IO-AREA (232:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (241:1)
               MOVE 'MAR FJOR '            TO OUTPUT-X-IO-AREA (243:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (252:1)
               MOVE 'APR FJOR '            TO OUTPUT-X-IO-AREA (254:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (263:1)
               MOVE 'MAI FJOR '            TO OUTPUT-X-IO-AREA (265:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (274:1)
               MOVE 'JUNI FJOR'            TO OUTPUT-X-IO-AREA (276:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (285:1)
               MOVE 'JULI FJOR'            TO OUTPUT-X-IO-AREA (287:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (296:1)
               MOVE 'AUG FJOR '            TO OUTPUT-X-IO-AREA (298:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (307:1)
               MOVE 'SEP FJOR '            TO OUTPUT-X-IO-AREA (309:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (318:1)
               MOVE 'OKT FJOR '            TO OUTPUT-X-IO-AREA (320:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (329:1)
               MOVE 'NOV FJOR '            TO OUTPUT-X-IO-AREA (331:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (340:1)
               MOVE 'DES FJOR '            TO OUTPUT-X-IO-AREA (342:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (351:1)
               MOVE 'JAN I ÅR '            TO OUTPUT-X-IO-AREA (353:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (362:1)
               MOVE 'FEB I ÅR '            TO OUTPUT-X-IO-AREA (364:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (373:1)
               MOVE 'MARS I ÅR'            TO OUTPUT-X-IO-AREA (375:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (384:1)
               MOVE 'APR I ÅR '            TO OUTPUT-X-IO-AREA (386:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (395:1)
               MOVE 'MAI I ÅR '            TO OUTPUT-X-IO-AREA (397:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (406:1)
               MOVE 'JUNI I ÅR'            TO OUTPUT-X-IO-AREA (408:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (417:1)
               MOVE 'JULI I ÅR'            TO OUTPUT-X-IO-AREA (419:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (428:1)
               MOVE 'AUG I ÅR '            TO OUTPUT-X-IO-AREA (430:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (439:1)
               MOVE 'SEP I ÅR '            TO OUTPUT-X-IO-AREA (441:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (450:1)
               MOVE 'OKT I ÅR '            TO OUTPUT-X-IO-AREA (452:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (461:1)
               MOVE 'NOV I ÅR '            TO OUTPUT-X-IO-AREA (463:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (472:1)
               MOVE 'DES I ÅR '            TO OUTPUT-X-IO-AREA (474:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (483:1)
      *UTPUT  D        L1 10
               WRITE OUTPUT-X-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ORDRE TOTALT.          ' TO LISTE-IO-AREA (14:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTMR                  TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE '01 MED MATCH           ' TO LISTE-IO-AREA (14:23)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTMR2                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE '01 IKKE MATCH          ' TO LISTE-IO-AREA (14:23)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTMR3                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE '02 IKKE MATCH          ' TO LISTE-IO-AREA (14:23)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-10)
      *UTPUT  D        01 10
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE REC1                   TO OUTPUT-X-IO-AREA (1:250)
               MOVE REC2                   TO OUTPUT-X-IO-AREA
                                                             (251:250)
      *                      40          53 "-"
               MOVE TGNANT                 TO OUTPUT-X-IO-AREA (53:1)
               MOVE NULL8                  TO EDIT-NULL8
               MOVE EDIT-NULL8             TO OUTPUT-X-IO-AREA (55:9)
               IF  (I-01 AND I-MR)
                   MOVE ANTDMN             TO EDIT-ANTDMN
                   MOVE EDIT-ANTDMN        TO OUTPUT-X-IO-AREA (55:9)
                   INITIALIZE ANTDMN
               END-IF
               IF  (I-42)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (65:1)
               END-IF
               MOVE NULL8                  TO EDIT-NULL8
               MOVE EDIT-NULL8             TO OUTPUT-X-IO-AREA (68:9)
               MOVE ANTAKM                 TO EDIT-ANTAKM
               MOVE EDIT-ANTAKM            TO OUTPUT-X-IO-AREA (68:9)
               INITIALIZE ANTAKM
      *                        NULL8     89 "     . 0 "
      *                      39          91 "-"
               MOVE TGNSUM                 TO OUTPUT-X-IO-AREA (91:1)
               MOVE NULL8                  TO EDIT-NULL8
               MOVE EDIT-NULL8             TO OUTPUT-X-IO-AREA (96:9)
               IF  (I-01 AND I-MR)
                   MOVE BELDMN             TO EDIT-BELDMN
                   MOVE EDIT-BELDMN        TO OUTPUT-X-IO-AREA (96:9)
                   INITIALIZE BELDMN
      *                        NULL8    119 "     . 0 "
               END-IF
               IF  (I-41)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (121:1)
               END-IF
               MOVE NULL8                  TO EDIT-NULL8
               MOVE EDIT-NULL8             TO OUTPUT-X-IO-AREA (127:9)
               MOVE BELAKM                 TO EDIT-BELAKM
               MOVE EDIT-BELAKM            TO OUTPUT-X-IO-AREA (127:9)
               INITIALIZE BELAKM
      *                                 137 " "
      *                        NULL8    150 "     . 0 "
               MOVE ';'                    TO OUTPUT-X-IO-AREA (151:1)
      *                                 152 " "
               MOVE NULL8                  TO EDIT-NULL8
               MOVE EDIT-NULL8             TO OUTPUT-X-IO-AREA (157:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (166:1)
               WRITE OUTPUT-X-IO-AREA
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
           SET INNFIL-LEVEL-INIT           TO TRUE
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNFIL-MC
                                              INNFIL-MP
           OPEN INPUT INNFIL
           SET INNFIL2-LEVEL-INIT          TO TRUE
           INITIALIZE INNFIL2-DATA-FIELDS
           SET INNFIL2-EOF-OFF             TO TRUE
           SET INNFIL2-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO INNFIL2-MC
                                              INNFIL2-MP
           OPEN INPUT INNFIL2
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTPUT-X.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE INNFIL2
           CLOSE VAGRMAS
           CLOSE KUNDEMA
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
