       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA186R.
      **********************************************  Z-WIN-RPG2   ****
      *  RUTINE FOR � HENTE SELGERNR PR. KUNDE/AVD.       *
      *  UPSI 8 SPESIALRUTINE FOR DEFASTAT. S OG B.       *
      *****************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA186.rpg
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
           SELECT PRTFILE
               ASSIGN TO UT-S-PRTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFILE-STATUS.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD PRTFILE
               BLOCK CONTAINS 4200
               RECORD CONTAINS 210.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(210).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PRTFILE-EOF-OFF         VALUE '0'.
               88  PRTFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRTFILE-READ-OFF        VALUE '0'.
               88  PRTFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRTFILE-PROCESS-OFF     VALUE '0'.
               88  PRTFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PRTFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  PRTFILE-LEVEL-INIT      VALUE '1'.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRTFILE-LEVEL-02.
               10  PRTFILE-02-L2.
                   15  PRTFILE-02-L2-FIRMA PICTURE X(3).
               10  PRTFILE-02-L1.
                   15  PRTFILE-02-L1-KUNDE PICTURE X(6).
           05  PRTFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
               10  VGRAVD                  PICTURE X(1).
           05  KUNDEMX-DATA-FIELDS.
               10  SNRA1                   PICTURE X(3).
               10  SNRA2                   PICTURE X(3).
               10  SNRA3                   PICTURE X(3).
               10  SNRA4                   PICTURE X(3).
               10  SNRA5                   PICTURE X(3).
               10  SNRA6                   PICTURE X(3).
               10  SNRA7                   PICTURE X(3).
               10  SNRA8                   PICTURE X(3).
               10  SNRA9                   PICTURE X(3).
               10  KATOS                   PICTURE X(3).
      *****************************************************
      *  RUTINE FOR OPPSLAG P� KUNDEMASTER 2              *
      *****************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  F9                      PICTURE X(9).
               10  KUNKEY                  PICTURE X(10).
               10  SELGNR                  PICTURE X(3).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PRTFILE-PROCESS
               SET PRTFILE-PROCESS-OFF     TO TRUE
               SET PRTFILE-READ            TO TRUE
           END-IF
 
           IF  PRTFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM PRTFILE-GET
               SET PRTFILE-READ-OFF        TO TRUE
               IF  NOT PRTFILE-EOF
                   SET PRTFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PRTFILE-PROCESS
               PERFORM PRTFILE-IDSET
           END-IF
 
           IF  PRTFILE-PROCESS
               PERFORM PRTFILE-CHK-LEVEL
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
 
           IF  PRTFILE-PROCESS
               PERFORM PRTFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  PRTFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               MOVE FIRMA                  TO F9 (1:3)
           END-IF
           IF  (I-L1)
               MOVE KUNDE                  TO F9 (4:6)
               MOVE F9                     TO KUNKEY (1:9)
               MOVE '1'                    TO KUNKEY (10:1)
               MOVE KUNKEY                 TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-09                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-09            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-09)
               MOVE '099'                  TO SELGNR
               GO TO SLUTT-T
      *****************************************************
      *  RUTINE FOR FINNE SELGERNR PR. AVD.               *
      *****************************************************
           END-IF
           SET NOT-I-76                    TO TRUE
           IF  FIRMA = '855'
               SET I-76                    TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  FIRMA = '956'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-U2)
               SET NOT-I-30                TO TRUE
               IF  FIRMA = '923'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  FIRMA = '918'
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '915'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '950'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               GO TO NOTAVD-T
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  VGRAVD = '1'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  VGRAVD = '2'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  VGRAVD = '3'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  VGRAVD = '4'
               SET I-14                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  VGRAVD = '5'
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  VGRAVD = '6'
               SET I-16                    TO TRUE
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  VGRAVD = '7'
               SET I-17                    TO TRUE
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  VGRAVD = '8'
               SET I-18                    TO TRUE
           END-IF
           SET NOT-I-19                    TO TRUE
           IF  VGRAVD = '9'
               SET I-19                    TO TRUE
           END-IF
           IF  (I-11)
               MOVE SNRA1                  TO SELGNR
           END-IF
           IF  (I-12)
               MOVE SNRA2                  TO SELGNR
           END-IF
           IF  (I-13)
               MOVE SNRA3                  TO SELGNR
           END-IF
           IF  (I-14)
               MOVE SNRA4                  TO SELGNR
           END-IF
           IF  (I-15)
               MOVE SNRA5                  TO SELGNR
           END-IF
           IF  (I-16)
               MOVE SNRA6                  TO SELGNR
           END-IF
           IF  (I-17)
               MOVE SNRA7                  TO SELGNR
           END-IF
           IF  (I-18)
               MOVE SNRA8                  TO SELGNR
           END-IF
           IF  (I-19)
               MOVE SNRA9                  TO SELGNR
           END-IF.
 
       NOTAVD-T.
           IF  (NOT-I-10)
               MOVE SNRA1                  TO SELGNR
           END-IF
           IF  (I-31)
               MOVE SNRA1                  TO SELGNR
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  SELGNR NOT > '000'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               MOVE '099'                  TO SELGNR
      *****************************************************************
      * RUTINE FOR OMD�PING AV SELGERNR. DEFASTAT S OG B.             *
      *****************************************************************
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  FIRMA = '918'
               SET I-51                    TO TRUE
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  KATOS = '002'
               SET I-52                    TO TRUE
           END-IF
           IF  (I-U8 AND I-51 AND I-52)
               MOVE '024'                  TO SELGNR
      *****************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       PRTFILE-GET SECTION.
       PRTFILE-GET-P.
           IF  PRTFILE-EOF-OFF
               READ PRTFILE
               AT END
                   SET PRTFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PRTFILE-FLDSET SECTION.
       PRTFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRTFILE-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE PRTFILE-IO-AREA (16:6) TO KUNDE (1:6)
               MOVE PRTFILE-IO-AREA (22:1) TO VGRAVD (1:1)
           END-EVALUATE.
 
       PRTFILE-IDSET SECTION.
       PRTFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       PRTFILE-CHK-LEVEL SECTION.
       PRTFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PRTFILE-LEVEL-02
               MOVE PRTFILE-IO-AREA (3:3)  TO PRTFILE-02-L2-FIRMA
               MOVE PRTFILE-IO-AREA (16:6) TO PRTFILE-02-L1-KUNDE
               IF  PRTFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PRTFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PRTFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PRTFILE-02-L2         TO THE-PRIOR-L2
               MOVE  PRTFILE-02-L1         TO THE-PRIOR-L1
               SET PRTFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (105:3) TO SNRA1 (1:3)
               MOVE KUNDEMX-IO-AREA (108:3) TO SNRA2 (1:3)
               MOVE KUNDEMX-IO-AREA (111:3) TO SNRA3 (1:3)
               MOVE KUNDEMX-IO-AREA (114:3) TO SNRA4 (1:3)
               MOVE KUNDEMX-IO-AREA (117:3) TO SNRA5 (1:3)
               MOVE KUNDEMX-IO-AREA (120:3) TO SNRA6 (1:3)
               MOVE KUNDEMX-IO-AREA (123:3) TO SNRA7 (1:3)
               MOVE KUNDEMX-IO-AREA (126:3) TO SNRA8 (1:3)
               MOVE KUNDEMX-IO-AREA (129:3) TO SNRA9 (1:3)
               MOVE KUNDEMX-IO-AREA (132:3) TO KATOS (1:3)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-05                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SELGNR                 TO PRTFILE-IO-AREA (6:3)
               IF  (I-76)
                   MOVE SNRA1              TO PRTFILE-IO-AREA (6:3)
               END-IF
               IF  (I-U2 AND I-30)
                   MOVE KATOS              TO PRTFILE-IO-AREA (6:3)
               END-IF
               REWRITE PRTFILE-IO-AREA
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
           SET PRTFILE-LEVEL-INIT          TO TRUE
           INITIALIZE PRTFILE-DATA-FIELDS
           SET PRTFILE-EOF-OFF             TO TRUE
           SET PRTFILE-PROCESS             TO TRUE
           OPEN I-O PRTFILE
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PRTFILE
           CLOSE KUNDEMX.
 
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
