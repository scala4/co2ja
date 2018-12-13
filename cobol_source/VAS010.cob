       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS010R.
      **********************************************  Z-WIN-RPG2   ****
      * OPPDATERE VARE.STAT.MASTER.                        *
      * I JANUAR MÅ IKKE JANUAR I FJOR OPPDATERES, DA DETTE*
      *    MEDFØRERER ATT JAN I FJOR BLIR NULLSTILT        *
      *    P.G.A. NYTT ÅR.                                 *
      * 29.10.97  PROGRAMMET SKREVET OM TIL SEQ. OUTPUT    *
      *           DA DET BLE PROBLEMER NÅR KSDS BLE ENDRET *
      *           TIL COMPRESSED.                          *
      * 02.06.03  SJEKK OM BELØP DENNE MND ER OVER 10 MILL.*
      *           DA SKAL DET IKKE ADDES DA PROG CANSELERER*
      * 26.04.05  FJERNER DATA PÅ FIRMA SOM ER MERKET      *
      *           SLETTET, ELLER SLETTING VAREDATA.        *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS010.rpg
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
           SELECT VARSTAA
               ASSIGN TO UT-S-VARSTAA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTAA-STATUS.
           SELECT VARSTAM
               ASSIGN TO VARSTAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VARSTAM-STATUS
               RECORD KEY IS VARSTAM-KEY1.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT VARSTUT
               ASSIGN TO UT-S-VARSTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTUT-STATUS.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARSTAA
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  VARSTAA-IO-AREA.
           05  VARSTAA-IO-AREA-X           PICTURE X(40).
       FD VARSTAM
               RECORD CONTAINS 500.
       01  VARSTAM-IO-AREA.
           05  VARSTAM-IO-AREA-X.
               10  VARSTAM-KEY1.
                   15  VARSTAM-KEY1N       PICTURE S9(12).
               10  FILLER                  PICTURE X(488).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VARSTUT
               BLOCK CONTAINS 1000
               RECORD CONTAINS 500.
       01  VARSTUT-IO-AREA.
           05  VARSTUT-IO-AREA-X           PICTURE X(500).
       FD TOTALER
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  A1A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A1B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A2A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A2B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A3A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A3B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A4A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A4B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M1A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M1B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M2A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M2B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M3A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M3B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M4A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  M4B-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  A1A-TABLE.
               10  A1A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A1A-I
                                                      A1A-S.
                   15  A1A                 PICTURE S9(7)V9(2).
           05  A1B-TABLE.
               10  A1B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A1B-I
                                                      A1B-S.
                   15  A1B                 PICTURE S9(7).
           05  A2A-TABLE.
               10  A2A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A2A-I
                                                      A2A-S.
                   15  A2A                 PICTURE S9(7)V9(2).
           05  A2B-TABLE.
               10  A2B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A2B-I
                                                      A2B-S.
                   15  A2B                 PICTURE S9(7).
           05  A3A-TABLE.
               10  A3A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A3A-I
                                                      A3A-S.
                   15  A3A                 PICTURE S9(7)V9(2).
           05  A3B-TABLE.
               10  A3B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A3B-I
                                                      A3B-S.
                   15  A3B                 PICTURE S9(7).
           05  A4A-TABLE.
               10  A4A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A4A-I
                                                      A4A-S.
                   15  A4A                 PICTURE S9(7)V9(2).
           05  A4B-TABLE.
               10  A4B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A4B-I
                                                      A4B-S.
                   15  A4B                 PICTURE S9(7).
           05  M1A-TABLE.
               10  M1A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M1A-I
                                                      M1A-S.
                   15  M1A                 PICTURE S9(7)V9(2).
           05  M1B-TABLE.
               10  M1B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M1B-I
                                                      M1B-S.
                   15  M1B                 PICTURE S9(7).
           05  M2A-TABLE.
               10  M2A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M2A-I
                                                      M2A-S.
                   15  M2A                 PICTURE S9(7)V9(2).
           05  M2B-TABLE.
               10  M2B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M2B-I
                                                      M2B-S.
                   15  M2B                 PICTURE S9(7).
           05  M3A-TABLE.
               10  M3A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M3A-I
                                                      M3A-S.
                   15  M3A                 PICTURE S9(7)V9(2).
           05  M3B-TABLE.
               10  M3B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M3B-I
                                                      M3B-S.
                   15  M3B                 PICTURE S9(7).
           05  M4A-TABLE.
               10  M4A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M4A-I
                                                      M4A-S.
                   15  M4A                 PICTURE S9(7)V9(2).
           05  M4B-TABLE.
               10  M4B-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY M4B-I
                                                      M4B-S.
                   15  M4B                 PICTURE S9(7).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VARSTAA-STATUS              PICTURE 99 VALUE 0.
           10  VARSTAM-STATUS              PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  VARSTUT-STATUS              PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAA-EOF-OFF         VALUE '0'.
               88  VARSTAA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAA-READ-OFF        VALUE '0'.
               88  VARSTAA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAA-PROCESS-OFF     VALUE '0'.
               88  VARSTAA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARSTAA-LEVEL-INIT-OFF  VALUE '0'.
               88  VARSTAA-LEVEL-INIT      VALUE '1'.
           05  VARSTAM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAM-EOF-OFF         VALUE '0'.
               88  VARSTAM-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAM-READ-OFF        VALUE '0'.
               88  VARSTAM-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAM-PROCESS-OFF     VALUE '0'.
               88  VARSTAM-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARSTAM-LEVEL-INIT-OFF  VALUE '0'.
               88  VARSTAM-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  TOTALER-DATA-FIELDS.
               10  TOTALER-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-CLR-IO          PICTURE X VALUE 'Y'.
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
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  VARSTAA-LEVEL-02.
               10  VARSTAA-02-L2.
                   15  VARSTAA-02-L2-FIRMA PICTURE X(3).
               10  VARSTAA-02-L1.
                   15  VARSTAA-02-L1-EDBKEY PICTURE X(9).
           05  VARSTAA-DATA-FIELDS.
               10  TYPE-X                  PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  EDBKEY                  PICTURE X(9).
               10  EDBNR                   PICTURE X(7).
               10  ADATA                   PICTURE X(24).
               10  VGRALF                  PICTURE X(8).
               10  ASDATO-IO.
                   15  ASDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  AA-ELGR                 PICTURE X(2).
               10  AMND                    PICTURE X(2).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  VARSTAA-MP                  PICTURE X(12).
           05  VARSTAA-MC                  PICTURE X(12).
           05  VARSTAA-M-02            REDEFINES VARSTAA-MC.
               10  VARSTAA-M-02-M2.
                   15  VARSTAA-M-02-M2-FIRMA-G.
                       20  VARSTAA-M-02-M2-FIRMA PICTURE X(3).
               10  VARSTAA-M-02-M1.
                   15  VARSTAA-M-02-M1-EDBKEY-G.
                       20  VARSTAA-M-02-M1-EDBKEY PICTURE X(9).
           05  VARSTAM-LEVEL-01.
               10  VARSTAM-01-L2.
                   15  VARSTAM-01-L2-FIRMA PICTURE X(3).
               10  VARSTAM-01-L1.
                   15  VARSTAM-01-L1-EDBKEY PICTURE X(9).
           05  VARSTAM-DATA-FIELDS.
               10  VSREC1                  PICTURE X(250).
               10  VSREC2                  PICTURE X(250).
               10  SSDATO-IO.
                   15  SSDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M1A-GRP.
                   15  XI-M1A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M1B-GRP.
                   15  XI-M1B              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M2A-GRP.
                   15  XI-M2A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M2B-GRP.
                   15  XI-M2B              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M3A-GRP.
                   15  XI-M3A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M3B-GRP.
                   15  XI-M3B              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M4A-GRP.
                   15  XI-M4A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-M4B-GRP.
                   15  XI-M4B              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  VARSTAM-MP                  PICTURE X(12).
           05  VARSTAM-MC                  PICTURE X(12).
           05  VARSTAM-M-01            REDEFINES VARSTAM-MC.
               10  VARSTAM-M-01-M2.
                   15  VARSTAM-M-01-M2-FIRMA-G.
                       20  VARSTAM-M-01-M2-FIRMA PICTURE X(3).
               10  VARSTAM-M-01-M1.
                   15  VARSTAM-M-01-M1-EDBKEY-G.
                       20  VARSTAM-M-01-M1-EDBKEY PICTURE X(9).
           05  PARAM-DATA-FIELDS.
               10  AART1                   PICTURE X(4).
               10  AAR1                    PICTURE X(2).
               10  AART2                   PICTURE X(4).
               10  AAR2                    PICTURE X(2).
               10  AART3                   PICTURE X(4).
               10  AAR3                    PICTURE X(2).
               10  AART4                   PICTURE X(4).
               10  AAR4                    PICTURE X(2).
               10  MNDOPP                  PICTURE X(1).
           05  FAKPAR-DATA-FIELDS.
               10  PAR                     PICTURE X(2).
               10  PMND                    PICTURE X(2).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  ANTM-IO.
                   15  ANTM                PICTURE S9(8).
               10  ANTA-IO.
                   15  ANTA                PICTURE S9(8).
               10  ANTOPP-IO.
                   15  ANTOPP              PICTURE S9(8).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(8).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  BEL10-IO.
                   15  BEL10               PICTURE S9(8)V9(2).
               10  ANTNUL-IO.
                   15  ANTNUL              PICTURE S9(8).
               10  ANTSLE-IO.
                   15  ANTSLE              PICTURE S9(8).
               10  F6-IO.
                   15  F6                  PICTURE S9(6).
               10  SSDATO-N-IO.
                   15  SSDATO-N            PICTURE S9(7).
               10  GMLDAT                  PICTURE X(6).
               10  DDA                     PICTURE X(2).
               10  AAA                     PICTURE X(2).
               10  GMLDA8                  PICTURE X(8).
               10  ASDATO-N-IO.
                   15  ASDATO-N            PICTURE S9(7).
               10  NYDATO                  PICTURE X(6).
               10  NYDAT8                  PICTURE X(8).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  DATO3-IO.
                   15  DATO3               PICTURE S9(6).
               10  Y-IO.
                   15  Y                   PICTURE S9(2).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VARSTAA-PROCESS
               SET VARSTAA-PROCESS-OFF     TO TRUE
               SET VARSTAA-READ            TO TRUE
           END-IF
 
           IF  VARSTAA-READ
               PERFORM VARSTAA-GET
               SET VARSTAA-READ-OFF        TO TRUE
               IF  NOT VARSTAA-EOF
                   PERFORM VARSTAA-MATCH-SET
               END-IF
           END-IF
 
           IF  VARSTAM-PROCESS
               SET VARSTAM-PROCESS-OFF     TO TRUE
               SET VARSTAM-READ            TO TRUE
           END-IF
 
           IF  VARSTAM-READ
               PERFORM VARSTAM-GET
               SET VARSTAM-READ-OFF        TO TRUE
               IF  NOT VARSTAM-EOF
                   PERFORM VARSTAM-MATCH-SET
               END-IF
           END-IF
 
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FAKPAR-PROCESS
               SET FAKPAR-PROCESS-OFF      TO TRUE
               SET FAKPAR-READ             TO TRUE
           END-IF
 
           IF  FAKPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKPAR-GET
               SET FAKPAR-READ-OFF         TO TRUE
               IF  NOT FAKPAR-EOF
                   SET FAKPAR-PROCESS      TO TRUE
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
 
           IF  VARSTAA-PROCESS
               PERFORM VARSTAA-IDSET
           END-IF
 
           IF  VARSTAM-PROCESS
               PERFORM VARSTAM-IDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
           END-IF
 
           IF  VARSTAA-PROCESS
               PERFORM VARSTAA-CHK-LEVEL
           END-IF
 
           IF  VARSTAM-PROCESS
               PERFORM VARSTAM-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VARSTAA-PROCESS
               PERFORM VARSTAA-FLDSET
           END-IF
 
           IF  VARSTAM-PROCESS
               PERFORM VARSTAM-FLDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARSTAA-PROCESS
           OR  VARSTAM-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-03)
               SET NOT-I-11                TO TRUE
               IF  UMONTH = 01
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  UMONTH = 02
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-03)
               SET NOT-I-12                TO TRUE
               IF  MNDOPP = 'J'
                   SET I-12                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           IF  (I-04 AND I-12)
               PERFORM FAPRUT-S
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               PERFORM FISLET-S
           END-IF
           IF  (I-L1)
               SET NOT-I-31                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-33                TO TRUE
               SET NOT-I-34                TO TRUE
               SET NOT-I-35                TO TRUE
               SET NOT-I-36                TO TRUE
               SET NOT-I-37                TO TRUE
               SET NOT-I-38                TO TRUE
               SET NOT-I-39                TO TRUE
               SET NOT-I-40                TO TRUE
               SET NOT-I-41                TO TRUE
               SET NOT-I-42                TO TRUE
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-91                TO TRUE
               SET NOT-I-95                TO TRUE
               SET NOT-I-96                TO TRUE
               SET NOT-I-97                TO TRUE
               SET NOT-I-98                TO TRUE
               SET NOT-I-60                TO TRUE
               PERFORM VARYING A1A-I FROM 1 BY 1
                         UNTIL A1A-I > A1A-MAX
                   MOVE 0                  TO A1A (A1A-I)
               END-PERFORM
               SET A1A-I                   TO 1
               PERFORM VARYING A1B-I FROM 1 BY 1
                         UNTIL A1B-I > A1B-MAX
                   MOVE 0                  TO A1B (A1B-I)
               END-PERFORM
               SET A1B-I                   TO 1
               PERFORM VARYING A2A-I FROM 1 BY 1
                         UNTIL A2A-I > A2A-MAX
                   MOVE 0                  TO A2A (A2A-I)
               END-PERFORM
               SET A2A-I                   TO 1
               PERFORM VARYING A2B-I FROM 1 BY 1
                         UNTIL A2B-I > A2B-MAX
                   MOVE 0                  TO A2B (A2B-I)
               END-PERFORM
               SET A2B-I                   TO 1
               PERFORM VARYING A3A-I FROM 1 BY 1
                         UNTIL A3A-I > A3A-MAX
                   MOVE 0                  TO A3A (A3A-I)
               END-PERFORM
               SET A3A-I                   TO 1
               PERFORM VARYING A3B-I FROM 1 BY 1
                         UNTIL A3B-I > A3B-MAX
                   MOVE 0                  TO A3B (A3B-I)
               END-PERFORM
               SET A3B-I                   TO 1
               PERFORM VARYING A4A-I FROM 1 BY 1
                         UNTIL A4A-I > A4A-MAX
                   MOVE 0                  TO A4A (A4A-I)
               END-PERFORM
               SET A4A-I                   TO 1
               PERFORM VARYING A4B-I FROM 1 BY 1
                         UNTIL A4B-I > A4B-MAX
                   MOVE 0                  TO A4B (A4B-I)
               END-PERFORM
               SET A4B-I                   TO 1
      *********  MASTER RECORD RUTINE  *******************************
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTM
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-12)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-12)
               GO TO NULMND-T
           END-IF
           IF  (I-01)
               GO TO SUMRUT-T
      *********  AJOURHOLD RECORD RUTINE   ***************************
           END-IF
           IF  (I-02)
               SET NOT-I-91                TO TRUE
               IF  TYPE-X = 'E'
                   SET I-91                TO TRUE
               END-IF
               ADD 1                       TO ANTA
           END-IF
           IF  (I-MR AND NOT-I-51)
               ADD 1                       TO ANTOPP
           END-IF
           IF  (I-MR)
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-MR AND NOT-I-50 AND NOT-I-68)
               ADD 1                       TO ANTNYE
           END-IF
           IF  (NOT-I-MR)
               SET I-50                    TO TRUE
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  AA-ELGR = AAR1
               SET I-88                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  AA-ELGR = AAR1
                   SET I-98                TO TRUE
               END-IF
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  AA-ELGR = AAR2
               SET I-87                    TO TRUE
           END-IF
           IF  (NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  AA-ELGR = AAR2
                   SET I-97                TO TRUE
               END-IF
           END-IF
           SET NOT-I-86                    TO TRUE
           IF  AA-ELGR = AAR3
               SET I-86                    TO TRUE
           END-IF
           IF  (NOT-I-96)
               SET NOT-I-96                TO TRUE
               IF  AA-ELGR = AAR3
                   SET I-96                TO TRUE
               END-IF
           END-IF
           SET NOT-I-85                    TO TRUE
           IF  AA-ELGR = AAR4
               SET I-85                    TO TRUE
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  AA-ELGR = AAR4
                   SET I-95                TO TRUE
               END-IF
           END-IF
           MOVE AMND                       TO X-IO
           SET NOT-I-30                    TO TRUE
           IF  X < 1
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30)
               MOVE 1                      TO X
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  X > 12
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30)
               MOVE 12                     TO X
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  X = 1
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  X = 2
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  X = 3
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  X = 4
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  X = 5
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  X = 6
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-37)
               SET NOT-I-37                TO TRUE
               IF  X = 7
                   SET I-37                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-38)
               SET NOT-I-38                TO TRUE
               IF  X = 8
                   SET I-38                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-39)
               SET NOT-I-39                TO TRUE
               IF  X = 9
                   SET I-39                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  X = 10
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  X = 11
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  X = 12
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-88)
               ADD ANT                     TO A1A (X)
               ADD BEL TO A1B (X)      GIVING BEL10 ROUNDED
               SET NOT-I-77                TO TRUE
               IF  BEL10 > 9999999,99
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-77)
               ADD BEL                     TO A1B (X) ROUNDED
           END-IF
           IF  (I-87)
               ADD ANT                     TO A2A (X)
               ADD BEL                     TO A2B (X) ROUNDED
           END-IF
           IF  (I-86)
               ADD ANT                     TO A3A (X)
               ADD BEL                     TO A3B (X) ROUNDED
           END-IF
           IF  (I-85)
               ADD ANT                     TO A4A (X)
               ADD BEL                     TO A4B (X) ROUNDED
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
           END-IF.
 
       SUMRUT-T.
      *********  TOTAL RUTINE MR RECORDS ENDRING/ERSTATNING I ÅR *****
           IF  (I-91 AND NOT-I-98)
               GO TO BYTA-ELGR2-T
           END-IF
           IF  (I-31)
               ADD A1A (1) TO ZERO     GIVING M1A (1)
               ADD A1B (1) TO ZERO     GIVING M1B (1)
           END-IF
           IF  (I-32)
               ADD A1A (2) TO ZERO     GIVING M1A (2)
               ADD A1B (2) TO ZERO     GIVING M1B (2)
           END-IF
           IF  (I-33)
               ADD A1A (3) TO ZERO     GIVING M1A (3)
               ADD A1B (3) TO ZERO     GIVING M1B (3)
           END-IF
           IF  (I-34)
               ADD A1A (4) TO ZERO     GIVING M1A (4)
               ADD A1B (4) TO ZERO     GIVING M1B (4)
           END-IF
           IF  (I-35)
               ADD A1A (5) TO ZERO     GIVING M1A (5)
               ADD A1B (5) TO ZERO     GIVING M1B (5)
           END-IF
           IF  (I-36)
               ADD A1A (6) TO ZERO     GIVING M1A (6)
               ADD A1B (6) TO ZERO     GIVING M1B (6)
           END-IF
           IF  (I-37)
               ADD A1A (7) TO ZERO     GIVING M1A (7)
               ADD A1B (7) TO ZERO     GIVING M1B (7)
           END-IF
           IF  (I-38)
               ADD A1A (8) TO ZERO     GIVING M1A (8)
               ADD A1B (8) TO ZERO     GIVING M1B (8)
           END-IF
           IF  (I-39)
               ADD A1A (9) TO ZERO     GIVING M1A (9)
               ADD A1B (9) TO ZERO     GIVING M1B (9)
           END-IF
           IF  (I-40)
               ADD A1A (10) TO ZERO    GIVING M1A (10)
               ADD A1B (10) TO ZERO    GIVING M1B (10)
           END-IF
           IF  (I-41)
               ADD A1A (11) TO ZERO    GIVING M1A (11)
               ADD A1B (11) TO ZERO    GIVING M1B (11)
           END-IF
           IF  (I-42)
               ADD A1A (12) TO ZERO    GIVING M1A (12)
               ADD A1B (12) TO ZERO    GIVING M1B (12)
      *********  TOTAL RUTINE MR RECORDS ENDRING/ERSTATNING I FJOR ***
           END-IF
           .
 
       BYTA-ELGR2-T.
           IF  (I-91 AND NOT-I-97)
               GO TO BYTA-ELGR3-T
           END-IF
           IF  (I-31 AND NOT-I-11)
               ADD A2A (1) TO ZERO     GIVING M2A (1)
               ADD A2B (1) TO ZERO     GIVING M2B (1)
           END-IF
           IF  (I-32 AND NOT-I-11)
               ADD A2A (2) TO ZERO     GIVING M2A (2)
               ADD A2B (2) TO ZERO     GIVING M2B (2)
           END-IF
           IF  (I-33)
               ADD A2A (3) TO ZERO     GIVING M2A (3)
               ADD A2B (3) TO ZERO     GIVING M2B (3)
           END-IF
           IF  (I-34)
               ADD A2A (4) TO ZERO     GIVING M2A (4)
               ADD A2B (4) TO ZERO     GIVING M2B (4)
           END-IF
           IF  (I-35)
               ADD A2A (5) TO ZERO     GIVING M2A (5)
               ADD A2B (5) TO ZERO     GIVING M2B (5)
           END-IF
           IF  (I-36)
               ADD A2A (6) TO ZERO     GIVING M2A (6)
               ADD A2B (6) TO ZERO     GIVING M2B (6)
           END-IF
           IF  (I-37)
               ADD A2A (7) TO ZERO     GIVING M2A (7)
               ADD A2B (7) TO ZERO     GIVING M2B (7)
           END-IF
           IF  (I-38)
               ADD A2A (8) TO ZERO     GIVING M2A (8)
               ADD A2B (8) TO ZERO     GIVING M2B (8)
           END-IF
           IF  (I-39)
               ADD A2A (9) TO ZERO     GIVING M2A (9)
               ADD A2B (9) TO ZERO     GIVING M2B (9)
           END-IF
           IF  (I-40)
               ADD A2A (10) TO ZERO    GIVING M2A (10)
               ADD A2B (10) TO ZERO    GIVING M2B (10)
           END-IF
           IF  (I-41)
               ADD A2A (11) TO ZERO    GIVING M2A (11)
               ADD A2B (11) TO ZERO    GIVING M2B (11)
           END-IF
           IF  (I-42)
               ADD A2A (12) TO ZERO    GIVING M2A (12)
               ADD A2B (12) TO ZERO    GIVING M2B (12)
      *********  TOTAL RUTINE MR RECORDS ENDRING/ERSTATNING I FORFJOR ***
           END-IF
           .
 
       BYTA-ELGR3-T.
           IF  (I-91 AND NOT-I-96)
               GO TO BYTA-ELGR4-T
           END-IF
           IF  (I-31)
               ADD A3A (1) TO ZERO     GIVING M3A (1)
               ADD A3B (1) TO ZERO     GIVING M3B (1)
           END-IF
           IF  (I-32)
               ADD A3A (2) TO ZERO     GIVING M3A (2)
               ADD A3B (2) TO ZERO     GIVING M3B (2)
           END-IF
           IF  (I-33)
               ADD A3A (3) TO ZERO     GIVING M3A (3)
               ADD A3B (3) TO ZERO     GIVING M3B (3)
           END-IF
           IF  (I-34)
               ADD A3A (4) TO ZERO     GIVING M3A (4)
               ADD A3B (4) TO ZERO     GIVING M3B (4)
           END-IF
           IF  (I-35)
               ADD A3A (5) TO ZERO     GIVING M3A (5)
               ADD A3B (5) TO ZERO     GIVING M3B (5)
           END-IF
           IF  (I-36)
               ADD A3A (6) TO ZERO     GIVING M3A (6)
               ADD A3B (6) TO ZERO     GIVING M3B (6)
           END-IF
           IF  (I-37)
               ADD A3A (7) TO ZERO     GIVING M3A (7)
               ADD A3B (7) TO ZERO     GIVING M3B (7)
           END-IF
           IF  (I-38)
               ADD A3A (8) TO ZERO     GIVING M3A (8)
               ADD A3B (8) TO ZERO     GIVING M3B (8)
           END-IF
           IF  (I-39)
               ADD A3A (9) TO ZERO     GIVING M3A (9)
               ADD A3B (9) TO ZERO     GIVING M3B (9)
           END-IF
           IF  (I-40)
               ADD A3A (10) TO ZERO    GIVING M3A (10)
               ADD A3B (10) TO ZERO    GIVING M3B (10)
           END-IF
           IF  (I-41)
               ADD A3A (11) TO ZERO    GIVING M3A (11)
               ADD A3B (11) TO ZERO    GIVING M3B (11)
           END-IF
           IF  (I-42)
               ADD A3A (12) TO ZERO    GIVING M3A (12)
               ADD A3B (12) TO ZERO    GIVING M3B (12)
      *********  TOTAL RUTINE MR RECORDS ENDRING/ERSTATNING I FOR FORFJOR ***
           END-IF
           .
 
       BYTA-ELGR4-T.
           IF  (I-91 AND NOT-I-95)
               GO TO BYTEND-T
           END-IF
           IF  (I-31)
               ADD A4A (1) TO ZERO     GIVING M4A (1)
               ADD A4B (1) TO ZERO     GIVING M4B (1)
           END-IF
           IF  (I-32)
               ADD A4A (2) TO ZERO     GIVING M4A (2)
               ADD A4B (2) TO ZERO     GIVING M4B (2)
           END-IF
           IF  (I-33)
               ADD A4A (3) TO ZERO     GIVING M4A (3)
               ADD A4B (3) TO ZERO     GIVING M4B (3)
           END-IF
           IF  (I-34)
               ADD A4A (4) TO ZERO     GIVING M4A (4)
               ADD A4B (4) TO ZERO     GIVING M4B (4)
           END-IF
           IF  (I-35)
               ADD A4A (5) TO ZERO     GIVING M4A (5)
               ADD A4B (5) TO ZERO     GIVING M4B (5)
           END-IF
           IF  (I-36)
               ADD A4A (6) TO ZERO     GIVING M4A (6)
               ADD A4B (6) TO ZERO     GIVING M4B (6)
           END-IF
           IF  (I-37)
               ADD A4A (7) TO ZERO     GIVING M4A (7)
               ADD A4B (7) TO ZERO     GIVING M4B (7)
           END-IF
           IF  (I-38)
               ADD A4A (8) TO ZERO     GIVING M4A (8)
               ADD A4B (8) TO ZERO     GIVING M4B (8)
           END-IF
           IF  (I-39)
               ADD A4A (9) TO ZERO     GIVING M4A (9)
               ADD A4B (9) TO ZERO     GIVING M4B (9)
           END-IF
           IF  (I-40)
               ADD A4A (10) TO ZERO    GIVING M4A (10)
               ADD A4B (10) TO ZERO    GIVING M4B (10)
           END-IF
           IF  (I-41)
               ADD A4A (11) TO ZERO    GIVING M4A (11)
               ADD A4B (11) TO ZERO    GIVING M4B (11)
           END-IF
           IF  (I-42)
               ADD A4A (12) TO ZERO    GIVING M4A (12)
               ADD A4B (12) TO ZERO    GIVING M4B (12)
      *********  TOTAL RUTINE MR RECORDS   ***************************
           END-IF
           .
 
       BYTEND-T.
           PERFORM MSDATO-S
           PERFORM ANDATO-S
           SET NOT-I-23                    TO TRUE
           IF  NYDAT8 > GMLDA8
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               PERFORM AADATO-S
           END-IF
           GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR Å NULLSTILLE STAT.RECORD"S SOM IKKE HAR            *
      * FAKTURERING VED MND. SLUTT.                                   *
      *                                                               *
      *****************************************************************
           .
 
       NULMND-T.
           IF  (I-61)
               SET NOT-I-60                TO TRUE
               IF  M1A (Y) NOT = 0,00
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-61 AND NOT-I-60)
               SET NOT-I-60                TO TRUE
               IF  M1B (Y) NOT = 0
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-62)
               SET NOT-I-60                TO TRUE
               IF  M2A (Y) NOT = 0,00
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-62 AND NOT-I-60)
               SET NOT-I-60                TO TRUE
               IF  M2B (Y) NOT = 0
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-61 AND I-60)
               ADD A1A (Y) TO ZERO     GIVING M1A (Y)
               ADD A1B (Y) TO ZERO     GIVING M1B (Y)
           END-IF
           IF  (I-62 AND I-60)
               ADD A2A (Y) TO ZERO     GIVING M2A (Y)
               ADD A2B (Y) TO ZERO     GIVING M2B (Y)
           END-IF
           IF  (NOT-I-MR AND I-12 AND I-60)
               ADD 1                       TO ANTNUL
      *****************************************************************
      *   TOTAL RUTINE.                                               *
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           IF  (I-01 AND I-68)
               ADD 1                       TO ANTSLE
           END-IF.
 
       MSDATO-S SECTION.
       MSDATO-S-P.
           MOVE SSDATO                     TO SSDATO-N
           MOVE SSDATO-N-IO (2:6)          TO F6-IO
           MOVE F6                         TO GMLDAT
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE GMLDAT (6:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO GMLDAT (6:1)
           MOVE GMLDAT (1:2)               TO DDA
           MOVE GMLDAT (5:2)               TO AAA
           MOVE AAA                        TO GMLDAT (1:2)
           MOVE DDA                        TO GMLDAT (5:2)
           MOVE 'B'                        TO DATOK
           MOVE GMLDAT                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO GMLDA8.
      *****************************************************
      * RUTINE FOR Å SETTE DATO TIL ALFANUMERIC.          *
      *****************************************************
 
       ANDATO-S SECTION.
       ANDATO-S-P.
           MOVE ASDATO                     TO ASDATO-N
           MOVE ASDATO-N-IO (2:6)          TO F6-IO
           MOVE F6                         TO NYDATO
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE NYDATO (6:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO NYDATO (6:1)
           MOVE 'B'                        TO DATOK
           MOVE NYDATO                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO NYDAT8.
      *****************************************************
      * RUTINE FOR Å SNU DATO FOR SISTE SALG TIL DDMMÅÅ   *
      *****************************************************
 
       AADATO-S SECTION.
       AADATO-S-P.
           MOVE ASDATO                     TO ASDATO-N
           MOVE ASDATO-N-IO (2:6)          TO F6-IO
           MOVE F6 (1:2)                   TO AA
           MOVE F6 (5:2)                   TO DD-IO
           MOVE F6                         TO DATO3-IO
           MOVE DD                         TO DATO3 (1:2)
           MOVE AA                         TO DATO3-IO (5:2).
      *****************************************************
      * RUTINE FOR Å FINNE FAKTURA MÅNED OG ÅR.           *
      *****************************************************
 
       FAPRUT-S SECTION.
       FAPRUT-S-P.
           MOVE PMND                       TO Y-IO
           SET NOT-I-30                    TO TRUE
           IF  Y < 1
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30)
               MOVE 1                      TO Y
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  Y > 12
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30)
               MOVE 12                     TO Y
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  PAR = AAR1
               SET I-61                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  PAR = AAR2
               SET I-62                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-68                    TO TRUE
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-66                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-66                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-66)
               SET NOT-I-68                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-68                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-66 AND NOT-I-68)
               SET NOT-I-68                TO TRUE
               IF  FIRMSL = 'K'
                   SET I-68                TO TRUE
               END-IF
           END-IF
           IF  (I-66)
               SET I-68                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-50)
               SET I-23                    TO TRUE
               PERFORM AADATO-S
      *****************************************************
      * RUTINE FOR Å SNU DATO FOR SISTE SALG TIL ÅÅMMDD   *
      *****************************************************
           END-IF
           .
 
       VARSTAA-GET SECTION.
       VARSTAA-GET-P.
           IF  VARSTAA-EOF-OFF
               READ VARSTAA
               AT END
                   SET VARSTAA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARSTAA-FLDSET SECTION.
       VARSTAA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARSTAA-IO-AREA (1:1)  TO TYPE-X (1:1)
               MOVE VARSTAA-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE VARSTAA-IO-AREA (5:9)  TO EDBKEY (1:9)
               MOVE VARSTAA-IO-AREA (5:7)  TO EDBNR (1:7)
               MOVE VARSTAA-IO-AREA (2:24) TO ADATA (1:24)
               MOVE VARSTAA-IO-AREA (14:8) TO VGRALF (1:8)
               MOVE VARSTAA-IO-AREA (22:4) TO ASDATO-IO
               MOVE VARSTAA-IO-AREA (26:2) TO AA-ELGR (1:2)
               MOVE VARSTAA-IO-AREA (28:2) TO AMND (1:2)
               MOVE VARSTAA-IO-AREA (31:5) TO ANT-IO
               MOVE VARSTAA-IO-AREA (36:5) TO BEL-IO
           END-EVALUATE.
 
       VARSTAA-IDSET SECTION.
       VARSTAA-IDSET-P.
           SET I-02                        TO TRUE.
 
       VARSTAA-CHK-LEVEL SECTION.
       VARSTAA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARSTAA-LEVEL-02
               MOVE VARSTAA-IO-AREA (2:3)  TO VARSTAA-02-L2-FIRMA
               MOVE VARSTAA-IO-AREA (5:9)  TO VARSTAA-02-L1-EDBKEY
               IF  VARSTAA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARSTAA-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARSTAA-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARSTAA-02-L2         TO THE-PRIOR-L2
               MOVE  VARSTAA-02-L1         TO THE-PRIOR-L1
               SET VARSTAA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VARSTAA-MATCH-SET SECTION.
       VARSTAA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VARSTAA-IO-AREA (2:3)  TO VARSTAA-M-02-M2-FIRMA
               MOVE VARSTAA-IO-AREA (5:9)  TO VARSTAA-M-02-M1-EDBKEY
           END-EVALUATE.
 
       VARSTAM-GET SECTION.
       VARSTAM-GET-P.
           IF  VARSTAM-EOF-OFF
               READ VARSTAM
               AT END
                   SET VARSTAM-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARSTAM-FLDSET SECTION.
       VARSTAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARSTAM-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE VARSTAM-IO-AREA (5:9)  TO EDBKEY (1:9)
               MOVE VARSTAM-IO-AREA (5:7)  TO EDBNR (1:7)
               MOVE VARSTAM-IO-AREA (1:250) TO VSREC1 (1:250)
               MOVE VARSTAM-IO-AREA (251:250) TO VSREC2 (1:250)
               MOVE VARSTAM-IO-AREA (22:4) TO SSDATO-IO
               MOVE 91                     TO BW-A
               PERFORM VARYING M1A-I FROM M1A-MAX BY -1
                         UNTIL M1A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-M1A-GRP
                   MOVE XI-M1A             TO M1A (M1A-I)
               END-PERFORM
               MOVE 139                    TO BW-A
               PERFORM VARYING M1B-I FROM M1B-MAX BY -1
                         UNTIL M1B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:4) TO XI-M1B-GRP
                   MOVE XI-M1B             TO M1B (M1B-I)
               END-PERFORM
               MOVE 203                    TO BW-A
               PERFORM VARYING M2A-I FROM M2A-MAX BY -1
                         UNTIL M2A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-M2A-GRP
                   MOVE XI-M2A             TO M2A (M2A-I)
               END-PERFORM
               MOVE 251                    TO BW-A
               PERFORM VARYING M2B-I FROM M2B-MAX BY -1
                         UNTIL M2B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:4) TO XI-M2B-GRP
                   MOVE XI-M2B             TO M2B (M2B-I)
               END-PERFORM
               MOVE 315                    TO BW-A
               PERFORM VARYING M3A-I FROM M3A-MAX BY -1
                         UNTIL M3A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-M3A-GRP
                   MOVE XI-M3A             TO M3A (M3A-I)
               END-PERFORM
               MOVE 363                    TO BW-A
               PERFORM VARYING M3B-I FROM M3B-MAX BY -1
                         UNTIL M3B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:4) TO XI-M3B-GRP
                   MOVE XI-M3B             TO M3B (M3B-I)
               END-PERFORM
               MOVE 427                    TO BW-A
               PERFORM VARYING M4A-I FROM M4A-MAX BY -1
                         UNTIL M4A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-M4A-GRP
                   MOVE XI-M4A             TO M4A (M4A-I)
               END-PERFORM
               MOVE 475                    TO BW-A
               PERFORM VARYING M4B-I FROM M4B-MAX BY -1
                         UNTIL M4B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:4) TO XI-M4B-GRP
                   MOVE XI-M4B             TO M4B (M4B-I)
               END-PERFORM
           END-EVALUATE.
 
       VARSTAM-IDSET SECTION.
       VARSTAM-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARSTAM-CHK-LEVEL SECTION.
       VARSTAM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARSTAM-LEVEL-01
               MOVE VARSTAM-IO-AREA (2:3)  TO VARSTAM-01-L2-FIRMA
               MOVE VARSTAM-IO-AREA (5:9)  TO VARSTAM-01-L1-EDBKEY
               IF  VARSTAM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARSTAM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARSTAM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARSTAM-01-L2         TO THE-PRIOR-L2
               MOVE  VARSTAM-01-L1         TO THE-PRIOR-L1
               SET VARSTAM-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VARSTAM-MATCH-SET SECTION.
       VARSTAM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VARSTAM-IO-AREA (2:3)  TO VARSTAM-M-01-M2-FIRMA
               MOVE VARSTAM-IO-AREA (5:9)  TO VARSTAM-M-01-M1-EDBKEY
           END-EVALUATE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARAM-IO-AREA (18:4)   TO AART1 (1:4)
               MOVE PARAM-IO-AREA (20:2)   TO AAR1 (1:2)
               MOVE PARAM-IO-AREA (23:4)   TO AART2 (1:4)
               MOVE PARAM-IO-AREA (25:2)   TO AAR2 (1:2)
               MOVE PARAM-IO-AREA (28:4)   TO AART3 (1:4)
               MOVE PARAM-IO-AREA (30:2)   TO AAR3 (1:2)
               MOVE PARAM-IO-AREA (33:4)   TO AART4 (1:4)
               MOVE PARAM-IO-AREA (35:2)   TO AAR4 (1:2)
               MOVE PARAM-IO-AREA (55:1)   TO MNDOPP (1:1)
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-03                        TO TRUE.
 
       FAKPAR-GET SECTION.
       FAKPAR-GET-P.
           IF  FAKPAR-EOF-OFF
               READ FAKPAR
               AT END
                   SET FAKPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (6:2)   TO PAR (1:2)
               MOVE FAKPAR-IO-AREA (8:2)   TO PMND (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-04                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
       TOTALER-PRINT-LINE SECTION.
       TOTALER-PRINT-LINE-P.
           IF  TOTALER-BEFORE-SKIP > 0
               PERFORM TOTALER-SKIP-BEFORE
           END-IF
           IF  TOTALER-BEFORE-SPACE > 0
               PERFORM TOTALER-SPACE-BEFORE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               IF  TOTALER-AFTER-SPACE > 0
                   PERFORM TOTALER-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               PERFORM TOTALER-SPACE-AFTER
           END-IF
           IF  TOTALER-LINE-COUNT NOT < TOTALER-MAX-LINES
               MOVE 7                      TO TOTALER-AFTER-SKIP
           END-IF.
 
       TOTALER-SKIP-BEFORE SECTION.
       TOTALER-SKIP-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-BEFORE-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-BEFORE SECTION.
       TOTALER-SPACE-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER TOTALER-BEFORE-SPACE
                                                                 LINES
           ADD TOTALER-BEFORE-SPACE        TO TOTALER-LINE-COUNT
           MOVE SPACES TO TOTALER-IO-AREA
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-BEFORE-SPACE.
 
       TOTALER-SKIP-AFTER SECTION.
       TOTALER-SKIP-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-AFTER-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-AFTER SECTION.
       TOTALER-SPACE-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE TOTALER-AFTER-SPACE LINES
           ADD TOTALER-AFTER-SPACE         TO TOTALER-LINE-COUNT
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VARSTAA-EOF
               MOVE HIGH-VALUES            TO VARSTAA-MC
                                              VARSTAA-MP
           END-IF
           IF  VARSTAM-EOF
               MOVE HIGH-VALUES            TO VARSTAM-MC
                                              VARSTAM-MP
           END-IF
           IF  VARSTAA-MC < VARSTAA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VARSTAM-MC < VARSTAM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VARSTAA-MC < VARSTAM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARSTAA-PROCESS     TO TRUE
                   MOVE VARSTAA-MC         TO VARSTAA-MP
                   IF  VARSTAA-MC = VARSTAM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARSTAM-MC < VARSTAA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARSTAM-PROCESS     TO TRUE
                   MOVE VARSTAM-MC         TO VARSTAM-MP
                   IF  VARSTAM-MC = VARSTAA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARSTAA-MC = VARSTAM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARSTAA-PROCESS     TO TRUE
                   MOVE VARSTAA-MC         TO VARSTAA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR AND I-51)
           AND (NOT-I-68)
               MOVE SPACES TO VARSTUT-IO-AREA
               INITIALIZE VARSTUT-IO-AREA
               MOVE VSREC1                 TO VARSTUT-IO-AREA (1:250)
               MOVE VSREC2                 TO VARSTUT-IO-AREA (251:250)
               MOVE '*'                    TO VARSTUT-IO-AREA (1:1)
               IF  (I-23)
                   MOVE VGRALF             TO VARSTUT-IO-AREA (14:8)
               END-IF
               IF  (I-23)
                   MOVE DATO3              TO XO-60P
                   MOVE XO-60P-EF          TO VARSTUT-IO-AREA (22:4)
               END-IF
               MOVE 91                     TO BW-A
               PERFORM VARYING M1A-I FROM M1A-MAX BY -1
                         UNTIL M1A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE M1A (M1A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 139                    TO BW-A
               PERFORM VARYING M1B-I FROM M1B-MAX BY -1
                         UNTIL M1B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE M1B (M1B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               MOVE 203                    TO BW-A
               PERFORM VARYING M2A-I FROM M2A-MAX BY -1
                         UNTIL M2A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE M2A (M2A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 251                    TO BW-A
               PERFORM VARYING M2B-I FROM M2B-MAX BY -1
                         UNTIL M2B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE M2B (M2B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               MOVE 315                    TO BW-A
               PERFORM VARYING M3A-I FROM M3A-MAX BY -1
                         UNTIL M3A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE M3A (M3A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 363                    TO BW-A
               PERFORM VARYING M3B-I FROM M3B-MAX BY -1
                         UNTIL M3B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE M3B (M3B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               MOVE 427                    TO BW-A
               PERFORM VARYING M4A-I FROM M4A-MAX BY -1
                         UNTIL M4A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE M4A (M4A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 475                    TO BW-A
               PERFORM VARYING M4B-I FROM M4B-MAX BY -1
                         UNTIL M4B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE M4B (M4B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               WRITE VARSTUT-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-68)
           OR  (I-01 AND NOT-I-51 AND NOT-I-68)
               MOVE SPACES TO VARSTUT-IO-AREA
               INITIALIZE VARSTUT-IO-AREA
               MOVE VSREC1                 TO VARSTUT-IO-AREA (1:250)
               MOVE VSREC2                 TO VARSTUT-IO-AREA (251:250)
               MOVE '1'                    TO VARSTUT-IO-AREA (1:1)
               IF  (I-61)
                   MOVE 91                 TO BW-A
                   PERFORM VARYING M1A-I FROM M1A-MAX BY -1
                             UNTIL M1A-I < 1
                       SUBTRACT 5        FROM BW-A
                       MOVE M1A (M1A-I)    TO XO-72P
                       MOVE XO-72P-EF      TO VARSTUT-IO-AREA (BW-A:5)
                   END-PERFORM
               END-IF
               IF  (I-61)
                   MOVE 139                TO BW-A
                   PERFORM VARYING M1B-I FROM M1B-MAX BY -1
                             UNTIL M1B-I < 1
                       SUBTRACT 4        FROM BW-A
                       MOVE M1B (M1B-I)    TO XO-70P
                       MOVE XO-70P-EF      TO VARSTUT-IO-AREA (BW-A:4)
                   END-PERFORM
               END-IF
               IF  (I-62)
                   MOVE 203                TO BW-A
                   PERFORM VARYING M2A-I FROM M2A-MAX BY -1
                             UNTIL M2A-I < 1
                       SUBTRACT 5        FROM BW-A
                       MOVE M2A (M2A-I)    TO XO-72P
                       MOVE XO-72P-EF      TO VARSTUT-IO-AREA (BW-A:5)
                   END-PERFORM
               END-IF
               IF  (I-62)
                   MOVE 251                TO BW-A
                   PERFORM VARYING M2B-I FROM M2B-MAX BY -1
                             UNTIL M2B-I < 1
                       SUBTRACT 4        FROM BW-A
                       MOVE M2B (M2B-I)    TO XO-70P
                       MOVE XO-70P-EF      TO VARSTUT-IO-AREA (BW-A:4)
                   END-PERFORM
               END-IF
               WRITE VARSTUT-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-12)
           AND (I-60 AND I-U1)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMA                  TO TOTALER-IO-AREA (2:3)
               MOVE EDBNR                  TO TOTALER-IO-AREA (6:7)
               MOVE 'NULLSTILT'            TO TOTALER-IO-AREA (27:9)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-03)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (3:8)
               MOVE 'AJOURHOLD AV VARE.STAT.' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 'MASTER'               TO TOTALER-IO-AREA (35:6)
               IF  (NOT-I-12)
                   MOVE 'DAGLIG OPPDATERING' TO TOTALER-IO-AREA (45:18)
               END-IF
               IF  (I-12)
                   MOVE 'MÅNEDS OPPDATERING' TO TOTALER-IO-AREA (45:18)
               END-IF
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-04)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FAKTURA PERIODE =  '  TO TOTALER-IO-AREA (12:19)
               MOVE PMND                   TO TOTALER-IO-AREA (31:2)
               MOVE PAR                    TO TOTALER-IO-AREA (34:2)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-50 AND NOT-I-68)
               MOVE SPACES TO VARSTUT-IO-AREA
               INITIALIZE VARSTUT-IO-AREA
               MOVE '+'                    TO VARSTUT-IO-AREA (1:1)
               MOVE ADATA                  TO VARSTUT-IO-AREA (2:24)
               MOVE DATO3                  TO XO-60P
               MOVE XO-60P-EF              TO VARSTUT-IO-AREA (22:4)
               MOVE AART1                  TO VARSTUT-IO-AREA (27:4)
               MOVE 91                     TO BW-A
               PERFORM VARYING A1A-I FROM A1A-MAX BY -1
                         UNTIL A1A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE A1A (A1A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 139                    TO BW-A
               PERFORM VARYING A1B-I FROM A1B-MAX BY -1
                         UNTIL A1B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE A1B (A1B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               MOVE AART2                  TO VARSTUT-IO-AREA (139:4)
               MOVE 203                    TO BW-A
               PERFORM VARYING A2A-I FROM A2A-MAX BY -1
                         UNTIL A2A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE A2A (A2A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 251                    TO BW-A
               PERFORM VARYING A2B-I FROM A2B-MAX BY -1
                         UNTIL A2B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE A2B (A2B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               MOVE AART3                  TO VARSTUT-IO-AREA (251:4)
               MOVE 315                    TO BW-A
               PERFORM VARYING A3A-I FROM A3A-MAX BY -1
                         UNTIL A3A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE A3A (A3A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 363                    TO BW-A
               PERFORM VARYING A3B-I FROM A3B-MAX BY -1
                         UNTIL A3B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE A3B (A3B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               MOVE AART4                  TO VARSTUT-IO-AREA (363:4)
               MOVE 427                    TO BW-A
               PERFORM VARYING A4A-I FROM A4A-MAX BY -1
                         UNTIL A4A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE A4A (A4A-I)        TO XO-72P
                   MOVE XO-72P-EF          TO VARSTUT-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE 475                    TO BW-A
               PERFORM VARYING A4B-I FROM A4B-MAX BY -1
                         UNTIL A4B-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE A4B (A4B-I)        TO XO-70P
                   MOVE XO-70P-EF          TO VARSTUT-IO-AREA (BW-A:4)
               END-PERFORM
               WRITE VARSTUT-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTM                   TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'VARE.STAT.REC. LEST    ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTSLE                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'VARE.STAT.REC. SLETTET ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTA                   TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'AJOURHOLDSREC. LEST.   ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTNYE                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'NYE VARE.STAT.REC.     ' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTOPP                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'VARE.STAT.REC OPPDATERT' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTNUL                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (1:10)
               MOVE 'VARE.STAT.REC NULLSTILT' TO TOTALER-IO-AREA
                                                               (12:23)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
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
           MOVE 4                          TO LR-CHECK
           SET VARSTAA-LEVEL-INIT          TO TRUE
           INITIALIZE VARSTAA-DATA-FIELDS
           SET VARSTAA-EOF-OFF             TO TRUE
           SET VARSTAA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VARSTAA-MC
                                              VARSTAA-MP
           OPEN INPUT VARSTAA
           SET VARSTAM-LEVEL-INIT          TO TRUE
           INITIALIZE VARSTAM-DATA-FIELDS
           SET VARSTAM-EOF-OFF             TO TRUE
           SET VARSTAM-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VARSTAM-MC
                                              VARSTAM-MP
           OPEN INPUT VARSTAM
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT VARSTUT
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           PERFORM VARYING A1A-I FROM 1 BY 1
                     UNTIL A1A-I > A1A-MAX
               INITIALIZE A1A (A1A-I)
           END-PERFORM
           SET A1A-I                       TO 1
           PERFORM VARYING A1B-I FROM 1 BY 1
                     UNTIL A1B-I > A1B-MAX
               INITIALIZE A1B (A1B-I)
           END-PERFORM
           SET A1B-I                       TO 1
           PERFORM VARYING A2A-I FROM 1 BY 1
                     UNTIL A2A-I > A2A-MAX
               INITIALIZE A2A (A2A-I)
           END-PERFORM
           SET A2A-I                       TO 1
           PERFORM VARYING A2B-I FROM 1 BY 1
                     UNTIL A2B-I > A2B-MAX
               INITIALIZE A2B (A2B-I)
           END-PERFORM
           SET A2B-I                       TO 1
           PERFORM VARYING A3A-I FROM 1 BY 1
                     UNTIL A3A-I > A3A-MAX
               INITIALIZE A3A (A3A-I)
           END-PERFORM
           SET A3A-I                       TO 1
           PERFORM VARYING A3B-I FROM 1 BY 1
                     UNTIL A3B-I > A3B-MAX
               INITIALIZE A3B (A3B-I)
           END-PERFORM
           SET A3B-I                       TO 1
           PERFORM VARYING A4A-I FROM 1 BY 1
                     UNTIL A4A-I > A4A-MAX
               INITIALIZE A4A (A4A-I)
           END-PERFORM
           SET A4A-I                       TO 1
           PERFORM VARYING A4B-I FROM 1 BY 1
                     UNTIL A4B-I > A4B-MAX
               INITIALIZE A4B (A4B-I)
           END-PERFORM
           SET A4B-I                       TO 1
           PERFORM VARYING M1A-I FROM 1 BY 1
                     UNTIL M1A-I > M1A-MAX
               INITIALIZE M1A (M1A-I)
           END-PERFORM
           SET M1A-I                       TO 1
           PERFORM VARYING M1B-I FROM 1 BY 1
                     UNTIL M1B-I > M1B-MAX
               INITIALIZE M1B (M1B-I)
           END-PERFORM
           SET M1B-I                       TO 1
           PERFORM VARYING M2A-I FROM 1 BY 1
                     UNTIL M2A-I > M2A-MAX
               INITIALIZE M2A (M2A-I)
           END-PERFORM
           SET M2A-I                       TO 1
           PERFORM VARYING M2B-I FROM 1 BY 1
                     UNTIL M2B-I > M2B-MAX
               INITIALIZE M2B (M2B-I)
           END-PERFORM
           SET M2B-I                       TO 1
           PERFORM VARYING M3A-I FROM 1 BY 1
                     UNTIL M3A-I > M3A-MAX
               INITIALIZE M3A (M3A-I)
           END-PERFORM
           SET M3A-I                       TO 1
           PERFORM VARYING M3B-I FROM 1 BY 1
                     UNTIL M3B-I > M3B-MAX
               INITIALIZE M3B (M3B-I)
           END-PERFORM
           SET M3B-I                       TO 1
           PERFORM VARYING M4A-I FROM 1 BY 1
                     UNTIL M4A-I > M4A-MAX
               INITIALIZE M4A (M4A-I)
           END-PERFORM
           SET M4A-I                       TO 1
           PERFORM VARYING M4B-I FROM 1 BY 1
                     UNTIL M4B-I > M4B-MAX
               INITIALIZE M4B (M4B-I)
           END-PERFORM
           SET M4B-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARSTAA
           CLOSE VARSTAM
           CLOSE PARAM
           CLOSE FAKPAR
           CLOSE FIRMAF
           CLOSE VARSTUT
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
