       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO805R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV RSK.RSK805                 ***TXT***OK MT
      * JCL:DOP.XDOP53D    ******************    ***TXT***   **********
      * JCL:%%%.RES55A     ******************    ***TXT***   **********
      * PROGRAM      : RKO805, DANNER RESKHIS FORMAT.                 *
      * PROGRAMMERER : MORTEN TUVRØNNINGEN                            *
      * ENDRINGER    :
      *E     29.12.16: UTVIDET LINJENR I KTOKUR OG RESKHIS            *
      *                20.01.99 OVERSKRIFT TIL FEILRECORDS BLE SKREVET*
      *                         SAMMEN MED 1. GANGS HEADING.          *
      *                21.01.99 LAGT INN HEADINGLINJE FOR Å SAMLE AV- *
      *                         STEMMING MED ØVRIGE LISTER.           *
      *                11.04.04 RETTET BILNR POS I RESKHIS.           *
      *                09.02.05 TAR MED VALUTABELØP I SALDO.          *
      *                29.09.05 UTVIDET KTOKUR FRA 89 TIL 120 BYTE    *
      *E  23.04.08: ENDRET EDITERING I AVSTEMMINGSSUM                 *
      *E  17.06.08: ENDRET STJERNERAMME RUNDT TOTAL/SUM               *
      *E  09.10.12: TAKLER UTVIDET BELØPSFELT                         *
      *             SKRIVER AVSTEMMINGSFIL                            *
      * FÅR          : KONTOKURANT, SEKV. OG VSAM.                    *
      * GJØR         : SUMMERER AVSTEMNINGSTALL, OG DANNER RESKHIS.   *
      *                VED KJØRING I RES55A (ÅRSAVSLUTNING) DANNES NY *
      *                FILE I SIN HELHET FRA GAMLE KONTOKURANT-FILER. *
      *                VED KJØRING I DOP53A LESES ONLINE FILE I BRUK  *
      *                RESKHIS INN SAMMEN MED DAGENS TRANSER OG DANNER*
      *                NY FILE INKL DAGENS TRANSER.                   *
      * GIR          : KONTOKURRANT-FILE 60-BYTE SEKV.                *
      *                KVITTERINGSLISTE MED TOTALER FOR TRANSER, SALDO*
      *                OG TOTALT PR FIRMA OG TOTALT OG PR RESKONTRO-  *
      *                GRUPPE.                                        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO805.rpg
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
           SELECT KTOKURI
               ASSIGN TO UT-S-KTOKURI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURI-STATUS.
           SELECT RESKHIS
               ASSIGN TO RESKHIS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKHIS-STATUS
               RECORD KEY IS RESKHIS-KEY1.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KTOKURO
               ASSIGN TO UT-S-KTOKURO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURO-STATUS.
           SELECT AVSTEMM
               ASSIGN TO UT-S-AVSTEMM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMM-STATUS.
           SELECT LISTE1
               ASSIGN TO UT-S-LISTE1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE1-STATUS.
           SELECT LISTE2
               ASSIGN TO UT-S-LISTE2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KTOKURI
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  KTOKURI-IO-AREA.
           05  KTOKURI-IO-AREA-X           PICTURE X(200).
       FD RESKHIS
               RECORD CONTAINS 120.
       01  RESKHIS-IO-AREA.
           05  RESKHIS-IO-AREA-X.
               10  RESKHIS-KEY1.
                   15  RESKHIS-KEY1N       PICTURE S9(16).
               10  FILLER                  PICTURE X(104).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KTOKURO
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  KTOKURO-IO-AREA.
           05  KTOKURO-IO-AREA-X           PICTURE X(120).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
       FD LISTE1
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE1-IO-PRINT.
           05  LISTE1-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE1-IO-AREA.
           05  LISTE1-IO-AREA-X            PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE2-IO-PRINT.
           05  LISTE2-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE2-IO-AREA.
           05  LISTE2-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KTOKURI-STATUS              PICTURE 99 VALUE 0.
           10  RESKHIS-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KTOKURO-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE1-STATUS               PICTURE 99 VALUE 0.
           10  LISTE2-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-EOF-OFF         VALUE '0'.
               88  KTOKURI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-READ-OFF        VALUE '0'.
               88  KTOKURI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-PROCESS-OFF     VALUE '0'.
               88  KTOKURI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KTOKURI-LEVEL-INIT-OFF  VALUE '0'.
               88  KTOKURI-LEVEL-INIT      VALUE '1'.
           05  RESKHIS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKHIS-EOF-OFF         VALUE '0'.
               88  RESKHIS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKHIS-READ-OFF        VALUE '0'.
               88  RESKHIS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKHIS-PROCESS-OFF     VALUE '0'.
               88  RESKHIS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKHIS-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKHIS-LEVEL-INIT      VALUE '1'.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  LISTE1-DATA-FIELDS.
               10  LISTE1-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-CLR-IO           PICTURE X VALUE 'Y'.
           05  LISTE2-DATA-FIELDS.
               10  LISTE2-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-CLR-IO           PICTURE X VALUE 'Y'.
           05  KTOKURI-LEVEL-01.
               10  KTOKURI-01-L3.
                   15  KTOKURI-01-L3-FIRMA PICTURE X(3).
               10  KTOKURI-01-L2.
                   15  KTOKURI-01-L2-RESKNR PICTURE X(6).
               10  KTOKURI-01-L1.
                   15  KTOKURI-01-L1-RECART PICTURE X(2).
           05  KTOKURI-DATA-FIELDS.
      *                                       1 200 REC89
               10  RECART                  PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  KRESGR                  PICTURE X(1).
               10  RESKNR                  PICTURE X(6).
               10  TRAKOD                  PICTURE X(2).
               10  BILDAT-IO.
                   15  BILDAT              PICTURE S9(6).
               10  BILNR-IO.
                   15  BILNR               PICTURE S9(6).
               10  REFNR-IO.
                   15  REFNR               PICTURE S9(6).
               10  FFDATO-IO.
                   15  FFDATO              PICTURE S9(6).
               10  TBELOP-IO.
                   15  TBELOP              PICTURE S9(7)V9(2).
               10  SBELOP-IO.
                   15  SBELOP              PICTURE S9(8)V9(2).
               10  BETM                    PICTURE X(2).
               10  VBEL-IO.
                   15  VBEL                PICTURE S9(8)V9(2).
               10  SVBEL-IO.
                   15  SVBEL               PICTURE S9(9)V9(2).
               10  BILART                  PICTURE X(1).
               10  VT                      PICTURE X(1).
               10  KBARH                   PICTURE X(2).
               10  KPER                    PICTURE X(6).
               10  KAAR                    PICTURE X(4).
               10  TEKST                   PICTURE X(24).
               10  BELU-IO.
                   15  BELU                PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALBEU-IO.
                   15  VALBEU              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  VTU                     PICTURE X(3).
               10  OPPHAV                  PICTURE X(4).
               10  PRDDTO-IO.
                   15  PRDDTO              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  PRDKLK-IO.
                   15  PRDKLK              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  KTOKURI-MP                  PICTURE X(9).
           05  KTOKURI-MC                  PICTURE X(9).
           05  KTOKURI-M-01            REDEFINES KTOKURI-MC.
               10  KTOKURI-M-01-M2.
                   15  KTOKURI-M-01-M2-FIRMA-G.
                       20  KTOKURI-M-01-M2-FIRMA PICTURE X(3).
               10  KTOKURI-M-01-M1.
                   15  KTOKURI-M-01-M1-RESKNR-G.
                       20  KTOKURI-M-01-M1-RESKNR PICTURE X(6).
           05  RESKHIS-LEVEL-04.
               10  RESKHIS-04-L3.
                   15  RESKHIS-04-L3-FIRMA PICTURE X(3).
               10  RESKHIS-04-L2.
                   15  RESKHIS-04-L2-RESKNR PICTURE X(6).
           05  RESKHIS-DATA-FIELDS.
               10  REC120                  PICTURE X(120).
               10  RRESGR                  PICTURE X(1).
               10  RBILD-IO.
                   15  RBILD               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RBILNR-IO.
                   15  RBILNR              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RBEL-IO.
                   15  RBEL                PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RBARH                   PICTURE X(2).
               10  RPER                    PICTURE X(6).
               10  RARH                    PICTURE X(2).
               10  RAAR                    PICTURE X(4).
               10  RAAR2                   PICTURE X(2).
      *****************************************************************
      * DANNER 6-SIFRET HJELPEFELT MED 0 FOR SALDO-REC.               *
      *****************************************************************
      * N12                Z-ADD0         NULL5   50        HJ NULLFELT
           05  RESKHIS-MP                  PICTURE X(9).
           05  RESKHIS-MC                  PICTURE X(9).
           05  RESKHIS-M-04            REDEFINES RESKHIS-MC.
               10  RESKHIS-M-04-M2.
                   15  RESKHIS-M-04-M2-FIRMA-G.
                       20  RESKHIS-M-04-M2-FIRMA PICTURE X(3).
               10  RESKHIS-M-04-M1.
                   15  RESKHIS-M-04-M1-RESKNR-G.
                       20  RESKHIS-M-04-M1-RESKNR PICTURE X(6).
           05  AUTOPAR-DATA-FIELDS.
               10  PARPER                  PICTURE X(6).
               10  PARAAR                  PICTURE X(4).
           05  FIRMAF-DATA-FIELDS.
      *                                       8  37 FNA
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  NULL6-IO.
                   15  NULL6               PICTURE S9(6).
               10  NULL7-IO.
                   15  NULL7               PICTURE S9(7).
               10  AAR4                    PICTURE X(4).
               10  MMDD                    PICTURE X(4).
               10  DDTO-IO.
                   15  DDTO                PICTURE S9(8).
               10  DKLK-IO.
                   15  DKLK                PICTURE S9(6).
               10  SVT                     PICTURE X(1).
               10  REGPER                  PICTURE X(6).
               10  REGAAR                  PICTURE X(4).
               10  BILARH                  PICTURE X(2).
               10  RESGRP                  PICTURE X(1).
               10  HJBEL-IO.
                   15  HJBEL               PICTURE S9(9)V9(2).
               10  HJVBEL-IO.
                   15  HJVBEL              PICTURE S9(9)V9(2).
               10  HJTBEL-IO.
                   15  HJTBEL              PICTURE S9(9)V9(2).
               10  BL3TOT-IO.
                   15  BL3TOT              PICTURE S9(9)V9(2).
               10  BLRTOT-IO.
                   15  BLRTOT              PICTURE S9(9)V9(2).
               10  BG0TOT-IO.
                   15  BG0TOT              PICTURE S9(9)V9(2).
               10  BG1TOT-IO.
                   15  BG1TOT              PICTURE S9(9)V9(2).
               10  BG2TOT-IO.
                   15  BG2TOT              PICTURE S9(9)V9(2).
               10  BG3TOT-IO.
                   15  BG3TOT              PICTURE S9(9)V9(2).
               10  BG4TOT-IO.
                   15  BG4TOT              PICTURE S9(9)V9(2).
               10  BG5TOT-IO.
                   15  BG5TOT              PICTURE S9(9)V9(2).
               10  BG6TOT-IO.
                   15  BG6TOT              PICTURE S9(9)V9(2).
               10  BG7TOT-IO.
                   15  BG7TOT              PICTURE S9(9)V9(2).
               10  BG8TOT-IO.
                   15  BG8TOT              PICTURE S9(9)V9(2).
               10  BG9TOT-IO.
                   15  BG9TOT              PICTURE S9(9)V9(2).
               10  BGXTOT-IO.
                   15  BGXTOT              PICTURE S9(9)V9(2).
               10  BG0TOU-IO.
                   15  BG0TOU              PICTURE S9(11)V9(2).
               10  BG1TOU-IO.
                   15  BG1TOU              PICTURE S9(11)V9(2).
               10  BG2TOU-IO.
                   15  BG2TOU              PICTURE S9(11)V9(2).
               10  BG3TOU-IO.
                   15  BG3TOU              PICTURE S9(11)V9(2).
               10  BG4TOU-IO.
                   15  BG4TOU              PICTURE S9(11)V9(2).
               10  BG5TOU-IO.
                   15  BG5TOU              PICTURE S9(11)V9(2).
               10  BG6TOU-IO.
                   15  BG6TOU              PICTURE S9(11)V9(2).
               10  BG7TOU-IO.
                   15  BG7TOU              PICTURE S9(11)V9(2).
               10  BG8TOU-IO.
                   15  BG8TOU              PICTURE S9(11)V9(2).
               10  BG9TOU-IO.
                   15  BG9TOU              PICTURE S9(11)V9(2).
               10  BGXTOU-IO.
                   15  BGXTOU              PICTURE S9(11)V9(2).
               10  BGTFJO-IO.
                   15  BGTFJO              PICTURE S9(9)V9(2).
               10  BGTFJU-IO.
                   15  BGTFJU              PICTURE S9(11)V9(2).
               10  BGTHIA-ELG-IO.
                   15  BGTHIA-ELG          PICTURE S9(9)V9(2).
               10  BGTHIU-IO.
                   15  BGTHIU              PICTURE S9(11)V9(2).
               10  BGTTOT-IO.
                   15  BGTTOT              PICTURE S9(9)V9(2).
               10  LRRSBE-IO.
                   15  LRRSBE              PICTURE S9(13)V9(2).
               10  BGTTOU-IO.
                   15  BGTTOU              PICTURE S9(11)V9(2).
               10  LRRSBU-IO.
                   15  LRRSBU              PICTURE S9(13)V9(2).
               10  AGTTOT-IO.
                   15  AGTTOT              PICTURE S9(9).
               10  LRRSAN-IO.
                   15  LRRSAN              PICTURE S9(13).
               10  AGTTOU-IO.
                   15  AGTTOU              PICTURE S9(9).
               10  AUTKEY                  PICTURE X(3).
               10  HDATO-IO.
                   15  HDATO               PICTURE S9(6).
               10  HAAR                    PICTURE X(2).
               10  HMND                    PICTURE X(2).
               10  HARH                    PICTURE X(2).
               10  BL3HIA-ELG-IO.
                   15  BL3HIA-ELG          PICTURE S9(9)V9(2).
               10  BL3FJO-IO.
                   15  BL3FJO              PICTURE S9(9)V9(2).
               10  AL3TOT-IO.
                   15  AL3TOT              PICTURE S9(8).
               10  BL3HIU-IO.
                   15  BL3HIU              PICTURE S9(11)V9(2).
               10  BL3FJU-IO.
                   15  BL3FJU              PICTURE S9(11)V9(2).
               10  AL3TOU-IO.
                   15  AL3TOU              PICTURE S9(8).
               10  L1SBEL-IO.
                   15  L1SBEL              PICTURE S9(9)V9(2).
               10  L1SBEU-IO.
                   15  L1SBEU              PICTURE S9(11)V9(2).
               10  L1VBEL-IO.
                   15  L1VBEL              PICTURE S9(9)V9(2).
               10  L1VBEU-IO.
                   15  L1VBEU              PICTURE S9(11)V9(4).
               10  BLRHIA-ELG-IO.
                   15  BLRHIA-ELG          PICTURE S9(9)V9(2).
               10  BLRFJO-IO.
                   15  BLRFJO              PICTURE S9(9)V9(2).
               10  ALRTOT-IO.
                   15  ALRTOT              PICTURE S9(8).
               10  BLRHIU-IO.
                   15  BLRHIU              PICTURE S9(11)V9(2).
               10  BLRFJU-IO.
                   15  BLRFJU              PICTURE S9(11)V9(2).
               10  ALRTOU-IO.
                   15  ALRTOU              PICTURE S9(8).
               10  BG0HIA-ELG-IO.
                   15  BG0HIA-ELG          PICTURE S9(9)V9(2).
               10  BG0FJO-IO.
                   15  BG0FJO              PICTURE S9(9)V9(2).
               10  AG0TOT-IO.
                   15  AG0TOT              PICTURE S9(8).
               10  BG0HIU-IO.
                   15  BG0HIU              PICTURE S9(11)V9(2).
               10  BG0FJU-IO.
                   15  BG0FJU              PICTURE S9(11)V9(2).
               10  AG0TOU-IO.
                   15  AG0TOU              PICTURE S9(8).
               10  BG1HIA-ELG-IO.
                   15  BG1HIA-ELG          PICTURE S9(9)V9(2).
               10  BG1FJO-IO.
                   15  BG1FJO              PICTURE S9(9)V9(2).
               10  AG1TOT-IO.
                   15  AG1TOT              PICTURE S9(8).
               10  BG1HIU-IO.
                   15  BG1HIU              PICTURE S9(11)V9(2).
               10  BG1FJU-IO.
                   15  BG1FJU              PICTURE S9(11)V9(2).
               10  AG1TOU-IO.
                   15  AG1TOU              PICTURE S9(8).
               10  BG2HIA-ELG-IO.
                   15  BG2HIA-ELG          PICTURE S9(9)V9(2).
               10  BG2FJO-IO.
                   15  BG2FJO              PICTURE S9(9)V9(2).
               10  AG2TOT-IO.
                   15  AG2TOT              PICTURE S9(8).
               10  BG2HIU-IO.
                   15  BG2HIU              PICTURE S9(11)V9(2).
               10  BG2FJU-IO.
                   15  BG2FJU              PICTURE S9(11)V9(2).
               10  AG2TOU-IO.
                   15  AG2TOU              PICTURE S9(8).
               10  BG3HIA-ELG-IO.
                   15  BG3HIA-ELG          PICTURE S9(9)V9(2).
               10  BG3FJO-IO.
                   15  BG3FJO              PICTURE S9(9)V9(2).
               10  AG3TOT-IO.
                   15  AG3TOT              PICTURE S9(8).
               10  BG3HIU-IO.
                   15  BG3HIU              PICTURE S9(11)V9(2).
               10  BG3FJU-IO.
                   15  BG3FJU              PICTURE S9(11)V9(2).
               10  AG3TOU-IO.
                   15  AG3TOU              PICTURE S9(8).
               10  BG4HIA-ELG-IO.
                   15  BG4HIA-ELG          PICTURE S9(9)V9(2).
               10  BG4FJO-IO.
                   15  BG4FJO              PICTURE S9(9)V9(2).
               10  AG4TOT-IO.
                   15  AG4TOT              PICTURE S9(8).
               10  BG4HIU-IO.
                   15  BG4HIU              PICTURE S9(11)V9(2).
               10  BG4FJU-IO.
                   15  BG4FJU              PICTURE S9(11)V9(2).
               10  AG4TOU-IO.
                   15  AG4TOU              PICTURE S9(8).
               10  BG5HIA-ELG-IO.
                   15  BG5HIA-ELG          PICTURE S9(9)V9(2).
               10  BG5FJO-IO.
                   15  BG5FJO              PICTURE S9(9)V9(2).
               10  AG5TOT-IO.
                   15  AG5TOT              PICTURE S9(8).
               10  BG5HIU-IO.
                   15  BG5HIU              PICTURE S9(11)V9(2).
               10  BG5FJU-IO.
                   15  BG5FJU              PICTURE S9(11)V9(2).
               10  AG5TOU-IO.
                   15  AG5TOU              PICTURE S9(8).
               10  BG6HIA-ELG-IO.
                   15  BG6HIA-ELG          PICTURE S9(9)V9(2).
               10  BG6FJO-IO.
                   15  BG6FJO              PICTURE S9(9)V9(2).
               10  AG6TOT-IO.
                   15  AG6TOT              PICTURE S9(8).
               10  BG6HIU-IO.
                   15  BG6HIU              PICTURE S9(11)V9(2).
               10  BG6FJU-IO.
                   15  BG6FJU              PICTURE S9(11)V9(2).
               10  AG6TOU-IO.
                   15  AG6TOU              PICTURE S9(8).
               10  BG7HIA-ELG-IO.
                   15  BG7HIA-ELG          PICTURE S9(9)V9(2).
               10  BG7FJO-IO.
                   15  BG7FJO              PICTURE S9(9)V9(2).
               10  AG7TOT-IO.
                   15  AG7TOT              PICTURE S9(8).
               10  BG7HIU-IO.
                   15  BG7HIU              PICTURE S9(11)V9(2).
               10  BG7FJU-IO.
                   15  BG7FJU              PICTURE S9(11)V9(2).
               10  AG7TOU-IO.
                   15  AG7TOU              PICTURE S9(8).
               10  BG8HIA-ELG-IO.
                   15  BG8HIA-ELG          PICTURE S9(9)V9(2).
               10  BG8FJO-IO.
                   15  BG8FJO              PICTURE S9(9)V9(2).
               10  AG8TOT-IO.
                   15  AG8TOT              PICTURE S9(8).
               10  BG8HIU-IO.
                   15  BG8HIU              PICTURE S9(11)V9(2).
               10  BG8FJU-IO.
                   15  BG8FJU              PICTURE S9(11)V9(2).
               10  AG8TOU-IO.
                   15  AG8TOU              PICTURE S9(8).
               10  BG9HIA-ELG-IO.
                   15  BG9HIA-ELG          PICTURE S9(9)V9(2).
               10  BG9FJO-IO.
                   15  BG9FJO              PICTURE S9(9)V9(2).
               10  AG9TOT-IO.
                   15  AG9TOT              PICTURE S9(8).
               10  BG9HIU-IO.
                   15  BG9HIU              PICTURE S9(11)V9(2).
               10  BG9FJU-IO.
                   15  BG9FJU              PICTURE S9(11)V9(2).
               10  AG9TOU-IO.
                   15  AG9TOU              PICTURE S9(8).
               10  BGXHIA-ELG-IO.
                   15  BGXHIA-ELG          PICTURE S9(9)V9(2).
               10  BGXFJO-IO.
                   15  BGXFJO              PICTURE S9(9)V9(2).
               10  AGXTOT-IO.
                   15  AGXTOT              PICTURE S9(8).
               10  BGXHIU-IO.
                   15  BGXHIU              PICTURE S9(11)V9(2).
               10  BGXFJU-IO.
                   15  BGXFJU              PICTURE S9(11)V9(2).
               10  AGXTOU-IO.
                   15  AGXTOU              PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-82P-EF.
                 15  XO-82P                PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-112P-EF.
                 15  XO-112P               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-114P-EF.
                 15  XO-114P               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80P-EF.
                 15  XO-80P                PICTURE S9(8) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
               10  EDIT-BLRTOT             PICTURE ZZZZZZZZZ,ZZ-.
               10  XO-90YY9R               PICTURE ZZZ.ZZZ.ZZ9-.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KTOKURI-PROCESS
               SET KTOKURI-PROCESS-OFF     TO TRUE
               SET KTOKURI-READ            TO TRUE
           END-IF
 
           IF  KTOKURI-READ
               PERFORM KTOKURI-GET
               SET KTOKURI-READ-OFF        TO TRUE
               IF  NOT KTOKURI-EOF
                   PERFORM KTOKURI-MATCH-SET
               END-IF
           END-IF
 
           IF  RESKHIS-PROCESS
               SET RESKHIS-PROCESS-OFF     TO TRUE
               SET RESKHIS-READ            TO TRUE
           END-IF
 
           IF  RESKHIS-READ
               PERFORM RESKHIS-GET
               SET RESKHIS-READ-OFF        TO TRUE
               IF  NOT RESKHIS-EOF
                   PERFORM RESKHIS-MATCH-SET
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
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-IDSET
           END-IF
 
           IF  RESKHIS-PROCESS
               PERFORM RESKHIS-IDSET
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-CHK-LEVEL
           END-IF
 
           IF  RESKHIS-PROCESS
               PERFORM RESKHIS-CHK-LEVEL
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
           PERFORM TOTAL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-FLDOFF
               PERFORM KTOKURI-FLDSET
           END-IF
 
           IF  RESKHIS-PROCESS
               PERFORM RESKHIS-FLDOFF
               PERFORM RESKHIS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KTOKURI-PROCESS
           OR  RESKHIS-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-12)
               MOVE 0                      TO NULL6
               MOVE 0                      TO NULL7
               PERFORM PARRUT-S
           END-IF
           IF  (NOT-I-12)
               MOVE UYEAR                  TO AAR4 (3:2)
               SET NOT-I-71                TO TRUE
               IF  UYEAR > 80
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-12 AND I-71)
               MOVE '19'                   TO AAR4 (1:2)
           END-IF
           IF  (NOT-I-12 AND NOT-I-71)
               MOVE '20'                   TO AAR4 (1:2)
           END-IF
           IF  (NOT-I-12)
               MOVE UMONTH                 TO MMDD (1:2)
               MOVE UDAY                   TO MMDD (3:2)
               MOVE AAR4                   TO DDTO (1:4)
               MOVE MMDD                   TO DDTO-IO (5:4)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO DKLK (1:6)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-04 AND NOT-I-U1)
               GO TO SLUTT-T
      *
      *****************************************************************
      * BRUDD PÅ FIRMANR:                                             *
      * - SJEKKER OM FIRMA ER SLETTET.                                *
      *****************************************************************
           END-IF
           IF  (I-L3)
               PERFORM FISLET-S
      *****************************************************************
      * BRUDD PÅ RECART:                                              *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE ' '                    TO SVT
      *
      *****************************************************************
      * BRUDD PÅ RECORDART:                                           *
      * - SJEKKER OM DET SALDO-RECORD ELLER TRANS.                    *
      *****************************************************************
           END-IF
           SET NOT-I-16                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-21                    TO TRUE
           IF  (I-01)
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               IF  RECART NOT = '30'
                   SET I-10                TO TRUE
               END-IF
               IF  RECART = '30'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-17)
               OR  (I-04 AND I-18)
               OR  (I-04 AND I-19)
               PERFORM DTORUT-S
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  (I-01)
               MOVE KPER                   TO REGPER
               MOVE KAAR                   TO REGAAR
               MOVE KBARH                  TO BILARH
           END-IF
           IF  (I-04)
               MOVE RPER                   TO REGPER
               MOVE RAAR                   TO REGAAR
               MOVE RBARH                  TO BILARH
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  REGAAR < PARAAR
               SET I-13                    TO TRUE
           END-IF
           IF  (I-13)
               SET NOT-I-14                TO TRUE
               IF  REGAAR < '1980'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-14)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE KRESGR                 TO RESGRP
           END-IF
           IF  (I-04)
               MOVE RRESGR                 TO RESGRP
           END-IF
           IF  (I-01 AND I-U1)
               ADD TBELOP TO ZERO      GIVING HJBEL
           END-IF
           IF  (I-01 AND NOT-I-U1 AND I-10)
               ADD TBELOP TO ZERO      GIVING HJBEL
           END-IF
           IF  (I-01 AND NOT-I-U1 AND I-11)
               ADD SBELOP TO ZERO      GIVING HJBEL
               ADD SVBEL TO ZERO       GIVING HJVBEL
               SET NOT-I-21                TO TRUE
               IF  VT > ' '
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-U1 AND I-11)
               AND (I-21)
               MOVE VT                     TO SVT
           END-IF
           IF  (I-04)
               ADD RBEL TO ZERO        GIVING HJBEL
           END-IF
           IF  (I-01 AND I-10)
               ADD HJBEL TO ZERO       GIVING HJTBEL
           END-IF
           IF  (I-01)
               OR  (I-04)
               PERFORM KTORUT-S
           END-IF.
 
       SLUTT-T.
      *                    MOVE "RESKNR  "BUGFL1  8        LEDETXT DEBUG
      *          BUGFL1    DEBUGBUGFILO   RESKNR           VIS FELT/IND
      * SUMMERER TOTALER
           CONTINUE.
 
       PARRUT-S SECTION.
       PARRUT-S-P.
           MOVE 'A01'                      TO AUTKEY
           MOVE AUTKEY                     TO AUTOPAR-KEY1
           READ AUTOPAR RECORD KEY IS AUTOPAR-KEY1
           INVALID KEY
               SET I-30                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-30                TO TRUE
               PERFORM AUTOPAR-FLDOFF
               PERFORM AUTOPAR-FLDSET
               PERFORM AUTOPAR-IDSET
           END-READ
           IF  (I-03 AND NOT-I-30 AND I-15)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30)
               SET I-LR                    TO TRUE
           END-IF.
      *
      ****************************************************************  *
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA                      *
      ****************************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           SET NOT-I-98                    TO TRUE
           IF  FIRMA = '   '
               SET I-98                    TO TRUE
           END-IF
           IF  (I-98)
               GO TO FIEND-T
           END-IF
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-96 AND I-02)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
 
       FIEND-T.
           CONTINUE.
      ****************************************************************  *
      *    SUBRUTINE FOR Å SETTE INN ÅRHUNDRE I PERIODE.             *
      ****************************************************************
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           IF  (I-18)
               ADD RBILD TO ZERO       GIVING HDATO
               MOVE HDATO (1:2)            TO HAAR
               MOVE HDATO (5:2)            TO HMND
           END-IF
           IF  (I-17 AND NOT-I-18)
               MOVE RAAR2                  TO HAAR
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  HAAR > '80'
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               MOVE '19'                   TO HARH
           END-IF
           IF  (NOT-I-80)
               MOVE '20'                   TO HARH
           END-IF
           IF  (I-19)
               MOVE HARH                   TO RBARH
           END-IF
           IF  (I-17)
               MOVE HARH                   TO RAAR (1:2)
           END-IF
           IF  (I-18)
               MOVE HARH                   TO RAAR (1:2)
               MOVE HAAR                   TO RAAR2
               MOVE HMND                   TO RPER (5:2)
           END-IF.
      ****************************************************************  *
      *    SUBRUTINE FOR Å AKKUMULERE TOTALER FRA KONTOKURANT (ÅRETS *
      *    ELLER DAGENS).                                            *
      ****************************************************************
 
       KTORUT-S SECTION.
       KTORUT-S-P.
      * AKKUMULERER PR FIRMA
           IF  (NOT-I-13)
               ADD HJBEL                   TO BL3HIA-ELG
           END-IF
           IF  (I-13)
               ADD HJBEL                   TO BL3FJO
           END-IF
           ADD 1                           TO AL3TOT
           IF  (NOT-I-13)
               ADD HJBEL                   TO BL3HIU
           END-IF
           IF  (I-13)
               ADD HJBEL                   TO BL3FJU
           END-IF
           ADD 1                           TO AL3TOU
           IF  (I-98)
               GO TO KTOEND-T
      * AKKUMULERER SALDOBELØP TIL NY SALDOREC PR RESKNR
           END-IF
           IF  (I-11 AND NOT-I-U1)
               ADD HJBEL                   TO L1SBEL
               ADD HJBEL                   TO L1SBEU
               ADD HJVBEL                  TO L1VBEL
               ADD HJVBEL                  TO L1VBEU
      * AKKUMULERER TOTALT
           END-IF
           IF  (NOT-I-13)
               ADD HJBEL                   TO BLRHIA-ELG
           END-IF
           IF  (I-13)
               ADD HJBEL                   TO BLRFJO
           END-IF
           ADD 1                           TO ALRTOT
           IF  (NOT-I-13)
               ADD HJBEL                   TO BLRHIU
           END-IF
           IF  (I-13)
               ADD HJBEL                   TO BLRFJU
           END-IF
           ADD 1                           TO ALRTOU
      * AKKUMULERER PR RESKONTROGRP
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '0'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG0HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG0FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG0TOT
               ADD 1                       TO AG0TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG0HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG0FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG0TOU
               ADD 1                       TO AG0TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '1'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG1HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG1FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG1TOT
               ADD 1                       TO AG1TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG1HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG1FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG1TOU
               ADD 1                       TO AG1TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '2'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG2HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG2FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG2TOT
               ADD 1                       TO AG2TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG2HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG2FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG2TOU
               ADD 1                       TO AG2TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '3'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG3HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG3FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG3TOT
               ADD 1                       TO AG3TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG3HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG3FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG3TOU
               ADD 1                       TO AG3TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '4'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG4HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG4FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG4TOT
               ADD 1                       TO AG4TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG4HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG4FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG4TOU
               ADD 1                       TO AG4TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '5'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG5HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG5FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG5TOT
               ADD 1                       TO AG5TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG5HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG5FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG5TOU
               ADD 1                       TO AG5TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '6'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG6HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG6FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG6TOT
               ADD 1                       TO AG6TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG6HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG6FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG6TOU
               ADD 1                       TO AG6TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '7'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG7HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG7FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG7TOT
               ADD 1                       TO AG7TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG7HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG7FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG7TOU
               ADD 1                       TO AG7TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '8'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG8HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG8FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG8TOT
               ADD 1                       TO AG8TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG8HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG8FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG8TOU
               ADD 1                       TO AG8TOU
               GO TO KTOEND-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  RESGRP = '9'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG9HIA-ELG
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG9FJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG9TOT
               ADD 1                       TO AG9TOT
           END-IF
           IF  (I-20 AND NOT-I-13)
               ADD HJBEL                   TO BG9HIU
           END-IF
           IF  (I-20 AND I-13)
               ADD HJBEL                   TO BG9FJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BG9TOU
               ADD 1                       TO AG9TOU
               GO TO KTOEND-T
           END-IF
           SET I-22                        TO TRUE
           IF  (I-22 AND NOT-I-23)
               SET I-23                    TO TRUE
           END-IF
           IF  (NOT-I-13)
               ADD HJBEL                   TO BGXHIA-ELG
           END-IF
           IF  (I-13)
               ADD HJBEL                   TO BGXFJO
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BGXTOT
               ADD 1                       TO AGXTOT
           END-IF
           IF  (NOT-I-13)
               ADD HJBEL                   TO BGXHIU
           END-IF
           IF  (I-13)
               ADD HJBEL                   TO BGXFJU
           END-IF
           IF  (I-20)
               ADD HJBEL                   TO BGXTOU
               ADD 1 TO AGXTOT         GIVING AGXTOU
           END-IF.
 
       KTOEND-T.
           CONTINUE.
      *
      *
      *****************************************************************
      * SKRIVER ORDINÆR TRANSAKSJON:                                  *
      *****************************************************************
      *
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L3)
               ADD BL3FJO TO BL3HIA-ELG GIVING BL3TOT
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD BLRFJO TO BLRHIA-ELG    GIVING BLRTOT
      *3         BL3HIU    ADD  BL3FJU    BL3TOU 132 TAS MED NÅR KUN UT-
      *R         BLRHIU    ADD  BLRFJU    BLRTOU 132 VIDET BELØP BRUKES.
      * SUMMERER TOTALER FOR RESKONTROGRUPPER FRA KONTOKURANT
           ADD BG0FJO TO BG0HIA-ELG    GIVING BG0TOT
           ADD BG1FJO TO BG1HIA-ELG    GIVING BG1TOT
           ADD BG2FJO TO BG2HIA-ELG    GIVING BG2TOT
           ADD BG3FJO TO BG3HIA-ELG    GIVING BG3TOT
           ADD BG4FJO TO BG4HIA-ELG    GIVING BG4TOT
           ADD BG5FJO TO BG5HIA-ELG    GIVING BG5TOT
           ADD BG6FJO TO BG6HIA-ELG    GIVING BG6TOT
           ADD BG7FJO TO BG7HIA-ELG    GIVING BG7TOT
           ADD BG8FJO TO BG8HIA-ELG    GIVING BG8TOT
           ADD BG9FJO TO BG9HIA-ELG    GIVING BG9TOT
           IF  (I-23)
               ADD BGXFJO TO BGXHIA-ELG GIVING BGXTOT
           END-IF
           ADD BG0FJU TO BG0HIU        GIVING BG0TOU
           ADD BG1FJU TO BG1HIU        GIVING BG1TOU
           ADD BG2FJU TO BG2HIU        GIVING BG2TOU
           ADD BG3FJU TO BG3HIU        GIVING BG3TOU
           ADD BG4FJU TO BG4HIU        GIVING BG4TOU
           ADD BG5FJU TO BG5HIU        GIVING BG5TOU
           ADD BG6FJU TO BG6HIU        GIVING BG6TOU
           ADD BG7FJU TO BG7HIU        GIVING BG7TOU
           ADD BG8FJU TO BG8HIU        GIVING BG8TOU
           ADD BG9FJU TO BG9HIU        GIVING BG9TOU
           IF  (I-23)
               ADD BGXFJU TO BGXHIU    GIVING BGXTOU
      * GRANDTOTAL RESKGRUPPER
           END-IF
           ADD BG0FJO                      TO BGTFJO
           ADD BG1FJO                      TO BGTFJO
           ADD BG2FJO                      TO BGTFJO
           ADD BG3FJO                      TO BGTFJO
           ADD BG4FJO                      TO BGTFJO
           ADD BG5FJO                      TO BGTFJO
           ADD BG6FJO                      TO BGTFJO
           ADD BG7FJO                      TO BGTFJO
           ADD BG8FJO                      TO BGTFJO
           ADD BG9FJO                      TO BGTFJO
           IF  (I-23)
               ADD BGXFJO                  TO BGTFJO
           END-IF
           ADD BG0FJO                      TO BGTFJU
           ADD BG1FJO                      TO BGTFJU
           ADD BG2FJO                      TO BGTFJU
           ADD BG3FJO                      TO BGTFJU
           ADD BG4FJO                      TO BGTFJU
           ADD BG5FJO                      TO BGTFJU
           ADD BG6FJO                      TO BGTFJU
           ADD BG7FJO                      TO BGTFJU
           ADD BG8FJO                      TO BGTFJU
           ADD BG9FJO                      TO BGTFJU
           IF  (I-23)
               ADD BGXFJO                  TO BGTFJU
      *
           END-IF
           ADD BG0HIA-ELG                  TO BGTHIA-ELG
           ADD BG1HIA-ELG                  TO BGTHIA-ELG
           ADD BG2HIA-ELG                  TO BGTHIA-ELG
           ADD BG3HIA-ELG                  TO BGTHIA-ELG
           ADD BG4HIA-ELG                  TO BGTHIA-ELG
           ADD BG5HIA-ELG                  TO BGTHIA-ELG
           ADD BG6HIA-ELG                  TO BGTHIA-ELG
           ADD BG7HIA-ELG                  TO BGTHIA-ELG
           ADD BG8HIA-ELG                  TO BGTHIA-ELG
           ADD BG9HIA-ELG                  TO BGTHIA-ELG
           IF  (I-23)
               ADD BGXHIA-ELG              TO BGTHIA-ELG
           END-IF
           ADD BG0HIU                      TO BGTHIU
           ADD BG1HIU                      TO BGTHIU
           ADD BG2HIU                      TO BGTHIU
           ADD BG3HIU                      TO BGTHIU
           ADD BG4HIU                      TO BGTHIU
           ADD BG5HIU                      TO BGTHIU
           ADD BG6HIU                      TO BGTHIU
           ADD BG7HIU                      TO BGTHIU
           ADD BG8HIU                      TO BGTHIU
           ADD BG9HIU                      TO BGTHIU
           IF  (I-23)
               ADD BGXHIU                  TO BGTHIU
      *
           END-IF
           ADD BG0TOT                      TO BGTTOT
           ADD BG1TOT                      TO BGTTOT
           ADD BG2TOT                      TO BGTTOT
           ADD BG3TOT                      TO BGTTOT
           ADD BG4TOT                      TO BGTTOT
           ADD BG5TOT                      TO BGTTOT
           ADD BG6TOT                      TO BGTTOT
           ADD BG7TOT                      TO BGTTOT
           ADD BG8TOT                      TO BGTTOT
           ADD BG9TOT                      TO BGTTOT
           IF  (I-23)
               ADD BGXTOT                  TO BGTTOT
           END-IF
           ADD BGTTOT TO ZERO          GIVING LRRSBE
           ADD BG0TOU                      TO BGTTOU
           ADD BG1TOU                      TO BGTTOU
           ADD BG2TOU                      TO BGTTOU
           ADD BG3TOU                      TO BGTTOU
           ADD BG4TOU                      TO BGTTOU
           ADD BG5TOU                      TO BGTTOU
           ADD BG6TOU                      TO BGTTOU
           ADD BG7TOU                      TO BGTTOU
           ADD BG8TOU                      TO BGTTOU
           ADD BG9TOU                      TO BGTTOU
           IF  (I-23)
               ADD BGXTOU                  TO BGTTOU
           END-IF
           ADD BGTTOU TO ZERO          GIVING LRRSBU
      *
           ADD AG0TOT                      TO AGTTOT
           ADD AG1TOT                      TO AGTTOT
           ADD AG2TOT                      TO AGTTOT
           ADD AG3TOT                      TO AGTTOT
           ADD AG4TOT                      TO AGTTOT
           ADD AG5TOT                      TO AGTTOT
           ADD AG6TOT                      TO AGTTOT
           ADD AG7TOT                      TO AGTTOT
           ADD AG8TOT                      TO AGTTOT
           ADD AG9TOT                      TO AGTTOT
           IF  (I-23)
               ADD AGXTOT                  TO AGTTOT
           END-IF
           ADD AGTTOT TO ZERO          GIVING LRRSAN
           ADD AG0TOU                      TO AGTTOU
           ADD AG1TOU                      TO AGTTOU
           ADD AG2TOU                      TO AGTTOU
           ADD AG3TOU                      TO AGTTOU
           ADD AG4TOU                      TO AGTTOU
           ADD AG5TOU                      TO AGTTOU
           ADD AG6TOU                      TO AGTTOU
           ADD AG7TOU                      TO AGTTOU
           ADD AG8TOU                      TO AGTTOU
           ADD AG9TOU                      TO AGTTOU
           IF  (I-23)
               ADD AGXTOU                  TO AGTTOU
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE PARAMETER PERIODER.                    * *
      *****************************************************************
           END-IF
           .
 
       KTOKURI-GET SECTION.
       KTOKURI-GET-P.
           IF  KTOKURI-EOF-OFF
               READ KTOKURI
               AT END
                   SET KTOKURI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KTOKURI-FLDOFF SECTION.
       KTOKURI-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-16                TO TRUE
           END-EVALUATE.
 
       KTOKURI-FLDSET SECTION.
       KTOKURI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KTOKURI-IO-AREA (1:2)  TO RECART (1:2)
               MOVE KTOKURI-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KTOKURI-IO-AREA (6:1)  TO KRESGR (1:1)
               MOVE KTOKURI-IO-AREA (6:6)  TO RESKNR (1:6)
               IF  RESKNR = SPACES
                   SET I-16                TO TRUE
               END-IF
               MOVE KTOKURI-IO-AREA (18:2) TO TRAKOD (1:2)
               MOVE KTOKURI-IO-AREA (20:6) TO BILDAT-IO
               INSPECT BILDAT-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (26:6) TO BILNR-IO
               INSPECT BILNR-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (12:6) TO REFNR-IO
               INSPECT REFNR-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (33:6) TO FFDATO-IO
               INSPECT FFDATO-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (39:9) TO TBELOP-IO
               INSPECT TBELOP-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (38:10) TO SBELOP-IO
               INSPECT SBELOP-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (49:2) TO BETM (1:2)
               MOVE KTOKURI-IO-AREA (51:10) TO VBEL-IO
               INSPECT VBEL-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (50:11) TO SVBEL-IO
               INSPECT SVBEL-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (32:1) TO BILART (1:1)
               MOVE KTOKURI-IO-AREA (70:1) TO VT (1:1)
               MOVE KTOKURI-IO-AREA (82:2) TO KBARH (1:2)
               MOVE KTOKURI-IO-AREA (84:6) TO KPER (1:6)
               MOVE KTOKURI-IO-AREA (84:4) TO KAAR (1:4)
               MOVE KTOKURI-IO-AREA (90:24) TO TEKST (1:24)
               MOVE KTOKURI-IO-AREA (114:7) TO BELU-IO
               MOVE KTOKURI-IO-AREA (121:8) TO VALBEU-IO
               MOVE KTOKURI-IO-AREA (129:3) TO VTU (1:3)
               MOVE KTOKURI-IO-AREA (188:4) TO OPPHAV (1:4)
               MOVE KTOKURI-IO-AREA (192:5) TO PRDDTO-IO
               MOVE KTOKURI-IO-AREA (197:4) TO PRDKLK-IO
           END-EVALUATE.
 
       KTOKURI-IDSET SECTION.
       KTOKURI-IDSET-P.
           SET I-01                        TO TRUE.
 
       KTOKURI-CHK-LEVEL SECTION.
       KTOKURI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KTOKURI-LEVEL-01
               MOVE KTOKURI-IO-AREA (3:3)  TO KTOKURI-01-L3-FIRMA
               MOVE KTOKURI-IO-AREA (6:6)  TO KTOKURI-01-L2-RESKNR
               MOVE KTOKURI-IO-AREA (1:2)  TO KTOKURI-01-L1-RECART
               IF  KTOKURI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KTOKURI-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  KTOKURI-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KTOKURI-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KTOKURI-01-L3         TO THE-PRIOR-L3
               MOVE  KTOKURI-01-L2         TO THE-PRIOR-L2
               MOVE  KTOKURI-01-L1         TO THE-PRIOR-L1
               SET KTOKURI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KTOKURI-MATCH-SET SECTION.
       KTOKURI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KTOKURI-IO-AREA (3:3)  TO KTOKURI-M-01-M2-FIRMA
               MOVE KTOKURI-IO-AREA (6:6)  TO KTOKURI-M-01-M1-RESKNR
           END-EVALUATE.
 
       RESKHIS-GET SECTION.
       RESKHIS-GET-P.
           IF  NOT-I-U1
               SET RESKHIS-EOF             TO TRUE
               SET RESKHIS-READ-OFF        TO TRUE
               SUBTRACT 1                FROM LR-CHECK
           END-IF
           IF  RESKHIS-EOF-OFF
               READ RESKHIS
               AT END
                   SET RESKHIS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKHIS-FLDOFF SECTION.
       RESKHIS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-19                TO TRUE
               SET NOT-I-17                TO TRUE
               SET NOT-I-18                TO TRUE
           END-EVALUATE.
 
       RESKHIS-FLDSET SECTION.
       RESKHIS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKHIS-IO-AREA (1:120) TO REC120 (1:120)
               MOVE RESKHIS-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE RESKHIS-IO-AREA (4:6)  TO RESKNR (1:6)
               MOVE RESKHIS-IO-AREA (4:1)  TO RRESGR (1:1)
               MOVE RESKHIS-IO-AREA (19:4) TO RBILD-IO
               MOVE RESKHIS-IO-AREA (23:4) TO RBILNR-IO
               MOVE RESKHIS-IO-AREA (35:6) TO RBEL-IO
               MOVE RESKHIS-IO-AREA (57:2) TO RBARH (1:2)
               IF  RBARH = SPACES
                   SET I-19                TO TRUE
               END-IF
               MOVE RESKHIS-IO-AREA (59:6) TO RPER (1:6)
               MOVE RESKHIS-IO-AREA (59:2) TO RARH (1:2)
               IF  RARH = SPACES
                   SET I-17                TO TRUE
               END-IF
               MOVE RESKHIS-IO-AREA (59:4) TO RAAR (1:4)
               MOVE RESKHIS-IO-AREA (61:2) TO RAAR2 (1:2)
               IF  RAAR2 = SPACES
                   SET I-18                TO TRUE
               END-IF
           END-EVALUATE.
 
       RESKHIS-IDSET SECTION.
       RESKHIS-IDSET-P.
           SET I-04                        TO TRUE.
 
       RESKHIS-CHK-LEVEL SECTION.
       RESKHIS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKHIS-LEVEL-04
               MOVE RESKHIS-IO-AREA (1:3)  TO RESKHIS-04-L3-FIRMA
               MOVE RESKHIS-IO-AREA (4:6)  TO RESKHIS-04-L2-RESKNR
               IF  RESKHIS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKHIS-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESKHIS-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  RESKHIS-04-L3         TO THE-PRIOR-L3
               MOVE  RESKHIS-04-L2         TO THE-PRIOR-L2
               SET RESKHIS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RESKHIS-MATCH-SET SECTION.
       RESKHIS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKHIS-IO-AREA (1:3)  TO RESKHIS-M-04-M2-FIRMA
               MOVE RESKHIS-IO-AREA (4:6)  TO RESKHIS-M-04-M1-RESKNR
           END-EVALUATE.
 
       AUTOPAR-FLDOFF SECTION.
       AUTOPAR-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (12:6) TO PARPER (1:6)
               IF  PARPER = SPACES
                   SET I-15                TO TRUE
               END-IF
               MOVE AUTOPAR-IO-AREA (12:4) TO PARAAR (1:4)
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
       LISTE1-PRINT-LINE SECTION.
       LISTE1-PRINT-LINE-P.
           IF  LISTE1-BEFORE-SKIP > 0
               PERFORM LISTE1-SKIP-BEFORE
           END-IF
           IF  LISTE1-BEFORE-SPACE > 0
               PERFORM LISTE1-SPACE-BEFORE
               IF  LISTE1-AFTER-SKIP > 0
                   PERFORM LISTE1-SKIP-AFTER
               END-IF
               IF  LISTE1-AFTER-SPACE > 0
                   PERFORM LISTE1-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE1-AFTER-SKIP > 0
                   PERFORM LISTE1-SKIP-AFTER
               END-IF
               PERFORM LISTE1-SPACE-AFTER
           END-IF
           IF  LISTE1-LINE-COUNT NOT < LISTE1-MAX-LINES
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
           END-IF.
 
       LISTE1-SKIP-BEFORE SECTION.
       LISTE1-SKIP-BEFORE-P.
           WRITE LISTE1-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE1-LINE-COUNT
           MOVE 0                          TO LISTE1-BEFORE-SKIP
           INITIALIZE LISTE1-IO-AREA.
 
       LISTE1-SPACE-BEFORE SECTION.
       LISTE1-SPACE-BEFORE-P.
           WRITE LISTE1-IO-PRINT        AFTER LISTE1-BEFORE-SPACE LINES
           ADD LISTE1-BEFORE-SPACE         TO LISTE1-LINE-COUNT
           MOVE SPACES TO LISTE1-IO-AREA
           INITIALIZE LISTE1-IO-AREA
           MOVE 0                          TO LISTE1-BEFORE-SPACE.
 
       LISTE1-SKIP-AFTER SECTION.
       LISTE1-SKIP-AFTER-P.
           WRITE LISTE1-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE1-LINE-COUNT
           MOVE 0                          TO LISTE1-AFTER-SKIP
           INITIALIZE LISTE1-IO-AREA.
 
       LISTE1-SPACE-AFTER SECTION.
       LISTE1-SPACE-AFTER-P.
           WRITE LISTE1-IO-PRINT       BEFORE LISTE1-AFTER-SPACE LINES
           ADD LISTE1-AFTER-SPACE          TO LISTE1-LINE-COUNT
           INITIALIZE LISTE1-IO-AREA
           MOVE 0                          TO LISTE1-AFTER-SPACE.
 
       LISTE2-PRINT-LINE SECTION.
       LISTE2-PRINT-LINE-P.
           IF  LISTE2-BEFORE-SKIP > 0
               PERFORM LISTE2-SKIP-BEFORE
           END-IF
           IF  LISTE2-BEFORE-SPACE > 0
               PERFORM LISTE2-SPACE-BEFORE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               IF  LISTE2-AFTER-SPACE > 0
                   PERFORM LISTE2-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               PERFORM LISTE2-SPACE-AFTER
           END-IF
           IF  LISTE2-LINE-COUNT NOT < LISTE2-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       LISTE2-SKIP-BEFORE SECTION.
       LISTE2-SKIP-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-BEFORE-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-BEFORE SECTION.
       LISTE2-SPACE-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER LISTE2-BEFORE-SPACE LINES
           ADD LISTE2-BEFORE-SPACE         TO LISTE2-LINE-COUNT
           MOVE SPACES TO LISTE2-IO-AREA
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-BEFORE-SPACE.
 
       LISTE2-SKIP-AFTER SECTION.
       LISTE2-SKIP-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-AFTER-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-AFTER SECTION.
       LISTE2-SPACE-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE LISTE2-AFTER-SPACE LINES
           ADD LISTE2-AFTER-SPACE          TO LISTE2-LINE-COUNT
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  KTOKURI-EOF
               MOVE HIGH-VALUES            TO KTOKURI-MC
                                              KTOKURI-MP
           END-IF
           IF  RESKHIS-EOF
               MOVE HIGH-VALUES            TO RESKHIS-MC
                                              RESKHIS-MP
           END-IF
           IF  KTOKURI-MC < KTOKURI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESKHIS-MC < RESKHIS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KTOKURI-MC < RESKHIS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KTOKURI-PROCESS     TO TRUE
                   MOVE KTOKURI-MC         TO KTOKURI-MP
                   IF  KTOKURI-MC = RESKHIS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESKHIS-MC < KTOKURI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKHIS-PROCESS     TO TRUE
                   MOVE RESKHIS-MC         TO RESKHIS-MP
                   IF  RESKHIS-MC = KTOKURI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KTOKURI-MC = RESKHIS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KTOKURI-PROCESS     TO TRUE
                   MOVE KTOKURI-MC         TO KTOKURI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-10 AND NOT-I-98)
           AND (NOT-I-14)
               MOVE SPACES TO KTOKURO-IO-AREA
               INITIALIZE KTOKURO-IO-AREA
               MOVE FIRMA                  TO KTOKURO-IO-AREA (1:3)
               MOVE RESKNR                 TO KTOKURO-IO-AREA (4:6)
               MOVE NULL7-IO               TO KTOKURO-IO-AREA (10:7)
               MOVE TRAKOD                 TO KTOKURO-IO-AREA (17:2)
               MOVE BILDAT                 TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (19:4)
               MOVE BILNR                  TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (23:4)
               MOVE REFNR                  TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (27:4)
               MOVE FFDATO                 TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (31:4)
               MOVE HJTBEL                 TO XO-92P
               MOVE XO-92P-EF              TO KTOKURO-IO-AREA (35:6)
               INITIALIZE HJTBEL
               MOVE BETM                   TO KTOKURO-IO-AREA (41:2)
               MOVE VBEL                   TO XO-82P
               MOVE XO-82P-EF              TO KTOKURO-IO-AREA (42:6)
               MOVE VT                     TO KTOKURO-IO-AREA (48:1)
               MOVE BILART                 TO KTOKURO-IO-AREA (49:1)
               MOVE BILARH                 TO KTOKURO-IO-AREA (57:2)
               MOVE REGPER                 TO KTOKURO-IO-AREA (59:6)
               MOVE TEKST                  TO KTOKURO-IO-AREA (65:24)
               INITIALIZE TEKST
               MOVE BELU                   TO XO-112P
               MOVE XO-112P-EF             TO KTOKURO-IO-AREA (89:7)
               MOVE VALBEU                 TO XO-114P
               MOVE XO-114P-EF             TO KTOKURO-IO-AREA (96:8)
               MOVE VTU                    TO KTOKURO-IO-AREA (104:3)
               MOVE OPPHAV                 TO KTOKURO-IO-AREA (108:4)
               MOVE PRDDTO                 TO XO-90P
               MOVE XO-90P-EF              TO KTOKURO-IO-AREA (112:5)
               MOVE PRDKLK                 TO XO-70P
               MOVE XO-70P-EF              TO KTOKURO-IO-AREA (117:4)
               WRITE KTOKURO-IO-AREA
           END-IF
           IF  (I-04 AND NOT-I-14 AND NOT-I-98)
               MOVE SPACES TO KTOKURO-IO-AREA
               INITIALIZE KTOKURO-IO-AREA
               MOVE REC120                 TO KTOKURO-IO-AREA (1:120)
               MOVE BILARH                 TO KTOKURO-IO-AREA (57:2)
               MOVE REGPER                 TO KTOKURO-IO-AREA (59:6)
      *
      *****************************************************************
      * SKRIVER SALDORECORD PR RESKONTRONR.                           *
      *****************************************************************
      *
               WRITE KTOKURO-IO-AREA
           END-IF
           IF  (I-01 AND I-14)
           OR  (I-04 AND I-14)
           OR  (I-01 AND I-16 AND NOT-I-22)
           OR  (I-01 AND I-22 AND NOT-I-16)
           OR  (I-04 AND I-22)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'FIR'                  TO LISTE1-IO-AREA (1:3)
               MOVE ' R.NR '               TO LISTE1-IO-AREA (5:6)
               MOVE ' BILDTO '             TO LISTE1-IO-AREA (12:8)
               MOVE ' B.NR '               TO LISTE1-IO-AREA (21:6)
               MOVE 'BELØP '               TO LISTE1-IO-AREA (36:6)
               MOVE 'BÅ'                   TO LISTE1-IO-AREA (43:2)
               MOVE ' PER '                TO LISTE1-IO-AREA (47:5)
               MOVE 'RA'                   TO LISTE1-IO-AREA (53:2)
               MOVE 'ANMERKNING     '      TO LISTE1-IO-AREA (56:15)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FIRMA                  TO LISTE1-IO-AREA (1:3)
               MOVE RESKNR                 TO LISTE1-IO-AREA (5:6)
               IF  (I-01)
                   MOVE BILDAT             TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO LISTE1-IO-AREA (12:8)
               END-IF
               IF  (I-04)
                   MOVE RBILD              TO XO-70U
                   MOVE XO-70U (1:7)       TO LISTE1-IO-AREA (13:7)
               END-IF
               IF  (I-01)
                   MOVE BILNR-IO           TO LISTE1-IO-AREA (21:6)
               END-IF
               IF  (I-04)
                   IF RBILNR < 0
                     MOVE RBILNR           TO XO-70D
                     MOVE XO-70D (1:7)     TO LISTE1-IO-AREA (20:7)
                   ELSE
                     MOVE RBILNR           TO XO-70U
                     MOVE XO-70U (1:7)     TO LISTE1-IO-AREA (20:7)
                   END-IF
               END-IF
               IF  (I-01 AND I-10)
                   MOVE TBELOP             TO XO-72YY9R
                   MOVE XO-72YY9R          TO LISTE1-IO-AREA (29:13)
               END-IF
               IF  (I-01 AND I-11)
                   MOVE SBELOP             TO XO-82YY9R
                   MOVE XO-82YY9R          TO LISTE1-IO-AREA (28:14)
               END-IF
               IF  (I-04)
                   MOVE RBEL               TO XO-92YY9R
                   MOVE XO-92YY9R          TO LISTE1-IO-AREA (27:15)
               END-IF
               IF  (I-01)
                   MOVE KBARH              TO LISTE1-IO-AREA (43:2)
               END-IF
               IF  (I-04)
                   MOVE RBARH              TO LISTE1-IO-AREA (43:2)
               END-IF
               IF  (I-01)
                   MOVE KPER               TO LISTE1-IO-AREA (46:6)
               END-IF
               IF  (I-04)
                   MOVE RPER               TO LISTE1-IO-AREA (46:6)
               END-IF
               IF  (I-01)
                   MOVE RECART             TO LISTE1-IO-AREA (53:2)
               END-IF
               IF  (I-01)
                   MOVE 'RESKNR NY      '  TO LISTE1-IO-AREA (56:15)
               END-IF
               IF  (I-04)
                   MOVE 'RESKNR GML     '  TO LISTE1-IO-AREA (56:15)
               END-IF
               IF  (I-14)
                   MOVE 'AVVIST RECORD  '  TO LISTE1-IO-AREA (56:15)
               END-IF
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO805 ' TO LISTE1-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE1-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (52:8)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 3                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               IF  (I-U1)
                   MOVE 'KONTOKURANT ON-LINE/VSAM' TO LISTE1-IO-AREA
                                                               (25:24)
               END-IF
               IF  (I-U1)
                   MOVE ' (RESKHIS) DANNET AV GÅR' TO LISTE1-IO-AREA
                                                               (49:24)
               END-IF
               IF  (I-U1)
                   MOVE 'DAGENS + DAGENS TRANSER.' TO LISTE1-IO-AREA
                                                               (73:24)
               END-IF
               IF  (NOT-I-U1)
                   MOVE 'KONTOKURANT ON-LINE/VSAM' TO LISTE1-IO-AREA
                                                               (25:24)
               END-IF
               IF  (NOT-I-U1)
                   MOVE ' (RESKHIS) DANNET FRA SC' TO LISTE1-IO-AREA
                                                               (49:24)
               END-IF
               IF  (NOT-I-U1)
                   MOVE 'RATCH/KONTOKURANT SEKV. ' TO LISTE1-IO-AREA
                                                               (73:24)
               END-IF
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (73:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'FIRMA'                TO LISTE1-IO-AREA (1:5)
               MOVE '    SALDO 31.12'      TO LISTE1-IO-AREA (16:15)
               MOVE '    HITTIL I ÅR'      TO LISTE1-IO-AREA (36:15)
               MOVE ' SALDO PR I DAG'      TO LISTE1-IO-AREA (56:15)
               MOVE ' ANTALL RECORDS'      TO LISTE1-IO-AREA (76:15)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO805 ' TO LISTE1-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE1-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (52:8)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 3                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               IF  (I-U1)
                   MOVE 'KONTOKURANT ON-LINE/VSAM' TO LISTE1-IO-AREA
                                                               (25:24)
               END-IF
               IF  (I-U1)
                   MOVE ' (RESKHIS) DANNET AV GÅR' TO LISTE1-IO-AREA
                                                               (49:24)
               END-IF
               IF  (I-U1)
                   MOVE 'DAGENS + DAGENS TRANSER.' TO LISTE1-IO-AREA
                                                               (73:24)
               END-IF
               IF  (NOT-I-U1)
                   MOVE 'KONTOKURANT ON-LINE/VSAM' TO LISTE1-IO-AREA
                                                               (25:24)
               END-IF
               IF  (NOT-I-U1)
                   MOVE ' (RESKHIS) DANNET FRA SC' TO LISTE1-IO-AREA
                                                               (49:24)
               END-IF
               IF  (NOT-I-U1)
                   MOVE 'RATCH/KONTOKURANT SEKV. ' TO LISTE1-IO-AREA
                                                               (73:24)
               END-IF
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (73:24)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'FIRMA'                TO LISTE1-IO-AREA (1:5)
               MOVE '    SALDO 31.12'      TO LISTE1-IO-AREA (16:15)
               MOVE '    HITTIL I ÅR'      TO LISTE1-IO-AREA (36:15)
               MOVE ' SALDO PR I DAG'      TO LISTE1-IO-AREA (56:15)
               MOVE ' ANTALL RECORDS'      TO LISTE1-IO-AREA (76:15)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-11 AND NOT-I-98)
           AND (NOT-I-14)
               MOVE SPACES TO KTOKURO-IO-AREA
               INITIALIZE KTOKURO-IO-AREA
               MOVE FIRMA                  TO KTOKURO-IO-AREA (1:3)
               MOVE RESKNR                 TO KTOKURO-IO-AREA (4:6)
               MOVE NULL7-IO               TO KTOKURO-IO-AREA (10:7)
               MOVE TRAKOD                 TO KTOKURO-IO-AREA (17:2)
               MOVE BILDAT                 TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (19:4)
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (23:4)
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (27:4)
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (31:4)
               MOVE L1SBEL                 TO XO-92P
               MOVE XO-92P-EF              TO KTOKURO-IO-AREA (35:6)
               INITIALIZE L1SBEL
               MOVE BETM                   TO KTOKURO-IO-AREA (41:2)
               MOVE L1VBEL                 TO XO-92P
               MOVE XO-92P-EF              TO KTOKURO-IO-AREA (42:6)
               INITIALIZE L1VBEL
               MOVE SVT                    TO KTOKURO-IO-AREA (48:1)
               MOVE BILART                 TO KTOKURO-IO-AREA (49:1)
               MOVE BILARH                 TO KTOKURO-IO-AREA (57:2)
               MOVE REGPER                 TO KTOKURO-IO-AREA (59:6)
               MOVE 'SALDO'                TO KTOKURO-IO-AREA (65:5)
               MOVE L1SBEU                 TO XO-112P
               MOVE XO-112P-EF             TO KTOKURO-IO-AREA (89:7)
               MOVE L1VBEU                 TO XO-114P
               MOVE XO-114P-EF             TO KTOKURO-IO-AREA (96:8)
               MOVE VTU                    TO KTOKURO-IO-AREA (104:3)
      *                        OPPHAV   111
               MOVE 'SALD'                 TO KTOKURO-IO-AREA (108:4)
               MOVE DDTO                   TO XO-80P
               MOVE XO-80P-EF              TO KTOKURO-IO-AREA (112:5)
               MOVE DKLK                   TO XO-60P
               MOVE XO-60P-EF              TO KTOKURO-IO-AREA (117:4)
               WRITE KTOKURO-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE DDTO-IO                TO AVSTEMM-IO-AREA (1:8)
               MOVE '090'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO805'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO805*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE DKLK-IO                TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDTO-IO                TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '090'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO805*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE DKLK-IO                TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDTO-IO                TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
           END-IF
           IF  (I-L3)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FIRMA                  TO LISTE1-IO-AREA (2:3)
               IF  (I-98)
                   MOVE '====>'            TO LISTE1-IO-AREA (6:5)
               END-IF
               MOVE BL3FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BL3HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BL3TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AL3TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               IF  (I-98)
                   MOVE '=> SLETTET'       TO LISTE1-IO-AREA (121:10)
               END-IF
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '*********************' TO LISTE1-IO-AREA (52:21)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SUM      '            TO LISTE1-IO-AREA (1:9)
               MOVE BLRFJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BLRHIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE '*'                    TO LISTE1-IO-AREA (53:1)
               MOVE BLRTOT                 TO EDIT-BLRTOT
               MOVE EDIT-BLRTOT            TO LISTE1-IO-AREA (58:13)
               INITIALIZE BLRTOT
               MOVE '*'                    TO LISTE1-IO-AREA (72:1)
               MOVE ALRTOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '*********************' TO LISTE1-IO-AREA (52:21)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '>>>>>>>>>>>>>>          ' TO LISTE1-IO-AREA (1:24)
               MOVE 'AVSTEMMES MOT JOBB DOP12' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE 'UD PROGRAM RSK040.      ' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '         <<<<<<<<<<<<<<<' TO LISTE1-IO-AREA
                                                               (73:24)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND NOT-I-U1)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '>>>>>>>>>>>>>>          ' TO LISTE1-IO-AREA (1:24)
               MOVE 'AVSTEMMES MOT JOBB RES50' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE 'AM PROGRAM RSK227 PR 31.' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '12.      <<<<<<<<<<<<<<<' TO LISTE1-IO-AREA
                                                               (73:24)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'AVSTEMMING PROG. RSK805 ' TO LISTE1-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE1-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (52:8)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 3                      TO LISTE1-BEFORE-SPACE
               MOVE 3                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SUM PR RESKONTROGRUPPE ' TO LISTE1-IO-AREA (38:23)
               MOVE 2                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'RESK. GRP'            TO LISTE1-IO-AREA (1:9)
               MOVE ' SALDO PR 31.12'      TO LISTE1-IO-AREA (16:15)
               MOVE '    HITTIL I ÅR'      TO LISTE1-IO-AREA (36:15)
               MOVE ' SALDO PR I DAG'      TO LISTE1-IO-AREA (56:15)
               MOVE ' ANTALL RECORDS'      TO LISTE1-IO-AREA (76:15)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '0'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG0FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG0HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG0TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG0TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '1'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG1FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG1HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG1TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG1TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '2'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG2FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG2HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG2TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG2TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '3'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG3FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG3HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG3TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG3TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '4'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG4FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG4HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG4TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG4TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '5'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG5FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG5HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG5TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG5TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '6'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG6FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG6HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG6TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG6TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '7'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG7FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG7HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG7TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG7TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '8'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG8FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG8HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG8TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG8TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '9'                    TO LISTE1-IO-AREA (8:1)
               MOVE BG9FJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BG9HIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BG9TOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AG9TOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-23)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'FEIL GRP'             TO LISTE1-IO-AREA (1:8)
               MOVE BGXFJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BGXHIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE BGXTOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE AGXTOT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (80:11)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '*********************' TO LISTE1-IO-AREA (51:21)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'TOTALT'               TO LISTE1-IO-AREA (1:6)
               MOVE BGTFJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (16:15)
               MOVE BGTHIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (36:15)
               MOVE '*'                    TO LISTE1-IO-AREA (51:1)
               MOVE BGTTOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE1-IO-AREA (56:15)
               MOVE '*'                    TO LISTE1-IO-AREA (71:1)
               MOVE AGTTOT                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE1-IO-AREA (79:12)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'UTVIDET BEL'          TO LISTE1-IO-AREA (1:11)
               MOVE BGTFJU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE1-IO-AREA (13:18)
               MOVE BGTHIU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE1-IO-AREA (33:18)
               MOVE '*'                    TO LISTE1-IO-AREA (51:1)
               MOVE BGTTOU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE1-IO-AREA (53:18)
               MOVE '*'                    TO LISTE1-IO-AREA (71:1)
               MOVE AGTTOU                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE1-IO-AREA (79:12)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '*********************' TO LISTE1-IO-AREA (51:21)
               MOVE 1                      TO LISTE1-AFTER-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND NOT-I-U1)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '>>>>>>>>>>>>>>          ' TO LISTE1-IO-AREA (1:24)
               MOVE 'AVSTEMMES MOT JOBB RES50' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE 'AM PROGRAM RSK227 PR 31.' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '12.      <<<<<<<<<<<<<<<' TO LISTE1-IO-AREA
                                                               (73:24)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '>>>>>>>>>>>>>>          ' TO LISTE1-IO-AREA (1:24)
               MOVE 'AVSTEMMES MOT JOBB XXXXX' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE 'XX PROGRAM RSKXXX (MÅNED' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE 'LIG).    <<<<<<<<<<<<<<<' TO LISTE1-IO-AREA
                                                               (73:24)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-30)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '>>>>>>>>>>>>>>>>>>>>>>  ' TO LISTE1-IO-AREA (1:24)
               MOVE 'REGNSKAPSPARAMETER MANGL' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE 'ER / ER IKKE NUMERISK.  ' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '  <<<<<<<<<<<<<<<<<<<<<<' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO805 ' TO LISTE2-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE2-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (52:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*********************' TO LISTE2-IO-AREA (51:21)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TOTAL'                TO LISTE2-IO-AREA (2:5)
               MOVE BGTFJO                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE2-IO-AREA (16:15)
               MOVE BGTHIA-ELG             TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE2-IO-AREA (36:15)
               MOVE '*'                    TO LISTE2-IO-AREA (51:1)
               MOVE BGTTOT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE2-IO-AREA (56:15)
               MOVE '*'                    TO LISTE2-IO-AREA (71:1)
               MOVE AGTTOT                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE2-IO-AREA (79:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'UTVIDET BEL'          TO LISTE2-IO-AREA (1:11)
               MOVE BGTFJU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE2-IO-AREA (13:18)
               MOVE BGTHIU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE2-IO-AREA (33:18)
               MOVE '*'                    TO LISTE2-IO-AREA (51:1)
               MOVE BGTTOU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE2-IO-AREA (53:18)
               MOVE '*'                    TO LISTE2-IO-AREA (71:1)
               MOVE AGTTOU                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE2-IO-AREA (79:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*********************' TO LISTE2-IO-AREA (51:21)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND NOT-I-U1)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '>>>>>>>>>>>>>>          ' TO LISTE2-IO-AREA (1:24)
               MOVE 'AVSTEMMES MOT JOBB RES50' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 'AM PROGRAM RSK227 PR 31.' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '12.      <<<<<<<<<<<<<<<' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '>>>>>>>>>>>>>>          ' TO LISTE2-IO-AREA (1:24)
               MOVE 'AVSTEMMES MOT JOBB XXXXX' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 'XX PROGRAM RSKXXX (MÅNED' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE 'LIG).    <<<<<<<<<<<<<<<' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-30)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '>>>>>>>>>>>>>>>>>>>>>>  ' TO LISTE2-IO-AREA (1:24)
               MOVE 'REGNSKAPSPARAMETER MANGL' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 'ER / ER IKKE NUMERISK.  ' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '  <<<<<<<<<<<<<<<<<<<<<<' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       TOTAL-OVERFLOW SECTION.
       TOTAL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO805 ' TO LISTE2-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE2-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (52:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
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
           SET KTOKURI-LEVEL-INIT          TO TRUE
           INITIALIZE KTOKURI-DATA-FIELDS
           SET KTOKURI-EOF-OFF             TO TRUE
           SET KTOKURI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KTOKURI-MC
                                              KTOKURI-MP
           OPEN INPUT KTOKURI
           SET RESKHIS-LEVEL-INIT          TO TRUE
           INITIALIZE RESKHIS-DATA-FIELDS
           SET RESKHIS-EOF-OFF             TO TRUE
           SET RESKHIS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESKHIS-MC
                                              RESKHIS-MP
           IF I-U1
               OPEN INPUT RESKHIS
           END-IF
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT KTOKURO
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE1
           INITIALIZE LISTE1-IO-AREA
           INITIALIZE LISTE1-DATA-FIELDS
           MOVE 57                         TO LISTE1-MAX-LINES
           OPEN OUTPUT LISTE2
           INITIALIZE LISTE2-IO-AREA
           INITIALIZE LISTE2-DATA-FIELDS
           MOVE 57                         TO LISTE2-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KTOKURI
           IF I-U1
               CLOSE RESKHIS
           END-IF
           CLOSE AUTOPAR
           CLOSE FIRMAF
           CLOSE KTOKURO
           CLOSE AVSTEMM
           IF LISTE1-IO-AREA NOT = SPACES
             WRITE LISTE1-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE1-IO-AREA
           END-IF
           CLOSE LISTE1
           IF LISTE2-IO-AREA NOT = SPACES
             WRITE LISTE2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE2-IO-AREA
           END-IF
           CLOSE LISTE2.
 
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
